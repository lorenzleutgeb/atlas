package xyz.leutgeb.lorenz.lac.commands;

import static com.google.common.collect.Sets.union;
import static com.ibm.icu.impl.Assert.fail;
import static java.util.Collections.emptyList;
import static picocli.CommandLine.Help.Visibility.ALWAYS;
import static xyz.leutgeb.lorenz.lac.typing.resources.coefficients.KnownCoefficient.ONE;
import static xyz.leutgeb.lorenz.lac.util.Util.append;

import com.google.common.collect.ArrayListMultimap;
import com.google.common.collect.HashMultimap;
import com.google.common.collect.Multimap;
import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Optional;
import java.util.Set;
import java.util.regex.Pattern;
import java.util.stream.Collectors;
import picocli.CommandLine;
import xyz.leutgeb.lorenz.lac.ast.FunctionDefinition;
import xyz.leutgeb.lorenz.lac.ast.Program;
import xyz.leutgeb.lorenz.lac.module.Loader;
import xyz.leutgeb.lorenz.lac.typing.resources.FunctionAnnotation;
import xyz.leutgeb.lorenz.lac.typing.resources.coefficients.Coefficient;
import xyz.leutgeb.lorenz.lac.typing.resources.coefficients.KnownCoefficient;
import xyz.leutgeb.lorenz.lac.typing.resources.coefficients.UnknownCoefficient;
import xyz.leutgeb.lorenz.lac.typing.resources.constraints.Constraint;
import xyz.leutgeb.lorenz.lac.typing.resources.constraints.LessThanOrEqualConstraint;
import xyz.leutgeb.lorenz.lac.typing.resources.optimiziation.Optimization;
import xyz.leutgeb.lorenz.lac.typing.resources.proving.Prover;
import xyz.leutgeb.lorenz.lac.typing.resources.solving.ConstraintSystemSolver;
import xyz.leutgeb.lorenz.lac.typing.simple.TypeError;
import xyz.leutgeb.lorenz.lac.unification.UnificationError;

@CommandLine.Command(name = "run")
public class Run implements Runnable {
  @CommandLine.Parameters(
      index = "0",
      arity = "1",
      paramLabel = "pattern",
      description =
          "Regular expression to select fully qualified names of functions to be checked. For example, to select all functions whose names begin with \"a\" and contain \"b\" inside a module that has a name ending in \"c\", use \".*c\\.a.*b.*\" (think carefully about escaping \"\\\").",
      defaultValue = ".*",
      showDefaultValue = ALWAYS)
  private Pattern pattern;

  @CommandLine.Option(
      defaultValue = ".",
      names = "--home",
      description = "Where to search for *.ml files containing function definitions.")
  private Path home;

  @CommandLine.Option(
      defaultValue = "false",
      names = "--infer",
      description =
          "When present cost annotations in the program source are ignored, instead a new typing is computed.")
  private Boolean infer;

  @CommandLine.Option(
      defaultValue = "false",
      names = "--relax-rank",
      description =
          "When present relaxes constraints that force the rank coefficient of result annotations to be non-zero.")
  private Boolean relaxRank;

  @CommandLine.Option(
      defaultValue = "false",
      names = "--rational",
      description =
          "When present, coefficients will not be searched over the integers but over the rationals.")
  private Boolean rational;

  @CommandLine.Option(names = "--name", description = "Name of the run.")
  private String name;

  @CommandLine.Option(
      names = "--tactics",
      description = "When present, tactics will be loaded from this directory.")
  private Path tactics;

  @Override
  public void run() {
    Loader loader = new Loader(home);
    try {
      loader.autoload();
    } catch (IOException e) {
      throw new RuntimeException(e);
    }
    Program program;
    try {
      program = loader.loadMatching(pattern);
    } catch (IOException e) {
      throw new RuntimeException(e);
    }
    if (name != null && !name.isBlank()) {
      program.setName(name);
    }
    program.normalize();
    try {
      program.infer();
    } catch (UnificationError | TypeError unificationError) {
      throw new RuntimeException(unificationError);
    }
    System.out.println("Loaded definitions:");
    program.printAllSimpleSignaturesInOrder(System.out);
    Multimap<String, FunctionDefinition> output = ArrayListMultimap.create();
    Multimap<String, String> imports = HashMultimap.create();
    for (var entry : program.getFunctionDefinitions().entrySet()) {
      var fd = entry.getValue();
      if (pattern.asMatchPredicate().test(fd.getFullyQualifiedName())) {
        output.put(fd.getModuleName(), fd);
        imports.putAll(
            fd.getModuleName(),
            fd.importedFunctions().stream().map(Loader::moduleName).collect(Collectors.toSet()));
      }
    }

    Map<String, Path> tacticsMap = new HashMap<>();
    if (tactics != null) {
      for (var entry : program.getFunctionDefinitions().entrySet()) {
        var fd = entry.getValue();

        var path =
            tactics.resolve(fd.getModuleName().replace(".", "/") + "/" + fd.getName() + ".txt");

        System.out.println(path);
        if (Files.exists(path) && Files.isReadable(path)) {
          System.out.println("Using tactic " + path);
          tacticsMap.put(fd.getFullyQualifiedName(), path);
        } else {
          System.out.println("No tactic for " + fd.getFullyQualifiedName());
        }
      }
    }

    Optional<Prover> optionalProver = program.proveWithTactics(new HashMap<>(), tacticsMap, infer);

    if (optionalProver.isEmpty()) {
      System.out.println("UNSAT");
      System.exit(1);
      return;
    }

    final var prover = optionalProver.get();

    final Set<Constraint> outsideConstraints = new HashSet<>();

    for (final var fqn : program.getFunctionDefinitions().keySet()) {
      final var fd = program.getFunctionDefinitions().get(fqn);
      if (fd.getInferredSignature().getAnnotation().get().to.size() == 1 && !relaxRank) {
        outsideConstraints.add(
            new LessThanOrEqualConstraint(
                ONE,
                fd.getInferredSignature().getAnnotation().get().to.getRankCoefficient(),
                "(outside) force rank"));
      }
    }

    // TODO: Autodetect rational domain in case we find rational annotation.
    ConstraintSystemSolver.Domain domain =
        rational ? ConstraintSystemSolver.Domain.RATIONAL : ConstraintSystemSolver.Domain.INTEGER;

    Optional<Map<Coefficient, KnownCoefficient>> solution = Optional.empty();

    if (infer) {

      final List<UnknownCoefficient> setCountingRankCoefficients = new ArrayList<>();
      final List<UnknownCoefficient> setCountingNonRankCoefficients = new ArrayList<>();

      final List<UnknownCoefficient> pairwiseDiffRankCoefficients = new ArrayList<>();
      final List<UnknownCoefficient> pairwiseDiffNonRankCoefficients = new ArrayList<>();

      final Set<Constraint> setCountingConstraints = new HashSet<>();
      final Set<Constraint> pairwiseDiffConstraints = new HashSet<>();

      /*
      ConstraintSystemSolver.Domain domain =
              annotations.values().stream().noneMatch(CombinedFunctionAnnotation::isNonInteger)
                      ? ConstraintSystemSolver.Domain.INTEGER
                      : ConstraintSystemSolver.Domain.RATIONAL;
       */

      for (final var fqn : program.getFunctionDefinitions().keySet()) {
        if (!program.getFunctionDefinitions().containsKey(fqn)) {
          fail("Could not find function definition for '" + fqn + "'.");
        }

        final var fd = program.getFunctionDefinitions().get(fqn);

        FunctionAnnotation inferredAnnotation = fd.getInferredSignature().getAnnotation().get();
        final var setCounting = Optimization.setCounting(inferredAnnotation);
        if (setCounting.isPresent()) {
          setCountingRankCoefficients.addAll(setCounting.get().rankCoefficients);
          setCountingNonRankCoefficients.addAll(setCounting.get().nonRankCoefficients);
          setCountingConstraints.addAll(setCounting.get().constraints);
        }

        final var pairwiseDiff = Optimization.pairwiseDiff(inferredAnnotation);
        if (pairwiseDiff.isPresent()) {
          pairwiseDiffRankCoefficients.addAll(pairwiseDiff.get().rankCoefficients);
          pairwiseDiffNonRankCoefficients.addAll(pairwiseDiff.get().nonRankCoefficients);
          pairwiseDiffConstraints.addAll(pairwiseDiff.get().constraints);
        }
      }

      final var minimizationConstraints =
          union(union(setCountingConstraints, pairwiseDiffConstraints), outsideConstraints);

      final var minimizationTargets =
          append(
              append(pairwiseDiffRankCoefficients, setCountingRankCoefficients),
              append(pairwiseDiffNonRankCoefficients, setCountingNonRankCoefficients));

      /*
      final var minimizationTargets =
              append(
                      append(setCountingRankCoefficients, pairwiseDiffRankCoefficients),
                      append(setCountingNonRankCoefficients, pairwiseDiffNonRankCoefficients));
       */

      solution = prover.solve(minimizationConstraints, minimizationTargets, "min", domain);
    } else {
      solution = prover.solve(outsideConstraints, emptyList(), "sat", domain);
    }

    if (infer) {
      System.out.println("Given for comparison: ");
      program.printAllAnnotatedSignaturesInOrder(System.out);
      System.out.println();
    }

    System.out.println(infer ? "Inferred results:" : "Checked results:");
    if (solution.isEmpty()) {
      System.out.println("UNSAT");
      System.exit(1);
      return;
    }
    program.ingest(solution);
    program.printAllInferredSignaturesInOrder(System.out);
  }
}
