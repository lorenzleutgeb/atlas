package xyz.leutgeb.lorenz.lac.commands;

import static java.util.Collections.emptyList;
import static java.util.Collections.emptySet;
import static picocli.CommandLine.Help.Visibility.ALWAYS;
import static xyz.leutgeb.lorenz.lac.util.Util.output;

import com.google.common.collect.ArrayListMultimap;
import com.google.common.collect.HashMultimap;
import com.google.common.collect.Multimap;
import jakarta.json.Json;
import jakarta.json.JsonObjectBuilder;
import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.time.Duration;
import java.time.Instant;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Optional;
import java.util.regex.Pattern;
import java.util.stream.Collectors;
import lombok.extern.slf4j.Slf4j;
import picocli.CommandLine;
import xyz.leutgeb.lorenz.lac.ast.FunctionDefinition;
import xyz.leutgeb.lorenz.lac.ast.Program;
import xyz.leutgeb.lorenz.lac.module.Loader;
import xyz.leutgeb.lorenz.lac.typing.resources.optimiziation.Optimization;
import xyz.leutgeb.lorenz.lac.typing.resources.proving.Prover;
import xyz.leutgeb.lorenz.lac.typing.resources.solving.ConstraintSystemSolver;
import xyz.leutgeb.lorenz.lac.typing.simple.TypeError;
import xyz.leutgeb.lorenz.lac.unification.UnificationError;

@CommandLine.Command(name = "run")
@Slf4j
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
      defaultValue = "false",
      names = "--infer",
      description =
          "When present cost annotations in the program source are ignored, instead a new typing is computed.")
  private Boolean infer;

  @CommandLine.Option(
      defaultValue = "false",
      names = "--relax-right",
      description =
          "When present relaxes constraints that force the rank coefficient of result to equal the rank coefficient of the input. Only works on functions that take exactly one tree and return a tree.")
  private Boolean relaxRight;

  @CommandLine.Option(names = "--name", description = "Name of the run.")
  private String name;

  @CommandLine.Option(
      names = "--tactics",
      description = "When present, tactics will be loaded from this directory.")
  private Path tactics;

  @CommandLine.Option(
      names = "--json",
      paramLabel = "FILE",
      description = "If present, detailled output in JSON format will be written to this file.")
  private Path json;

  @CommandLine.Spec(CommandLine.Spec.Target.SELF)
  private CommandLine.Model.CommandSpec selfSpec;

  @Override
  public void run() {
    final var start = Instant.now();
    Loader loader = Loader.atDefaultHome();
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

    if (program.isEmpty()) {
      log.error("Program to analyze is empty, nothing to do!");
      System.exit(3);
    }

    program.normalize();

    try {
      program.infer();
    } catch (UnificationError | TypeError unificationError) {
      throw new RuntimeException(unificationError);
    }
    program.analyzeSizes();
    // log.info("Loaded definitions:");
    // program.printAllSimpleSignaturesInOrder(System.out);
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

    log.info("Output will go to {}", program.getBasePath().toAbsolutePath());

    Map<String, Path> tacticsMap = new HashMap<>();

    // log.info(infer ? "Given for comparison:" : "Will check following types:");

    for (int i = 0; i < program.getOrder().size(); i++) {
      final var stratum = program.getOrder().get(i);
      for (var fqn : stratum) {
        FunctionDefinition fd = program.getFunctionDefinitions().get(fqn);

        if (tactics != null) {
          var path =
              tactics.resolve(fd.getModuleName().replace(".", "/") + "/" + fd.getName() + ".txt");
          if (Files.exists(path) && Files.isReadable(path)) {
            tacticsMap.put(fd.getFullyQualifiedName(), path);
          }
        }

        log.info(fd.getAnnotatedSignatureString());

        log.info("\tDependencies: " + fd.getOcurringFunctionsNonRecursive());
        log.info("\tSource:       " + fd.getBody().getSource().getRoot());

        if (tactics != null) {
          final var p = tacticsMap.get(fd.getFullyQualifiedName());
          if (p == null) {
            log.info("\tTactic:       n/a (will use automatic proof generation)");
          } else {
            log.info("\tTactic:       " + p.toAbsolutePath());
          }
        }
      }
    }

    log.info("Generating constraints...");
    Optional<Prover> optionalProver = program.proveWithTactics(new HashMap<>(), tacticsMap, infer);

    ConstraintSystemSolver.Result result = ConstraintSystemSolver.Result.unknown();

    if (optionalProver.isEmpty()) {
      result = ConstraintSystemSolver.Result.unsat();
      log.info("Nonterminating function definition detected. Aborting.");
    } else {
      final var prover = optionalProver.get();

      if (infer) {
        var multiTarget =
            Optimization.combine(
                program, program.getRoots(), Optimization::squareWeightedComponentWiseDifference);

        log.info("Solving constraints...");
        result = prover.solve(multiTarget.constraints, List.of(multiTarget.target), "min");
      } else {
        log.info("Solving constraints...");
        result = prover.solve(emptySet(), emptyList(), "sat");
      }

      log.info("Done. Result(s): ");
      if (!result.hasSolution()) {
        log.info(result.getStatus().toString());
      }
      program.ingest(result.getSolution());
      program.printAllInferredSignaturesInOrder(System.out);
    }

    final var stop = Instant.now();

    if (json != null) {
      JsonObjectBuilder builder = Json.createObjectBuilder();

      builder.add("result", program.inferredSignaturesToJson());
      builder.add("duration", Json.createValue(Duration.between(start, stop).toString()));

      JsonObjectBuilder z3ObjectBuilder = Json.createObjectBuilder();
      z3ObjectBuilder.add("status", Json.createValue(result.getStatus().toString()));

      JsonObjectBuilder z3StatisticsBuilder = Json.createObjectBuilder();
      result.getStatistics().forEach(z3StatisticsBuilder::add);
      z3ObjectBuilder.add("statistics", z3StatisticsBuilder.build());

      if (result.getSmtFile().isPresent()) {
        z3ObjectBuilder.add("file", Json.createValue(result.getSmtFile().get().toString()));
      }

      builder.add("z3", z3ObjectBuilder.build());

      log.info("Writing JSON output to {}", json);
      try (final var out = output(json)) {
        Json.createWriter(out).writeObject(builder.build());
      } catch (IOException ioException) {
        log.error("Failed to write JSON output.", ioException);
      }
    }

    System.exit(result.toExitCode());
  }
}
