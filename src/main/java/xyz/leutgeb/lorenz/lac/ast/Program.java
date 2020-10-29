package xyz.leutgeb.lorenz.lac.ast;

import static java.util.stream.Collectors.joining;
import static xyz.leutgeb.lorenz.lac.util.Util.flatten;
import static xyz.leutgeb.lorenz.lac.util.Util.output;
import static xyz.leutgeb.lorenz.lac.util.Util.randomHex;

import com.google.common.collect.ArrayListMultimap;
import com.google.common.collect.Multimap;
import java.io.IOException;
import java.io.PrintStream;
import java.nio.file.Path;
import java.util.Collections;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Optional;
import java.util.Set;
import java.util.function.BiFunction;
import java.util.function.Consumer;
import java.util.function.Predicate;
import java.util.stream.Collectors;
import lombok.Getter;
import lombok.Setter;
import lombok.extern.slf4j.Slf4j;
import xyz.leutgeb.lorenz.lac.typing.resources.AnnotatingGlobals;
import xyz.leutgeb.lorenz.lac.typing.resources.CombinedFunctionAnnotation;
import xyz.leutgeb.lorenz.lac.typing.resources.FunctionAnnotation;
import xyz.leutgeb.lorenz.lac.typing.resources.coefficients.Coefficient;
import xyz.leutgeb.lorenz.lac.typing.resources.coefficients.KnownCoefficient;
import xyz.leutgeb.lorenz.lac.typing.resources.constraints.Constraint;
import xyz.leutgeb.lorenz.lac.typing.resources.heuristics.SmartRangeHeuristic;
import xyz.leutgeb.lorenz.lac.typing.resources.proving.Obligation;
import xyz.leutgeb.lorenz.lac.typing.resources.proving.Prover;
import xyz.leutgeb.lorenz.lac.typing.resources.solving.ConstraintSystemSolver;
import xyz.leutgeb.lorenz.lac.typing.simple.TypeError;
import xyz.leutgeb.lorenz.lac.unification.Equivalence;
import xyz.leutgeb.lorenz.lac.unification.UnificationContext;
import xyz.leutgeb.lorenz.lac.unification.UnificationError;
import xyz.leutgeb.lorenz.lac.util.Util;

@Slf4j
public class Program {
  @Getter private final Map<String, FunctionDefinition> functionDefinitions;

  // Note that this could also be List<Set<...>> since the order of nodes within the same
  // SCC doesn't really matter. However we chose to still sort them to get consistent outputs
  // without resorting in multiple places.
  @Getter private final List<List<String>> order;

  @Getter @Setter private String name;
  @Getter private final Path basePath;

  private boolean normalized;
  private boolean inferred;

  public Program(
      Map<String, FunctionDefinition> functionDefinitions,
      List<List<String>> order,
      Path basePath) {
    this.functionDefinitions = functionDefinitions;
    this.order = order;
    this.name = flatten(this.order).stream().map(Util::fqnToFlatFilename).collect(joining("+"));
    this.basePath = basePath;
  }

  public void infer() throws UnificationError, TypeError {
    normalize();

    if (inferred) {
      return;
    }

    var root = UnificationContext.root();
    for (List<String> component : order) {
      final var ctx = root.childWithNewProblem();
      for (var fqn : component) {
        var fd = get(fqn);
        ctx.putSignature(fd.getFullyQualifiedName(), fd.stubSignature(ctx));
      }

      for (var fqn : component) {
        var fd = get(fqn);
        fd.infer(ctx);
      }

      var solution = Equivalence.solve(ctx.getEquivalences());
      for (var fqn : component) {
        var fd = get(fqn);
        fd.resolve(solution, ctx.getSignatures().get(fqn));
        ctx.putSignature(fqn, fd.getInferredSignature());
      }
    }

    inferred = true;
  }

  private FunctionDefinition get(String fqn) {
    return functionDefinitions.get(fqn);
  }

  public Optional<Map<Coefficient, KnownCoefficient>> solve() {
    return solve(
        new HashMap<>(),
        new HashSet<>(),
        (program, constraints) -> ConstraintSystemSolver.solve(constraints, basePath));
  }

  public Optional<Map<Coefficient, KnownCoefficient>> solve(
      Map<String, CombinedFunctionAnnotation> functionAnnotations) {
    return solve(
        functionAnnotations,
        new HashSet<>(),
        (program, constraints) -> ConstraintSystemSolver.solve(constraints, basePath));
  }

  public Optional<Map<Coefficient, KnownCoefficient>> solve(
      Map<String, CombinedFunctionAnnotation> functionAnnotations,
      Set<Constraint> outsideConstraints) {
    return solve(
        functionAnnotations,
        outsideConstraints,
        (program, constraints) -> ConstraintSystemSolver.solve(constraints, basePath));
  }

  public Optional<Prover> prove(
      Map<String, CombinedFunctionAnnotation> functionAnnotations, boolean infer) {
    return proveWithTactics(functionAnnotations, Collections.emptyMap(), infer);
  }

  public Optional<Prover> proveWithTactics(
      Map<String, CombinedFunctionAnnotation> functionAnnotations,
      Map<String, Path> tactics,
      boolean infer) {
    final var heuristic = SmartRangeHeuristic.DEFAULT;
    final var prover = new Prover(name + randomHex(), null, basePath);

    if (functionDefinitions.values().stream()
        .map(FunctionDefinition::runaway)
        .anyMatch(Predicate.not(Set::isEmpty))) {
      return Optional.empty();
    }

    final var called = calledFunctionNames();
    forEach(
        fd ->
            fd.stubAnnotations(
                functionAnnotations,
                heuristic,
                called.contains(fd.getFullyQualifiedName()),
                infer));
    forEach(
        fd -> {

          /*
          if (!functionAnnotations.get(fd.getFullyQualifiedName()).withCost.isUnknown()
              && functionAnnotations.get(fd.getFullyQualifiedName()).withoutCost.stream()
                  .noneMatch(FunctionAnnotation::isUnknown)) {
            log.info(
                "Skipping type inference for {} because all given annotations are known.",
                fd.getFullyQualifiedName());
            return;
          }
          */
          /*
                 if (!tactics.containsKey(fd.getFullyQualifiedName())) {
                   log.info(
                       "Skipping type inference for {} and assuming {} is correct, because no tactics were provided.",
                       fd.getFullyQualifiedName(),
                       fd.getAnnotation());
                   return;
                 }
          */

          final var globals =
              new AnnotatingGlobals(functionAnnotations, fd.getSizeAnalysis(), heuristic);
          prover.setGlobals(globals);

          Obligation typingObligation = fd.getTypingObligation(1);

          if (tactics.containsKey(fd.getFullyQualifiedName())) {
            try {
              prover.read(typingObligation, tactics.get(fd.getFullyQualifiedName()));
            } catch (IOException e) {
              throw new RuntimeException(e);
            }
          } else {
            prover.setWeakenAggressively(true);
            prover.prove(typingObligation);
            prover.setWeakenAggressively(false);
          }

          for (var cfAnnotation : fd.getInferredSignature().getAnnotation().get().withoutCost) {
            if (cfAnnotation.isZero()) {
              log.debug("Skipping {}", cfAnnotation);
              continue;
            }

            /*
            if (!cfAnnotation.isUnknown()) {
              log.info(
                      "Skipping type inference for {} | {} because all given annotations are known.",
                      fd.getFullyQualifiedName(), cfAnnotation);
              return;
            }
             */

            final var cfRoot =
                new Obligation(
                    fd.treeLikeArguments(), cfAnnotation.from, fd.getBody(), cfAnnotation.to, 0);

            if (tactics.containsKey(fd.getFullyQualifiedName())) {
              try {
                log.debug("Using tactic to prove cf typing!");
                prover.read(cfRoot, tactics.get(fd.getFullyQualifiedName()));
              } catch (IOException e) {
                throw new RuntimeException(e);
              }
            } else {
              prover.setWeakenAggressively(true);
              prover.prove(cfRoot);
              prover.setWeakenAggressively(false);
            }
          }
        });

    return Optional.of(prover);
  }

  public Optional<Map<Coefficient, KnownCoefficient>> solve(
      Map<String, CombinedFunctionAnnotation> functionAnnotations,
      Set<Constraint> outsideConstraints,
      BiFunction<Program, Set<Constraint>, Optional<Map<Coefficient, KnownCoefficient>>> solving) {

    final var optionalProver = prove(functionAnnotations, true);
    if (optionalProver.isEmpty()) {
      return Optional.empty();
    }
    final var prover = optionalProver.get();
    final var accumulatedConstraints = prover.getAccumulatedConstraints();
    accumulatedConstraints.addAll(outsideConstraints);

    prover.plot();

    // This is the entrypoint of new-style solving. We get a bunch of constraints
    // that need to be fulfilled in order to typecheck the program.
    Optional<Map<Coefficient, KnownCoefficient>> solution =
        solving.apply(this, accumulatedConstraints);

    if (accumulatedConstraints.size() < 50) {
      Constraint.plot(name, accumulatedConstraints, basePath);
    }

    if (solution.isPresent()) {
      prover.plotWithSolution(solution.get());
    }

    return solution;
  }

  public void ingest(Optional<Map<Coefficient, KnownCoefficient>> solution) {
    if (solution.isPresent()) {
      forEach(fd -> fd.substitute(solution.get()));
      if (!(functionDefinitions.size() == 1)) {
        // log.info(namesAsSet() + ":");
      }
      for (var group : order) {
        for (var fqn : group) {
          // log.info(functionDefinitions.get(fqn).getAnnotationString());
        }
      }
    } else {
      // log.info(namesAsSet() + " | UNSAT");
    }
  }

  @Deprecated
  public void mockIngest(Optional<Map<Coefficient, KnownCoefficient>> solution) {
    if (solution.isPresent()) {
      if (!(functionDefinitions.size() == 1)) {
        log.info(namesAsSet() + ":");
      }
      for (var group : order) {
        for (var fqn : group) {
          log.info(
              functionDefinitions
                  .get(fqn)
                  .getMockedAnnotationString(
                      new CombinedFunctionAnnotation(
                          new FunctionAnnotation(
                              functionDefinitions
                                  .get(fqn)
                                  .getInferredSignature()
                                  .getAnnotation()
                                  .get()
                                  .withCost
                                  .from
                                  .substitute(solution.get()),
                              functionDefinitions
                                  .get(fqn)
                                  .getInferredSignature()
                                  .getAnnotation()
                                  .get()
                                  .withCost
                                  .to
                                  .substitute(solution.get())),
                          functionDefinitions
                              .get(fqn)
                              .getInferredSignature()
                              .getAnnotation()
                              .get()
                              .withoutCost
                              .stream()
                              .map(annotation -> annotation.substitute(solution.get()))
                              .collect(Collectors.toSet()))));
        }
      }
    } else {
      log.info(namesAsSet() + " | UNSAT");
    }
  }

  public void normalize() {
    if (normalized) {
      return;
    }
    forEach(FunctionDefinition::normalize);

    normalized = true;
  }

  public void unshare() {
    unshare(Expression.DEFAULT_LAZY);
  }

  public void unshare(boolean lazy) {
    forEach(x -> x.unshare(lazy));
  }

  public void analyzeSizes() {
    forEach(FunctionDefinition::analyzeSizes);
  }

  private void forEach(Consumer<FunctionDefinition> f) {
    functionDefinitions.values().forEach(f);
  }

  private String namesAsSet() {
    // SCCs are split by ";" and function names within SCCs are split by ",".
    // That's reasonably readable without brackets for sets/sequences.
    return order.stream()
        .map(x -> x.stream().sorted().map(Object::toString).collect(Collectors.joining(", ")))
        .collect(Collectors.joining("; "));
  }

  private Set<String> calledFunctionNames() {
    final var result = new HashSet<String>();
    forEach(fd -> result.addAll(fd.getOcurringFunctions()));
    return result;
  }

  public void printAllSimpleSignaturesInOrder(PrintStream out) {
    for (int i = 0; i < order.size(); i++) {
      final var stratum = order.get(i);
      for (var fqn : stratum) {
        FunctionDefinition fd = functionDefinitions.get(fqn);
        out.println(fd.getSimpleSignatureString());
      }
    }
  }

  public void printAllAnnotatedSignaturesInOrder(PrintStream out) {
    for (int i = 0; i < order.size(); i++) {
      final var stratum = order.get(i);
      for (var fqn : stratum) {
        FunctionDefinition fd = functionDefinitions.get(fqn);
        out.println(fd.getAnnotatedSignatureString());
      }
    }
  }

  public void printAllInferredSignaturesInOrder(PrintStream out) {
    for (int i = 0; i < order.size(); i++) {
      final var stratum = order.get(i);
      for (var fqn : stratum) {
        FunctionDefinition fd = functionDefinitions.get(fqn);
        out.println(fd.getInferredSignatureString());
      }
    }
  }

  public void dumpToJsh(Path path) {
    try {
      infer();
    } catch (UnificationError | TypeError unificationError) {
      throw new RuntimeException(unificationError);
    }

    Multimap<String, FunctionDefinition> output = ArrayListMultimap.create();
    for (var entry : getFunctionDefinitions().entrySet()) {
      var fd = entry.getValue();
      output.put(fd.getModuleName(), fd);
    }

    try (final var stream = new PrintStream(output(path))) {
      stream.println("import java.util.Objects;");
      stream.println("import xyz.leutgeb.lorenz.lac.Tree;");
      stream.println("import static xyz.leutgeb.lorenz.lac.Tree.node;");
      stream.println("import static xyz.leutgeb.lorenz.lac.Tree.leaf;");

      for (var e : output.keySet()) {
        var lastModulePart = e.substring(Math.max(0, e.lastIndexOf(".")));

        stream.println("// This file was generated automatically.");
        final boolean toplevel = "_".equals(lastModulePart);

        if (!toplevel) {
          stream.println("class " + lastModulePart + " {");
          stream.println();
        }

        for (var fd : output.get(e)) {
          fd.printJavaTo(stream, !toplevel);
        }

        if (!toplevel) {
          stream.println("}");
        }
      }
    } catch (IOException ex) {
      throw new RuntimeException(ex);
    }
  }
}
