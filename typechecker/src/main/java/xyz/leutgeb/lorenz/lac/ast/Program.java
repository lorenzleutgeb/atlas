package xyz.leutgeb.lorenz.lac.ast;

import static java.util.stream.Collectors.joining;
import static xyz.leutgeb.lorenz.lac.util.Util.flatten;
import static xyz.leutgeb.lorenz.lac.util.Util.randomHex;

import java.io.IOException;
import java.io.PrintStream;
import java.nio.file.Path;
import java.nio.file.Paths;
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
  private final List<List<String>> order;

  @Getter @Setter private String name;
  private final Path basePath;

  public Program(Map<String, FunctionDefinition> functionDefinitions, List<List<String>> order) {
    this.functionDefinitions = functionDefinitions;
    this.order = order;
    this.name = flatten(this.order).stream().map(Util::fqnToFlatFilename).collect(joining("+"));
    this.basePath = Paths.get(".", "out");
  }

  public void infer() throws UnificationError, TypeError {
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
  }

  private FunctionDefinition get(String fqn) {
    return functionDefinitions.get(fqn);
  }

  public Optional<Map<Coefficient, KnownCoefficient>> solve() {
    return solve(
        new HashMap<>(),
        new HashSet<>(),
        (program, constraints) -> ConstraintSystemSolver.solve(constraints, name));
  }

  public Optional<Map<Coefficient, KnownCoefficient>> solve(
      Map<String, CombinedFunctionAnnotation> functionAnnotations) {
    return solve(
        functionAnnotations,
        new HashSet<>(),
        (program, constraints) -> ConstraintSystemSolver.solve(constraints, name));
  }

  public Optional<Map<Coefficient, KnownCoefficient>> solve(
      Map<String, CombinedFunctionAnnotation> functionAnnotations,
      Set<Constraint> outsideConstraints) {
    return solve(
        functionAnnotations,
        outsideConstraints,
        (program, constraints) -> ConstraintSystemSolver.solve(constraints, name));
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
      log.info(namesAsSet() + " | UNSAT");
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
          if (!functionAnnotations.get(fd.getFullyQualifiedName()).withCost().isUnknown()
              && functionAnnotations.get(fd.getFullyQualifiedName()).withoutCost().stream()
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

          for (var cfAnnotation : fd.getCfAnnotations()) {
            /*
            if (cfAnnotation.from().coefficientsEqual(Annotation.zero(cfAnnotation.from().size())) &&
            cfAnnotation.to().coefficientsEqual(Annotation.zero(cfAnnotation.to().size()))) {
            	log.info("Skipping {}", cfAnnotation);
              continue;
            }
            */

            // prover.setWeakenAggressively(true);

            final var cfRoot =
                new Obligation(
                    fd.treeLikeArguments(),
                    cfAnnotation.from(),
                    fd.getBody(),
                    cfAnnotation.to(),
                    0);

            /*
            prover.prove(
                    );
             */
            // prover.setWeakenAggressively(false);

            if (false && tactics.containsKey(fd.getFullyQualifiedName())) {
              try {
                log.info("Using tactic to prove cf typing!");
                prover.read(cfRoot, tactics.get(fd.getFullyQualifiedName()));
              } catch (IOException e) {
                throw new RuntimeException(e);
              }
            } else {
              prover.prove(cfRoot);
            }
          }
        });

    /*
    try {
      prover.plot();
    } catch (IOException e) {
      e.printStackTrace();
    }
     */

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

    try {
      prover.plot();
    } catch (IOException e) {
      e.printStackTrace();
    }

    // This is the entrypoint of new-style solving. We get a bunch of constraints
    // that need to be fulfilled in order to typecheck the program.
    Optional<Map<Coefficient, KnownCoefficient>> solution =
        solving.apply(this, accumulatedConstraints);

    if (accumulatedConstraints.size() < 50) {
      try {
        Constraint.plot(name, accumulatedConstraints, basePath);
      } catch (IOException e) {
        // Not critical...
        e.printStackTrace();
      }
    }

    if (solution.isPresent()) {
      try {
        prover.plotWithSolution(solution.get());
      } catch (IOException e) {
        // Not critical...
        e.printStackTrace();
      }
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
                                  .from()
                                  .substitute(solution.get()),
                              functionDefinitions
                                  .get(fqn)
                                  .getInferredSignature()
                                  .getAnnotation()
                                  .get()
                                  .to()
                                  .substitute(solution.get())),
                          functionDefinitions.get(fqn).getCfAnnotations().stream()
                              .map(annotation -> annotation.substitute(solution.get()))
                              .collect(Collectors.toSet()))));
        }
      }
    } else {
      log.info(namesAsSet() + " | UNSAT");
    }
  }

  public void normalize() {
    forEach(FunctionDefinition::normalize);
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
      final var multiple = stratum.size() > 1;

      if (multiple) {
        out.println(stratum.stream().collect(joining(", ")) + ":");
      }

      for (var fqn : stratum) {
        FunctionDefinition fd = functionDefinitions.get(fqn);
        out.println(
            (multiple ? "\t" : "")
                + fd.getFullyQualifiedName()
                + " ∷ "
                + fd.getInferredSignature());
      }
    }
  }

  public void printAllAnnotatedSignaturesInOrder(PrintStream out) {
    for (int i = 0; i < order.size(); i++) {
      final var stratum = order.get(i);

      for (var fqn : stratum) {
        FunctionDefinition fd = functionDefinitions.get(fqn);
        if (fd.getInferredSignature().getAnnotation().isPresent()) {
          out.println(fd.getFullyQualifiedName() + " ∷ " + fd.getAnnotatedSignature().fullGlory());
        }
      }
    }
  }

  public void printAllInferredSignaturesInOrder(PrintStream out) {
    for (int i = 0; i < order.size(); i++) {
      final var stratum = order.get(i);

      for (var fqn : stratum) {
        FunctionDefinition fd = functionDefinitions.get(fqn);
        if (fd.getInferredSignature().getAnnotation().isPresent()) {
          out.println(fd.getFullyQualifiedName() + " ∷ " + fd.getInferredSignature().fullGlory());
        } else {
          out.println(fd.getFullyQualifiedName() + " ∷ ?");
        }
      }
    }
  }
}
