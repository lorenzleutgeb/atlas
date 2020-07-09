package xyz.leutgeb.lorenz.lac.ast;

import static java.util.Optional.empty;
import static java.util.stream.Collectors.joining;
import static xyz.leutgeb.lorenz.lac.Util.flatten;

import java.io.IOException;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.util.*;
import java.util.function.Consumer;
import java.util.function.Predicate;
import java.util.stream.Collectors;
import lombok.Getter;
import lombok.extern.slf4j.Slf4j;
import xyz.leutgeb.lorenz.lac.Util;
import xyz.leutgeb.lorenz.lac.typing.resources.AnnotatingGlobals;
import xyz.leutgeb.lorenz.lac.typing.resources.FunctionAnnotation;
import xyz.leutgeb.lorenz.lac.typing.resources.coefficients.Coefficient;
import xyz.leutgeb.lorenz.lac.typing.resources.coefficients.KnownCoefficient;
import xyz.leutgeb.lorenz.lac.typing.resources.constraints.Constraint;
import xyz.leutgeb.lorenz.lac.typing.resources.heuristics.SmartRangeHeuristic;
import xyz.leutgeb.lorenz.lac.typing.resources.proving.Prover;
import xyz.leutgeb.lorenz.lac.typing.resources.solving.ConstraintSystemSolver;
import xyz.leutgeb.lorenz.lac.typing.simple.TypeError;
import xyz.leutgeb.lorenz.lac.unification.Equivalence;
import xyz.leutgeb.lorenz.lac.unification.UnificationContext;
import xyz.leutgeb.lorenz.lac.unification.UnificationError;

@Slf4j
public class Program {
  @Getter private final Map<String, FunctionDefinition> functionDefinitions;

  // Note that this could also be List<Set<...>> since the order of nodes within the same
  // SCC doesn't really matter. However we chose to still sort them to get consistent outputs
  // without resorting in multiple places.
  private final List<List<String>> order;
  private final String name;
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

  public Optional<Map<String, FunctionAnnotation>> solve() {
    return solve(new HashMap<>(), new HashSet<>());
  }

  public Optional<Map<String, FunctionAnnotation>> solve(
      Map<String, FunctionAnnotation> functionAnnotations) {
    return solve(functionAnnotations, new HashSet<>());
  }

  public Optional<Map<String, FunctionAnnotation>> solve(
      Map<String, FunctionAnnotation> functionAnnotations, Set<Constraint> outsideConstraints) {
    final var costFreeFunctionAnnotations = new HashMap<String, FunctionAnnotation>();
    final var heuristic = SmartRangeHeuristic.DEFAULT;
    final var prover = new Prover(name, null, basePath);

    // SCCs are split by ";" and function names within SCCs are split by ",".
    // That's reasonably readable without brackes for sets/sequences.
    final var namesAsSet =
        order.stream()
            .map(x -> x.stream().sorted().map(Object::toString).collect(Collectors.joining(", ")))
            .collect(Collectors.joining("; "));

    if (functionDefinitions.values().stream()
        .map(FunctionDefinition::runaway)
        .anyMatch(Predicate.not(Set::isEmpty))) {
      log.info(namesAsSet + " | UNSAT");
      return Optional.empty();
    }

    forEach(fd -> fd.stubAnnotations(functionAnnotations, costFreeFunctionAnnotations, heuristic));

    forEach(
        fd -> {
          final var globals =
              new AnnotatingGlobals(
                  functionAnnotations,
                  costFreeFunctionAnnotations,
                  fd.getSizeAnalysis(),
                  heuristic);
          prover.setGlobals(globals);
          prover.prove(fd.getTypingObligation(1));
        });

    try {
      prover.plot();
    } catch (IOException e) {
      e.printStackTrace();
    }

    final var accumulatedConstraints = prover.getAccumulatedConstraints();
    accumulatedConstraints.addAll(outsideConstraints);
    log.info(accumulatedConstraints.size() + " constraints accumulated");

    // This is the entrypoint of new-style solving. We get a bunch of constraints
    // that need to be fulfilled in order to typecheck the program.
    Optional<Map<Coefficient, KnownCoefficient>> solution =
        ConstraintSystemSolver.solve(accumulatedConstraints, name);

    if (accumulatedConstraints.size() < 50) {
      try {
        Constraint.plot(name, accumulatedConstraints, basePath);
      } catch (IOException e) {
        e.printStackTrace();
      }
    }

    if (solution.isPresent()) {
      forEach(fd -> fd.substitute(solution.get()));
      if (!(functionDefinitions.size() == 1)) {
        log.info(namesAsSet + ":");
      }
      for (var group : order) {
        for (var fqn : group) {
          log.info(functionDefinitions.get(fqn).getAnnotationString());
        }
      }
      return Optional.of(functionAnnotations);
    } else {
      log.info(namesAsSet + " | UNSAT");
      return empty();
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

  private void forEach(Consumer<FunctionDefinition> f) {
    functionDefinitions.values().forEach(f);
  }
}
