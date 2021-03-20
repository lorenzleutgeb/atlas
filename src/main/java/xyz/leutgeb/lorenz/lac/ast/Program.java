package xyz.leutgeb.lorenz.lac.ast;

import com.google.common.collect.ArrayListMultimap;
import com.google.common.collect.Multimap;
import com.microsoft.z3.Status;
import jakarta.json.Json;
import jakarta.json.JsonArray;
import lombok.Getter;
import lombok.Setter;
import lombok.extern.slf4j.Slf4j;
import org.jgrapht.Graph;
import org.jgrapht.graph.DefaultEdge;
import org.jgrapht.traverse.TopologicalOrderIterator;
import xyz.leutgeb.lorenz.lac.typing.resources.AnnotatingGlobals;
import xyz.leutgeb.lorenz.lac.typing.resources.Annotation;
import xyz.leutgeb.lorenz.lac.typing.resources.CombinedFunctionAnnotation;
import xyz.leutgeb.lorenz.lac.typing.resources.constraints.Constraint;
import xyz.leutgeb.lorenz.lac.typing.resources.constraints.EqualityConstraint;
import xyz.leutgeb.lorenz.lac.typing.resources.heuristics.SmartRangeHeuristic;
import xyz.leutgeb.lorenz.lac.typing.resources.optimiziation.Optimization;
import xyz.leutgeb.lorenz.lac.typing.resources.proving.Obligation;
import xyz.leutgeb.lorenz.lac.typing.resources.proving.Prover;
import xyz.leutgeb.lorenz.lac.typing.resources.solving.ConstraintSystemSolver;
import xyz.leutgeb.lorenz.lac.typing.simple.TypeError;
import xyz.leutgeb.lorenz.lac.unification.Equivalence;
import xyz.leutgeb.lorenz.lac.unification.UnificationContext;
import xyz.leutgeb.lorenz.lac.unification.UnificationError;
import xyz.leutgeb.lorenz.lac.util.Scheduler;
import xyz.leutgeb.lorenz.lac.util.Util;

import java.io.IOException;
import java.io.PrintStream;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Optional;
import java.util.Set;
import java.util.Spliterators;
import java.util.concurrent.TimeUnit;
import java.util.function.Consumer;
import java.util.function.Predicate;
import java.util.stream.Collectors;
import java.util.stream.StreamSupport;

import static java.util.Collections.emptyMap;
import static java.util.Collections.synchronizedMap;
import static java.util.Collections.unmodifiableSet;
import static java.util.Optional.empty;
import static java.util.stream.Collectors.joining;
import static xyz.leutgeb.lorenz.lac.util.Util.bug;
import static xyz.leutgeb.lorenz.lac.util.Util.flatten;
import static xyz.leutgeb.lorenz.lac.util.Util.output;
import static xyz.leutgeb.lorenz.lac.util.Util.randomHex;

@Slf4j
public class Program {
  @Getter private final Map<String, FunctionDefinition> functionDefinitions;

  // Note that this could also be List<Set<...>> since the order of nodes within the same
  // SCC doesn't really matter. However we chose to still sort them to get consistent outputs
  // without resorting in multiple places.
  @Getter private final List<List<String>> order;
  private final Graph<Graph<String, DefaultEdge>, DefaultEdge> condensation;

  @Getter @Setter private String name;
  @Getter private final Path basePath;
  @Getter private final Set<String> roots;

  private boolean normalized;
  private boolean inferred;

  public Program(
      Map<String, FunctionDefinition> functionDefinitions,
      Path basePath,
      Set<String> roots,
      Graph<Graph<String, DefaultEdge>, DefaultEdge> condensation) {
    this.functionDefinitions = functionDefinitions;
    this.condensation = condensation;

    this.order =
        StreamSupport.stream(
                Spliterators.spliteratorUnknownSize(
                    new TopologicalOrderIterator<>(condensation), 0),
                false)
            .map(Graph::vertexSet)
            .map(List::copyOf)
            .collect(Collectors.toList());

    this.name = flatten(this.order).stream().map(Util::fqnToFlatFilename).collect(joining("+"));
    this.basePath = basePath;
    this.roots = unmodifiableSet(roots);
  }

  public void infer() throws UnificationError, TypeError {
    normalize();

    if (isEmpty()) {
      inferred = true;
    }

    if (inferred) {
      return;
    }

    inferParallel();

    inferred = true;
  }

  private void inferParallel() throws UnificationError, TypeError {
    var root = UnificationContext.root();
    var scheduler =
        new Scheduler<Void, Graph<String, DefaultEdge>, DefaultEdge>(
            condensation,
            (alternative) ->
                () -> {
                  try {
                    final var ctx = root.childWithNewProblem();
                    for (var fqn : alternative.vertexSet()) {
                      var fd = get(fqn);
                      ctx.putSignature(fd.getFullyQualifiedName(), fd.stubSignature(ctx));
                    }
                    for (var fqn : alternative.vertexSet()) {
                      var fd = get(fqn);
                      fd.infer(ctx);
                    }
                    var solution = Equivalence.solve(ctx.getEquivalences());
                    for (var fqn : alternative.vertexSet()) {
                      var fd = get(fqn);
                      fd.resolve(solution, ctx.getSignatures().get(fqn));
                      ctx.putSignature(fqn, fd.getInferredSignature());
                    }
                  } catch (UnificationError | TypeError e) {
                    throw new RuntimeException(e);
                  }
                  return null;
                });

    try {
      scheduler.run(8, 1, TimeUnit.MINUTES);
    } catch (InterruptedException e) {
      e.printStackTrace();
    }
  }

  private void inferSequential() throws UnificationError, TypeError {
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

  public ConstraintSystemSolver.Result solve(
      Map<String, CombinedFunctionAnnotation> functionAnnotations,
      Map<String, Path> tactics,
      boolean infer,
      Set<Constraint> externalConstraints
  ) {
    if (functionDefinitions.values().stream()
        .map(FunctionDefinition::runaway)
        .anyMatch(Predicate.not(Set::isEmpty))) {
      return new ConstraintSystemSolver.Result(Status.UNSATISFIABLE, empty(), emptyMap(), empty());
    }

    final var heuristic = SmartRangeHeuristic.DEFAULT;
    final var called = calledFunctionNames();

    final Map<String, Annotation> rightSidesPerModule = synchronizedMap(new HashMap<>());

    final var scheduler =
        new Scheduler<>(
            this.condensation,
            (scc) ->
                () -> {
                  final var prover = new Prover(name + randomHex(), null, basePath);

                  Set<FunctionDefinition> fds =
                      scc.vertexSet().stream()
                          .map(functionDefinitions::get)
                          .collect(Collectors.toSet());

                  // Stub annotations.
                  for (var fd : fds) {
                    fd.stubAnnotations(
                        functionAnnotations,
                        heuristic,
                        called.contains(fd.getFullyQualifiedName()) ? 1 : 0,
                        infer);
                  }

                  Set<Constraint> external = new HashSet<>();
                  external.addAll(externalConstraints);
                  Set<String> refresh = new HashSet<>();

                  // Set right sides equal.
                  for (var fd : fds) {
                    if (!fd.returnsTree()) {
                      continue;
                    }
                    final var module = fd.getModuleName();
                    external.addAll(
                        EqualityConstraint.eq(
                            fd.getInferredSignature().getAnnotation().get().withCost.to,
                            rightSidesPerModule.computeIfAbsent(
                                module,
                                (x) -> {
                                  refresh.add(module);
                                  return SmartRangeHeuristic.DEFAULT.generate(module, 1);
                                }),
                            "(fix) right side for " + fd.getFullyQualifiedName()));
                  }

                  // Solve.
                  for (var fd : fds) {
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
                      prover.prove(typingObligation);
                    }

                    for (var cfAnnotation :
                        fd.getInferredSignature().getAnnotation().get().withoutCost) {
                      if (cfAnnotation.isZero()) {
                        log.debug("Skipping {}", cfAnnotation);
                        continue;
                      }

                      final var cfRoot =
                          new Obligation(
                              fd.treeLikeArguments(),
                              cfAnnotation.from,
                              fd.getBody(),
                              cfAnnotation.to,
                              0,
                              empty());

                      if (tactics.containsKey(fd.getFullyQualifiedName())) {
                        try {
                          log.debug("Using tactic to prove cf typing!");
                          prover.read(cfRoot, tactics.get(fd.getFullyQualifiedName()));
                        } catch (IOException e) {
                          throw new RuntimeException(e);
                        }
                      } else {
                        prover.prove(cfRoot);
                      }
                    }
                  }

                  final var optimization = Optimization.standard(fds);
                  external.addAll(optimization.getConstraints());

                  final var result = prover.solve(external, List.of(optimization.target));
                  if (!result.isSatisfiable()) {
                    return result;
                  }

                  for (var module : refresh) {
                    rightSidesPerModule.computeIfPresent(
                        module, (k, v) -> v.substitute(result.getSolution().get()));
                  }

                  for (var fqn : scc.vertexSet()) {
                    functionAnnotations.put(
                        fqn,
                        get(fqn)
                            .getInferredSignature()
                            .getAnnotation()
                            .get()
                            .substitute(result.getSolution().get()));
                    get(fqn).substitute(result.getSolution().get());

                    printAllInferredSignaturesInOrder(System.out);
                    printAllBoundsInOrder(System.out);
                  }

                  return result;
                });

    Map<Graph<String, DefaultEdge>, Scheduler.Result<ConstraintSystemSolver.Result>> result;
    try {
      result = scheduler.run(8, Integer.MAX_VALUE, TimeUnit.DAYS);
    } catch (InterruptedException e) {
      throw new RuntimeException(e);
    }

    final var aggregate =
        result.values().stream()
            .reduce((a, b) -> Scheduler.Result.merge(a, b, ConstraintSystemSolver.Result::merge));

    if (aggregate.isEmpty()) {
      throw bug("could not aggregate results");
    }

    final var aggregateGet = aggregate.get();

    if (aggregateGet.getExecutionException() != null) {
      throw new RuntimeException(aggregateGet.getExecutionException());
    }

    if (aggregateGet.getCancellationException() != null) {
      throw new RuntimeException(aggregateGet.getCancellationException());
    }

    return aggregateGet.getValue();
  }

  @Deprecated
  public Optional<Prover> proveWithTactics(
      Map<String, CombinedFunctionAnnotation> functionAnnotations,
      Map<String, Path> tactics,
      boolean infer) {
    final var heuristic = SmartRangeHeuristic.DEFAULT;
    final var prover = new Prover(name + randomHex(), null, basePath);

    if (functionDefinitions.values().stream()
        .map(FunctionDefinition::runaway)
        .anyMatch(Predicate.not(Set::isEmpty))) {
      return empty();
    }

    final var called = calledFunctionNames();
    forEach(
        fd ->
            fd.stubAnnotations(
                functionAnnotations,
                heuristic,
                called.contains(fd.getFullyQualifiedName()) ? 3 : 0,
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
            prover.prove(typingObligation);
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
                    fd.treeLikeArguments(),
                    cfAnnotation.from,
                    fd.getBody(),
                    cfAnnotation.to,
                    0,
                    empty());

            if (tactics.containsKey(fd.getFullyQualifiedName())) {
              try {
                log.debug("Using tactic to prove cf typing!");
                prover.read(cfRoot, tactics.get(fd.getFullyQualifiedName()));
              } catch (IOException e) {
                throw new RuntimeException(e);
              }
            } else {
              prover.prove(cfRoot);
            }
          }
        });

    return Optional.of(prover);
  }

  public boolean isEmpty() {
    return order.isEmpty() && functionDefinitions.isEmpty();
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

  public void forEach(Consumer<FunctionDefinition> f) {
    functionDefinitions.values().forEach(f);
  }

  private String namesAsSet() {
    // SCCs are split by ";" and function names within SCCs are split by ",".
    // That's reasonably readable without brackets for sets/sequences.
    return order.stream()
        .map(x -> x.stream().sorted().map(Object::toString).collect(Collectors.joining(", ")))
        .collect(Collectors.joining("; "));
  }

  public Set<String> calledFunctionNames() {
    final var result = new HashSet<String>();
    forEach(fd -> result.addAll(fd.getOcurringFunctions()));
    return result;
  }

  public void printAllSimpleSignaturesInOrder(PrintStream out) {
    for (final List<String> stratum : order) {
      for (var fqn : stratum) {
        FunctionDefinition fd = functionDefinitions.get(fqn);
        out.println(fd.getSimpleSignatureString());
      }
    }
  }

  public void printAllAnnotatedSignaturesInOrder(PrintStream out) {
    for (final List<String> stratum : order) {
      for (var fqn : stratum) {
        FunctionDefinition fd = functionDefinitions.get(fqn);
        out.println(fd.getAnnotatedSignatureString());
      }
    }
  }

  public void printAllBoundsInOrder(PrintStream out) {
    for (final List<String> stratum : order) {
      for (var fqn : stratum) {
        FunctionDefinition fd = functionDefinitions.get(fqn);
        out.println(fd.getBoundString());
      }
    }
  }

  public void printAllInferredSignaturesInOrder(PrintStream out) {
    for (final List<String> stratum : order) {
      for (var fqn : stratum) {
        FunctionDefinition fd = functionDefinitions.get(fqn);
        out.println(fd.getInferredSignatureString());
      }
    }
  }

  public JsonArray inferredSignaturesToJson() {
    var builder = Json.createArrayBuilder();
    for (List<String> stratum : order) {
      final var stratumBuilder = Json.createArrayBuilder();
      for (var fqn : stratum) {
        FunctionDefinition fd = functionDefinitions.get(fqn);
        stratumBuilder.add(fd.inferredSignatureToJson());
      }
      builder.add(stratumBuilder.build());
    }
    return builder.build();
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

  public Map<String, Path> lookupTactics(Map<String, String> tactics, Path base) {
    final var result = new HashMap<String, Path>();

    for (final var entry : functionDefinitions.entrySet()) {
      final Path path;

      if (tactics.containsKey(entry.getKey())) {
        path = base.resolve(Path.of(tactics.get(entry.getKey()) + ".txt"));
      } else {
        path = base.resolve(entry.getValue().tactic());
        if (!Files.exists(path)) {
          continue;
        }
      }

      result.put(entry.getKey(), path);
    }

    return result;
  }

  public Set<Constraint> sameRightSide() {
    return rightSide(SmartRangeHeuristic.DEFAULT.generate("right", 1));
  }

  public Set<Constraint> zeroRightSide() {
    return rightSide(Annotation.zero(1));
  }

  public Set<Constraint> rightSide(Annotation rightSide) {
    final var constraints = new HashSet<Constraint>();
    forEach(
        fd -> {
          final Annotation to = fd.getInferredSignature().getAnnotation().get().withCost.to;
          if (to.size() != 1) {
            return;
          }
          constraints.addAll(EqualityConstraint.eq(to, rightSide, "(fix) Q'"));
        });
    return constraints;
  }
}
