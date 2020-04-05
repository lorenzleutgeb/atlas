package xyz.leutgeb.lorenz.lac.ast;

import static guru.nidi.graphviz.model.Factory.graph;
import static java.util.Optional.empty;

import guru.nidi.graphviz.engine.Engine;
import guru.nidi.graphviz.engine.Format;
import guru.nidi.graphviz.engine.Graphviz;
import guru.nidi.graphviz.model.Graph;
import java.io.IOException;
import java.io.PrintStream;
import java.nio.file.Files;
import java.nio.file.Paths;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Optional;
import java.util.Set;
import java.util.stream.Collectors;
import lombok.Getter;
import org.hipparchus.util.Pair;
import xyz.leutgeb.lorenz.lac.typing.resources.AnnotatingGlobals;
import xyz.leutgeb.lorenz.lac.typing.resources.Annotation;
import xyz.leutgeb.lorenz.lac.typing.resources.coefficients.Coefficient;
import xyz.leutgeb.lorenz.lac.typing.resources.coefficients.KnownCoefficient;
import xyz.leutgeb.lorenz.lac.typing.resources.constraints.Constraint;
import xyz.leutgeb.lorenz.lac.typing.resources.solving.ConstraintSystemException;
import xyz.leutgeb.lorenz.lac.typing.resources.solving.ConstraintSystemSolver;
import xyz.leutgeb.lorenz.lac.typing.resources.solving.ConstraintSystemUnsatisfiableException;
import xyz.leutgeb.lorenz.lac.typing.simple.TypeError;
import xyz.leutgeb.lorenz.lac.unification.Equivalence;
import xyz.leutgeb.lorenz.lac.unification.UnificationContext;
import xyz.leutgeb.lorenz.lac.unification.UnificationError;

public class Program {
  @Getter private final Map<String, FunctionDefinition> functionDefinitions;
  private final List<Set<String>> order;

  public Program(Map<String, FunctionDefinition> functionDefinitions, List<Set<String>> order) {
    // functionDefinitions.forEach(Objects::requireNonNull);
    this.functionDefinitions = functionDefinitions;
    this.order = order;
  }

  public void infer() throws UnificationError, TypeError {
    normalize();
    var root = UnificationContext.root();
    for (Set<String> component : order) {
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

  public Optional<Map<String, Pair<Annotation, Annotation>>> solve()
      throws UnificationError, TypeError, ConstraintSystemException {
    return solve(new HashMap<>(), new HashSet<>());
  }

  public Optional<Map<String, Pair<Annotation, Annotation>>> solve(
      Map<String, Pair<Annotation, Annotation>> functionAnnotations)
      throws UnificationError, TypeError, ConstraintSystemException {
    return solve(functionAnnotations, new HashSet<>());
  }

  public Optional<Map<String, Pair<Annotation, Annotation>>> solve(
      Map<String, Pair<Annotation, Annotation>> functionAnnotations,
      Set<Constraint> outsideConstraints)
      throws UnificationError, TypeError, ConstraintSystemException {
    final var costFreeFunctionAnnotations = new HashMap<String, Pair<Annotation, Annotation>>();
    final var name =
        functionDefinitions.keySet().stream()
            .map(fqn -> fqn.replace(".", "~"))
            .collect(Collectors.joining("+"));

    final var basePath = Paths.get(".", "out");

    unshare();
    for (var entry : functionDefinitions.entrySet()) {
      try (final var out =
          Files.newOutputStream(
              basePath.resolve(entry.getKey().replace(".", "~") + "-unshared.ml"))) {
        entry.getValue().printTo(new PrintStream(out));
      } catch (IOException e) {
        e.printStackTrace();
      }
    }

    final var accumulatedConstraints = new HashSet<>(outsideConstraints);

    for (var fd : functionDefinitions.values()) {
      final var globals =
          new AnnotatingGlobals(functionAnnotations, costFreeFunctionAnnotations, null, 1);
      try (final var out =
          Files.newOutputStream(basePath.resolve(fd.getFullyQualifiedName() + "-sizes.svg"))) {
        fd.stubAnnotations(
            functionAnnotations, costFreeFunctionAnnotations, globals.getHeuristic(), out);
      } catch (IOException e) {
        e.printStackTrace();
      }
    }

    for (var fd : functionDefinitions.values()) {
      try (final var out =
          Files.newOutputStream(basePath.resolve(fd.getFullyQualifiedName() + "-proof.svg"))) {
        final var globals =
            new AnnotatingGlobals(
                functionAnnotations, costFreeFunctionAnnotations, fd.getSizeAnalysis(), 1);
        accumulatedConstraints.addAll(fd.infer(globals, out));
      } catch (IOException e) {
        e.printStackTrace();
      }
      // System.out.println(fd.getFullyQualifiedName());
    }

    System.out.println(accumulatedConstraints.size() + " constraints accumulated");

    // This is the entrypoint of new-style solving. We get a bunch of constraints
    // that need to be fulfilled in order to typecheck the program.
    Optional<Map<Coefficient, KnownCoefficient>> solution =
        ConstraintSystemSolver.solve(accumulatedConstraints, name);
    // final boolean solved = constraints.solve();

    if (false) {
      Graph g =
          ConstraintSystemSolver.toGraph(
              graph(name)
                  .directed()
                  .graphAttr()
                  .with("ranksep", "2.5")
                  .graphAttr()
                  .with("splines", "ortho") /*.graphAttr().with(Rank.RankDir.BOTTOM_TO_TOP)*/,
              accumulatedConstraints);
      var viz = Graphviz.fromGraph(g);
      try (final var out = Files.newOutputStream(basePath.resolve(name + "-constraints.svg"))) {
        viz.engine(Engine.DOT).render(Format.SVG).toOutputStream(out);
      } catch (IOException e) {
        e.printStackTrace();
      }
    }

    if (false) {
      throw new ConstraintSystemUnsatisfiableException("constraint system is unsatisfiable");
    }

    final var namesAsSet =
        functionDefinitions.values().stream()
            .map(FunctionDefinition::getFullyQualifiedName)
            .collect(Collectors.joining(", ", "{", "}"));

    final var single = functionDefinitions.size() == 1;

    if (solution.isPresent() && !single) {
      System.out.println(namesAsSet + ":");
    }

    for (var fd : functionDefinitions.values()) {
      // fd.printAnnotation(System.out);
      try (final var out =
          Files.newOutputStream(
              basePath.resolve(fd.getFullyQualifiedName().replace(".", "~") + ".svg"))) {
        fd.toGraph(out);
      } catch (IOException e) {
        e.printStackTrace();
      }
      if (solution.isPresent()) {
        fd.substitute(solution.get());
        if (!single) {
          System.out.print("\t");
        }
        fd.printAnnotation(System.out);
      }
    }

    if (solution.isEmpty()) {
      if (functionDefinitions.size() > 1) {
        System.out.println(namesAsSet + " | UNSAT");
      } else {
        System.out.println(
            functionDefinitions.values().iterator().next().getFullyQualifiedName() + " | UNSAT");
      }
      return empty();
    }
    return Optional.of(functionAnnotations);
  }

  public void printTo(PrintStream out) {
    try {
      infer();
    } catch (UnificationError | TypeError unificationError) {
      throw new RuntimeException(unificationError);
    }
    for (var entry : functionDefinitions.values()) {
      entry.printTo(out);
      out.println();
    }
  }

  private void normalize() {
    // Maybe let the Loader normalize directly?
    for (var fd : functionDefinitions.values()) {
      fd.normalize();
    }
  }

  private void unshare() {
    for (var fd : functionDefinitions.values()) {
      fd.unshare();
    }
  }

  public void printHaskellTo(PrintStream out) {
    // TODO(lorenzleutgeb): It seems that `infer` is not idempotent. If we call it here,
    // we get strange results for the types of function definitions in this program.
    // That is wrong, since `infer` should indeed be idempotent.
    for (var entry : functionDefinitions.values()) {
      entry.printHaskellTo(out);
      out.println();
    }
  }
}
