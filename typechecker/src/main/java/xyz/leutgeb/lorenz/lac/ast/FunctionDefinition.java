package xyz.leutgeb.lorenz.lac.ast;

import static com.google.common.collect.Sets.difference;
import static guru.nidi.graphviz.attribute.Records.turn;
import static guru.nidi.graphviz.model.Factory.graph;
import static guru.nidi.graphviz.model.Factory.node;
import static java.util.Collections.emptySet;
import static java.util.Collections.singleton;

import guru.nidi.graphviz.attribute.Records;
import guru.nidi.graphviz.attribute.Shape;
import guru.nidi.graphviz.engine.Format;
import guru.nidi.graphviz.engine.Graphviz;
import guru.nidi.graphviz.model.Graph;
import guru.nidi.graphviz.model.Node;
import java.io.IOException;
import java.io.OutputStream;
import java.io.PrintStream;
import java.util.ArrayList;
import java.util.Collections;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.stream.Collectors;
import lombok.Data;
import lombok.extern.log4j.Log4j2;
import org.hipparchus.util.Pair;
import xyz.leutgeb.lorenz.lac.IntIdGenerator;
import xyz.leutgeb.lorenz.lac.Loader;
import xyz.leutgeb.lorenz.lac.typing.resources.AnnotatingContext;
import xyz.leutgeb.lorenz.lac.typing.resources.AnnotatingGlobals;
import xyz.leutgeb.lorenz.lac.typing.resources.Annotation;
import xyz.leutgeb.lorenz.lac.typing.resources.coefficients.Coefficient;
import xyz.leutgeb.lorenz.lac.typing.resources.coefficients.KnownCoefficient;
import xyz.leutgeb.lorenz.lac.typing.resources.constraints.Constraint;
import xyz.leutgeb.lorenz.lac.typing.resources.proving.Obligation;
import xyz.leutgeb.lorenz.lac.typing.resources.proving.Prover;
import xyz.leutgeb.lorenz.lac.typing.resources.solving.ConstraintSystemUnsatisfiableException;
import xyz.leutgeb.lorenz.lac.typing.simple.FunctionSignature;
import xyz.leutgeb.lorenz.lac.typing.simple.TypeError;
import xyz.leutgeb.lorenz.lac.typing.simple.TypeVariable;
import xyz.leutgeb.lorenz.lac.typing.simple.types.BoolType;
import xyz.leutgeb.lorenz.lac.typing.simple.types.FunctionType;
import xyz.leutgeb.lorenz.lac.typing.simple.types.TreeType;
import xyz.leutgeb.lorenz.lac.typing.simple.types.Type;
import xyz.leutgeb.lorenz.lac.unification.UnificationContext;
import xyz.leutgeb.lorenz.lac.unification.UnificationError;
import xyz.leutgeb.lorenz.lac.unification.UnificationVariable;

@Data
@Log4j2
public class FunctionDefinition {
  private final String moduleName;
  private final String name;
  private final List<String> arguments;
  private Expression body;
  private FunctionSignature inferredSignature;
  private FunctionSignature annotatedSignature;
  private Pair<Annotation, Annotation> annotation;

  public FunctionDefinition(
      String moduleName,
      String name,
      List<String> arguments,
      Expression body,
      FunctionSignature annotatedSignature) {
    this.moduleName = moduleName;
    this.name = name;
    this.arguments = arguments;
    this.body = body;
    this.annotatedSignature = annotatedSignature;
  }

  public FunctionSignature inferAnnotations(UnificationContext context)
      throws UnificationError, TypeError {
    if (inferredSignature != null) {
      return inferredSignature;
    }

    var sub = context.childWithNewUnfication();
    List<Type> from = new ArrayList<>(arguments.size());

    if (annotatedSignature == null) {
      for (String argument : arguments) {
        Type var = sub.getProblem().fresh();
        from.add(var);
        sub.putType(argument, var);
      }
    } else {
      if (annotatedSignature.getType().getFrom().getElements().size() != arguments.size()) {
        throw new TypeError();
      }
      for (int i = 0; i < arguments.size(); i++) {
        Type ty = annotatedSignature.getType().getFrom().getElements().get(i);
        UnificationVariable var = sub.getProblem().fresh();
        from.add(var);
        sub.putType(arguments.get(i), var);
        sub.getProblem().addIfNotEqual(var, ty);
      }
    }

    UnificationVariable to = sub.getProblem().fresh();
    if (annotatedSignature != null) {
      sub.getProblem().addIfNotEqual(to, annotatedSignature.getType().getTo());
    }
    FunctionType result = new FunctionType(from, to);
    sub.putSignature(getFullyQualifiedName(), new FunctionSignature(emptySet(), result));

    sub.getProblem().addIfNotEqual(to, body.infer(sub));

    var solution = sub.getProblem().solveAndGeneralize(result);

    inferredSignature =
        new FunctionSignature(
            sub.getProblem().getConstraints().stream()
                .map(tc -> tc.apply(solution))
                .collect(Collectors.toSet()),
            (FunctionType) solution.apply(result));
    body.resolveType(solution);
    if (annotatedSignature != null && !inferredSignature.equals(annotatedSignature)) {
      throw new TypeError.AnnotationMismatch(
          getFullyQualifiedName(), annotatedSignature, inferredSignature);
    }
    return inferredSignature;
  }

  public void normalize() {
    if (inferredSignature != null) {
      return;
    }
    body = body.normalizeAndBind(new IntIdGenerator());
  }

  public void substitute(Map<Coefficient, KnownCoefficient> solution) {
    annotation =
        new Pair<>(
            annotation.getFirst().substitute(solution),
            annotation.getSecond().substitute(solution));
  }

  private List<String> treeLikeArguments() {
    if (inferredSignature == null) {
      throw new IllegalStateException();
    }
    var types = inferredSignature.getType().getFrom().getElements();
    final var ids = new ArrayList<String>(types.size());
    for (int i = 0; i < arguments.size(); i++) {
      if (types.get(i) instanceof TreeType) {
        ids.add(arguments.get(i));
      } else if (types.get(i) != BoolType.INSTANCE && !(types.get(i) instanceof TypeVariable)) {
        throw new RuntimeException("unknown type");
      }
    }
    return ids;
  }

  public Set<Constraint> inferAnnotations(
      Map<String, Pair<Annotation, Annotation>> functionAnnotations,
      Map<String, Pair<Annotation, Annotation>> costFreeFunctionAnnotations,
      AnnotatingGlobals globals,
      OutputStream out)
      throws UnificationError, TypeError, ConstraintSystemUnsatisfiableException {
    if (inferredSignature == null) {
      throw new IllegalStateException();
    }

    final var treeLikeArguments = treeLikeArguments();
    var predefined = functionAnnotations.get(getFullyQualifiedName());

    final var returnsTree = body.getType() instanceof TreeType;

    if (predefined == null) {
      annotation =
          new Pair<>(
              globals.getHeuristic().generate("args", treeLikeArguments.size()),
              returnsTree ? globals.getHeuristic().generate("return", 1) : Annotation.empty());
    } else {
      if (predefined.getFirst().size() != treeLikeArguments.size()) {
        throw new IllegalArgumentException(
            "the predefined annotation for parameters of "
                + getFullyQualifiedName()
                + " is expected to be of size "
                + treeLikeArguments.size()
                + " but it is only of size "
                + predefined.getFirst().size());
      }
      if ((returnsTree && predefined.getSecond().size() != 1)
          || (!returnsTree && predefined.getSecond().size() != 0)) {

        throw new IllegalArgumentException(
            "the predefined annotation for the result of "
                + getFullyQualifiedName()
                + " is expected to be of size "
                + treeLikeArguments.size()
                + " but it is only of size "
                + predefined.getSecond().size());
      }
      annotation = predefined;
    }

    var costFreeAnnotation =
        new Pair<>(
            globals.getHeuristic().generate("cfargs", annotation.getFirst()),
            globals.getHeuristic().generate("cfreturn", annotation.getSecond()));

    functionAnnotations.put(getFullyQualifiedName(), annotation);
    // costFreeFunctionAnnotations.put(getFullyQualifiedName(), costFreeAnnotation);

    /*
    var returnedAnnotation =
        body.inferAnnotations(
            new AnnotatingContext(treeLikeArguments, annotation.getFirst()), globals);
    */

    final var obligation =
        new Obligation(
            new AnnotatingContext(treeLikeArguments, annotation.getFirst()),
            body,
            annotation.getSecond(),
            1);

    /*
    var returnedCostFreeAnnotation =
        body.inferAnnotations(
            new AnnotatingContext(treeLikeArguments, annotation.getFirst()), globals.costFree());
     */

    /*
    final var costFreeObligation =
        new Obligation(
            new AnnotatingContext(treeLikeArguments, annotation.getFirst()),
            body,
            costFreeAnnotation.getSecond(),
            0);
     */

    // constraints.eq(costFreeAnnotation.getSecond(), returnedCostFreeAnnotation);
    // constraints.eq(annotation.getSecond(), returnedAnnotation);

    // return Sets.union(
    //    Prover.prove(obligation, globals), Prover.prove(costFreeObligation, globals.costFree()));
    return Prover.prove(obligation, globals, out);
  }

  public void printAnnotation(PrintStream out) {
    if (annotation == null) {
      throw new IllegalStateException();
    }

    out.println(
        moduleName
            + "."
            + name
            + (arguments.isEmpty() ? "" : " ")
            + String.join(" ", arguments)
            + " | "
            + annotation.getFirst().toStringForParameters(treeLikeArguments(), true)
            + " -> "
            + annotation.getSecond().toStringForParameters(Collections.singletonList("_"), true));
  }

  public void printTo(PrintStream out) {
    out.print(name);
    out.print(" ∷ ");
    out.println(inferredSignature);
    out.print(name);
    out.print(" ");
    out.print(String.join(" ", arguments));
    out.print(" = ");
    body.printTo(out, 1);
    out.println();
  }

  public void printHaskellTo(PrintStream out) {
    out.print(name);
    out.print(" :: ");
    out.println(inferredSignature.toHaskell());
    out.print(name);
    out.print(" ");
    out.print(String.join(" ", arguments));
    out.print(" = ");
    body.printHaskellTo(out, 1);
    out.println();
    out.println();
  }

  public void toGraph(OutputStream out) throws IOException {
    // Graphviz.useEngine(new GraphvizCmdLineEngine());
    Graph g = graph(name).directed(); // .graphAttr();//.with(RankDir.BOTTOM_TO_TOP);
    Node root =
        node(name)
            .with(
                Shape.DOUBLE_OCTAGON,
                Records.of(
                    turn(
                        name,
                        inferredSignature.toString().replace(">", "\\>").replace("<", "\\<"),
                        annotation.getFirst().getName()
                            + " → "
                            + annotation.getSecond().getName())));
    Graph result = body.toGraph(g, root);
    var viz = Graphviz.fromGraph(result);
    viz.render(Format.SVG).toOutputStream(out);
  }

  public String getFullyQualifiedName() {
    return moduleName + "." + name;
  }

  public Set<String> importedFunctions() {
    return getOcurringFunctions().stream()
        .filter(f -> !Loader.moduleName(f).equals(moduleName))
        .collect(Collectors.toSet());
  }

  public Set<String> getOcurringFunctions() {
    return body.getOccurringFunctions();
  }

  public Set<String> getOcurringFunctionsNonRecursive() {
    return difference(getOcurringFunctions(), singleton(getFullyQualifiedName()));
  }

  public void unshare() {
    body = body.unshare(new HashMap<>(), new IntIdGenerator());
  }

  @Override
  public String toString() {
    return moduleName
        + "."
        + name
        + " "
        + String.join(" ", arguments)
        + " = "
        + body.terminalOrBox();
  }
}
