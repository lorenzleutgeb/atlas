package xyz.leutgeb.lorenz.lac.ast;

import static com.google.common.collect.Sets.difference;
import static guru.nidi.graphviz.attribute.Records.turn;
import static guru.nidi.graphviz.model.Factory.graph;
import static guru.nidi.graphviz.model.Factory.node;
import static java.util.Collections.emptySet;
import static java.util.Collections.singleton;
import static java.util.stream.Collectors.toList;
import static xyz.leutgeb.lorenz.lac.typing.simple.TypeConstraint.minimize;

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
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.stream.Collectors;
import java.util.stream.Stream;
import lombok.Data;
import lombok.extern.log4j.Log4j2;
import org.hipparchus.util.Pair;
import org.jgrapht.graph.DirectedMultigraph;
import org.jgrapht.nio.AttributeType;
import org.jgrapht.nio.DefaultAttribute;
import xyz.leutgeb.lorenz.lac.IntIdGenerator;
import xyz.leutgeb.lorenz.lac.Loader;
import xyz.leutgeb.lorenz.lac.NidiExporter;
import xyz.leutgeb.lorenz.lac.SizeEdge;
import xyz.leutgeb.lorenz.lac.Util;
import xyz.leutgeb.lorenz.lac.typing.resources.AnnotatingContext;
import xyz.leutgeb.lorenz.lac.typing.resources.AnnotatingGlobals;
import xyz.leutgeb.lorenz.lac.typing.resources.Annotation;
import xyz.leutgeb.lorenz.lac.typing.resources.coefficients.Coefficient;
import xyz.leutgeb.lorenz.lac.typing.resources.coefficients.KnownCoefficient;
import xyz.leutgeb.lorenz.lac.typing.resources.constraints.Constraint;
import xyz.leutgeb.lorenz.lac.typing.resources.heuristics.AnnotationHeuristic;
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
import xyz.leutgeb.lorenz.lac.unification.Generalizer;
import xyz.leutgeb.lorenz.lac.unification.Substitution;
import xyz.leutgeb.lorenz.lac.unification.UnificationContext;
import xyz.leutgeb.lorenz.lac.unification.UnificationError;

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
  private org.jgrapht.Graph<Identifier, SizeEdge> sizeAnalysis;

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

  public FunctionSignature stubSignature(UnificationContext problem) {
    return inferredSignature =
        new FunctionSignature(
            emptySet(),
            new FunctionType(
                Stream.generate(problem::fresh).limit(arguments.size()).collect(toList()),
                problem.fresh()));
  }

  public void infer(UnificationContext context) throws UnificationError, TypeError {
    /*
    if (inferredSignature != null) {
      return inferredSignature;
    }
    */

    // var sub = context.childWithNewUnfication();
    final var sub = context.childWithNewVariables(getFullyQualifiedName());
    for (int i = 0; i < arguments.size(); i++) {
      sub.putType(
          arguments.get(i), inferredSignature.getType().getFrom().getElements().get(i), null);
    }

    // TODO: Maybe do this in stub?
    if (annotatedSignature != null) {
      if (annotatedSignature.getType().getFrom().getElements().size() != arguments.size()) {
        throw new TypeError();
      }
      for (int i = 0; i < arguments.size(); i++) {
        Type ty = annotatedSignature.getType().getFrom().getElements().get(i);
        Type var = inferredSignature.getType().getFrom().getElements().get(i);
        sub.addIfNotEqual(var, ty);
      }
      sub.addIfNotEqual(inferredSignature.getType().getTo(), annotatedSignature.getType().getTo());
    }

    sub.addIfNotEqual(inferredSignature.getType().getTo(), body.infer(sub));
  }

  public void resolve(Substitution solution, FunctionSignature signature)
      throws TypeError.AnnotationMismatch {
    var subsGenBase = solution.apply(signature.getType());
    var generalizer = new Generalizer();
    subsGenBase.generalize(generalizer);
    var x = solution.compose(generalizer.toSubstitution());
    final var relevantConstraints = signature.getConstraints();
    var tmp = new FunctionSignature(relevantConstraints, signature.getType());
    tmp = tmp.apply(x);
    inferredSignature = new FunctionSignature(minimize(tmp.getConstraints()), tmp.getType());
    body.resolveType(x);
    if (annotatedSignature != null && !inferredSignature.equals(annotatedSignature)) {
      throw new TypeError.AnnotationMismatch(
          getFullyQualifiedName(), annotatedSignature, inferredSignature);
    }
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

  public void stubAnnotations(
      Map<String, Pair<Annotation, Annotation>> functionAnnotations,
      Map<String, Pair<Annotation, Annotation>> costFreeFunctionAnnotations,
      AnnotationHeuristic heuristic,
      OutputStream out) {

    sizeAnalysis = new DirectedMultigraph<Identifier, SizeEdge>(SizeEdge.class);
    body.analyzeSizes(sizeAnalysis);

    final NidiExporter<Identifier, SizeEdge> exporter =
        new NidiExporter<>(
            identifier ->
                identifier.getName()
                    + "_"
                    + (identifier.getIntro() == null
                        ? "null"
                        : (identifier.getIntro().getFqn()
                            + "_"
                            + Util.stamp(identifier.getIntro().getExpression()))));
    exporter.setVertexAttributeProvider(
        v -> {
          return Map.of("label", new DefaultAttribute<>(v.getName(), AttributeType.STRING));
        });
    exporter.setEdgeAttributeProvider(
        e -> {
          return Map.of(
              // "label",
              // new DefaultAttribute<>(e.getKind().toString(), AttributeType.STRING),
              "color",
              new DefaultAttribute<>(
                  e.getKind().equals(SizeEdge.Kind.EQ) ? "blue4" : "red", AttributeType.STRING));
        });
    final var exp = exporter.transform(sizeAnalysis);
    final var viz = Graphviz.fromGraph(exp);
    try {
      viz.render(Format.SVG).toOutputStream(out);
    } catch (IOException e) {
      e.printStackTrace();
    }

    final var treeLikeArguments = treeLikeArguments();
    var predefined = functionAnnotations.get(getFullyQualifiedName());

    final var returnsTree = body.getType() instanceof TreeType;

    if (predefined == null) {
      annotation =
          new Pair<>(
              heuristic.generate("args", treeLikeArguments.size()),
              returnsTree ? heuristic.generate("return", 1) : Annotation.empty());
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
            heuristic.generate("cfargs", annotation.getFirst()),
            heuristic.generate("cfreturn", annotation.getSecond()));

    functionAnnotations.put(getFullyQualifiedName(), annotation);
    costFreeFunctionAnnotations.put(getFullyQualifiedName(), costFreeAnnotation);
  }

  public Set<Constraint> infer(AnnotatingGlobals globals, OutputStream out)
      throws UnificationError, TypeError, ConstraintSystemUnsatisfiableException {
    if (inferredSignature == null) {
      throw new IllegalStateException();
    }

    /*
    var returnedAnnotation =
        body.inferAnnotations(
            new AnnotatingContext(treeLikeArguments, annotation.getFirst()), globals);
    */

    final var obligation =
        new Obligation(
            new AnnotatingContext(treeLikeArguments(), annotation.getFirst()),
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
    body = body.unshare(new IntIdGenerator());
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
