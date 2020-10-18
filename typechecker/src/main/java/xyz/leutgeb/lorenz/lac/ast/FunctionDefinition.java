package xyz.leutgeb.lorenz.lac.ast;

import com.google.common.collect.Sets;
import guru.nidi.graphviz.attribute.Records;
import guru.nidi.graphviz.attribute.Shape;
import guru.nidi.graphviz.engine.Format;
import guru.nidi.graphviz.engine.Graphviz;
import guru.nidi.graphviz.model.Graph;
import guru.nidi.graphviz.model.Node;
import lombok.Data;
import lombok.extern.slf4j.Slf4j;
import org.jgrapht.graph.DirectedMultigraph;
import xyz.leutgeb.lorenz.lac.module.Loader;
import xyz.leutgeb.lorenz.lac.typing.resources.AnnotatingContext;
import xyz.leutgeb.lorenz.lac.typing.resources.Annotation;
import xyz.leutgeb.lorenz.lac.typing.resources.CombinedFunctionAnnotation;
import xyz.leutgeb.lorenz.lac.typing.resources.FunctionAnnotation;
import xyz.leutgeb.lorenz.lac.typing.resources.coefficients.Coefficient;
import xyz.leutgeb.lorenz.lac.typing.resources.coefficients.KnownCoefficient;
import xyz.leutgeb.lorenz.lac.typing.resources.heuristics.AnnotationHeuristic;
import xyz.leutgeb.lorenz.lac.typing.resources.proving.Obligation;
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
import xyz.leutgeb.lorenz.lac.util.IntIdGenerator;
import xyz.leutgeb.lorenz.lac.util.SizeEdge;

import java.io.IOException;
import java.io.OutputStream;
import java.io.PrintStream;
import java.util.ArrayList;
import java.util.List;
import java.util.Map;
import java.util.Optional;
import java.util.Set;
import java.util.stream.Collectors;
import java.util.stream.Stream;

import static com.google.common.collect.Sets.difference;
import static guru.nidi.graphviz.attribute.Records.turn;
import static guru.nidi.graphviz.model.Factory.graph;
import static guru.nidi.graphviz.model.Factory.node;
import static java.util.Collections.emptySet;
import static java.util.Collections.singleton;
import static java.util.stream.Collectors.toList;
import static xyz.leutgeb.lorenz.lac.typing.simple.TypeConstraint.minimize;
import static xyz.leutgeb.lorenz.lac.util.Util.bug;

@Data
@Slf4j
public class FunctionDefinition {
  private final String moduleName;
  private final String name;
  private final List<String> arguments;
  private Expression body;
  private FunctionSignature inferredSignature;
  private FunctionSignature annotatedSignature;
  // private FunctionAnnotation inferredAnnotation;
  private Set<FunctionAnnotation> cfAnnotations;
  private org.jgrapht.Graph<Identifier, SizeEdge> sizeAnalysis = null;

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

    final var sub = context.childWithNewVariables(getFullyQualifiedName());
    for (int i = 0; i < arguments.size(); i++) {
      sub.putType(
          arguments.get(i), inferredSignature.getType().getFrom().getElements().get(i), null);
    }

    // TODO(lorenz.leutgeb): Maybe do this in stub?
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
    body = body.normalizeAndBind(IntIdGenerator.human());
  }

  public void substitute(Map<Coefficient, KnownCoefficient> solution) {
    inferredSignature =
        new FunctionSignature(
            inferredSignature.getConstraints(),
            inferredSignature.getType(),
            inferredSignature.getAnnotation().map(a -> a.substitute(solution)));
  }

  public List<Identifier> treeLikeArguments() {
    if (inferredSignature == null) {
      throw new IllegalStateException();
    }
    final var bodyFreeVariables = body.freeVariables();

    final var bodyFreeVariablesOrdered = new ArrayList<Identifier>(bodyFreeVariables.size());

    var types = inferredSignature.getType().getFrom().getElements();

    for (int i = 0; i < arguments.size(); i++) {
      if (types.get(i) instanceof TreeType) {
        for (var freeVar : bodyFreeVariables) {
          if (freeVar.getName().equals(arguments.get(i))) {
            bodyFreeVariablesOrdered.add(freeVar);
            break;
          }
        }
      } else if (types.get(i) != BoolType.INSTANCE && !(types.get(i) instanceof TypeVariable)) {
        throw new RuntimeException("unknown type");
      }
    }
    if (bodyFreeVariablesOrdered.size() != bodyFreeVariables.size()) {
      throw bug("hmm");
    }
    return bodyFreeVariablesOrdered;
  }

  public void analyzeSizes() {
    if (sizeAnalysis != null) {
      return;
    }
    sizeAnalysis = new DirectedMultigraph<>(SizeEdge.class);
    body.analyzeSizes(sizeAnalysis);
  }

  public void stubAnnotations(
      Map<String, CombinedFunctionAnnotation> functionAnnotations,
      AnnotationHeuristic heuristic,
      boolean infer) {
    stubAnnotations(functionAnnotations, heuristic, true);
  }

  public void stubAnnotations(
      Map<String, CombinedFunctionAnnotation> functionAnnotations,
      AnnotationHeuristic heuristic,
      boolean cf,
      boolean infer) {

    analyzeSizes();

    final var treeLikeArguments = treeLikeArguments();
    var predefined = functionAnnotations.get(getFullyQualifiedName());

    final var returnsTree = body.getType() instanceof TreeType;

    if (predefined == null) {
      var inferredAnnotation =
          new FunctionAnnotation(
              heuristic.generate("args", treeLikeArguments.size()),
              heuristic.generate("return", returnsTree ? 1 : 0));

      var costFreeAnnotation =
          new FunctionAnnotation(
              heuristic.generate("cfargs1", inferredAnnotation.from),
              heuristic.generate("cfreturn1", inferredAnnotation.to));

      var costFreeAnnotation2 =
          new FunctionAnnotation(
              heuristic.generate("cfargs2", inferredAnnotation.from),
              heuristic.generate("cfreturn2", inferredAnnotation.to));

      var zeroAnnotation =
          new FunctionAnnotation(
              Annotation.zero(inferredAnnotation.from.size(), "cfargszero"),
              Annotation.zero(inferredAnnotation.to.size(), "cfreturnzero"));

      if (cf) {
        this.cfAnnotations = Set.of(costFreeAnnotation, costFreeAnnotation2, zeroAnnotation);
      } else {
        this.cfAnnotations = emptySet();
      }
      // TODO: Sort this out...
      if (infer || annotatedSignature.getAnnotation().isEmpty()) {
        inferredSignature =
            new FunctionSignature(
                inferredSignature.getConstraints(),
                inferredSignature.getType(),
                Optional.of(inferredAnnotation));
      } else {
        inferredSignature =
            new FunctionSignature(
                inferredSignature.getConstraints(),
                inferredSignature.getType(),
                annotatedSignature.getAnnotation());
      }
      functionAnnotations.put(
          getFullyQualifiedName(),
          new CombinedFunctionAnnotation(inferredAnnotation, cfAnnotations));
    } else {
      if (predefined.withCost.from.size() != treeLikeArguments.size()) {
        throw new IllegalArgumentException(
            "the predefined annotation for parameters of "
                + getFullyQualifiedName()
                + " is expected to be of size "
                + treeLikeArguments.size()
                + " but it is only of size "
                + predefined.withCost.from.size());
      }
      if (returnsTree
          ? predefined.withCost.to.size() != 1
          : predefined.withCost.to.size() != 0) {
        throw new IllegalArgumentException(
            "the predefined annotation for the result of "
                + getFullyQualifiedName()
                + " is expected to be of size "
                + treeLikeArguments.size()
                + " but it is only of size "
                + predefined.withCost.to.size());
      }
      inferredSignature =
          new FunctionSignature(
              inferredSignature.getConstraints(),
              inferredSignature.getType(),
              Optional.of(predefined.withCost));
      cfAnnotations = predefined.withoutCost;
    }
  }

  public Set<TypeVariable> runaway() {
    return Sets.difference(
        inferredSignature.getType().getTo().variables(),
        inferredSignature.getType().getFrom().variables());
  }

  public Obligation getTypingObligation(int cost) {
    return new Obligation(
        new AnnotatingContext(treeLikeArguments(), inferredSignature.getAnnotation().get().from),
        body,
        inferredSignature.getAnnotation().get().to,
        cost);
  }

  public String getAnnotationString() {
    if (inferredSignature.getAnnotation().isEmpty()) {
      throw new IllegalStateException();
    }

    return moduleName
        + "."
        + name
        + (arguments.isEmpty() ? "" : " ")
        + String.join(" ", arguments)
        + " | "
        + new CombinedFunctionAnnotation(inferredSignature.getAnnotation().get(), cfAnnotations);
  }

  public String getMockedAnnotationString(CombinedFunctionAnnotation annotation) {
    return moduleName
        + "."
        + name
        + (arguments.isEmpty() ? "" : " ")
        + String.join(" ", arguments)
        + " | "
        + annotation;
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
    body.printHaskellTo(out, 1, getFullyQualifiedName());
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
                        inferredSignature.getAnnotation().get().from.getNameAndId()
                            + " → "
                            + inferredSignature.getAnnotation().get().to.getNameAndId())));
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
    unshare(Expression.DEFAULT_LAZY);
  }

  public void unshare(boolean lazy) {
    body = body.unshare(IntIdGenerator.human(), lazy);
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
