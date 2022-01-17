package xyz.leutgeb.lorenz.atlas.ast;

import static guru.nidi.graphviz.attribute.Records.turn;
import static guru.nidi.graphviz.model.Factory.graph;
import static guru.nidi.graphviz.model.Factory.node;
import static java.util.Collections.emptySet;
import static java.util.Collections.unmodifiableSet;
import static java.util.stream.Collectors.toList;
import static xyz.leutgeb.lorenz.atlas.typing.simple.TypeConstraint.minimize;
import static xyz.leutgeb.lorenz.atlas.util.Util.bug;
import static xyz.leutgeb.lorenz.atlas.util.Util.indent;

import com.google.common.collect.Sets;
import com.google.common.collect.Streams;
import guru.nidi.graphviz.attribute.Records;
import guru.nidi.graphviz.attribute.Shape;
import guru.nidi.graphviz.engine.Format;
import guru.nidi.graphviz.engine.Graphviz;
import guru.nidi.graphviz.model.Graph;
import guru.nidi.graphviz.model.Node;
import jakarta.json.Json;
import jakarta.json.JsonObject;
import java.io.File;
import java.io.OutputStream;
import java.io.PrintStream;
import java.nio.file.Path;
import java.util.ArrayList;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Objects;
import java.util.Optional;
import java.util.Set;
import java.util.stream.Collectors;
import java.util.stream.Stream;
import lombok.Data;
import lombok.EqualsAndHashCode;
import lombok.extern.slf4j.Slf4j;
import org.jgrapht.graph.DirectedMultigraph;
import org.jgrapht.nio.AttributeType;
import org.jgrapht.nio.DefaultAttribute;
import xyz.leutgeb.lorenz.atlas.ast.expressions.Expression;
import xyz.leutgeb.lorenz.atlas.ast.expressions.IdentifierExpression;
import xyz.leutgeb.lorenz.atlas.module.Loader;
import xyz.leutgeb.lorenz.atlas.typing.resources.AnnotatingContext;
import xyz.leutgeb.lorenz.atlas.typing.resources.CombinedFunctionAnnotation;
import xyz.leutgeb.lorenz.atlas.typing.resources.FunctionAnnotation;
import xyz.leutgeb.lorenz.atlas.typing.resources.coefficients.Coefficient;
import xyz.leutgeb.lorenz.atlas.typing.resources.coefficients.KnownCoefficient;
import xyz.leutgeb.lorenz.atlas.typing.resources.heuristics.AnnotationHeuristic;
import xyz.leutgeb.lorenz.atlas.typing.resources.proving.Obligation;
import xyz.leutgeb.lorenz.atlas.typing.simple.FunctionSignature;
import xyz.leutgeb.lorenz.atlas.typing.simple.TypeClass;
import xyz.leutgeb.lorenz.atlas.typing.simple.TypeError;
import xyz.leutgeb.lorenz.atlas.typing.simple.TypeVariable;
import xyz.leutgeb.lorenz.atlas.typing.simple.types.BoolType;
import xyz.leutgeb.lorenz.atlas.typing.simple.types.FunctionType;
import xyz.leutgeb.lorenz.atlas.typing.simple.types.TreeType;
import xyz.leutgeb.lorenz.atlas.typing.simple.types.Type;
import xyz.leutgeb.lorenz.atlas.unification.Generalizer;
import xyz.leutgeb.lorenz.atlas.unification.Substitution;
import xyz.leutgeb.lorenz.atlas.unification.UnificationContext;
import xyz.leutgeb.lorenz.atlas.util.IntIdGenerator;
import xyz.leutgeb.lorenz.atlas.util.NidiExporter;
import xyz.leutgeb.lorenz.atlas.util.SizeEdge;

@Data
@EqualsAndHashCode(doNotUseGetters = true)
@Slf4j
public class FunctionDefinition {
  private final String moduleName;
  private final String name;
  private final List<String> arguments;
  private Expression body;
  private FunctionSignature inferredSignature;
  private FunctionSignature annotatedSignature;

  private org.jgrapht.Graph<IdentifierExpression, SizeEdge> sizeAnalysis = null;
  private Obligation typingObligation;

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

  public void infer(UnificationContext context) throws TypeError {
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

    // TODO(lorenzleutgeb): Maybe do this in stub?
    if (annotatedSignature != null) {
      final var expected = annotatedSignature.getType().getFrom().getElements().size();
      final var actual = arguments.size();
      if (expected != actual) {
        throw new TypeError(
            "The type declaration for "
                + getFullyQualifiedName()
                + " requires "
                + expected
                + " arguments, but "
                + arguments.size()
                + " are given at definition.");
      }
      for (int i = 0; i < arguments.size(); i++) {
        Type ty = annotatedSignature.getType().getFrom().getElements().get(i);
        Type var = inferredSignature.getType().getFrom().getElements().get(i);
        sub.addEquivalenceIfNotEqual(var, ty);
      }
      sub.addEquivalenceIfNotEqual(
          inferredSignature.getType().getTo(), annotatedSignature.getType().getTo());
    }

    sub.addEquivalenceIfNotEqual(inferredSignature.getType().getTo(), body.infer(sub));
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
    body = body.normalizeAndBind(IntIdGenerator.fromOneInclusive());
  }

  public void substitute(Map<Coefficient, KnownCoefficient> solution) {
    inferredSignature =
        new FunctionSignature(
            inferredSignature.getConstraints(),
            inferredSignature.getType(),
            inferredSignature.getAnnotation().map(a -> a.substitute(solution)));
  }

  public List<IdentifierExpression> treeLikeArguments() {
    // NOTE(lorenzleutgeb): A declared argument that is not used in the body
    // is missing from the return value of this function, e.g.
    //
    //  f ∷ Tree α * α → α
    //  f t x = x
    //
    // will not have `t` as a tree like argument.

    if (inferredSignature == null) {
      throw new IllegalStateException();
    }
    final var bodyFreeVariables = body.freeVariables();

    final var bodyFreeVariablesOrdered =
        new ArrayList<IdentifierExpression>(bodyFreeVariables.size());

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

    final var exporter =
        new NidiExporter<IdentifierExpression, SizeEdge>(
            identifier ->
                identifier.getName() + "_" + (identifier.getIntro() == null ? "null" : identifier));
    exporter.setVertexAttributeProvider(
        v -> Map.of("label", new DefaultAttribute<>(v.getName(), AttributeType.STRING)));
    exporter.setEdgeAttributeProvider(
        e ->
            Map.of(
                // "label",
                // new DefaultAttribute<>(e.getKind().toString(), AttributeType.STRING),
                "color",
                new DefaultAttribute<>(
                    e.getKind().equals(SizeEdge.Kind.EQ) ? "blue4" : "red", AttributeType.STRING)));

    /*
    final var exp = exporter.transform(sizeAnalysis);

    var lel = new GraphvizCmdLineEngine();
    lel.timeout(2, TimeUnit.MINUTES);
    Graphviz.useEngine(lel);

    final var viz = Graphviz.fromGraph(exp);
    try {
      File out =
          new File(
              new File("out"),
              Util.fqnToFlatFilename(getFullyQualifiedName()) + "-" + "-sizes.svg");
      viz.render(Format.SVG).toOutputStream(new PrintStream(out));
    } catch (IOException e) {
      e.printStackTrace();
    }
     */
  }

  public boolean returnsTree() {
    int treeCount = body.getType().countTrees().get();

    if (treeCount > 1) {
      throw new UnsupportedOperationException("returning more than one tree is not supported.");
    }

    return treeCount == 1;
  }

  public void stubAnnotations(
      Map<String, CombinedFunctionAnnotation> functionAnnotations,
      AnnotationHeuristic heuristic,
      int cf,
      boolean infer) {

    analyzeSizes();

    final var treeLikeArguments = treeLikeArguments();
    var predefined = functionAnnotations.get(getFullyQualifiedName());

    if (predefined == null) {
      var inferredAnnotation =
          new FunctionAnnotation(
              heuristic.generate(getName() + treeLikeArguments(), treeLikeArguments.size()),
              heuristic.generate(getName() + "[_]", returnsTree() ? 1 : 0));

      Set<FunctionAnnotation> cfAnnotations = new HashSet<>();

      /*
      if (cf > 0) {
        cfAnnotations.add(
            new FunctionAnnotation(
                Annotation.zero(inferredAnnotation.from.size(), "Qcf0"),
                Annotation.zero(inferredAnnotation.to.size(), "Qcf'")));
      }
       */

      for (int i = 1; i <= cf; i++) {
        cfAnnotations.add(
            new FunctionAnnotation(
                heuristic.generate(
                    getName() + treeLikeArguments() + "cf" + i, inferredAnnotation.from),
                heuristic.generate(getName() + "[_]" + "cf" + i, inferredAnnotation.to)));
      }

      final var combined =
          new CombinedFunctionAnnotation(inferredAnnotation, unmodifiableSet(cfAnnotations));
      // TODO: Sort this out...
      if (infer || annotatedSignature.getAnnotation().isEmpty()) {
        inferredSignature =
            new FunctionSignature(
                inferredSignature.getConstraints(),
                inferredSignature.getType(),
                Optional.of(combined));
      } else {
        inferredSignature =
            new FunctionSignature(
                inferredSignature.getConstraints(),
                inferredSignature.getType(),
                annotatedSignature.getAnnotation());
      }
      functionAnnotations.put(getFullyQualifiedName(), combined);
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
      if (returnsTree() ? predefined.withCost.to.size() != 1 : predefined.withCost.to.size() != 0) {
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
              Optional.of(predefined));
    }
  }

  public Set<TypeVariable> runaway() {
    return Sets.difference(
        inferredSignature.getType().getTo().variables(),
        inferredSignature.getType().getFrom().variables());
  }

  public Obligation getTypingObligation() {
    if (typingObligation != null) {
      return typingObligation;
    }
    typingObligation =
        new Obligation(
            new AnnotatingContext(
                treeLikeArguments(), inferredSignature.getAnnotation().get().withCost.from),
            body,
            inferredSignature.getAnnotation().get().withCost.to,
            true);

    return typingObligation;
  }

  public String getInferredSignatureString() {
    return moduleName
        + "."
        + name
        + " ∷ "
        + inferredSignature.getType()
        + inferredSignature
            .getAnnotation()
            .map(Objects::toString)
            .map(x -> " | " + x)
            .orElse(" | ?");
  }

  public JsonObject inferredSignatureToJson() {
    final var builder = Json.createObjectBuilder();

    builder.add("module", moduleName);
    builder.add("name", name);
    builder.add("signature", inferredSignature.toJson());
    return builder.build();
  }

  public String getAnnotatedSignatureString() {
    return moduleName
        + "."
        + name
        + " ∷ "
        + inferredSignature.getType()
        + annotatedSignature
            .getAnnotation()
            .map(Objects::toString)
            .map(x -> " | " + x)
            .orElse(" | ?");
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

  public void toGraph(OutputStream out) {
    try {
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
                          inferredSignature.getAnnotation().get().withCost.from.getNameAndId()
                              + " → "
                              + inferredSignature
                                  .getAnnotation()
                                  .get()
                                  .withCost
                                  .to
                                  .getNameAndId())));
      Graph result = body.toGraph(g, root);
      var viz = Graphviz.fromGraph(result);
      viz.render(Format.SVG).toOutputStream(out);
    } catch (Exception e) {
      log.warn("Non-critical exception thrown.", e);
    }
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

  public void unshare() {
    unshare(Expression.DEFAULT_LAZY);
  }

  public void unshare(boolean lazy) {
    body = body.unshare(IntIdGenerator.fromOneInclusive(), lazy);
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

  public String getSimpleSignatureString() {
    return moduleName
        + "."
        + name
        + " ∷ "
        + (inferredSignature == null ? "?" : inferredSignature.getType());
  }

  public void printJavaTo(PrintStream out, boolean makeStatic) {
    out.print("public ");

    if (makeStatic) {
      out.print("static ");
    }

    final var variables = inferredSignature.getType().variables();
    if (!variables.isEmpty()) {
      out.print("<");
      out.print(
          variables.stream()
              .map(
                  variable -> {
                    for (var constraint : inferredSignature.getConstraints()) {
                      if (constraint.getTypeClass().equals(TypeClass.ORD)
                          && constraint.getConstrained().size() == 1
                          && constraint.getConstrained().get(0).equals(variable)) {
                        return variable.toJava() + " extends Comparable<" + variable.toJava() + ">";
                      }
                    }
                    return variable.toJava();
                  })
              .collect(Collectors.joining(", ")));
      out.print("> ");
    }

    out.print(inferredSignature.getType().getTo().toJava());
    out.print(" ");
    out.print(name);
    out.print("(");
    out.print(
        Streams.zip(
                inferredSignature.getType().getFrom().getElements().stream(),
                arguments.stream(),
                (type, name) -> type.toJava() + " " + name)
            .collect(Collectors.joining(", ")));
    out.print(") {");

    if (body.isTerminal()) {
      indent(out, 1);
      out.println("return (");
      body.printJavaTo(out, 1, getFullyQualifiedName());
      indent(out, 1);
      out.println(");");
    } else {
      body.printJavaTo(out, 1, getFullyQualifiedName());
    }

    out.println("}");
    out.println();
  }

  public Path tactic() {
    return Path.of(
        getModuleName().replace(".", File.separator) + File.separator + getName() + ".txt");
  }

  public String getBoundString() {
    final List<String> treeArguments =
        treeLikeArguments().stream().map(IdentifierExpression::getName).toList();

    return moduleName
        + "."
        + name
        + ": "
        + inferredSignature.getAnnotation().map(x -> x.getBounds(treeArguments)).orElse("?");
  }
}
