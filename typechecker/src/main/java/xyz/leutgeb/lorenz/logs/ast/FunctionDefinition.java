package xyz.leutgeb.lorenz.logs.ast;

import static guru.nidi.graphviz.attribute.Records.turn;
import static guru.nidi.graphviz.model.Factory.graph;
import static guru.nidi.graphviz.model.Factory.node;
import static java.util.Collections.emptySet;
import static java.util.Collections.singleton;

import com.google.common.collect.Sets;
import guru.nidi.graphviz.attribute.Records;
import guru.nidi.graphviz.attribute.Shape;
import guru.nidi.graphviz.engine.Format;
import guru.nidi.graphviz.engine.Graphviz;
import guru.nidi.graphviz.model.Graph;
import guru.nidi.graphviz.model.Node;
import java.io.File;
import java.io.IOException;
import java.io.PrintStream;
import java.util.ArrayList;
import java.util.Collections;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.Stack;
import java.util.stream.Collectors;
import lombok.Data;
import lombok.extern.log4j.Log4j2;
import org.hipparchus.util.Pair;
import xyz.leutgeb.lorenz.logs.Context;
import xyz.leutgeb.lorenz.logs.Loader;
import xyz.leutgeb.lorenz.logs.resources.AnnotatingContext;
import xyz.leutgeb.lorenz.logs.resources.AnnotatingGlobals;
import xyz.leutgeb.lorenz.logs.resources.Annotation;
import xyz.leutgeb.lorenz.logs.resources.Constraints;
import xyz.leutgeb.lorenz.logs.typing.FunctionSignature;
import xyz.leutgeb.lorenz.logs.typing.TypeError;
import xyz.leutgeb.lorenz.logs.typing.TypeVariable;
import xyz.leutgeb.lorenz.logs.typing.types.BoolType;
import xyz.leutgeb.lorenz.logs.typing.types.FunctionType;
import xyz.leutgeb.lorenz.logs.typing.types.TreeType;
import xyz.leutgeb.lorenz.logs.typing.types.Type;
import xyz.leutgeb.lorenz.logs.unification.UnificationError;

@Data
@Log4j2
public class FunctionDefinition {
  private final String moduleName;
  private final String name;
  private final List<String> arguments;
  private Expression body;
  private FunctionSignature signature;
  private Pair<Annotation, Annotation> annotation;

  public FunctionDefinition(
      String moduleName, String name, List<String> arguments, Expression body) {
    this.moduleName = moduleName;
    this.name = name;
    this.arguments = arguments;
    this.body = body;
  }

  public FunctionSignature infer(Context context) throws UnificationError, TypeError {
    if (signature != null) {
      return signature;
    }

    var sub = context.childWithNewUnfication();
    List<Type> from = new ArrayList<>(arguments.size());

    for (String argument : arguments) {
      Type var = sub.getProblem().fresh();
      from.add(var);
      sub.putType(argument, var);
    }

    Type to = sub.getProblem().fresh();
    FunctionType result = new FunctionType(from, to);
    sub.putSignature(getFullyQualifiedName(), new FunctionSignature(emptySet(), result));

    sub.getProblem().addIfNotEqual(to, body.infer(sub));

    // Now we are set for unification!
    var solution = sub.getProblem().solveAndGeneralize(result);

    signature =
        new FunctionSignature(
            sub.getProblem().getConstraints().stream()
                .map(tc -> tc.apply(solution))
                .collect(Collectors.toSet()),
            (FunctionType) solution.apply(result));
    body.resolveType(solution);
    return signature;
  }

  public void normalize() {
    if (signature != null) {
      return;
    }
    var context = new Stack<Pair<Identifier, Expression>>();
    body = body.normalize(context).bindAll(context);
  }

  public void substitute(Constraints constraints) {
    annotation =
        new Pair<>(
            annotation.getFirst().substitute(constraints),
            annotation.getSecond().substitute(constraints));
  }

  private List<String> treeLikeArguments() {
    if (signature == null) {
      throw new IllegalStateException();
    }
    var types = signature.getType().getFrom().getElements();
    final var ids = new ArrayList<String>(types.size());
    for (int i = 0; i < arguments.size(); i++) {
      if (types.get(i) instanceof TreeType) {
        ids.add(arguments.get(i));
      } else if (types.get(i) == BoolType.INSTANCE || types.get(i) instanceof TypeVariable) {
        log.warn("Not adding " + arguments.get(i) + " to AnnotatingContext");
      } else {
        throw new RuntimeException("unknown type");
      }
    }
    return ids;
  }

  public void infer(
      Map<String, Pair<Annotation, Annotation>> functionAnnotations, Constraints constraints)
      throws UnificationError, TypeError {
    setupForInference(constraints);
    inferAnnotation(functionAnnotations, constraints);
  }

  public void check(
      Map<String, Pair<Annotation, Annotation>> functionAnnotations,
      Constraints constraints,
      Annotation from,
      Annotation to)
      throws UnificationError, TypeError {
    annotation = new Pair<>(from, to);
    inferAnnotation(functionAnnotations, constraints);
  }

  public void inferAnnotation(
      Map<String, Pair<Annotation, Annotation>> functionAnnotations, Constraints constraints)
      throws UnificationError, TypeError {

    if (signature == null) {
      throw new IllegalStateException();
    }

    final var ids = treeLikeArguments();

    functionAnnotations.put(getFullyQualifiedName(), annotation);
    final var globals = new AnnotatingGlobals(functionAnnotations, constraints, 1);

    body.inferAnnotations(new AnnotatingContext(ids, annotation.getFirst()), globals);
  }

  public void setupForInference(Constraints constraints) {
    if (signature == null) {
      throw new IllegalStateException();
    }

    final var ids = treeLikeArguments();
    annotation =
        new Pair<>(
            constraints.heuristic(ids.size()),
            body.getType() instanceof TreeType ? constraints.heuristic(1) : Annotation.empty());
  }

  public void printAnnotation(PrintStream out) {
    if (annotation == null) {
      throw new IllegalStateException();
    }

    out.println(
        name
            + (arguments.isEmpty() ? "" : " ")
            + String.join(" ", arguments)
            + " | "
            + annotation.getFirst().toStringForParameters(treeLikeArguments(), false)
            + " -> "
            + annotation.getSecond().toStringForParameters(Collections.singletonList("_"), false));
  }

  public void printTo(PrintStream out) {
    out.print("(* ");
    out.print(name);
    out.print(" : ");
    out.print(signature);
    out.println(" *)");
    out.print(name);
    out.print(" ");
    out.print(String.join(" ", arguments));
    out.print(" = ");
    body.printTo(out, 1);
  }

  public void printHaskellTo(PrintStream out) {
    out.print(name);
    out.print(" :: ");
    out.println(signature.toHaskell());
    out.print(name);
    out.print(" ");
    out.print(String.join(" ", arguments));
    out.print(" = ");
    body.printHaskellTo(out, 1);
    out.println();
    out.println();
  }

  public void toGraph() throws IOException {
    Graph g = graph(name).directed(); // .graphAttr();//.with(RankDir.BOTTOM_TO_TOP);
    Node root =
        node(name)
            .with(
                Shape.DOUBLE_OCTAGON,
                Records.of(
                    turn(
                        name,
                        signature.toString().replace(">", "\\>").replace("<", "\\<"),
                        annotation.getFirst().toShortString()
                            + " → "
                            + annotation.getSecond().toShortString())));
    Graph result = body.toGraph(g, root);
    var viz = Graphviz.fromGraph(result);
    viz.render(Format.SVG).toFile(new File("out", name + ".svg"));
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
    return body.getOcurringFunctions();
  }

  public Set<String> getOcurringFunctionsNonRecursive() {
    return Sets.difference(getOcurringFunctions(), singleton(getFullyQualifiedName()));
  }

  public void unshare() {
    body = body.unshare(new HashMap<>());
  }
}
