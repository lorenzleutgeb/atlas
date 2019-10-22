package xyz.leutgeb.lorenz.logs.ast;

import static guru.nidi.graphviz.attribute.Records.turn;
import static guru.nidi.graphviz.model.Factory.graph;
import static guru.nidi.graphviz.model.Factory.node;
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
    Type result = new FunctionType(from, to);
    sub.putType(getFullyQualifiedName(), result);

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

  public Pair<Annotation, Annotation> inferAnnotation(
      Map<String, Pair<Annotation, Annotation>> functionAnnotations)
      throws UnificationError, TypeError {

    if (signature == null) {
      throw new IllegalStateException();
    }

    final var constraints = new Constraints(name);

    // Enable syntax-directed application of (share) here!
    // body = body.unshare();

    var types = signature.getType().getFrom().getElements();
    final var ids = new ArrayList<String>(types.size());
    var trees = 0;
    for (int i = 0; i < arguments.size(); i++) {
      if (types.get(i) instanceof TreeType) {
        ids.add(arguments.get(i));
        trees++;
      } else if (types.get(i) == BoolType.INSTANCE || types.get(i) instanceof TypeVariable) {
        log.warn("Not adding " + arguments.get(i) + " to AnnotatingContext");
      } else {
        throw new RuntimeException("unknown type");
      }
    }

    var initialGammaQ = new AnnotatingContext(ids, constraints.heuristic(trees));
    var q = initialGammaQ.getAnnotation();

    var qprime = body.getType() instanceof TreeType ? constraints.heuristic(1) : Annotation.empty();

    functionAnnotations.put(getFullyQualifiedName(), new Pair<>(q, qprime));
    final var globals = new AnnotatingGlobals(functionAnnotations, constraints, 1);

    final var result = new Pair<>(q, body.inferAnnotations(initialGammaQ, globals));

    /*
    try {
      this.toGraph(result);
    } catch (IOException e) {
      e.printStackTrace();
    }
     */

    constraints.solve();

    /*
    Graph g = constraints.toGraph(graph(name).directed().graphAttr().with(RankDir.BOTTOM_TO_TOP));
    var viz = Graphviz.fromGraph(g);
    try {
      viz.engine(Engine.CIRCO)
          .render(Format.SVG)
              // TODO(lorenzleutgeb): Remove hardcoded path here.
          .toFile(new File(new File("..", "out"), name + "-constraints.svg"));
    } catch (IOException e) {
      e.printStackTrace();
    }
    */

    // TODO: Remove this side-effect!
    System.out.println(
        name
            + (arguments.isEmpty() ? "" : " ")
            + String.join(" ", arguments)
            + " | "
            + q.substitute(constraints).toStringForParameters(ids, false)
            + " -> "
            + qprime
                .substitute(constraints)
                .toStringForParameters(Collections.singletonList("_"), false));

    return result;
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

  public void toGraph(Pair<Annotation, Annotation> annotations) throws IOException {
    Graph g = graph(name).directed(); // .graphAttr();//.with(RankDir.BOTTOM_TO_TOP);
    Node root =
        node(name)
            .with(
                Shape.DOUBLE_OCTAGON,
                Records.of(
                    turn(
                        name,
                        signature.toString().replace(">", "\\>"),
                        annotations.getFirst().toShortString()
                            + " → "
                            + annotations.getSecond().toShortString())));
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
}
