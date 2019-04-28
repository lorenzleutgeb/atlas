package xyz.leutgeb.lorenz.logs.ast;

import java.io.PrintStream;
import java.util.ArrayList;
import java.util.List;
import java.util.Stack;
import java.util.stream.Collectors;
import lombok.Data;
import org.hipparchus.util.Pair;
import xyz.leutgeb.lorenz.logs.Context;
import xyz.leutgeb.lorenz.logs.resources.AnnotatedType;
import xyz.leutgeb.lorenz.logs.resources.Annotation;
import xyz.leutgeb.lorenz.logs.type.FunctionType;
import xyz.leutgeb.lorenz.logs.type.TreeType;
import xyz.leutgeb.lorenz.logs.type.Type;
import xyz.leutgeb.lorenz.logs.type.TypeError;
import xyz.leutgeb.lorenz.logs.unification.UnificationError;

@Data
public class FunctionDefinition {
  private final String name;
  private final List<String> arguments;
  private final Expression body;
  private FunctionType type;

  public FunctionType infer(Context context) throws UnificationError, TypeError {
    if (type != null) {
      return type;
    }

    var sub = context.child();
    List<Type> from = new ArrayList<>(arguments.size());

    for (String argument : arguments) {
      Type var = sub.getProblem().fresh();
      from.add(var);
      sub.put(argument, var);
    }

    Type to = sub.getProblem().fresh();
    Type result = new FunctionType(from, to);
    sub.put(name, result);

    sub.getProblem().add(to, body.infer(sub));

    // Now we are set for unification!
    var solution = sub.getProblem().solveAndGeneralize();

    type = (FunctionType) solution.apply(result);
    body.resolveType(solution);
    return type;
  }

  public FunctionDefinition normalize() {
    var context = new Stack<Pair<Identifier, Expression>>();
    return new FunctionDefinition(name, arguments, body.normalize(context).bindAll(context));
  }

  public Pair<AnnotatedType, AnnotatedType> inferAnnotation(Context context)
      throws UnificationError, TypeError {
    if (type == null) {
      throw new IllegalStateException();
    }
    var trees =
        type.getFrom()
            .getElements()
            .stream()
            .filter(x -> x instanceof TreeType)
            .collect(Collectors.toList());
    // if (trees.size() != 1) {
    //  throw new UnsupportedOperationException(
    //      "analysis is only supported for functions that take exactly one tree argument");
    // }
    var constraints = context.getConstraints();
    var q = constraints.heuristic(trees.size());
    if (!(type.getTo() instanceof TreeType)) {
      throw new UnsupportedOperationException(
          "analysis is only supported for functions that return a tree");
    }
    var result =
        new Pair<>(
            new AnnotatedType(type.getFrom(), q), body.inferAnnotations(context, Annotation.EMPTY));
    return result;
  }

  public void printTo(PrintStream out) {
    out.print(name);
    out.print(" ");
    out.print(arguments.stream().collect(Collectors.joining(" ")));
    out.print(" = ");
    body.printTo(out, 1);
  }
}
