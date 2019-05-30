package xyz.leutgeb.lorenz.logs.ast;

import java.io.PrintStream;
import java.util.ArrayList;
import java.util.List;
import java.util.Stack;
import lombok.Data;
import lombok.extern.log4j.Log4j2;
import org.hipparchus.util.Pair;
import xyz.leutgeb.lorenz.logs.Context;
import xyz.leutgeb.lorenz.logs.resources.AnnotatedType;
import xyz.leutgeb.lorenz.logs.resources.Annotation;
import xyz.leutgeb.lorenz.logs.typing.FunctionSignature;
import xyz.leutgeb.lorenz.logs.typing.TypeError;
import xyz.leutgeb.lorenz.logs.typing.TypeVariable;
import xyz.leutgeb.lorenz.logs.typing.types.BoolType;
import xyz.leutgeb.lorenz.logs.typing.types.FunctionType;
import xyz.leutgeb.lorenz.logs.typing.types.TreeType;
import xyz.leutgeb.lorenz.logs.typing.types.Type;
import xyz.leutgeb.lorenz.logs.unification.Equivalence;
import xyz.leutgeb.lorenz.logs.unification.UnificationError;

@Data
@Log4j2
public class FunctionDefinition {
  private final String name;
  private final List<String> arguments;
  private final Expression body;
  private FunctionSignature signature;

  public FunctionSignature infer(Context context) throws UnificationError, TypeError {
    if (signature != null) {
      return signature;
    }

    var sub = context.child();
    List<Type> from = new ArrayList<>(arguments.size());

    for (String argument : arguments) {
      Type var = sub.getProblem().fresh();
      from.add(var);
      sub.putType(argument, var);
    }

    Type to = sub.getProblem().fresh();
    Type result = new FunctionType(from, to);
    sub.putType(name, result);

    sub.getProblem().add(new Equivalence(to, body.infer(sub)));

    // Now we are set for unification!
    var solution = sub.getProblem().solveAndGeneralize(result);

    signature =
        new FunctionSignature(
            (FunctionType) solution.apply(result), sub.getProblem().getConstraints());
    body.resolveType(solution);
    return signature;
  }

  public FunctionDefinition normalize() {
    var context = new Stack<Pair<Identifier, Expression>>();
    return new FunctionDefinition(name, arguments, body.normalize(context).bindAll(context));
  }

  public Pair<AnnotatedType, AnnotatedType> inferAnnotation(Context context)
      throws UnificationError, TypeError {
    if (signature == null) {
      throw new IllegalStateException();
    }

    var sub = context.child();
    var types = signature.getType().getFrom().getElements();
    int trees = 0;
    for (int i = 0; i < arguments.size(); i++) {
      if (types.get(i) instanceof TreeType) {
        sub.putAnnotation(arguments.get(i), sub.getConstraints().heuristic(1));
        trees++;
      } else if (types.get(i) == BoolType.INSTANCE || types.get(i) instanceof TypeVariable) {
        log.warn("Not adding any annotation for " + arguments.get(i));
        // sub.putAnnotation(arguments.get(i), Annotation.);
      } else {
        throw new RuntimeException("unknown type");
      }
    }
    // if (trees.size() != 1) {
    //  throw new UnsupportedOperationException(
    //      "analysis is only supported for functions that take exactly one tree argument");
    // }
    var constraints = sub.getConstraints();
    var q = constraints.heuristic(trees);

    var qprime = body.getType() instanceof TreeType ? constraints.heuristic(1) : Annotation.EMPTY;

    sub.putFunctionAnnotation(name, new Pair<>(q, qprime));

    // if (!(signature.getType().getTo() instanceof TreeType)) {
    final var result =
        new Pair<>(
            new AnnotatedType(signature.getType().getFrom(), q),
            new AnnotatedType(body.getType(), body.inferAnnotations(sub)));

    constraints.solve();
    System.out.println(q.substitute(constraints));
    return result;
  }

  public void printTo(PrintStream out) {
    out.print("(* ");
    out.print(name);
    out.print(" :: ");
    out.print(signature);
    out.println(" *)");
    out.print(name);
    out.print(" ");
    out.print(String.join(" ", arguments));
    out.print(" = ");
    body.printTo(out, 1);
  }
}
