package xyz.leutgeb.lorenz.logs.ast;

import java.io.PrintStream;
import java.util.ArrayList;
import java.util.List;
import java.util.Map;
import java.util.Stack;
import lombok.Data;
import lombok.extern.log4j.Log4j2;
import org.hipparchus.util.Pair;
import xyz.leutgeb.lorenz.logs.Context;
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

  public Pair<Annotation, Annotation> inferAnnotation(
      Map<String, Pair<Annotation, Annotation>> functionAnnotations)
      throws UnificationError, TypeError {

    if (signature == null) {
      throw new IllegalStateException();
    }

    final var constraints = new Constraints();

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

    functionAnnotations.put(name, new Pair<>(q, qprime));
    final var globals = new AnnotatingGlobals(functionAnnotations, constraints);

    // if (!(signature.getType().getTo() instanceof TreeType)) {
    final var result = new Pair<>(q, body.inferAnnotations(initialGammaQ, globals));

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
