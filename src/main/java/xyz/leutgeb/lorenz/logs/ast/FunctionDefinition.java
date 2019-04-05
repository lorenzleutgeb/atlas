package xyz.leutgeb.lorenz.logs.ast;

import java.util.ArrayList;
import java.util.List;
import java.util.Stack;
import lombok.Data;
import org.hipparchus.util.Pair;
import xyz.leutgeb.lorenz.logs.Context;
import xyz.leutgeb.lorenz.logs.type.FunctionType;
import xyz.leutgeb.lorenz.logs.type.Generalizer;
import xyz.leutgeb.lorenz.logs.type.Type;
import xyz.leutgeb.lorenz.logs.type.TypeError;
import xyz.leutgeb.lorenz.logs.unification.UnificationError;

@Data
public class FunctionDefinition {
  private final String name;
  private final List<String> arguments;
  private final Expression body;

  public FunctionType infer(Context context) throws UnificationError, TypeError {
    Context sub = new Context(context);
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
    var solution = sub.getProblem().solve();
    return (FunctionType) (solution.apply(result)).generalize(new Generalizer());
  }

  public FunctionDefinition normalize() {
    var context = new Stack<Pair<Identifier, Expression>>();
    return new FunctionDefinition(name, arguments, body.normalize(context).bindAll(context));
  }
}
