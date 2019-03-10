package xyz.leutgeb.lorenz.logs.ast;

import java.util.ArrayList;
import java.util.List;
import lombok.Data;
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
      sub.insert(argument, var);
    }

    Type to = sub.getProblem().fresh();
    Type result = new FunctionType(from, to);
    sub.insert(name, result);

    sub.getProblem().add(to, body.infer(sub));

    // Now we are set for unification!
    var solution = sub.getProblem().solve();
    return (FunctionType) (solution.apply(result)).generalize(new Generalizer());
  }
}
