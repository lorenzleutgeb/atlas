package xyz.leutgeb.lorenz.logs.ast;

import java.util.List;
import lombok.Data;
import xyz.leutgeb.lorenz.logs.Context;
import xyz.leutgeb.lorenz.logs.type.Type;
import xyz.leutgeb.lorenz.logs.type.TypeError;
import xyz.leutgeb.lorenz.logs.unification.UnificationError;

@Data
public class MatchExpression extends Expression {
  private final Expression test;
  private final List<Case> cases;

  @Override
  public Type infer(Context context) throws UnificationError, TypeError {
    final var result = context.getProblem().fresh();
    final var testType = context.getProblem().fresh();
    context.getProblem().add(testType, test.infer(context));

    for (Case it : cases) {
      var matcher = it.getMatcher();
      var body = it.getBody();

      // The matcher is an identifier. We therefore create a new context, add a type variable for it
      // and decompose with the type of test.
      if (matcher instanceof Identifier) {
        var id = (Identifier) matcher;
        Context sub = new Context(context);
        var fresh = sub.getProblem().fresh();
        sub.put(id.getName(), fresh);
        sub.getProblem().add(testType, matcher.infer(sub));
        sub.getProblem().add(result, body.infer(sub));
      } else if (matcher instanceof Tuple) {
        var tuple = (Tuple) matcher;

        if (tuple.getElements().size() != 3) {
          throw new UnsupportedOperationException();
        }

        Context sub = new Context(context);
        for (int i = 0; i < 3; i++) {
          if (!(tuple.getElements().get(i) instanceof Identifier)) {
            throw new UnsupportedOperationException();
          }
          String name = ((Identifier) tuple.getElements().get(i)).getName();
          var fresh = sub.getProblem().fresh();
          sub.put(name, fresh);
        }

        sub.getProblem().add(testType, it.getMatcher().infer(sub));
        sub.getProblem().add(result, it.getBody().infer(sub));
      } else {
        throw new UnsupportedOperationException();
      }
    }
    return result;
  }
}
