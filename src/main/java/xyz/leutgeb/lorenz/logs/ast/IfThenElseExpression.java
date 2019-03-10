package xyz.leutgeb.lorenz.logs.ast;

import lombok.Data;
import xyz.leutgeb.lorenz.logs.Context;
import xyz.leutgeb.lorenz.logs.type.Type;
import xyz.leutgeb.lorenz.logs.type.TypeError;
import xyz.leutgeb.lorenz.logs.unification.UnificationError;

@Data
public class IfThenElseExpression extends Expression {
  private final BooleanExpression condition;
  private final Expression truthy;
  private final Expression falsy;

  @Override
  public Type infer(Context context) throws UnificationError, TypeError {
    var result = context.getProblem().fresh();
    context.getProblem().add(result, truthy.infer(context));
    context.getProblem().add(result, falsy.infer(context));
    condition.infer(context);
    return result;
  }
}
