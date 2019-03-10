package xyz.leutgeb.lorenz.logs.ast;

import lombok.Data;
import xyz.leutgeb.lorenz.logs.Context;
import xyz.leutgeb.lorenz.logs.type.Type;
import xyz.leutgeb.lorenz.logs.type.TypeError;
import xyz.leutgeb.lorenz.logs.unification.UnificationError;

@Data
public class LetExpression extends Expression {
  private final Identifier declared;
  private final Expression body;

  @Override
  public Type infer(Context context) throws UnificationError, TypeError {
    var result = context.getProblem().fresh();
    context.getProblem().add(result, declared.infer(context));
    context.getProblem().add(result, body.infer(context));
    return result;
  }
}
