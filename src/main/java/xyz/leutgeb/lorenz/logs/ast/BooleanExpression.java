package xyz.leutgeb.lorenz.logs.ast;

import lombok.Data;
import lombok.NonNull;
import xyz.leutgeb.lorenz.logs.Context;
import xyz.leutgeb.lorenz.logs.type.BoolType;
import xyz.leutgeb.lorenz.logs.type.Type;
import xyz.leutgeb.lorenz.logs.type.TypeError;
import xyz.leutgeb.lorenz.logs.unification.UnificationError;

@Data
public class BooleanExpression extends Expression {
  @NonNull private final Expression left;

  @NonNull private final ComparisonOperator operator;

  @NonNull private final Expression right;

  @Override
  public Type infer(Context context) throws UnificationError, TypeError {
    context.getProblem().add(right.infer(context), left.infer(context));
    return BoolType.INSTANCE;
  }
}
