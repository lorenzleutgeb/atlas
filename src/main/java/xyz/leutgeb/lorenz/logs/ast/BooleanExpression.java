package xyz.leutgeb.lorenz.logs.ast;

import java.util.Stack;
import lombok.NonNull;
import lombok.Value;
import org.hipparchus.util.Pair;
import xyz.leutgeb.lorenz.logs.Context;
import xyz.leutgeb.lorenz.logs.type.BoolType;
import xyz.leutgeb.lorenz.logs.type.Type;
import xyz.leutgeb.lorenz.logs.type.TypeError;
import xyz.leutgeb.lorenz.logs.unification.UnificationError;

@Value
public class BooleanExpression extends Expression {
  @NonNull Expression left;
  @NonNull ComparisonOperator operator;
  @NonNull Expression right;

  public BooleanExpression(
      Source source,
      @NonNull Expression left,
      @NonNull ComparisonOperator operator,
      @NonNull Expression right) {
    super(source);
    this.left = left;
    this.operator = operator;
    this.right = right;
  }

  @Override
  public Type infer(Context context) throws UnificationError, TypeError {
    // The next two lines remove polymorphism in favor of the "abstract base type".
    // context.getProblem().add(right.infer(context), BaseType.INSTANCE);
    // context.getProblem().add(left.infer(context), BaseType.INSTANCE);

    context.getProblem().add(right.infer(context), left.infer(context));
    return BoolType.INSTANCE;
  }

  @Override
  public Expression normalize(Stack<Pair<Identifier, Expression>> context) {
    return this;
  }
}
