package xyz.leutgeb.lorenz.logs.ast;

import java.util.Stack;
import lombok.Data;
import org.hipparchus.util.Pair;
import xyz.leutgeb.lorenz.logs.Context;
import xyz.leutgeb.lorenz.logs.type.Type;
import xyz.leutgeb.lorenz.logs.type.TypeError;
import xyz.leutgeb.lorenz.logs.unification.UnificationError;

@Data
public class IfThenElseExpression extends Expression {
  private final BooleanExpression condition;
  private final Expression truthy;
  private final Expression falsy;

  public IfThenElseExpression(
      Source source, BooleanExpression condition, Expression truthy, Expression falsy) {
    super(source);
    this.condition = condition;
    this.truthy = truthy;
    this.falsy = falsy;
  }

  @Override
  public Type infer(Context context) throws UnificationError, TypeError {
    var result = context.getProblem().fresh();
    context.getProblem().add(result, truthy.infer(context));
    context.getProblem().add(result, falsy.infer(context));
    condition.infer(context);
    return result;
  }

  @Override
  public Expression normalize(Stack<Pair<Identifier, Expression>> context) {
    return new IfThenElseExpression(
        source, (BooleanExpression) condition.normalize(context), truthy, falsy);
  }

  /*
  public IfThenElseExpression normalize() {
    return new IfThenElseExpression(
        this.interval, condition, truthy.normalize(), falsy.normalize());
  }
   */
}
