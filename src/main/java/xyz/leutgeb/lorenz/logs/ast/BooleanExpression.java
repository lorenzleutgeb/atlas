package xyz.leutgeb.lorenz.logs.ast;

import java.util.Stack;
import java.util.stream.Stream;
import lombok.EqualsAndHashCode;
import lombok.NonNull;
import lombok.Value;
import org.hipparchus.util.Pair;
import xyz.leutgeb.lorenz.logs.Context;
import xyz.leutgeb.lorenz.logs.type.BoolType;
import xyz.leutgeb.lorenz.logs.type.Type;
import xyz.leutgeb.lorenz.logs.type.TypeError;
import xyz.leutgeb.lorenz.logs.unification.UnificationError;

@Value
@EqualsAndHashCode(callSuper = true)
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
  public Stream<? extends Expression> getChildren() {
    return Stream.of(left, right);
  }

  @Override
  public Type inferInternal(Context context) throws UnificationError, TypeError {
    // The next two lines remove polymorphism in favor of the "abstract base type".
    // context.getProblem().add(right.infer(context), BaseType.INSTANCE);
    // context.getProblem().add(left.infer(context), BaseType.INSTANCE);

    context.getProblem().add(right.infer(context), left.infer(context));
    return BoolType.INSTANCE;
  }

  @Override
  public Expression normalize(Stack<Pair<Identifier, Expression>> context) {
    if (left.isImmediate() && right.isImmediate()) {
      return this;
    }

    var newLeft = left;
    var newRight = right;

    if (!left.isImmediate()) {
      Identifier id = Identifier.getSugar();
      context.push(new Pair<>(id, left.normalize(context)));
      newLeft = id;
    }

    if (!right.isImmediate()) {
      Identifier id = Identifier.getSugar();
      context.push(new Pair<>(id, right.normalize(context)));
      newRight = id;
    }

    return new BooleanExpression(source, newLeft, operator, newRight);
  }
}
