package xyz.leutgeb.lorenz.logs.ast;

import java.io.PrintStream;
import java.util.Stack;
import java.util.stream.Stream;
import lombok.EqualsAndHashCode;
import lombok.NonNull;
import lombok.Value;
import org.hipparchus.util.Pair;
import xyz.leutgeb.lorenz.logs.Context;
import xyz.leutgeb.lorenz.logs.type.BoolType;
import xyz.leutgeb.lorenz.logs.type.Type;
import xyz.leutgeb.lorenz.logs.type.TypeClass;
import xyz.leutgeb.lorenz.logs.type.TypeConstraint;
import xyz.leutgeb.lorenz.logs.type.TypeError;
import xyz.leutgeb.lorenz.logs.type.TypeVariable;
import xyz.leutgeb.lorenz.logs.unification.Substitution;
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
    // The next two lines remove polymorphism in favor of the "abstract base signature".
    // context.getProblem().add(right.infer(context), BaseType.INSTANCE);
    // context.getProblem().add(left.infer(context), BaseType.INSTANCE);

    var ty = context.getProblem().fresh();
    context.getProblem().add(this, right.infer(context), ty);
    context.getProblem().add(this, left.infer(context), ty);

    if (operator == ComparisonOperator.EQ || operator == ComparisonOperator.NE) {
      context
          .getProblem()
          .addConstraint(
              new TypeConstraint(TypeClass.EQ, new Substitution(TypeVariable.GAMMA, ty)));
    } else {
      context
          .getProblem()
          .addConstraint(
              new TypeConstraint(TypeClass.ORD, new Substitution(TypeVariable.GAMMA, ty)));
    }
    return BoolType.INSTANCE;
  }

  @Override
  public Expression normalize(Stack<Pair<Identifier, Expression>> context) {
    return new BooleanExpression(
        Derived.anf(source), left.normalize(context), operator, right.normalize(context));
  }

  @Override
  public void printTo(PrintStream out, int indentation) {
    left.printTo(out, indentation);
    out.print(" ");
    operator.printTo(out);
    out.print(" ");
    right.printTo(out, indentation);
  }

  @Override
  public boolean isImmediate() {
    return left.isImmediate() && right.isImmediate();
  }
}
