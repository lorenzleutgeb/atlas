package xyz.leutgeb.lorenz.lac.ast;

import static com.google.common.collect.Sets.union;

import java.io.PrintStream;
import java.util.HashSet;
import java.util.Map;
import java.util.Set;
import java.util.Stack;
import java.util.stream.Stream;
import lombok.EqualsAndHashCode;
import lombok.NonNull;
import lombok.Value;
import org.hipparchus.util.Pair;
import xyz.leutgeb.lorenz.lac.IntIdGenerator;
import xyz.leutgeb.lorenz.lac.ast.sources.Derived;
import xyz.leutgeb.lorenz.lac.ast.sources.Source;
import xyz.leutgeb.lorenz.lac.typing.simple.TypeClass;
import xyz.leutgeb.lorenz.lac.typing.simple.TypeConstraint;
import xyz.leutgeb.lorenz.lac.typing.simple.TypeError;
import xyz.leutgeb.lorenz.lac.typing.simple.types.BoolType;
import xyz.leutgeb.lorenz.lac.typing.simple.types.Type;
import xyz.leutgeb.lorenz.lac.unification.UnificationContext;
import xyz.leutgeb.lorenz.lac.unification.UnificationError;

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

  private BooleanExpression(
      Source source, Expression left, ComparisonOperator operator, Expression right, Type type) {
    super(source);
    this.left = left;
    this.operator = operator;
    this.right = right;
    this.type = type;
  }

  @Override
  public Stream<? extends Expression> getChildren() {
    return Stream.of(left, right);
  }

  @Override
  public Type inferInternal(UnificationContext context) throws UnificationError, TypeError {
    // The next two lines hide polymorphism in favor of the "abstract base signature".
    // context.getProblem().add(right.infer(context), BaseType.INSTANCE);
    // context.getProblem().add(left.infer(context), BaseType.INSTANCE);

    var ty = context.getProblem().fresh();
    context.getProblem().addIfNotEqual(this, right.infer(context), ty);
    context.getProblem().addIfNotEqual(this, left.infer(context), ty);

    if (operator == ComparisonOperator.EQ || operator == ComparisonOperator.NE) {
      context.getProblem().addConstraint(new TypeConstraint(TypeClass.EQ, ty));
    } else {
      context.getProblem().addConstraint(new TypeConstraint(TypeClass.ORD, ty));
    }
    return BoolType.INSTANCE;
  }

  @Override
  public Expression normalize(
      Stack<Pair<Identifier, Expression>> context, IntIdGenerator idGenerator) {
    // TODO: Only create new expression if necessary!
    return new BooleanExpression(
        Derived.anf(this),
        left.normalize(context, idGenerator),
        operator,
        right.normalize(context, idGenerator));
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
  public void printHaskellTo(PrintStream out, int indentation) {
    left.printHaskellTo(out, indentation);
    out.print(" ");
    operator.printTo(out);
    out.print(" ");
    right.printHaskellTo(out, indentation);
  }

  @Override
  public boolean isImmediate() {
    return false;
    // return left.isImmediate() && right.isImmediate();
  }

  @Override
  public Expression rename(Map<String, String> renaming) {
    // TODO: Only create new expression if necessary.
    return new BooleanExpression(
        Derived.rename(this), left.rename(renaming), operator, right.rename(renaming), type);
  }

  @Override
  public String toString() {
    return left + " " + operator + " " + right;
  }

  @Override
  public Expression unshare(Map<String, Integer> unshared, IntIdGenerator idGenerator) {
    if (!(left instanceof Identifier) || !(right instanceof Identifier)) {
      throw new IllegalStateException("must be in anf");
    }
    if (!left.equals(right)) {
      return this;
    }
    var down = ShareExpression.clone((Identifier) left, unshared, idGenerator);
    return new ShareExpression(
        this,
        (Identifier) left,
        down,
        new BooleanExpression(source, down.getFirst(), operator, down.getSecond(), type));
  }

  @Override
  public Set<Identifier> freeVariables() {
    return new HashSet<>(union(left.freeVariables(), right.freeVariables()));
  }

  @Override
  public boolean isTerminal() {
    return true;
  }
}
