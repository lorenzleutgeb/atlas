package xyz.leutgeb.lorenz.logs.ast;

import com.google.common.collect.Sets;
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
import xyz.leutgeb.lorenz.logs.Context;
import xyz.leutgeb.lorenz.logs.ast.sources.Derived;
import xyz.leutgeb.lorenz.logs.ast.sources.Renamed;
import xyz.leutgeb.lorenz.logs.ast.sources.Source;
import xyz.leutgeb.lorenz.logs.resources.AnnotatingContext;
import xyz.leutgeb.lorenz.logs.resources.AnnotatingGlobals;
import xyz.leutgeb.lorenz.logs.resources.Annotation;
import xyz.leutgeb.lorenz.logs.typing.TypeClass;
import xyz.leutgeb.lorenz.logs.typing.TypeConstraint;
import xyz.leutgeb.lorenz.logs.typing.TypeError;
import xyz.leutgeb.lorenz.logs.typing.types.BoolType;
import xyz.leutgeb.lorenz.logs.typing.types.Type;
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

  public BooleanExpression(
      Source source, Expression left, ComparisonOperator operator, Expression right, Type type) {
    super(source);
    this.left = left;
    this.operator = operator;
    this.right = right;
    this.type = type;
    this.typeResolved = true;
  }

  @Override
  public Stream<? extends Expression> getChildren() {
    return Stream.of(left, right);
  }

  @Override
  public Type inferInternal(Context context) throws UnificationError, TypeError {
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
  public Annotation inferAnnotationsInternal(AnnotatingContext context, AnnotatingGlobals globals)
      throws UnificationError, TypeError {
    return Annotation.empty();
  }

  @Override
  public Expression rename(Map<String, String> renaming) {
    return new BooleanExpression(
        new Renamed(source), left.rename(renaming), operator, right.rename(renaming), type);
  }

  @Override
  public String toString() {
    return left + " " + operator + " " + right;
  }

  @Override
  public Expression unshare() {
    if (!(left instanceof Identifier) || !(right instanceof Identifier)) {
      throw new IllegalStateException("must be in anf");
    }
    if (!left.equals(right)) {
      return this;
    }
    var down = ShareExpression.clone((Identifier) left);
    return new ShareExpression(
        (Identifier) left,
        down,
        new BooleanExpression(source, down.getFirst(), operator, down.getSecond(), type));
  }

  @Override
  public Set<String> freeVariables() {
    return new HashSet<>(Sets.union(left.freeVariables(), right.freeVariables()));
  }
}
