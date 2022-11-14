package xyz.leutgeb.lorenz.atlas.ast.expressions;

import lombok.EqualsAndHashCode;
import lombok.NonNull;
import lombok.Value;
import xyz.leutgeb.lorenz.atlas.ast.ArithmeticOperator;
import xyz.leutgeb.lorenz.atlas.ast.Normalization;
import xyz.leutgeb.lorenz.atlas.ast.sources.Derived;
import xyz.leutgeb.lorenz.atlas.ast.sources.Source;
import xyz.leutgeb.lorenz.atlas.typing.simple.TypeClass;
import xyz.leutgeb.lorenz.atlas.typing.simple.TypeConstraint;
import xyz.leutgeb.lorenz.atlas.typing.simple.TypeError;
import xyz.leutgeb.lorenz.atlas.typing.simple.types.BoolType;
import xyz.leutgeb.lorenz.atlas.typing.simple.types.Type;
import xyz.leutgeb.lorenz.atlas.unification.UnificationContext;
import xyz.leutgeb.lorenz.atlas.util.IntIdGenerator;

import java.io.PrintStream;
import java.util.Map;
import java.util.Stack;
import java.util.stream.Stream;

@Value
@EqualsAndHashCode(callSuper = true)
public class ArithmeticExpression extends Expression {

  @NonNull Expression left;
  @NonNull ArithmeticOperator operator;
  @NonNull Expression right;

  public ArithmeticExpression(
      Source source,
      @NonNull Expression left,
      @NonNull ArithmeticOperator operator,
      @NonNull Expression right) {
    super(source);
    this.left = left;
    this.operator = operator;
    this.right = right;
  }

  private ArithmeticExpression(
      Source source, Expression left, ArithmeticOperator operator, Expression right, Type type) {
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
  public Type inferInternal(UnificationContext context) throws TypeError {
    var ty = context.fresh();
    context.addEquivalenceIfNotEqual(right.infer(context), ty, source);
    context.addEquivalenceIfNotEqual(left.infer(context), ty, source);
    context
        .getSignature(context.getFunctionInScope(), source)
        .addConstraint(new TypeConstraint(TypeClass.NUM, ty));
    return ty;
  }

  @Override
  public Expression normalize(Stack<Normalization> context, IntIdGenerator idGenerator) {
    // TODO(lorenzleutgeb): Only create new expression if necessary!
    return new ArithmeticExpression(
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
  public void printHaskellTo(PrintStream out, int indentation, String currentFunction) {
    left.printHaskellTo(out, indentation, currentFunction);
    out.print(" ");
    operator.printHaskellTo(out);
    out.print(" ");
    right.printHaskellTo(out, indentation, currentFunction);
  }

  @Override
  public void printJavaTo(PrintStream out, int indentation, String currentFunction) {
    operator.printJavaTo(
        ((IdentifierExpression) left).getName(), ((IdentifierExpression) right).getName(), out);
  }

  @Override
  public Expression unshare(IntIdGenerator idGenerator, boolean lazy) {
    return this;
  }

  @Override
  public boolean isImmediate() {
    return false;
  }

  @Override
  public Expression rename(Map<String, String> renaming) {
    if (freeVariables().stream()
        .map(IdentifierExpression::getName)
        .anyMatch(renaming::containsKey)) {
      return new ArithmeticExpression(
          Derived.rename(this), left.rename(renaming), operator, right.rename(renaming), type);
    }
    return this;
  }

  @Override
  public String toString() {
    return left + " " + operator + " " + right;
  }

  @Override
  public boolean isTerminal() {
    return true;
  }
}
