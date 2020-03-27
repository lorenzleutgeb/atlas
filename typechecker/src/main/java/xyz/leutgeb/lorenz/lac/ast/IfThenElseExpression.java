package xyz.leutgeb.lorenz.lac.ast;

import static com.google.common.collect.Sets.intersection;
import static xyz.leutgeb.lorenz.lac.Util.indent;
import static xyz.leutgeb.lorenz.lac.Util.notImplemented;
import static xyz.leutgeb.lorenz.lac.Util.pick;

import java.io.PrintStream;
import java.util.Map;
import java.util.Set;
import java.util.Stack;
import java.util.stream.Stream;
import lombok.Data;
import lombok.EqualsAndHashCode;
import lombok.extern.log4j.Log4j2;
import org.hipparchus.util.Pair;
import xyz.leutgeb.lorenz.lac.IntIdGenerator;
import xyz.leutgeb.lorenz.lac.ast.sources.Derived;
import xyz.leutgeb.lorenz.lac.ast.sources.Source;
import xyz.leutgeb.lorenz.lac.typing.simple.TypeError;
import xyz.leutgeb.lorenz.lac.typing.simple.types.BoolType;
import xyz.leutgeb.lorenz.lac.typing.simple.types.Type;
import xyz.leutgeb.lorenz.lac.unification.UnificationContext;
import xyz.leutgeb.lorenz.lac.unification.UnificationError;

@Data
@EqualsAndHashCode(callSuper = true)
@Log4j2
public class IfThenElseExpression extends Expression {
  private final Expression condition;
  private final Expression truthy;
  private final Expression falsy;

  public IfThenElseExpression(
      Source source, Expression condition, Expression truthy, Expression falsy) {
    super(source);
    this.condition = condition;
    this.truthy = truthy;
    this.falsy = falsy;
  }

  private IfThenElseExpression(
      Source source, Expression condition, Expression truthy, Expression falsy, Type type) {
    super(source, type);
    this.condition = condition;
    this.truthy = truthy;
    this.falsy = falsy;
  }

  @Override
  public Stream<? extends Expression> getChildren() {
    return Stream.of(condition, truthy, falsy);
  }

  @Override
  public xyz.leutgeb.lorenz.lac.typing.simple.types.Type inferInternal(UnificationContext context)
      throws UnificationError, TypeError {
    var result = context.getProblem().fresh();
    context.getProblem().addIfNotEqual(this, result, truthy.infer(context));
    context.getProblem().addIfNotEqual(this, result, falsy.infer(context));
    context.getProblem().addIfNotEqual(this, BoolType.INSTANCE, condition.infer(context));
    return result;
  }

  @Override
  public Expression normalize(
      Stack<Pair<Identifier, Expression>> context, IntIdGenerator idGenerator) {
    // TODO: Only create a new expression if normalization actually changes something!
    return new IfThenElseExpression(
        Derived.anf(this),
        condition.forceImmediate(context, idGenerator),
        truthy.normalizeAndBind(idGenerator),
        falsy.normalizeAndBind(idGenerator));
  }

  @Override
  public Expression rename(Map<String, String> renaming) {
    // TODO: Only create new expression if a rename is really necessary!
    return new IfThenElseExpression(
        Derived.rename(this),
        condition.rename(renaming),
        truthy.rename(renaming),
        falsy.rename(renaming),
        type);
  }

  @Override
  public void printTo(PrintStream out, int indentation) {
    out.print("if ");
    condition.printTo(out, indentation);
    out.println();
    indent(out, indentation);
    out.print("then ");
    truthy.printTo(out, indentation + 1);
    out.println();
    indent(out, indentation);
    out.print("else ");
    falsy.printTo(out, indentation + 1);
  }

  @Override
  public void printHaskellTo(PrintStream out, int indentation) {
    out.print("if ");
    condition.printHaskellTo(out, indentation);
    out.println();
    indent(out, indentation);
    out.print("then ");
    truthy.printHaskellTo(out, indentation + 1);
    out.println();
    indent(out, indentation);
    out.print("else ");
    falsy.printHaskellTo(out, indentation + 1);
  }

  @Override
  public Expression unshare(Map<String, Integer> unshared, IntIdGenerator idGenerator) {
    if (!(condition instanceof Identifier)) {
      throw new IllegalStateException("must be in anf");
    }

    // First, ensure that subexpressions are unshared.
    final var newT = truthy.unshare(unshared, idGenerator);
    final var newF = falsy.unshare(unshared, idGenerator);

    Set<Identifier> freeT = newT.freeVariables();
    Set<Identifier> freeF = newF.freeVariables();
    Set<Identifier> cond = condition.freeVariables();

    if (!intersection(freeT, cond).isEmpty() || !intersection(freeF, cond).isEmpty()) {
      throw notImplemented(
          "unsharing if-then-else across condition and branch is not supported (can only unshare between then-branch and else-branch)");
    }

    var intersection = intersection(freeT, freeF);

    if (intersection.size() == 0) {
      return new IfThenElseExpression(source, condition, newT, newF, type);
    }

    var target = pick(intersection);
    var down = ShareExpression.clone(target, unshared, idGenerator);
    var result = ShareExpression.rename(target, down, Pair.create(newT, newF));

    var replacement =
        new ShareExpression(
            this,
            target,
            down,
            new IfThenElseExpression(
                source, condition, result.getFirst(), result.getSecond(), type));

    return intersection.size() > 1 ? replacement.unshare(unshared, idGenerator) : replacement;
  }

  @Override
  public String toString() {
    return "if " + condition + " then " + truthy.terminalOrBox() + " else " + falsy.terminalOrBox();
  }
}
