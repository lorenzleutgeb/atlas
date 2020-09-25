package xyz.leutgeb.lorenz.lac.ast;

import static com.google.common.collect.Sets.intersection;
import static xyz.leutgeb.lorenz.lac.util.Util.*;

import java.io.PrintStream;
import java.util.Map;
import java.util.Set;
import java.util.Stack;
import java.util.stream.Stream;
import lombok.Data;
import lombok.EqualsAndHashCode;
import lombok.extern.slf4j.Slf4j;
import xyz.leutgeb.lorenz.lac.ast.sources.Derived;
import xyz.leutgeb.lorenz.lac.ast.sources.Source;
import xyz.leutgeb.lorenz.lac.typing.simple.TypeError;
import xyz.leutgeb.lorenz.lac.typing.simple.types.BoolType;
import xyz.leutgeb.lorenz.lac.typing.simple.types.Type;
import xyz.leutgeb.lorenz.lac.unification.UnificationContext;
import xyz.leutgeb.lorenz.lac.unification.UnificationError;
import xyz.leutgeb.lorenz.lac.util.IntIdGenerator;
import xyz.leutgeb.lorenz.lac.util.Pair;

@Data
@EqualsAndHashCode(callSuper = true)
@Slf4j
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
    var result = context.fresh();
    context.addIfNotEqual(result, truthy.infer(context));
    context.addIfNotEqual(result, falsy.infer(context));
    context.addIfNotEqual(BoolType.INSTANCE, condition.infer(context));
    return result;
  }

  @Override
  public Expression normalize(Stack<Normalization> context, IntIdGenerator idGenerator) {
    // TODO(lorenz.leutgeb): Only create a new expression if normalization actually changes
    // something!
    return new IfThenElseExpression(
        Derived.anf(this),
        // condition.forceImmediate(context, idGenerator),
        condition,
        truthy.normalizeAndBind(idGenerator),
        falsy.normalizeAndBind(idGenerator));
  }

  @Override
  public Expression rename(Map<String, String> renaming) {
    // TODO(lorenz.leutgeb): Only create new expression if a rename is really necessary!
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
  public Expression unshare(IntIdGenerator idGenerator, boolean lazy) {
    /*
    if (!(condition instanceof Identifier)) {
      throw new IllegalStateException("must be in anf");
    }
     */

    // First, ensure that subexpressions are unshared.
    final var newT = truthy.unshare(idGenerator, lazy);
    final var newF = falsy.unshare(idGenerator, lazy);

    Set<Identifier> freeT = newT.freeVariables();
    Set<Identifier> freeF = newF.freeVariables();
    Set<Identifier> cond = condition.freeVariables();

    var intersection = intersection(freeT, freeF);

    if (intersection.size() == 0) {
      return new IfThenElseExpression(source, condition, newT, newF, type);
    }

    if (lazy) {
      log.info("Did not create sharing expression for {} because unsharing is lazy.", intersection);
      return new IfThenElseExpression(source, condition, newT, newF, type);
    }

    if (!intersection(freeT, cond).isEmpty() || !intersection(freeF, cond).isEmpty()) {
      throw notImplemented(
          "unsharing if-then-else across condition and branch is not supported (can only unshare between then-branch and else-branch)");
    }

    var target = pick(intersection);
    var down = ShareExpression.clone(target, idGenerator);
    var result = ShareExpression.rename(target, down, Pair.of(newT, newF));

    Expression newThis =
        new IfThenElseExpression(source, condition, result.getLeft(), result.getRight(), type);

    if (intersection.size() > 1) {
      newThis = newThis.unshare(idGenerator, lazy);
    }

    return new ShareExpression(this, target, down, newThis);
  }

  @Override
  public String toString() {
    return "if " + condition + " then " + truthy.terminalOrBox() + " else " + falsy.terminalOrBox();
  }
}
