package xyz.leutgeb.lorenz.atlas.ast.expressions;

import static com.google.common.collect.Sets.intersection;
import static xyz.leutgeb.lorenz.atlas.util.Util.*;

import java.io.PrintStream;
import java.util.Map;
import java.util.Set;
import java.util.Stack;
import java.util.stream.Stream;
import lombok.Data;
import lombok.EqualsAndHashCode;
import lombok.extern.slf4j.Slf4j;
import xyz.leutgeb.lorenz.atlas.ast.Normalization;
import xyz.leutgeb.lorenz.atlas.ast.sources.Derived;
import xyz.leutgeb.lorenz.atlas.ast.sources.Source;
import xyz.leutgeb.lorenz.atlas.typing.simple.TypeError;
import xyz.leutgeb.lorenz.atlas.typing.simple.types.BoolType;
import xyz.leutgeb.lorenz.atlas.typing.simple.types.Type;
import xyz.leutgeb.lorenz.atlas.unification.UnificationContext;
import xyz.leutgeb.lorenz.atlas.util.IntIdGenerator;
import xyz.leutgeb.lorenz.atlas.util.Pair;

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
  public xyz.leutgeb.lorenz.atlas.typing.simple.types.Type inferInternal(UnificationContext context)
      throws TypeError {
    var result = context.fresh();
    context.addEquivalenceIfNotEqual(result, truthy.infer(context), source);
    context.addEquivalenceIfNotEqual(result, falsy.infer(context), source);
    context.addEquivalenceIfNotEqual(BoolType.INSTANCE, condition.infer(context), source);
    return result;
  }

  @Override
  public Expression normalize(Stack<Normalization> context, IntIdGenerator idGenerator) {
    // TODO(lorenzleutgeb): Only create a new expression if normalization actually changes
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
    // TODO(lorenzleutgeb): Only create new expression if a rename is really necessary!
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
  public void printHaskellTo(PrintStream out, int indentation, String currentFunction) {
    out.print("if ");
    condition.printHaskellTo(out, indentation, currentFunction);
    out.println();
    indent(out, indentation);
    out.print("then ");
    truthy.printHaskellTo(out, indentation + 1, currentFunction);
    out.println();
    indent(out, indentation);
    out.print("else ");
    falsy.printHaskellTo(out, indentation + 1, currentFunction);
  }

  @Override
  public void printJavaTo(PrintStream out, int indentation, String currentFunction) {
    indent(out, indentation);
    out.print("if (");
    condition.printJavaTo(out, indentation, currentFunction);
    out.println();
    indent(out, indentation);
    out.println(") {");

    indent(out, indentation);
    if (truthy.isTerminal()) {
      indent(out, indentation + 1);
      out.println("return (");
      truthy.printJavaTo(out, indentation + 1, currentFunction);
      indent(out, indentation + 1);
      out.println(");");
    } else {
      truthy.printJavaTo(out, indentation + 1, currentFunction);
    }

    // out.println();
    indent(out, indentation);
    out.println("} else {");

    indent(out, indentation);
    if (falsy.isTerminal()) {
      indent(out, indentation + 1);
      out.println("return (");
      falsy.printJavaTo(out, indentation + 1, currentFunction);
      indent(out, indentation + 1);
      out.println(");");
    } else {
      falsy.printJavaTo(out, indentation + 1, currentFunction);
    }

    indent(out, indentation);
    out.println("}");
  }

  @Override
  public Expression unshare(IntIdGenerator idGenerator, boolean lazy) {
    /*
    if (!(condition instanceof IdentifierExpression)) {
      throw new IllegalStateException("must be in anf");
    }
     */

    // First, ensure that subexpressions are unshared.
    final var newT = truthy.unshare(idGenerator, lazy);
    final var newF = falsy.unshare(idGenerator, lazy);

    Set<IdentifierExpression> freeT = newT.freeVariables();
    Set<IdentifierExpression> freeF = newF.freeVariables();
    Set<IdentifierExpression> cond = condition.freeVariables();

    var intersection = intersection(freeT, freeF);

    if (intersection.size() == 0) {
      return new IfThenElseExpression(source, condition, newT, newF, type);
    }

    if (lazy) {
      // log.info("Did not create sharing expression for {} because unsharing is lazy.",
      // intersection);
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
