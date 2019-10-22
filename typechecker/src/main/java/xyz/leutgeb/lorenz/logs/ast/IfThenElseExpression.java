package xyz.leutgeb.lorenz.logs.ast;

import static xyz.leutgeb.lorenz.logs.Util.indent;

import com.google.common.collect.Sets;
import java.io.PrintStream;
import java.util.Map;
import java.util.Set;
import java.util.Stack;
import java.util.stream.Stream;
import lombok.Data;
import lombok.EqualsAndHashCode;
import lombok.extern.log4j.Log4j2;
import org.hipparchus.util.Pair;
import xyz.leutgeb.lorenz.logs.Context;
import xyz.leutgeb.lorenz.logs.ast.sources.Derived;
import xyz.leutgeb.lorenz.logs.ast.sources.Predefined;
import xyz.leutgeb.lorenz.logs.ast.sources.Renamed;
import xyz.leutgeb.lorenz.logs.ast.sources.Source;
import xyz.leutgeb.lorenz.logs.resources.AnnotatingContext;
import xyz.leutgeb.lorenz.logs.resources.AnnotatingGlobals;
import xyz.leutgeb.lorenz.logs.resources.Annotation;
import xyz.leutgeb.lorenz.logs.typing.TypeError;
import xyz.leutgeb.lorenz.logs.typing.types.BoolType;
import xyz.leutgeb.lorenz.logs.typing.types.Type;
import xyz.leutgeb.lorenz.logs.unification.UnificationError;

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
    /*
    if (!(condition instanceof BooleanExpression)) {
      log.warn(
          "Encountered an if-then-else expression which does not use a simple comparison as condition. Resource bound inference will not work.");
    }
     */
    this.condition = condition;
    this.truthy = truthy;
    this.falsy = falsy;
  }

  public IfThenElseExpression(
      Source source, Expression condition, Expression truthy, Expression falsy, Type type) {
    super(source, type);
    if (!(condition instanceof BooleanExpression)) {
      log.warn(
          "Encountered an if-then-else expression which does not use a simple comparison as condition. Resource bound inference will not work.");
    }
    this.condition = condition;
    this.truthy = truthy;
    this.falsy = falsy;
  }

  @Override
  public Stream<? extends Expression> getChildren() {
    return Stream.of(condition, truthy, falsy);
  }

  @Override
  public xyz.leutgeb.lorenz.logs.typing.types.Type inferInternal(Context context)
      throws UnificationError, TypeError {
    var result = context.getProblem().fresh();
    context.getProblem().addIfNotEqual(this, result, truthy.infer(context));
    context.getProblem().addIfNotEqual(this, result, falsy.infer(context));
    context.getProblem().addIfNotEqual(this, BoolType.INSTANCE, condition.infer(context));
    return result;
  }

  @Override
  public Expression normalize(Stack<Pair<Identifier, Expression>> context) {
    return new IfThenElseExpression(
        Derived.anf(source),
        condition.forceImmediate(context),
        truthy.normalizeAndBind(),
        falsy.normalizeAndBind());
  }

  @Override
  public Expression rename(Map<String, String> renaming) {
    return new IfThenElseExpression(
        new Renamed(source),
        condition.rename(renaming),
        truthy.rename(renaming),
        falsy.rename(renaming),
        type);
  }

  @Override
  public Annotation inferAnnotationsInternal(AnnotatingContext context, AnnotatingGlobals globals)
      throws UnificationError, TypeError {
    final var constraints = globals.getConstraints();
    // NOTE: We do not need to remove x from the context, since it must be a
    // boolean and therefore is not added to the context in the first place.
    final var truthyAnnotation = truthy.inferAnnotations(context, globals);
    final var falsyAnnotation = falsy.inferAnnotations(context, globals);
    constraints.eq(this, truthyAnnotation, falsyAnnotation);
    return truthyAnnotation;
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
  public Expression unshare() {
    // if (!(condition instanceof Identifier)) {
    //  throw new IllegalStateException("must be in anf");
    // }

    Set<String> freeT = truthy.freeVariables();
    Set<String> freeF = falsy.freeVariables();
    Set<String> cond = condition.freeVariables();
    if (!Sets.intersection(freeT, cond).isEmpty() || !Sets.intersection(freeF, cond).isEmpty()) {
      throw new UnsupportedOperationException("please implement");
    }

    var intersection = Sets.intersection(freeT, freeF);

    if (intersection.size() > 1) {
      throw new UnsupportedOperationException("please implement");
    }

    if (intersection.size() == 0) {
      return this;
    }

    var target = new Identifier(Predefined.INSTANCE, intersection.iterator().next());
    var down = ShareExpression.clone(target);
    var result = ShareExpression.rename(target, down, Pair.create(truthy, falsy));

    return new ShareExpression(
        target,
        down,
        new IfThenElseExpression(source, condition, result.getFirst(), result.getSecond(), type));
  }
}
