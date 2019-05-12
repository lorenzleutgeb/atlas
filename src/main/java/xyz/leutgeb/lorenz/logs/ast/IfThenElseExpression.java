package xyz.leutgeb.lorenz.logs.ast;

import static xyz.leutgeb.lorenz.logs.Util.indent;

import java.io.PrintStream;
import java.util.Stack;
import java.util.stream.Stream;
import lombok.Data;
import lombok.EqualsAndHashCode;
import lombok.extern.log4j.Log4j2;
import org.hipparchus.util.Pair;
import xyz.leutgeb.lorenz.logs.Context;
import xyz.leutgeb.lorenz.logs.ast.sources.Derived;
import xyz.leutgeb.lorenz.logs.resources.Annotation;
import xyz.leutgeb.lorenz.logs.typing.BoolType;
import xyz.leutgeb.lorenz.logs.typing.Type;
import xyz.leutgeb.lorenz.logs.typing.TypeError;
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
  public Type inferInternal(Context context) throws UnificationError, TypeError {
    var result = context.getProblem().fresh();
    context.getProblem().add(this, result, truthy.infer(context));
    context.getProblem().add(this, result, falsy.infer(context));
    context.getProblem().add(this, BoolType.INSTANCE, condition.infer(context));
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
  public Annotation inferAnnotations(Context context) throws UnificationError, TypeError {
    var truthyAnnotation = truthy.inferAnnotations(context);
    var falsyAnnotation = falsy.inferAnnotations(context);
    context.getConstraints().eq(truthyAnnotation, falsyAnnotation);
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
}
