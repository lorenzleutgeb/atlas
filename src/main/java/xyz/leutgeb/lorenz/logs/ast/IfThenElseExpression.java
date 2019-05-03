package xyz.leutgeb.lorenz.logs.ast;

import static xyz.leutgeb.lorenz.logs.Util.indent;

import java.io.PrintStream;
import java.util.List;
import java.util.Map;
import java.util.Stack;
import java.util.stream.Stream;
import lombok.Data;
import lombok.EqualsAndHashCode;
import lombok.extern.log4j.Log4j2;
import org.hipparchus.util.Pair;
import xyz.leutgeb.lorenz.logs.Context;
import xyz.leutgeb.lorenz.logs.resources.AnnotatedType;
import xyz.leutgeb.lorenz.logs.resources.Annotation;
import xyz.leutgeb.lorenz.logs.resources.coefficients.Coefficient;
import xyz.leutgeb.lorenz.logs.type.BoolType;
import xyz.leutgeb.lorenz.logs.type.Type;
import xyz.leutgeb.lorenz.logs.type.TypeError;
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
    context.getProblem().add(result, truthy.infer(context));
    context.getProblem().add(result, falsy.infer(context));
    context.getProblem().add(BoolType.INSTANCE, condition.infer(context));
    return result;
  }

  @Override
  public Expression normalize(Stack<Pair<Identifier, Expression>> context) {
    return new IfThenElseExpression(
        source, condition.normalize(context), truthy.normalizeAndBind(), falsy.normalizeAndBind());
  }

  @Override
  public AnnotatedType inferAnnotations(Context context, Annotation typingContext)
      throws UnificationError, TypeError {
    var truthyAnnotation = truthy.inferAnnotations(context, typingContext);
    var falsyAnnotation = falsy.inferAnnotations(context, typingContext);

    // Equate truthy and falsy, bailing if this is impossible.

    var truthyCA = (Annotation) truthyAnnotation.getAnnotation();
    var falsyCA = (Annotation) falsyAnnotation.getAnnotation();

    if (truthyCA.getSize() != falsyCA.getSize()) {
      throw new UnsupportedOperationException("truthy and falsy have different arity");
    }

    if (truthyCA.getCoefficients().size() != falsyCA.getCoefficients().size()) {
      throw new UnsupportedOperationException(
          "truthy and falsy annotation have different number of coefficients");
    }

    for (Map.Entry<List<Integer>, Coefficient> entry :
        truthyAnnotation.getAnnotation().getCoefficients().entrySet()) {
      var other = falsyAnnotation.getAnnotation().getCoefficients().get(entry.getKey());

      if (other == null) {
        throw new UnsupportedOperationException("some coefficient is missing from falsy");
      }

      context.getConstraints().eq(entry.getValue(), other);
    }

    return new AnnotatedType(infer(context), truthyCA);
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
