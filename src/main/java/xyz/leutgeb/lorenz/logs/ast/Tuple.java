package xyz.leutgeb.lorenz.logs.ast;

import java.util.List;
import java.util.Stack;
import java.util.stream.Stream;
import lombok.EqualsAndHashCode;
import lombok.Value;
import org.hipparchus.util.Pair;
import xyz.leutgeb.lorenz.logs.Context;
import xyz.leutgeb.lorenz.logs.resources.AnnotatedType;
import xyz.leutgeb.lorenz.logs.resources.Annotation;
import xyz.leutgeb.lorenz.logs.resources.EqualityConstraint;
import xyz.leutgeb.lorenz.logs.type.TreeType;
import xyz.leutgeb.lorenz.logs.type.Type;
import xyz.leutgeb.lorenz.logs.type.TypeError;
import xyz.leutgeb.lorenz.logs.unification.UnificationError;

@Value
@EqualsAndHashCode(callSuper = true)
public class Tuple extends TupleElement {
  List<TupleElement> elements;

  public Tuple(Source source, List<TupleElement> elements) {
    super(source);
    if (elements.size() != 3) {
      throw new IllegalArgumentException("only tuples with exactly three elements are supported");
    }
    this.elements = elements;
  }

  public TupleElement getLeft() {
    return elements.get(0);
  }

  public TupleElement getRight() {
    return elements.get(2);
  }

  public TupleElement getMiddle() {
    return elements.get(1);
  }

  @Override
  public boolean isImmediate() {
    return elements.stream().allMatch(Expression::isImmediate);
  }

  @Override
  public Stream<? extends Expression> getChildren() {
    return elements.stream();
  }

  @Override
  public Type inferInternal(Context context) throws UnificationError, TypeError {
    var elementType = context.getProblem().fresh();
    var result = new TreeType(elementType);
    context.getProblem().add(result, getLeft().infer(context));
    context.getProblem().add(elementType, getMiddle().infer(context));
    context.getProblem().add(result, getRight().infer(context));
    return result;
  }

  @Override
  public Expression normalize(Stack<Pair<Identifier, Expression>> context) {
    if (elements.stream().allMatch(Expression::isImmediate)) {
      return this;
    }
    throw new UnsupportedOperationException("desugaring of tuples is not implemented");
  }

  @Override
  public AnnotatedType inferAnnotations(Context context, Annotation typingContext)
      throws UnificationError, TypeError {
    var constraints = context.getConstraints();
    var q = constraints.heuristic(2);
    var result = constraints.heuristic(1);

    var q1 = q.getRankCoeffients().get(0);
    var q2 = q.getRankCoeffients().get(1);
    var qx = result.getRankCoeffients().get(0);
    constraints.eq(q1, q2, qx);

    var q100 = q.getOrFresh(constraints, 1, 0, 0);
    var q010 = q.getOrFresh(constraints, 1, 0, 0);
    constraints.eq(q100, q010, qx);

    for (var e : q.getCoefficients().entrySet()) {
      var a = e.getKey().get(0);
      if (!a.equals(e.getKey().get(1))) {
        continue;
      }
      var b = e.getKey().get(2);
      constraints.add(new EqualityConstraint(e.getValue(), result.getOrFresh(constraints, a, b)));
    }

    return new AnnotatedType(infer(context), result);
  }
}
