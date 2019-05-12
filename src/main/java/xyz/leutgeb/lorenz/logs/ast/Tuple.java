package xyz.leutgeb.lorenz.logs.ast;

import java.io.PrintStream;
import java.util.Arrays;
import java.util.List;
import java.util.Stack;
import java.util.stream.Collectors;
import java.util.stream.Stream;
import lombok.EqualsAndHashCode;
import lombok.Value;
import org.hipparchus.util.Pair;
import xyz.leutgeb.lorenz.logs.Context;
import xyz.leutgeb.lorenz.logs.ast.sources.Derived;
import xyz.leutgeb.lorenz.logs.ast.sources.Source;
import xyz.leutgeb.lorenz.logs.resources.Annotation;
import xyz.leutgeb.lorenz.logs.resources.constraints.EqualityConstraint;
import xyz.leutgeb.lorenz.logs.typing.TypeError;
import xyz.leutgeb.lorenz.logs.typing.types.TreeType;
import xyz.leutgeb.lorenz.logs.typing.types.Type;
import xyz.leutgeb.lorenz.logs.unification.UnificationError;

@Value
@EqualsAndHashCode(callSuper = true)
public class Tuple extends Expression {
  List<Expression> elements;

  public Tuple(Source source, List<Expression> elements) {
    super(source);
    if (elements.size() != 3) {
      throw new IllegalArgumentException("only tuples with exactly three elements are supported");
    }
    this.elements = elements;
  }

  public Expression getLeft() {
    return elements.get(0);
  }

  public Expression getRight() {
    return elements.get(2);
  }

  public Expression getMiddle() {
    return elements.get(1);
  }

  @Override
  public Stream<? extends Expression> getChildren() {
    return elements.stream();
  }

  @Override
  public Type inferInternal(Context context) throws UnificationError, TypeError {
    var elementType = context.getProblem().fresh();
    var result = new TreeType(elementType);
    context.getProblem().add(this, result, getLeft().infer(context));
    context.getProblem().add(this, elementType, getMiddle().infer(context));
    context.getProblem().add(this, result, getRight().infer(context));
    return result;
  }

  @Override
  public Expression normalize(Stack<Pair<Identifier, Expression>> context) {
    if (elements.stream().allMatch(Expression::isImmediate)) {
      return this;
    }
    return new Tuple(
        Derived.anf(source),
        elements.stream().map(e -> e.forceImmediate(context)).collect(Collectors.toList()));
  }

  @Override
  public Annotation inferAnnotations(Context context) throws UnificationError, TypeError {
    var constraints = context.getConstraints();
    var x1q = context.lookupAnnotation(((Identifier) getLeft()).getName());
    var x3q = context.lookupAnnotation(((Identifier) getRight()).getName());

    // var q = constraints.heuristic(2);
    var result = constraints.heuristic(1);

    // q_1 = q_2 = q'
    // var q1 = q.getRankCoefficients().get(0);
    var q1 = x1q.getRankCoefficient();
    // var q2 = q.getRankCoefficients().get(1);
    var q2 = x3q.getRankCoefficient();
    var qx = result.getRankCoefficient();
    constraints.eq(q1, q2, qx);

    // q_{1,0,0} = q_{0,1,0} = q_*'
    // var q100 = q.getOrFresh(constraints, 1, 0, 0);
    var q100 = x1q.getOrFresh(constraints, 1, 0);
    // var q010 = q.getOrFresh(constraints, 1, 0, 0);
    var q010 = x3q.getOrFresh(constraints, 1, 0);
    constraints.eq(q100, q010, qx);

    // q_{a,a,b} = q'_{a,b}
    for (var e : result.getCoefficients().entrySet()) {
      var a = e.getKey().get(0);
      var b = e.getKey().get(1);
      constraints.add(
          new EqualityConstraint(e.getValue(), x1q.getCoefficients().get(Arrays.asList(a, b))));
      constraints.add(
          new EqualityConstraint(e.getValue(), x3q.getCoefficients().get(Arrays.asList(a, b))));
    }

    return result;
  }

  @Override
  public void printTo(PrintStream out, int indentation) {
    out.print("(");
    for (int i = 0; i < elements.size(); i++) {
      elements.get(i).printTo(out, indentation);
      if (i < elements.size() - 1) {
        out.print(", ");
      }
    }
    out.print(")");
  }

  @Override
  public String toString() {
    return "(" + elements.stream().map(Object::toString).collect(Collectors.joining(", ")) + ")";
  }
}
