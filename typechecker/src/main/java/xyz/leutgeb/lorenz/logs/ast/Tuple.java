package xyz.leutgeb.lorenz.logs.ast;

import com.google.common.collect.Sets;
import java.io.PrintStream;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.Stack;
import java.util.stream.Collectors;
import java.util.stream.Stream;
import lombok.EqualsAndHashCode;
import lombok.Value;
import org.hipparchus.util.Pair;
import xyz.leutgeb.lorenz.logs.Context;
import xyz.leutgeb.lorenz.logs.ast.sources.Derived;
import xyz.leutgeb.lorenz.logs.ast.sources.Renamed;
import xyz.leutgeb.lorenz.logs.ast.sources.Source;
import xyz.leutgeb.lorenz.logs.resources.AnnotatingContext;
import xyz.leutgeb.lorenz.logs.resources.AnnotatingGlobals;
import xyz.leutgeb.lorenz.logs.resources.Annotation;
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

  public Tuple(Source source, List<Expression> elements, Type type) {
    super(source, type);
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
    context.getProblem().addIfNotEqual(this, result, getLeft().infer(context).wiggle(context));
    context
        .getProblem()
        .addIfNotEqual(this, elementType, getMiddle().infer(context).wiggle(context));
    context.getProblem().addIfNotEqual(this, result, getRight().infer(context).wiggle(context));
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
  public Expression rename(Map<String, String> renaming) {
    return new Tuple(
        new Renamed(source),
        elements.stream().map(e -> e.rename(renaming)).collect(Collectors.toList()),
        type);
  }

  @Override
  public Annotation inferAnnotationsInternal(AnnotatingContext x123q, AnnotatingGlobals globals)
      throws UnificationError, TypeError {
    final var constraints = globals.getConstraints();

    final var leftId = ((Identifier) getLeft()).getName();
    final var rightId = ((Identifier) getRight()).getName();

    // Apply (w : var) and (w) since this is a leaf node.
    var q =
        x123q
            .weakenIdentifiersExcept(this, globals.getConstraints(), Set.of(leftId, rightId))
            .less(this, constraints);

    var result = constraints.heuristic(1);

    // q_1 = q_2 = q_{*}'
    var q1 = q.getRankCoefficient(leftId);
    var q2 = q.getRankCoefficient(rightId);
    var qx = result.getRankCoefficient();
    constraints.eq(this, q1, q2, qx);

    // q_{1,0,0} = q_{0,1,0} = q_{*}'
    var q100 = q.getCoefficient(Map.of(leftId, 1, rightId, 0), 0);
    var q010 = q.getCoefficient(Map.of(leftId, 0, rightId, 1), 0);
    constraints.eq(this, q100, q010, qx);

    // q_{a,a,b} = q'_{a,b}
    q.streamIndices()
        .filter(index -> index.getFirst().get(leftId).equals(index.getFirst().get(rightId)))
        .forEach(
            index -> {
              var a = index.getFirst().get(leftId);
              var b = index.getSecond();
              constraints.eq(this, result.getCoefficient(a, b), q.getCoefficient(index));
            });

    return result.greater(this, constraints);
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
  public void printHaskellTo(PrintStream out, int indentation) {
    out.print("(Node ");
    for (int i = 0; i < elements.size(); i++) {
      elements.get(i).printHaskellTo(out, indentation);
      if (i < elements.size() - 1) {
        out.print(" ");
      }
    }
    out.print(")");
  }

  @Override
  public String toString() {
    return "(" + elements.stream().map(Object::toString).collect(Collectors.joining(", ")) + ")";
  }

  @Override
  public Expression unshare() {
    // NOTE: The only sharing possible is left and right, since sharing of either of the
    // two with middle would mean a type error.
    if (!(getLeft() instanceof Identifier) || !(getRight() instanceof Identifier)) {
      throw new IllegalStateException("must be in anf");
    }
    if (!getLeft().equals(getRight())) {
      return this;
    }
    var down = ShareExpression.clone((Identifier) getLeft());
    return new ShareExpression(
        (Identifier) getLeft(),
        down,
        new Tuple(source, List.of(down.getFirst(), getMiddle(), down.getSecond()), type));
  }

  @Override
  public Set<String> freeVariables() {
    return new HashSet<>(Sets.union(getLeft().freeVariables(), getRight().freeVariables()));
  }
}
