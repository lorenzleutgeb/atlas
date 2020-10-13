package xyz.leutgeb.lorenz.lac.ast;

import static com.google.common.collect.Sets.union;
import static xyz.leutgeb.lorenz.lac.util.Util.mapToString;

import java.io.PrintStream;
import java.util.*;
import java.util.stream.Collectors;
import java.util.stream.Stream;
import lombok.EqualsAndHashCode;
import lombok.Value;
import xyz.leutgeb.lorenz.lac.ast.sources.Derived;
import xyz.leutgeb.lorenz.lac.ast.sources.Source;
import xyz.leutgeb.lorenz.lac.typing.simple.TypeError;
import xyz.leutgeb.lorenz.lac.typing.simple.types.TreeType;
import xyz.leutgeb.lorenz.lac.typing.simple.types.Type;
import xyz.leutgeb.lorenz.lac.unification.UnificationContext;
import xyz.leutgeb.lorenz.lac.unification.UnificationError;
import xyz.leutgeb.lorenz.lac.util.IntIdGenerator;

@Value
@EqualsAndHashCode(callSuper = true)
public class NodeExpression extends Expression {
  List<Expression> elements;

  public NodeExpression(Source source, List<Expression> elements) {
    super(source);
    if (elements.size() != 3) {
      throw new IllegalArgumentException("only tuples with exactly three elements are supported");
    }
    this.elements = elements;
  }

  public NodeExpression(Source source, List<Expression> elements, Type type) {
    // TODO(lorenz.leutgeb): This constructor was made public for testing purposes only. Make it
    // private again?
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

  private Expression getMiddle() {
    return elements.get(1);
  }

  @Override
  public Stream<? extends Expression> getChildren() {
    return elements.stream();
  }

  @Override
  public Type inferInternal(UnificationContext context) throws UnificationError, TypeError {
    var elementType = context.fresh();
    var result = new TreeType(elementType);
    context.addIfNotEqual(result, getLeft().infer(context).wiggle(context));
    context.addIfNotEqual(elementType, getMiddle().infer(context).wiggle(context));
    context.addIfNotEqual(result, getRight().infer(context).wiggle(context));
    return result;
  }

  @Override
  public Expression normalize(Stack<Normalization> context, IntIdGenerator idGenerator) {
    if (elements.stream().allMatch(Expression::isImmediate)) {
      return this;
    }
    return new NodeExpression(
        Derived.anf(this),
        elements.stream()
            .map(e -> e.forceImmediate(context, idGenerator))
            .collect(Collectors.toList()));
  }

  @Override
  public Expression rename(Map<String, String> renaming) {
    // TODO(lorenz.leutgeb): Only create new expression if renaming is necessary!
    return new NodeExpression(
        Derived.rename(this),
        elements.stream().map(e -> e.rename(renaming)).collect(Collectors.toList()),
        type);
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
    return "(" + mapToString(elements.stream()).collect(Collectors.joining(", ")) + ")";
  }

  @Override
  public Expression unshare(IntIdGenerator idGenerator, boolean lazy) {
    // NOTE: The only sharing possible is left and right, since sharing of either of the
    // two with middle would mean a type error.
    if (!(getLeft() instanceof Identifier) || !(getRight() instanceof Identifier)) {
      throw new IllegalStateException("must be in anf");
    }
    if (!getLeft().equals(getRight())) {
      return this;
    }
    var down = ShareExpression.clone((Identifier) getLeft(), idGenerator);
    return new ShareExpression(
        this,
        (Identifier) getLeft(),
        down,
        new NodeExpression(source, List.of(down.getLeft(), getMiddle(), down.getRight()), type));
  }

  @Override
  public Set<Identifier> freeVariables() {
    return new HashSet<>(union(getLeft().freeVariables(), getRight().freeVariables()));
  }

  @Override
  public boolean isTerminal() {
    return true;
  }
}
