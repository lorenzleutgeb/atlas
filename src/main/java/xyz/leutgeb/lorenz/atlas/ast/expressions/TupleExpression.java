package xyz.leutgeb.lorenz.atlas.ast.expressions;

import static xyz.leutgeb.lorenz.atlas.util.Util.mapToString;

import java.io.PrintStream;
import java.util.ArrayList;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Optional;
import java.util.Set;
import java.util.Stack;
import java.util.stream.Collectors;
import java.util.stream.Stream;
import lombok.EqualsAndHashCode;
import lombok.Value;
import xyz.leutgeb.lorenz.atlas.ast.Normalization;
import xyz.leutgeb.lorenz.atlas.ast.sources.Derived;
import xyz.leutgeb.lorenz.atlas.ast.sources.Source;
import xyz.leutgeb.lorenz.atlas.typing.simple.TypeError;
import xyz.leutgeb.lorenz.atlas.typing.simple.types.ProductType;
import xyz.leutgeb.lorenz.atlas.typing.simple.types.TreeType;
import xyz.leutgeb.lorenz.atlas.typing.simple.types.Type;
import xyz.leutgeb.lorenz.atlas.unification.UnificationContext;
import xyz.leutgeb.lorenz.atlas.util.IntIdGenerator;

@Value
@EqualsAndHashCode(callSuper = true)
public class TupleExpression extends Expression {
  List<Expression> elements;

  public TupleExpression(Source source, List<Expression> elements) {
    super(source);
    this.elements = elements;
  }

  private TupleExpression(Source source, List<Expression> elements, Type type) {
    super(source, type);
    this.elements = elements;
  }

  @Override
  public Stream<? extends Expression> getChildren() {
    return elements.stream();
  }

  @Override
  public Type inferInternal(UnificationContext context) throws TypeError {
    var elementTypeVars = new ArrayList<Type>(elements.size());

    for (var element : elements) {
      final var elementTypeVar = context.fresh();
      final var elementType = element.infer(context).wiggle(context);
      elementTypeVars.add(elementTypeVar);
      context.addEquivalenceIfNotEqual(elementTypeVar, elementType, source);
    }
    return (new ProductType(elementTypeVars)).wiggle(context);
  }

  @Override
  public Expression normalize(Stack<Normalization> context, IntIdGenerator idGenerator) {
    if (elements.stream().allMatch(Expression::isImmediate)) {
      return this;
    }

    // Normally, `leaf` must be pulled out, but tuples are an exception.
    if (elements.stream().anyMatch(IdentifierExpression::isLeaf)) {
      return this;
    }

    return new TupleExpression(
        Derived.anf(this),
        elements.stream()
            .map(e -> e.forceImmediate(context, idGenerator))
            .collect(Collectors.toList()));
  }

  @Override
  public Expression rename(Map<String, String> renaming) {
    // TODO(lorenzleutgeb): Only create new expression if renaming is necessary!
    return new TupleExpression(
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
  public void printHaskellTo(PrintStream out, int indentation, String currentFunction) {
    out.print("(");
    for (int i = 0; i < elements.size(); i++) {
      elements.get(i).printHaskellTo(out, indentation, currentFunction);
      if (i < elements.size() - 1) {
        out.print(", ");
      }
    }
    out.print(")");
  }

  @Override
  public void printJavaTo(PrintStream out, int indentation, String currentFunction) {
    throw new UnsupportedOperationException("not implemented");
  }

  @Override
  public String toString() {
    return "(" + mapToString(elements.stream()).collect(Collectors.joining(", ")) + ")";
  }

  @Override
  public Expression unshare(IntIdGenerator idGenerator, boolean lazy) {
    boolean haveTree = false;
    for (var element : elements) {
      if (!(element instanceof IdentifierExpression)) {
        throw new IllegalStateException("must be in anf");
      }
      if (element.getType() instanceof TreeType) {
        if (haveTree) {
          throw new UnsupportedOperationException("not implemented");
        }
        haveTree = true;
      }
    }
    return this;
  }

  @Override
  public Set<IdentifierExpression> freeVariables() {
    final var result = new HashSet<IdentifierExpression>();
    for (var element : elements) {
      result.addAll(element.freeVariables());
    }
    return result;
  }

  @Override
  public boolean isTerminal() {
    return true;
  }

  @Override
  public boolean isTreeConstruction() {
    return getTree().isPresent();
  }

  public Optional<IdentifierExpression> getTree() {
    for (var element : elements) {
      if (element instanceof IdentifierExpression && element.getType() instanceof TreeType) {
        return Optional.of((IdentifierExpression) element);
      }
    }
    return Optional.empty();
  }
}
