package xyz.leutgeb.lorenz.logs.ast;

import java.util.List;
import java.util.Stack;
import lombok.Value;
import org.hipparchus.util.Pair;
import xyz.leutgeb.lorenz.logs.Context;
import xyz.leutgeb.lorenz.logs.type.TreeType;
import xyz.leutgeb.lorenz.logs.type.Type;
import xyz.leutgeb.lorenz.logs.type.TypeError;
import xyz.leutgeb.lorenz.logs.unification.UnificationError;

@Value
public class Tuple extends TupleElement {
  List<TupleElement> elements;

  public Tuple(Source source, List<TupleElement> elements) {
    super(source);
    this.elements = elements;
  }

  @Override
  public boolean isImmediate() {
    return elements.stream().allMatch(Expression::isImmediate);
  }

  @Override
  public Type infer(Context context) throws UnificationError, TypeError {
    if (elements.size() != 3) {
      throw new UnsupportedOperationException();
    }
    var elementType = context.getProblem().fresh();
    var result = new TreeType(elementType);
    context.getProblem().add(result, elements.get(0).infer(context));
    context.getProblem().add(elementType, elements.get(1).infer(context));
    context.getProblem().add(result, elements.get(2).infer(context));
    return result;
  }

  @Override
  public Expression normalize(Stack<Pair<Identifier, Expression>> context) {
    if (elements.stream().allMatch(Expression::isImmediate)) {
      return this;
    }
    throw new UnsupportedOperationException("desugaring of tuples is not implemented");
  }
}
