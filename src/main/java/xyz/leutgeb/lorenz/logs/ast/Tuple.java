package xyz.leutgeb.lorenz.logs.ast;

import java.util.List;
import lombok.Data;
import xyz.leutgeb.lorenz.logs.Context;
import xyz.leutgeb.lorenz.logs.type.TreeType;
import xyz.leutgeb.lorenz.logs.type.Type;
import xyz.leutgeb.lorenz.logs.type.TypeError;
import xyz.leutgeb.lorenz.logs.unification.UnificationError;

@Data
public class Tuple extends TupleElement {
  private final List<TupleElement> elements;

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
}
