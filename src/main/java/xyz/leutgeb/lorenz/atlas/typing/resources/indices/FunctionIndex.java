package xyz.leutgeb.lorenz.atlas.typing.resources.indices;

import java.util.Map;
import java.util.function.Function;
import xyz.leutgeb.lorenz.atlas.ast.expressions.IdentifierExpression;
import xyz.leutgeb.lorenz.atlas.util.Util;

public class FunctionIndex implements Index {
  final Function<IdentifierExpression, Integer> associatedIndices;
  final Integer offsetIndex;

  public FunctionIndex(
      Function<IdentifierExpression, Integer> associatedIndices, Integer offsetIndex) {
    this.associatedIndices = associatedIndices;
    this.offsetIndex = offsetIndex;
  }

  @Override
  public Integer getAssociatedIndex(IdentifierExpression id) {
    return associatedIndices.apply(id);
  }

  @Override
  public Integer getOffsetIndex() {
    return offsetIndex;
  }

  @Override
  public Index mask(Map<IdentifierExpression, Integer> maskMap) {
    return new FunctionIndex(Util.fallback(maskMap::get, associatedIndices), offsetIndex);
  }

  @Override
  public Index mask(Integer maskedOffsetIndex) {
    return new FunctionIndex(associatedIndices, maskedOffsetIndex);
  }

  @Override
  public Index mask(Function<IdentifierExpression, Integer> maskFunction) {
    return new FunctionIndex(maskFunction, offsetIndex);
  }

  @Override
  public Index addToOffset(int x) {
    throw new UnsupportedOperationException("not implemented");
  }
}
