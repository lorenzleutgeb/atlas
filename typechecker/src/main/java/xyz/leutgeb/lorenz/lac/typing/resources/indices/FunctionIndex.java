package xyz.leutgeb.lorenz.lac.typing.resources.indices;

import java.util.Map;
import java.util.function.Function;
import xyz.leutgeb.lorenz.lac.Util;

public class FunctionIndex implements Index {
  Function<String, Integer> associatedIndices;
  Integer offsetIndex;

  public FunctionIndex(Function<String, Integer> associatedIndices, Integer offsetIndex) {
    this.associatedIndices = associatedIndices;
    this.offsetIndex = offsetIndex;
  }

  @Override
  public Integer getAssociatedIndex(String id) {
    return associatedIndices.apply(id);
  }

  @Override
  public Integer getOffsetIndex() {
    return offsetIndex;
  }

  @Override
  public Index mask(Map<String, Integer> maskMap) {
    return new FunctionIndex(Util.fallback(maskMap::get, associatedIndices), offsetIndex);
  }

  @Override
  public Index mask(Integer maskedOffsetIndex) {
    return new FunctionIndex(associatedIndices, maskedOffsetIndex);
  }

  @Override
  public Index mask(Function<String, Integer> maskFunction) {
    return new FunctionIndex(maskFunction, offsetIndex);
  }
}
