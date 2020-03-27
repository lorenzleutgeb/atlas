package xyz.leutgeb.lorenz.lac.typing.resources.indices;

import java.util.HashMap;
import java.util.Map;
import java.util.function.Function;
import lombok.Data;
import xyz.leutgeb.lorenz.lac.Util;

@Data
public class MapIndex implements Index {
  private final Map<String, Integer> associatedIndices;
  private final Integer offsetIndex;

  public Index mask(Map<String, Integer> maskMap) {
    final var copy = new HashMap<>(associatedIndices);
    copy.putAll(maskMap);
    return new MapIndex(copy, offsetIndex);
  }

  @Override
  public Index mask(Integer maskedOffsetIndex) {
    return new MapIndex(associatedIndices, maskedOffsetIndex);
  }

  @Override
  public Index mask(Function<String, Integer> maskFunction) {
    return new FunctionIndex(Util.fallback(maskFunction, associatedIndices::get), offsetIndex);
  }

  @Override
  public Integer getAssociatedIndex(String id) {
    return associatedIndices.get(id);
  }
}
