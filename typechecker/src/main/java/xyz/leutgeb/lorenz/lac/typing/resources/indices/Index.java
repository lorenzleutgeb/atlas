package xyz.leutgeb.lorenz.lac.typing.resources.indices;

import java.util.Map;
import java.util.function.Function;
import xyz.leutgeb.lorenz.lac.Util;

public interface Index {
  Integer getAssociatedIndex(String id);

  Integer getOffsetIndex();

  Index mask(Map<String, Integer> maskMap);

  Index mask(Integer maskedOffsetIndex);

  Index mask(Function<String, Integer> maskFunction);

  default Index mask(String id, Integer associatedIndex) {
    return mask(Map.of(id, associatedIndex));
  }

  default Index padWithZero() {
    return new FunctionIndex(Util.fallback(this::getAssociatedIndex, (id) -> 0), getOffsetIndex());
  }
}
