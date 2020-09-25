package xyz.leutgeb.lorenz.lac.typing.resources.indices;

import java.util.Map;
import java.util.function.Function;
import xyz.leutgeb.lorenz.lac.ast.Identifier;
import xyz.leutgeb.lorenz.lac.util.Util;

public interface Index {
  Integer getAssociatedIndex(Identifier id);

  Integer getOffsetIndex();

  Index mask(Map<Identifier, Integer> maskMap);

  Index mask(Integer maskedOffsetIndex);

  Index mask(Function<Identifier, Integer> maskFunction);

  default Index mask(Identifier id, Integer associatedIndex) {
    return mask(Map.of(id, associatedIndex));
  }

  default Index padWithZero() {
    return new FunctionIndex(Util.fallback(this::getAssociatedIndex, (id) -> 0), getOffsetIndex());
  }
}
