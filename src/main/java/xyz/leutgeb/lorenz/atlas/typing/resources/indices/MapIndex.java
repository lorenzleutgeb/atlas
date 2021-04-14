package xyz.leutgeb.lorenz.atlas.typing.resources.indices;

import static java.util.stream.Collectors.joining;

import java.util.HashMap;
import java.util.Map;
import java.util.function.Function;
import lombok.Data;
import xyz.leutgeb.lorenz.atlas.ast.Identifier;
import xyz.leutgeb.lorenz.atlas.util.Util;

@Data
public class MapIndex implements Index {
  private final Map<Identifier, Integer> associatedIndices;
  private final Integer offsetIndex;

  public Index mask(Map<Identifier, Integer> maskMap) {
    final var copy = new HashMap<>(associatedIndices);
    copy.putAll(maskMap);
    return new MapIndex(copy, offsetIndex);
  }

  @Override
  public Index mask(Integer maskedOffsetIndex) {
    return new MapIndex(associatedIndices, maskedOffsetIndex);
  }

  @Override
  public Index mask(Function<Identifier, Integer> maskFunction) {
    return new FunctionIndex(Util.fallback(maskFunction, associatedIndices::get), offsetIndex);
  }

  @Override
  public Integer getAssociatedIndex(Identifier id) {
    return associatedIndices.get(id);
  }

  @Override
  public String toString() {
    return "["
        + associatedIndices.entrySet().stream()
            .map(entry -> entry.getKey() + " ↦ " + entry.getValue())
            .collect(joining(", ", "[", "]"))
        + ", "
        + getOffsetIndex()
        + "]";
  }
}
