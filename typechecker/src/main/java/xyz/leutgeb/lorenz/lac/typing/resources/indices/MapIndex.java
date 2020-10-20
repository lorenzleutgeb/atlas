package xyz.leutgeb.lorenz.lac.typing.resources.indices;

import static java.util.stream.Collectors.joining;

import java.util.HashMap;
import java.util.Map;
import java.util.function.Function;
import lombok.AllArgsConstructor;
import lombok.Data;
import xyz.leutgeb.lorenz.lac.ast.Identifier;
import xyz.leutgeb.lorenz.lac.util.Util;

@Data
@AllArgsConstructor
public class MapIndex implements Index {
  private final Map<Identifier, Integer> associatedIndices;
  private final Integer offsetIndex;
  private final boolean padded;

  public MapIndex(Map<Identifier, Integer> associatedIndices, Integer offsetIndex) {
    this(associatedIndices, offsetIndex, false);
  }

  public Index mask(Map<Identifier, Integer> maskMap) {
    final var copy = new HashMap<>(associatedIndices);
    copy.putAll(maskMap);
    return new MapIndex(copy, offsetIndex, padded);
  }

  @Override
  public Index mask(Integer maskedOffsetIndex) {
    return new MapIndex(associatedIndices, maskedOffsetIndex, padded);
  }

  @Override
  public Index mask(Function<Identifier, Integer> maskFunction) {
    return new FunctionIndex(Util.fallback(maskFunction, associatedIndices::get), offsetIndex);
  }

  @Override
  public Index padWithZero() {
    if (padded) {
      return this;
    }
    return new MapIndex(associatedIndices, offsetIndex, true);
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
