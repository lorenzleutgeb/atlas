package xyz.leutgeb.lorenz.lac.typing.resources.indices;

import java.util.Collection;
import java.util.Map;
import java.util.Set;
import java.util.function.Function;
import java.util.function.Predicate;
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

  default boolean agreeOnAssociatedIndices(Index other, Set<Identifier> identifiers) {
    return identifiers.stream()
        .allMatch(id -> getAssociatedIndex(id).equals(other.getAssociatedIndex(id)));
  }

  default boolean allAssociatedIndicesMatch(
      Collection<Identifier> ids, Predicate<Integer> predicate) {
    return ids.stream().map(this::getAssociatedIndex).allMatch(predicate);
  }

  @Deprecated
  default boolean nonZeroOrEmptyOn(Collection<Identifier> ids) {
    if (ids.isEmpty()) {
      return true;
    }
    return ids.stream().anyMatch(id -> getAssociatedIndex(id) != 0);
  }

  @Deprecated
  default boolean zeroAndNonEmptyOn(Collection<Identifier> ids) {
    if (ids.isEmpty()) {
      return false;
    }
    return ids.stream().allMatch(id -> getAssociatedIndex(id) == 0);
  }
}
