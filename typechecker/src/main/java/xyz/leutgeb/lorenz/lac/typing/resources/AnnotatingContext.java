package xyz.leutgeb.lorenz.lac.typing.resources;

import static xyz.leutgeb.lorenz.lac.Util.bug;

import java.util.ArrayList;
import java.util.Collection;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.function.Function;
import java.util.stream.Collectors;
import java.util.stream.IntStream;
import java.util.stream.Stream;
import lombok.Data;
import lombok.Getter;
import lombok.Value;
import lombok.extern.log4j.Log4j2;
import org.hipparchus.util.Pair;
import xyz.leutgeb.lorenz.lac.ast.Identifier;
import xyz.leutgeb.lorenz.lac.typing.resources.coefficients.Coefficient;
import xyz.leutgeb.lorenz.lac.typing.resources.indices.Index;
import xyz.leutgeb.lorenz.lac.typing.resources.indices.MapIndex;
import xyz.leutgeb.lorenz.lac.typing.simple.types.TreeType;

@Log4j2
@Data
public class AnnotatingContext {
  @Getter private final List<String> ids;
  private Annotation annotation;

  public AnnotatingContext(List<String> ids, Annotation annotation) {
    if (ids.size() != annotation.size()) {
      throw new IllegalArgumentException("sizes must match");
    }
    // if (ids.stream().anyMatch(Identifier::isConstant)) {
    //  throw new IllegalArgumentException("constant in context");
    // }
    this.ids = ids;
    checkIds();
    this.annotation = annotation;
  }

  private static void ensureAllTrees(Collection<Identifier> ids) {
    if (ids.stream().anyMatch(id -> !(id.getType() instanceof TreeType))) {
      throw new IllegalArgumentException();
    }
  }

  private void checkIds() {
    if (ids.size() != (new HashSet<>(ids)).size()) {
      throw new IllegalStateException();
    }
  }

  public Coefficient getCoefficient(Function<String, Integer> indexer, Integer c) {
    final var index = new ArrayList<Integer>(size() + 1);
    for (var id : ids) {
      index.add(indexer.apply(id));
    }
    index.add(c);
    return annotation.getCoefficient(index);
  }

  public Coefficient getCoefficientOrZero(Function<String, Integer> indexer, Integer c) {
    final var index = new ArrayList<Integer>(size() + 1);
    for (var id : ids) {
      index.add(indexer.apply(id));
    }
    index.add(c);
    return annotation.getCoefficientOrZero(index);
  }

  public Coefficient getCoefficient(Map<String, Integer> indexer, Integer c) {
    if (indexer.size() < size()) {
      throw new IllegalArgumentException("indexer does not cover context");
    }
    return getCoefficient(indexer::get, c);
  }

  public Coefficient getCoefficient(Pair<Map<String, Integer>, Integer> index) {
    return getCoefficient(index.getFirst(), index.getSecond());
  }

  public Coefficient getCoefficient(Index index) {
    return getCoefficient(index::getAssociatedIndex, index.getOffsetIndex());
  }

  public Coefficient getCoefficientOrZero(Index index) {
    return getCoefficientOrZero(index::getAssociatedIndex, index.getOffsetIndex());
  }

  public Stream<Entry> stream() {
    checkIds();
    return annotation
        .streamCoefficients()
        .map(
            entry -> {
              final var index =
                  IntStream.range(0, size())
                      .boxed()
                      .collect(Collectors.toMap(ids::get, entry.getKey()::get));
              return new Entry(index, entry.getKey().get(size()), entry.getValue());
            });
  }

  public Coefficient getCoefficientOrZero(Map<String, Integer> indexer, Integer c) {
    if (indexer.size() < size()) {
      throw new IllegalArgumentException("indexer does not cover context");
    }
    final var index = new ArrayList<Integer>(size() + 1);
    for (var id : ids) {
      index.add(indexer.get(id));
    }
    index.add(c);
    return annotation.getCoefficientOrZero(index);
  }

  private int indexOf(String id) {
    for (int i = 0; i < ids.size(); i++) {
      if (ids.get(i).equals(id)) {
        return i;
      }
    }
    throw bug("unknown id '" + id + "'");
  }

  public int getIndex(String id) {
    return ids.indexOf(id);
  }

  public int size() {
    final var annotationSize = this.annotation.size();
    final var idsSize = this.ids.size();

    if (annotationSize != idsSize) {
      throw new IllegalStateException("unclear what the size of this context is");
    }

    return idsSize;
  }

  public boolean isEmpty() {
    return size() == 0;
  }

  public Coefficient getRankCoefficient(String id) {
    return annotation.getRankCoefficient(indexOf(id));
  }

  @Override
  public String toString() {
    // return annotation.toStringForParameters(ids, true);
    final var idStr = ids.isEmpty() ? "Ø" : String.join(", ", ids);
    return idStr + " | " + this.annotation.name;
  }

  @Value
  public static class Entry extends MapIndex {
    Coefficient value;

    public Entry(Map<String, Integer> associatedIndices, Integer offsetIndex, Coefficient value) {
      super(associatedIndices, offsetIndex);
      this.value = value;
    }
  }
}
