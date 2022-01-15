package xyz.leutgeb.lorenz.atlas.typing.resources;

import static java.util.stream.Collectors.joining;
import static xyz.leutgeb.lorenz.atlas.util.Util.bug;

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
import lombok.EqualsAndHashCode;
import lombok.Getter;
import lombok.Value;
import lombok.extern.slf4j.Slf4j;
import xyz.leutgeb.lorenz.atlas.ast.Identifier;
import xyz.leutgeb.lorenz.atlas.typing.resources.coefficients.Coefficient;
import xyz.leutgeb.lorenz.atlas.typing.resources.coefficients.KnownCoefficient;
import xyz.leutgeb.lorenz.atlas.typing.resources.indices.Index;
import xyz.leutgeb.lorenz.atlas.typing.resources.indices.MapIndex;

@Slf4j
@Data
public class AnnotatingContext {
  @Getter private final List<Identifier> ids;

  private Annotation annotation;

  public AnnotatingContext(List<Identifier> ids, String name) {
    this.ids = ids;
    checkIds();
    this.annotation = new Annotation(ids.size(), name);
  }

  public AnnotatingContext(List<Identifier> ids, Annotation annotation) {
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

  private void checkIds() {
    if (ids.size() != (new HashSet<>(ids)).size()) {
      throw new IllegalStateException();
    }
  }

  public Coefficient getCoefficient(Function<Identifier, Integer> indexer, Integer c) {
    final var index = new ArrayList<Integer>(size() + 1);
    for (var id : ids) {
      index.add(indexer.apply(id));
    }
    index.add(c);
    return annotation.getCoefficient(index);
  }

  public List<Integer> toListIndex(Function<Identifier, Integer> indexer, Integer c) {
    final var index = new ArrayList<Integer>(size() + 1);
    for (var id : ids) {
      Integer mapped = indexer.apply(id);
      if (mapped == null) {
        throw bug("indexer missed on '" + id + "'");
      }
      index.add(mapped);
    }
    index.add(c);
    return index;
  }

  public List<Integer> toListIndex(Index index) {
    return toListIndex(index::getAssociatedIndex, index.getOffsetIndex());
  }

  public Coefficient getCoefficientOrDefine(Function<Identifier, Integer> indexer, Integer c) {
    return annotation.getCoefficientOrDefine(toListIndex(indexer, c));
  }

  public Coefficient getCoefficientOrZero(Function<Identifier, Integer> indexer, Integer c) {
    return annotation.getCoefficientOrZero(toListIndex(indexer, c));
  }

  public Coefficient getCoefficient(Map<Identifier, Integer> indexer, Integer c) {
    if (indexer.size() < size()) {
      throw new IllegalArgumentException("indexer does not cover context");
    }
    return getCoefficient(indexer::get, c);
  }

  public Coefficient getCoefficient(Index index) {
    return getCoefficient(index::getAssociatedIndex, index.getOffsetIndex());
  }

  public Coefficient getCoefficientOrZero(Index index) {
    return getCoefficientOrZero(index::getAssociatedIndex, index.getOffsetIndex());
  }

  public Stream<Entry> streamNonRank() {
    checkIds();
    return annotation
        .streamNonRankCoefficients()
        .map(
            entry -> {
              final var index =
                  IntStream.range(0, size())
                      .boxed()
                      .collect(Collectors.toMap(ids::get, entry.getKey()::get));
              return new Entry(index, entry.getKey().get(size()), entry.getValue());
            });
  }

  public Coefficient getCoefficientOrZero(Map<Identifier, Integer> indexer, Integer c) {
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

  /*
  public void setCoefficient(Map<Identifier, Integer> indexer, Integer c, Coefficient value) {
    if (indexer.size() < size()) {
      throw new IllegalArgumentException("indexer does not cover context");
    }
    final var index = new ArrayList<Integer>(size() + 1);
    for (var id : ids) {
      index.add(indexer.get(id));
    }
    index.add(c);
    annotation.setCoefficient(index, value);
  }
   */

  private int indexOf(Identifier id) {
    for (int i = 0; i < ids.size(); i++) {
      if (ids.get(i).equals(id)) {
        return i;
      }
    }
    throw bug("unknown id '" + id + "'");
  }

  public int getIndex(Identifier id) {
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

  public Coefficient getRankCoefficient(Identifier id) {
    return annotation.getRankCoefficient(indexOf(id));
  }

  @Override
  public String toString() {
    final var idStr =
        ids.isEmpty() ? "Ø" : ids.stream().map(Object::toString).collect(joining(", "));
    return idStr + " | " + /*this.annotation + " named " +*/ this.annotation.getName();
  }

  public String potentialString() {
    return annotation.toLongString(ids.stream().map(Object::toString).toList());
  }

  public String toShortPotentialString() {
    return ids.toString() + "|" + annotation;
  }

  public AnnotatingContext substitute(Map<Coefficient, KnownCoefficient> solution) {
    return new AnnotatingContext(List.copyOf(ids), annotation.substitute(solution));
  }

  public AnnotatingContext reorder(List<Identifier> reorderedIds) {
    if (reorderedIds.size() != ids.size()) {
      throw new IllegalArgumentException();
    }
    if (reorderedIds.equals(ids)) {
      return this;
    }
    final var reorderedIndices = ids.stream().map(reorderedIds::indexOf).toList();
    if (reorderedIndices.contains(-1)) {
      throw new IllegalArgumentException();
    }
    return new AnnotatingContext(reorderedIds, annotation.reorder(reorderedIndices));
  }

  public AnnotatingContext reorderByName(String... reorderedIds) {
    return reorderByName(List.of(reorderedIds));
  }

  public static List<Identifier> reorderByName(
      Collection<Identifier> ids, List<String> reorderedIds) {
    if (reorderedIds.size() != ids.size()) {
      throw new IllegalArgumentException();
    }
    final var idStrings = ids.stream().map(Object::toString).toList();
    return reorderedIds.stream()
        .map(
            idString ->
                ids.stream()
                    .filter((Identifier id) -> id.getName().equals(idString))
                    .findFirst()
                    .get())
        .toList();
  }

  public AnnotatingContext reorderByName(List<String> reorderedIds) {
    if (reorderedIds.size() != ids.size()) {
      throw new IllegalArgumentException();
    }
    final var idStrings = ids.stream().map(Object::toString).toList();
    if (reorderedIds.equals(idStrings)) {
      return this;
    }
    /*
    final var reorderedIndices =
        idStrings.stream().map(reorderedIds::indexOf).collect(toUnmodifiableList());
     */
    final var reorderedIndices = reorderedIds.stream().map(idStrings::indexOf).toList();
    if (reorderedIndices.contains(-1)) {
      throw new IllegalArgumentException();
    }
    return new AnnotatingContext(
        reorderedIds.stream()
            .map(
                idString ->
                    ids.stream()
                        .filter((Identifier id) -> id.getName().equals(idString))
                        .findFirst()
                        .get())
            .toList(),
        annotation.reorder(reorderedIndices));
  }

  public AnnotatingContext reorderLexicographically() {
    return reorderByName(ids.stream().map(Identifier::getName).sorted().toList());
  }

  public Coefficient getCoefficientOrDefine(Index index) {
    return annotation.getCoefficientOrDefine(toListIndex(index));
  }

  public Coefficient getRankCoefficientOrDefine(Identifier id) {
    return annotation.getRankCoefficientOrDefine(indexOf(id));
  }

  public Coefficient getRankCoefficientOrZero(Identifier id) {
    return annotation.getRankCoefficientOrZero(indexOf(id));
  }

  @Value
  @EqualsAndHashCode(callSuper = false)
  public static class Entry extends MapIndex {
    Coefficient value;

    public Entry(
        Map<Identifier, Integer> associatedIndices, Integer offsetIndex, Coefficient value) {
      super(associatedIndices, offsetIndex);
      this.value = value;
    }

    @Override
    public String toString() {
      return super.toString() + " = " + value;
    }

    public String toIndexString() {
      return super.toString();
    }
  }

  public AnnotatingContext rename(String newName) {
    return new AnnotatingContext(ids, annotation.rename(newName));
  }
}
