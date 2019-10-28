package xyz.leutgeb.lorenz.logs.resources;

import static xyz.leutgeb.lorenz.logs.Util.bug;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collection;
import java.util.Collections;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.function.BiFunction;
import java.util.function.Function;
import java.util.function.Predicate;
import java.util.stream.Collectors;
import java.util.stream.IntStream;
import java.util.stream.Stream;
import lombok.Data;
import lombok.Getter;
import lombok.extern.log4j.Log4j2;
import org.hipparchus.util.Pair;
import xyz.leutgeb.lorenz.logs.ast.Expression;
import xyz.leutgeb.lorenz.logs.ast.Identifier;
import xyz.leutgeb.lorenz.logs.resources.coefficients.Coefficient;
import xyz.leutgeb.lorenz.logs.resources.coefficients.KnownCoefficient;
import xyz.leutgeb.lorenz.logs.typing.types.TreeType;

@Log4j2
@Data
public class AnnotatingContext {
  @Getter private final List<String> ids;
  private Annotation annotation;

  private void checkIds() {
    if (ids.size() != (new HashSet<>(ids)).size()) {
      throw new IllegalStateException();
    }
  }

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

  public AnnotatingContext less(Expression source, Constraints constraints) {
    return new AnnotatingContext(new ArrayList<>(ids), annotation.less(source, constraints));
  }

  public AnnotatingContext greater(Expression source, Constraints constraints) {
    return new AnnotatingContext(new ArrayList<>(ids), annotation.greater(source, constraints));
  }

  public Coefficient getCoefficient(Function<String, Integer> indexer, Integer c) {
    final var index = new ArrayList<Integer>(size() + 1);
    for (var id : ids) {
      index.add(indexer.apply(id));
    }
    index.add(c);
    return annotation.getCoefficient(index);
  }

  public Coefficient getCoefficient(Map<String, Integer> indexer, Integer c) {
    if (indexer.size() < size()) {
      throw new IllegalArgumentException("indexer does not cover context");
    }
    final var inspect = getCoefficient(indexer::get, c);
    return inspect;
  }

  public Coefficient getCoefficient(Pair<Map<String, Integer>, Integer> index) {
    return getCoefficient(index.getFirst(), index.getSecond());
  }

  public Set<Map<String, Integer>> getIndices(
      BiFunction<String, Integer, Boolean> f, Predicate<Integer> g) {
    final var result = new HashSet<Map<String, Integer>>();
    outer:
    for (var entry : annotation.getCoefficients()) {
      if (!g.test(entry.getKey().get(size()))) {
        continue;
      }
      for (int i = 0; i < size() - 1; i++) {
        if (!f.apply(ids.get(i), entry.getKey().get(i))) {
          continue outer;
        }
      }
      final var index = new HashMap<String, Integer>();
      for (int i = 0; i < size() - 1; i++) {
        index.put(ids.get(i), entry.getKey().get(i));
      }
      index.put(null, entry.getKey().get(size()));
      result.add(index);
    }
    return result;
  }

  public Stream<Pair<Map<String, Integer>, Integer>> streamIndices() {
    checkIds();
    return annotation
        .streamCoefficients()
        .map(
            entry -> {
              final var index =
                  IntStream.range(0, size())
                      .boxed()
                      .collect(Collectors.toMap(ids::get, entry.getKey()::get));
              return new Pair<>(index, entry.getKey().get(size()));
            });
  }

  @Deprecated
  public AnnotatingContext partition(
      Expression source, Constraints constraints, Predicate<String> isLeft) {
    final var leftIds = new ArrayList<Pair<Integer, String>>(ids.size());
    final var rightIds = new ArrayList<Pair<Integer, String>>(ids.size());

    for (int i = 0; i < ids.size(); i++) {
      (isLeft.test(ids.get(i)) ? leftIds : rightIds).add(new Pair<>(i, ids.get(i)));
    }

    final var both = new ArrayList<>(leftIds);
    both.addAll(rightIds);

    final var result =
        new AnnotatingContext(
            both.stream().map(Pair::getSecond).collect(Collectors.toList()),
            constraints.heuristic(both.size()));

    for (int i = 0; i < result.getAnnotation().size(); i++) {
      constraints.eq(
          source,
          annotation.getRankCoefficient(both.get(i).getFirst()),
          result.getAnnotation().getRankCoefficient(i));
    }

    // TODO: Other coefficients.

    return result;
  }

  public AnnotatingContext pop(Constraints constraints, Identifier toPop) {
    final int i = ids.indexOf(toPop.getName());
    final var newIds = new ArrayList<String>(this.ids.size() - 1);
    newIds.addAll(this.ids.subList(0, i));
    newIds.addAll(this.ids.subList(i + 1, this.ids.size()));
    return new AnnotatingContext(newIds, constraints.heuristic(this.annotation.size() - 1));
  }

  public AnnotatingContext extend(Constraints constraints, Identifier... ids) {
    return extend(constraints, Arrays.asList(ids));
  }

  private void ensureAllTrees(Collection<Identifier> ids) {
    if (ids.stream().anyMatch(id -> !(id.getType() instanceof TreeType))) {
      throw new IllegalArgumentException();
    }
  }

  public AnnotatingContext extend(Constraints constraints, Collection<Identifier> ids) {
    ensureAllTrees(ids);
    final var newIds = new ArrayList<String>(ids.size() + this.ids.size());
    newIds.addAll(this.ids);
    newIds.addAll(ids.stream().map(Identifier::getName).collect(Collectors.toList()));
    final var annotation = constraints.heuristic(newIds.size());
    return new AnnotatingContext(newIds, annotation);
  }

  public AnnotatingContext weaken(Expression source, Constraints constraints, String id) {
    // ensureAllTrees(Collections.singleton(id));

    final var remainingIds = new ArrayList<>(ids);
    if (!remainingIds.remove(id)) {
      throw bug("unknown variable");
    }
    final var result = new AnnotatingContext(remainingIds, constraints.heuristic(size() - 1));

    // r_{(\vec{a}, b)} = q_{(\vec{a}, 0, b)}
    streamIndices()
        .filter(index -> index.getFirst().get(id) == 0)
        .forEach(
            index -> constraints.eq(source, result.getCoefficient(index), getCoefficient(index)));

    // r_i = q_i
    for (var e : remainingIds) {
      constraints.eq(source, result.getRankCoefficient(e), getRankCoefficient(e));
    }

    return result;
  }

  public AnnotatingContext weakenIdentifiersExcept(
      Expression source, Constraints constraints, Set<String> ids) {
    if (ids.size() == this.ids.size() && ids.containsAll(this.ids)) {
      return this;
    }
    var result = this;
    for (var e : this.ids) {
      if (ids.contains(e)) {
        continue;
      }
      result = result.weaken(source, constraints, e);
    }
    return result;
  }

  /**
   * Applies the rule (w : var) to generate a new context which only spans the identifiers given.
   * This is useful towards applying rules that require a specific context, such as (nil), which
   * requires the empty context, (node), which requires a context with exactly three elements,
   * (var), which requires a context with exactly one element, (app), which requires a context that
   * corresponds to the parameters of the function being called.
   *
   * <p>Note that this method also enforces order on the resulting context.
   */
  @Deprecated
  public AnnotatingContext weakenIdentifiersExceptOld(
      Expression source, Constraints constraints, List<Identifier> ids) {
    ensureAllTrees(ids);

    if (!this.ids.containsAll(
        ids.stream()
            .map(Identifier::getName)
            .filter(s -> !s.equals("nil") && !s.equals("true") && !s.equals("false"))
            .collect(Collectors.toList()))) {
      throw bug("some id is not contained");
    }

    if (ids.size() > size()) {
      throw bug("Weakening to more variables is not possible.");
    }

    if (ids.size() == size()) {
      return this;
    }

    final var r = constraints.heuristic(ids.size());

    // The (w : var) rule as
    // stated/defined in the paper requires the element of the context
    // to be removed to be in the rightmost/last position. It then
    // elegantly uses r_{\vec{a}, b} = q_{\vec{a}, 0, b}. I think
    // it is possible to hide an element at an arbitrary position,
    // as sketched by r_{\vec{a}, \vec{a'}, b} = q_{\vec{a}, 0, \vec{a'}, b},
    // such that |\vec{a}| = i - 1, |\vec{a'}| = |Q| - a.

    final var locations = new ArrayList<Integer>(ids.size());
    final var newIds = new ArrayList<String>(ids.size());

    for (int j = 0; j < ids.size(); j++) {
      final var needle = ids.get(j);
      int index = -1;

      for (int i = 0; i < this.ids.size(); i++) {
        final var id = this.ids.get(i);
        if (needle.getName().equals(id)) {
          newIds.add(id);
          index = i;
          locations.add(i);
          break;
        }
      }

      if (index == -1) {
        throw bug("could not find index for id " + needle);
      }

      // Generate constraints.

      // r_{\vec{a}, \vec{a'}, b} = q_{\vec{a}, 0, \vec{a'}, b}
      for (Map.Entry<List<Integer>, Coefficient> e : this.getAnnotation().getCoefficients()) {
        if (e.getKey().get(index) == 0) {
          final var other = e.getKey().subList(0, index);
          final var otherClone = new ArrayList<Integer>(other.size());
          otherClone.addAll(other);
          otherClone.addAll(e.getKey().subList(index + 1, e.getKey().size()));
          constraints.eq(source, e.getValue(), r.getCoefficient(otherClone));
        }
      }

      // r_i = q_i
      constraints.eq(
          source, r.getRankCoefficient(j), this.getAnnotation().getRankCoefficient(index));
    }

    return new AnnotatingContext(newIds, r);
  }

  private String prime(String id) {
    var tmp = id + "'";
    while (ids.contains(tmp)) {
      tmp += "'";
    }
    return tmp;
  }

  /** @deprecated use {@link Expression#unshare} */
  @Deprecated
  public Pair<String, AnnotatingContext> share(
      Expression source, Constraints constraints, String id) {
    if (!this.ids.contains(id)) {
      throw bug("id not contained");
    }

    final var resultAnnotation = constraints.heuristic(size() + 1);
    final var primed = prime(id);
    final var resultIds = new ArrayList<String>(size() + 1);
    resultIds.addAll(this.ids);
    resultIds.add(primed);

    // Generate constraints according to sharing operator.
    final var result = new AnnotatingContext(resultIds, resultAnnotation);

    // TODO: Equate all coefficients for elements of this.ids not in ids
    constraints.eqSum(
        source,
        this.getRankCoefficient(id),
        List.of(result.getRankCoefficient(id), result.getRankCoefficient(primed)));
    for (var it : ids) {
      if (!it.equals(id)) {
        constraints.eq(source, this.getRankCoefficient(it), result.getRankCoefficient(it));
      }
    }

    result
        .streamIndices()
        .forEach(
            index -> {
              final var sum = index.getFirst().get(id) + index.getFirst().get(primed);
              final var copy = new HashMap<>(index.getFirst());
              copy.put(id, sum);
              constraints.eq(
                  source,
                  result.getCoefficient(index),
                  this.getCoefficientOrZero(copy, index.getSecond()));
            });

    return new Pair<>(primed, result);
  }

  private Coefficient getCoefficientOrZero(HashMap<String, Integer> copy, Integer second) {
    final var result = getCoefficient(copy, second);
    return result != null ? result : KnownCoefficient.ZERO;
  }

  /** @deprecated use {@link Expression#unshare} */
  @Deprecated
  public Pair<Map<String, String>, AnnotatingContext> share(
      Expression source, Constraints constraints, List<String> ids) {
    if (ids.isEmpty()) {
      return new Pair<>(Collections.emptyMap(), this);
    }

    final var idsAsSet = new HashSet<>(ids);
    if (idsAsSet.size() != ids.size()) {
      throw bug("ids contain duplicate");
    }
    if (!this.ids.containsAll(ids)) {
      throw bug("some id not contained");
    }
    final var renaming = new HashMap<String, String>();
    var result = this;
    for (var id : ids) {
      var it = result.share(source, constraints, id);
      result = it.getSecond();
      renaming.put(id, it.getFirst());
    }
    return new Pair<>(renaming, result);
  }

  public int indexOf(String id) {
    for (int i = 0; i < ids.size(); i++) {
      if (ids.get(i).equals(id)) {
        return i;
      }
    }
    throw bug("unknown id '" + id + "'");
  }

  public AnnotatingContext weakenIdentifiersExcept(
      Expression source, Constraints constraints, Identifier id) {
    return weakenIdentifiersExcept(source, constraints, Collections.singleton(id.getName()));
  }

  public AnnotatingContext weakenAllIdentifiers(Expression source, Constraints constraints) {
    return weakenIdentifiersExcept(source, constraints, Collections.emptySet());
  }

  public int getIndex(Identifier id) {
    return ids.indexOf(id.getName());
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
}
