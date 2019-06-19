package xyz.leutgeb.lorenz.logs.resources;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collection;
import java.util.Collections;
import java.util.List;
import java.util.Map;
import java.util.stream.Collectors;
import lombok.Data;
import lombok.extern.log4j.Log4j2;
import xyz.leutgeb.lorenz.logs.ast.Identifier;
import xyz.leutgeb.lorenz.logs.resources.coefficients.Coefficient;
import xyz.leutgeb.lorenz.logs.typing.types.TreeType;

@Log4j2
@Data
public class AnnotatingContext {
  private final List<String> ids;
  private Annotation annotation;

  public AnnotatingContext(List<String> ids, Annotation annotation) {
    if (ids.size() != annotation.size()) {
      throw new IllegalArgumentException("sizes must match");
    }
    this.ids = ids;
    this.annotation = annotation;
  }

  public AnnotatingContext pop(Constraints constraints) {
    final var newIds = new ArrayList<String>(this.ids.size() - 1);
    newIds.addAll(this.ids.subList(0, this.ids.size() - 1));
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

  /**
   * Applies the rule (w : var) to generate a new context which only spans the identifiers given.
   * This is useful towards applying rules that require a specific context, such as (nil), which
   * requires the empty context, (node), which requires a context with exactly three elements,
   * (var), which requires a context with exactly one element, (app), which requires a context that
   * corresponds to the parameters of the function being called.
   *
   * <p>Note that this method also enforces order on the resulting context.
   */
  public AnnotatingContext weakenIdentifiersExcept(Constraints constraints, List<Identifier> ids) {
    ensureAllTrees(ids);

    if (!this.ids.containsAll(ids.stream().map(Identifier::getName).collect(Collectors.toList()))) {
      throw new IllegalArgumentException("some id is not contained");
    }

    if (ids.size() == size()) {
      return this;
    }

    final var r = constraints.heuristic(ids.size());

    // TODO(lorenz.leutgeb): Clarify assumption: The (w : var) rule as
    // stated/defined in the paper requires the element of the context
    // to be removed to be in the rightmost/last position. It then
    // elegantly uses r_{\vec{a}, 0, b} = q_{\vec{a}, 0, b}. I think
    // it is possible to remove an element at an arbitrary position,
    // as sketched by r_{\vec{a}, 0, \vec{a'}, b} = q_{\vec{a}, 0, \vec{a'}, b},
    // such that |\vec{a}| = i - 1, |\vec{a'}| = |Q| - a.

    final var locations = new ArrayList<Integer>(ids.size());
    final var newIds = new ArrayList<String>(ids.size());
    for (int j = 0; j < ids.size(); j++) {
      final var needle = ids.get(j);
      boolean found = false;
      int index = -1;

      for (int i = 0; i < this.ids.size(); i++) {
        final var id = this.ids.get(i);
        if (needle.getName().equals(id)) {
          newIds.add(id);
          index = i;
          found = true;
          locations.add(i);
          break;
        }
      }

      if (!found) {
        throw new RuntimeException();
      }

      // Generate constraints.

      // r_{\vec{a}, 0, \vec{a'}, b} = q_{\vec{a}, 0, \vec{a'}, b}

      for (Map.Entry<List<Integer>, Coefficient> e :
          this.getAnnotation().getCoefficients().entrySet()) {
        if (e.getKey().get(index) == 0) {
          final var other = e.getKey().subList(0, index);
          final var otherClone = new ArrayList<Integer>(other.size());
          otherClone.addAll(other);
          otherClone.addAll(e.getKey().subList(index + 1, e.getKey().size()));
          constraints.eq(e.getValue(), r.getCoefficients().get(otherClone));
        }
      }

      // r_i = q_i
      constraints.eq(
          r.getRankCoefficients().get(j), this.getAnnotation().getRankCoefficients().get(index));
    }

    for (int i = 0; i < locations.size(); i++) {}

    return new AnnotatingContext(newIds, r);
  }

  public AnnotatingContext weakenIdentifiersExcept(Constraints constraints, Identifier id) {
    return weakenIdentifiersExcept(constraints, Collections.singletonList(id));
  }

  public AnnotatingContext weakenAllIdentifiers(Constraints constraints) {
    return weakenIdentifiersExcept(constraints, Collections.emptyList());
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

  public AnnotatingContext weaken(Constraints constraints) {
    final var result = constraints.heuristic(annotation.size());
    for (Map.Entry<List<Integer>, Coefficient> e : annotation.getCoefficients().entrySet()) {
      constraints.le(result.getCoefficients().get(e.getKey()), e.getValue());
    }
    for (int i = 0; i < annotation.getRankCoefficients().size(); i++) {
      constraints.le(result.getRankCoefficients().get(i), annotation.getRankCoefficients().get(i));
    }
    return new AnnotatingContext(ids, result);
  }
}
