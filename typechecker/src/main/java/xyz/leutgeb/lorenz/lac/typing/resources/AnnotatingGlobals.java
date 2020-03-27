package xyz.leutgeb.lorenz.lac.typing.resources;

import java.util.Map;
import lombok.AccessLevel;
import lombok.Getter;
import lombok.Value;
import org.hipparchus.util.Pair;
import xyz.leutgeb.lorenz.lac.typing.resources.heuristics.AnnotationHeuristic;
import xyz.leutgeb.lorenz.lac.typing.resources.heuristics.RangeHeuristic;

@Value
public final class AnnotatingGlobals {
  @Getter(value = AccessLevel.NONE)
  private final Map<String, Pair<Annotation, Annotation>> functionAnnotations;

  @Getter(value = AccessLevel.NONE)
  private final Map<String, Pair<Annotation, Annotation>> costFreeFunctionAnnotations;

  private final int cost;
  private final AnnotationHeuristic heuristic = RangeHeuristic.DEFAULT;

  public AnnotatingGlobals(
      Map<String, Pair<Annotation, Annotation>> functionAnnotations,
      Map<String, Pair<Annotation, Annotation>> costFreeFunctionAnnotations,
      int cost) {
    if (!(cost == 0 || cost == 1)) {
      throw new IllegalArgumentException("cost must be one or zero");
    }
    this.costFreeFunctionAnnotations = costFreeFunctionAnnotations;
    this.functionAnnotations = functionAnnotations;
    this.cost = cost;
  }

  public AnnotatingGlobals costFree() {
    if (cost == 0) {
      return this;
    }
    return new AnnotatingGlobals(functionAnnotations, costFreeFunctionAnnotations, 0);
  }

  public boolean isCostFree() {
    return cost == 0;
  }

  public void addFunctionAnnotation(
      String name, Pair<Annotation, Annotation> withCost, Pair<Annotation, Annotation> costFree) {
    if (withCost.getSecond().size() > 1 || costFree.getSecond().size() > 1) {
      throw new IllegalArgumentException("annotation of result can be of size one at most");
    }
    if (withCost.getFirst().size() != costFree.getFirst().size()) {
      throw new IllegalArgumentException("size mismatch");
    }
    if (withCost.getSecond().size() != costFree.getSecond().size()) {
      throw new IllegalArgumentException("size mismatch");
    }
    functionAnnotations.put(name, withCost);
    costFreeFunctionAnnotations.put(name, costFree);
  }

  public Pair<Annotation, Annotation> getDependingOnCost(String name) {
    if (cost == 0) {
      return costFreeFunctionAnnotations.get(name);
    } else if (cost == 1) {
      return functionAnnotations.get(name);
    }
    throw new IllegalStateException("cost is neither one nor zero");
  }
}
