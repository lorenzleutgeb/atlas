package xyz.leutgeb.lorenz.atlas.typing.resources.heuristics;

import static com.google.common.collect.Lists.cartesianProduct;
import static java.util.stream.IntStream.range;
import static java.util.stream.Stream.concat;
import static xyz.leutgeb.lorenz.atlas.typing.resources.Annotation.isConstantIndex;
import static xyz.leutgeb.lorenz.atlas.typing.resources.Annotation.isUnitIndex;

import java.util.List;
import java.util.Set;
import java.util.function.Predicate;
import java.util.stream.Stream;
import lombok.Data;
import xyz.leutgeb.lorenz.atlas.typing.resources.Annotation;
import xyz.leutgeb.lorenz.atlas.util.Util;

@Data
public class SmartRangeHeuristic implements AnnotationHeuristic {
  public static final SmartRangeHeuristic DEFAULT =
      new SmartRangeHeuristic(Set.of(0, 1), Set.of(0, 1, 2));

  private final List<Integer> as;
  private final List<Integer> bs;
  private final boolean nonZero;

  public SmartRangeHeuristic(Set<Integer> as, Set<Integer> bs, boolean nonZero) {
    this.as = List.copyOf(as);
    this.bs = List.copyOf(bs);
    this.nonZero = nonZero;
  }

  public SmartRangeHeuristic(Set<Integer> as, Set<Integer> bs) {
    this(as, bs, true);
  }

  @Override
  public Annotation generate(String name, int size) {
    return new Annotation(
        size, range(0, size).boxed().toList(), generateInternal(size).toList(), name);
  }

  public Stream<List<Integer>> generateInternal(int treeSize) {
    return cartesianProduct(
            concat(Stream.generate(() -> as).limit(treeSize), Stream.of(bs)).toList())
        .stream()
        .filter(Predicate.not(Util::isAllZeroes))
        // TODO: Be smart about when to allow constants, especially 1. Only when there's a leaf?
        .filter(index -> !isConstantIndex(index) || isUnitIndex(index));
  }
}
