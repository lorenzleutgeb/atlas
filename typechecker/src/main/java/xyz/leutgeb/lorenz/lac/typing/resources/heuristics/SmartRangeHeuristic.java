package xyz.leutgeb.lorenz.lac.typing.resources.heuristics;

import static com.google.common.collect.Lists.cartesianProduct;
import static java.util.function.Function.identity;
import static java.util.stream.Collectors.*;
import static java.util.stream.IntStream.range;
import static java.util.stream.Stream.concat;

import java.util.List;
import java.util.Set;
import java.util.function.Predicate;
import java.util.stream.Stream;
import lombok.Data;
import xyz.leutgeb.lorenz.lac.typing.resources.Annotation;
import xyz.leutgeb.lorenz.lac.typing.resources.coefficients.UnknownCoefficient;
import xyz.leutgeb.lorenz.lac.util.Util;

@Data
public class SmartRangeHeuristic implements AnnotationHeuristic {
  public static final SmartRangeHeuristic DEFAULT =
      new SmartRangeHeuristic(Set.of(0, 1), Set.of(0, 1, 2));
  private final List<Integer> as;
  private final List<Integer> bs;
  private int fresheness = 0;

  public SmartRangeHeuristic(Set<Integer> as, Set<Integer> bs) {
    this.as = List.copyOf(as);
    this.bs = List.copyOf(bs);
  }

  @Override
  public Annotation generate(String namePrefix, int size) {
    final var currentFreshness = this.fresheness++;

    return new Annotation(
        range(0, size)
            .mapToObj(i -> currentFreshness + Util.generateSubscript(i))
            .map(UnknownCoefficient::new)
            .collect(toList()),
        cartesianProduct(
                concat(Stream.generate(() -> as).limit(size), Stream.of(bs))
                    .collect(toUnmodifiableList()))
            .stream()
            .filter(index -> !Util.isConstant(index) || index.get(size).equals(2))
            .filter(Predicate.not(Util::isZero))
            .collect(
                toMap(
                    identity(),
                    l ->
                        new UnknownCoefficient(
                            currentFreshness
                                + l.stream()
                                    .map(Util::generateSubscript)
                                    .collect(joining(" ", "₍", "₎"))))),
        namePrefix + " #" + currentFreshness);
  }
}
