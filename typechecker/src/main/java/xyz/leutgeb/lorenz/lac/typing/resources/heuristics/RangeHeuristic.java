package xyz.leutgeb.lorenz.lac.typing.resources.heuristics;

import static com.google.common.collect.Lists.cartesianProduct;
import static java.util.function.Function.identity;
import static java.util.stream.Collectors.toList;
import static java.util.stream.Collectors.toMap;
import static java.util.stream.IntStream.range;

import java.util.List;
import java.util.Map;
import java.util.stream.Collectors;
import java.util.stream.Stream;
import lombok.Data;
import xyz.leutgeb.lorenz.lac.Util;
import xyz.leutgeb.lorenz.lac.typing.resources.Annotation;
import xyz.leutgeb.lorenz.lac.typing.resources.coefficients.Coefficient;
import xyz.leutgeb.lorenz.lac.typing.resources.coefficients.UnknownCoefficient;

@Data
public class RangeHeuristic implements AnnotationHeuristic {
  public static final RangeHeuristic DEFAULT = new RangeHeuristic(0, 3);
  private final int from, to;
  private int fresheness = 0;

  public RangeHeuristic(int from, int to) {
    if (from != 0) {
      throw new UnsupportedOperationException("from != 0 not implemented");
    }
    if (to <= from || to < 3) {
      throw new IllegalArgumentException();
    }
    this.from = from;
    this.to = to;
  }

  @Override
  public Annotation generate(String namePrefix, int size) {
    final var currentFreshness = this.fresheness++;

    final List<Coefficient> rankCoefficients =
        range(0, size)
            .mapToObj(i -> fresheness + Util.generateSubscript(i))
            .map(UnknownCoefficient::new)
            .collect(toList());

    final var span =
        Stream.generate(() -> range(from, to).boxed().collect(toList()))
            .limit(size + 1)
            .collect(toList());

    final Map<List<Integer>, Coefficient> coefficients =
        cartesianProduct(span).stream()
            .collect(
                toMap(
                    identity(),
                    l ->
                        new UnknownCoefficient(
                            // "q"
                            // +
                            currentFreshness
                                // + ","
                                // + namePrefix
                                + "₍"
                                // + "<SUB>("
                                + l.stream()
                                    .map(Util::generateSubscript)
                                    // .map(String::valueOf)
                                    .collect(Collectors.joining(" "))
                                + "₎")
                    // + ")</SUB>"));
                    ));

    return new Annotation(rankCoefficients, coefficients, String.valueOf(currentFreshness));
  }
}
