package xyz.leutgeb.lorenz.logs.resources;

import com.google.common.collect.Lists;
import java.util.ArrayList;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.stream.Collectors;
import lombok.Data;
import xyz.leutgeb.lorenz.logs.Util;
import xyz.leutgeb.lorenz.logs.resources.coefficients.Coefficient;

@Data
public class RangeHeuristic implements AnnotationHeuristic {
  public static final RangeHeuristic DEFAULT = new RangeHeuristic(0, 3);
  private int fresheness = 0;
  private final int from, to;

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
  public Annotation generate(int size, Constraints context) {
    final var rankCoefficients = new ArrayList<Coefficient>(size);
    for (int i = 0; i < size; i++) {
      rankCoefficients.add(context.unknown(fresheness + Util.generateSubscript(i)));
    }

    var span = new ArrayList<List<Integer>>(size);
    for (int j = 0; j <= size; j++) {
      var inner = new ArrayList<Integer>(to - from);
      for (int i = from; i < to; i++) {
        inner.add(i);
      }
      span.add(inner);
    }

    var cartesian = Lists.cartesianProduct(span);
    var coefficients = new LinkedHashMap<List<Integer>, Coefficient>();
    for (List<Integer> l : cartesian) {
      coefficients.put(
          l,
          context.unknown(
              fresheness
                  + "₍"
                  + l.stream().map(Util::generateSubscript).collect(Collectors.joining(" "))
                  + "₎"));
    }

    fresheness++;
    return new Annotation(rankCoefficients, coefficients);
  }
}
