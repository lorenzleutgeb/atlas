package xyz.leutgeb.lorenz.logs.resources;

import com.google.common.collect.Lists;
import java.util.ArrayList;
import java.util.List;
import lombok.Value;

@Value
public class RangeHeuristic implements AnnotationHeuristic {
  public static final RangeHeuristic DEFAULT = new RangeHeuristic(0, 5);
  int from, to;

  @Override
  public Annotation generate(int size, Constraints context) {
    var result = new Annotation(size);
    for (int i = 0; i < size; i++) {
      result.getRankCoefficients().set(i, context.unknown());
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
    for (List<Integer> l : cartesian) {
      result.add(l, context.unknown());
    }

    return result;
  }
}
