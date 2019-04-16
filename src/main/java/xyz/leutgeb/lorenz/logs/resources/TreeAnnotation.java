package xyz.leutgeb.lorenz.logs.resources;

import java.util.HashMap;
import java.util.Map;
import lombok.Value;
import org.hipparchus.util.Pair;
import xyz.leutgeb.lorenz.logs.resources.coefficients.Coefficient;
import xyz.leutgeb.lorenz.logs.resources.coefficients.KnownCoefficient;

@Deprecated
@Value
public class TreeAnnotation extends Annotation {
  // If the rank coefficient is zero, it must be explicitly set to a value of zero.
  Coefficient rankCoefficient;

  // All other pairs not part of this map are assumed to have a coefficient of zero.
  Map<Pair<Integer, Integer>, Coefficient> coefficients;

  public TreeAnnotation(Constraints context) {
    this(context.unknown());
  }

  public TreeAnnotation(Coefficient rankCoefficient) {
    this.rankCoefficient = rankCoefficient;
    this.coefficients = new HashMap<>();
  }

  public Coefficient get(Pair<Integer, Integer> bf) {
    return coefficients.getOrDefault(bf, KnownCoefficient.ZERO);
  }

  public Coefficient getOrFresh(Constraints context, Pair<Integer, Integer> bf) {
    return coefficients.computeIfAbsent(bf, k -> context.unknown());
  }

  public Coefficient getOrFresh(Constraints context, int a, int b) {
    return getOrFresh(context, new Pair<>(a, b));
  }
}
