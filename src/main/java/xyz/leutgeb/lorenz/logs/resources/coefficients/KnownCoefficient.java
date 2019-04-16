package xyz.leutgeb.lorenz.logs.resources.coefficients;

import lombok.Value;
import org.hipparchus.fraction.Fraction;

@Value
public class KnownCoefficient extends Coefficient {
  public static final KnownCoefficient ZERO = new KnownCoefficient(Fraction.ZERO);

  Fraction value;
}
