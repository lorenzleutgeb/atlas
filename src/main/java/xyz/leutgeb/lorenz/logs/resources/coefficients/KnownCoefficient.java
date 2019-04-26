package xyz.leutgeb.lorenz.logs.resources.coefficients;

import lombok.EqualsAndHashCode;
import lombok.Value;
import org.hipparchus.fraction.Fraction;

@Value
@EqualsAndHashCode(callSuper = true)
public class KnownCoefficient extends Coefficient {
  public static final KnownCoefficient ZERO = new KnownCoefficient(Fraction.ZERO);

  Fraction value;
}
