package xyz.leutgeb.lorenz.logs.resources;

import lombok.Value;
import org.hipparchus.fraction.Fraction;

@Value
public class KnownCoefficient extends Coefficient {
  Fraction value;
}
