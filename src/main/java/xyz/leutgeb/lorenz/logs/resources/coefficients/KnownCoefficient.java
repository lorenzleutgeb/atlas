package xyz.leutgeb.lorenz.logs.resources.coefficients;

import com.microsoft.z3.Context;
import com.microsoft.z3.RealExpr;
import lombok.EqualsAndHashCode;
import lombok.Value;
import org.hipparchus.fraction.Fraction;

@Value
@EqualsAndHashCode(callSuper = true)
public class KnownCoefficient extends Coefficient {
  public static final KnownCoefficient ZERO = new KnownCoefficient(Fraction.ZERO);

  Fraction value;

  public RealExpr encode(Context context, RealExpr[] coefficients) {
    return context.mkReal(value.getNumerator(), value.getDenominator());
  }

  @Override
  public String toString() {
    return value.toString();
  }
}
