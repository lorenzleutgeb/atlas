package xyz.leutgeb.lorenz.lac.typing.resources.coefficients;

import com.microsoft.z3.Context;
import com.microsoft.z3.RealExpr;
import java.util.Map;
import lombok.EqualsAndHashCode;
import lombok.Value;
import org.hipparchus.fraction.Fraction;

@Value
@EqualsAndHashCode(callSuper = false)
public class KnownCoefficient extends Coefficient {
  public static final KnownCoefficient ZERO = new KnownCoefficient(Fraction.ZERO);
  public static final KnownCoefficient ONE = new KnownCoefficient(Fraction.ONE);
  public static final KnownCoefficient TWO = new KnownCoefficient(Fraction.TWO);

  Fraction value;

  public RealExpr encode(Context context, Map<Coefficient, RealExpr> coefficients) {
    return context.mkReal(value.getNumerator(), value.getDenominator());
  }

  @Override
  public Coefficient replace(Coefficient target, Coefficient replacement) {
    return this;
  }

  @Override
  public String toString() {
    return value.toString();
  }
}
