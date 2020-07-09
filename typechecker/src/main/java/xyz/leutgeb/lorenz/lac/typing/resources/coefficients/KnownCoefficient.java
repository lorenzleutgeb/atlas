package xyz.leutgeb.lorenz.lac.typing.resources.coefficients;

import com.microsoft.z3.Context;
import com.microsoft.z3.RealExpr;
import java.util.Map;
import lombok.EqualsAndHashCode;
import lombok.Value;
import org.hipparchus.fraction.Fraction;

@Value
@EqualsAndHashCode
public class KnownCoefficient implements Coefficient {
  public static final KnownCoefficient ZERO = new KnownCoefficient(Fraction.ZERO);
  public static final KnownCoefficient ONE = new KnownCoefficient(Fraction.ONE);
  public static final KnownCoefficient TWO = new KnownCoefficient(Fraction.TWO);
  public static final KnownCoefficient THREE = new KnownCoefficient(new Fraction(3));

  Fraction value;

  public RealExpr encode(Context context, Map<Coefficient, RealExpr> coefficients) {
    return context.mkReal(value.getNumerator(), value.getDenominator());
  }

  @Override
  public Coefficient replace(Coefficient target, Coefficient replacement) {
    return this;
  }

  @Override
  public Coefficient canonical() {
    return this;
  }

  @Override
  public Coefficient negate() {
    return new KnownCoefficient(value.negate());
  }

  @Override
  public String toString() {
    return value.toString();
  }
}
