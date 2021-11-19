package xyz.leutgeb.lorenz.atlas.typing.resources.coefficients;

import com.microsoft.z3.ArithExpr;
import com.microsoft.z3.Context;
import com.microsoft.z3.RealExpr;
import com.microsoft.z3.RealSort;
import java.util.Map;
import org.hipparchus.fraction.Fraction;

public interface Coefficient {
  static KnownCoefficient of(int value) {
    return new KnownCoefficient(value);
  }

  static KnownCoefficient of(int numerator, int denominator) {
    return new KnownCoefficient(new Fraction(numerator, denominator));
  }

  static KnownCoefficient of(Fraction fraction) {
    return new KnownCoefficient(fraction);
  }

  ArithExpr<RealSort> encode(Context ctx, Map<UnknownCoefficient, RealExpr> coefficients);

  Coefficient replace(Coefficient target, Coefficient replacement);

  Coefficient canonical();

  Coefficient negate();
}
