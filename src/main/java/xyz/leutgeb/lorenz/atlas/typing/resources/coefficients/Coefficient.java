package xyz.leutgeb.lorenz.atlas.typing.resources.coefficients;

import com.microsoft.z3.ArithExpr;
import com.microsoft.z3.Context;
import com.microsoft.z3.RealExpr;
import com.microsoft.z3.RealSort;
import java.util.Map;
import org.hipparchus.fraction.Fraction;

public interface Coefficient {

  static KnownCoefficient known(int value) {
    return new KnownCoefficient(value);
  }

  static KnownCoefficient known(int numerator, int denominator) {
    return new KnownCoefficient(new Fraction(numerator, denominator));
  }

  static KnownCoefficient known(Fraction fraction) {
    return new KnownCoefficient(fraction);
  }

  static UnknownCoefficient unknownMaybeNegative(String name) {
    return new UnknownCoefficient(name, false, true);
  }

  static UnknownCoefficient unknownFromPrefix(String namePrefix) {
    return UnknownCoefficient.fromPrefix(namePrefix);
  }

  static UnknownCoefficient unknown(String name) {
    return UnknownCoefficient.raw(name);
  }

  ArithExpr<RealSort> encode(Context ctx, Map<UnknownCoefficient, RealExpr> coefficients);

  Coefficient replace(Coefficient target, Coefficient replacement);

  Coefficient canonical();

  Coefficient negate();
}
