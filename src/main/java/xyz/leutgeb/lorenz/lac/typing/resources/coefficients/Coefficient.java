package xyz.leutgeb.lorenz.lac.typing.resources.coefficients;

import com.microsoft.z3.ArithExpr;
import com.microsoft.z3.Context;
import java.util.Map;
import xyz.leutgeb.lorenz.lac.util.Fraction;

public interface Coefficient {
  static KnownCoefficient of(int value) {
    return new KnownCoefficient(value);
  }

  static KnownCoefficient of(int numerator, int denominator) {
    return new KnownCoefficient(new Fraction(numerator, denominator));
  }

  ArithExpr encode(Context ctx, Map<UnknownCoefficient, ArithExpr> coefficients);

  Coefficient replace(Coefficient target, Coefficient replacement);

  Coefficient canonical();

  Coefficient negate();
}
