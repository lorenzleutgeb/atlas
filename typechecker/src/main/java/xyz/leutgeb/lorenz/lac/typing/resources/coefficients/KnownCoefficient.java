package xyz.leutgeb.lorenz.lac.typing.resources.coefficients;

import com.microsoft.z3.ArithExpr;
import com.microsoft.z3.Context;
import java.util.Map;
import java.util.function.Predicate;
import lombok.EqualsAndHashCode;
import lombok.Value;
import xyz.leutgeb.lorenz.lac.util.Fraction;

@Value
@EqualsAndHashCode
public class KnownCoefficient implements Coefficient {
  public static final KnownCoefficient ZERO = new KnownCoefficient(Fraction.ZERO);
  public static final KnownCoefficient ONE = new KnownCoefficient(Fraction.ONE);
  public static final KnownCoefficient TWO = new KnownCoefficient(Fraction.TWO);
  public static final KnownCoefficient THREE = new KnownCoefficient(Fraction.THREE);
  public static final KnownCoefficient MINUS_TWO = new KnownCoefficient(Fraction.TWO.negate());

  Fraction value;

  public KnownCoefficient(Fraction value) {
    this.value = value;
  }

  public KnownCoefficient(int value) {
    this(new Fraction(value));
  }

  public ArithExpr encode(Context context, Map<UnknownCoefficient, ArithExpr> coefficients) {
    // if (value.getDenominator() != 1) {
    //  throw bug("oops");
    // }
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

  public static final Predicate<Coefficient> IS = x -> x instanceof KnownCoefficient;
}
