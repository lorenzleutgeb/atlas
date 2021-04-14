package xyz.leutgeb.lorenz.atlas.typing.resources.coefficients;

import static xyz.leutgeb.lorenz.atlas.util.Util.bug;
import static xyz.leutgeb.lorenz.atlas.util.Util.isInteger;

import com.microsoft.z3.ArithExpr;
import com.microsoft.z3.Context;
import java.util.Map;
import java.util.function.Predicate;
import lombok.EqualsAndHashCode;
import lombok.Value;
import org.hipparchus.fraction.Fraction;
import xyz.leutgeb.lorenz.atlas.typing.resources.solving.ConstraintSystemSolver;

@Value
@EqualsAndHashCode
public class KnownCoefficient implements Coefficient {
  public static final KnownCoefficient ZERO = new KnownCoefficient(Fraction.ZERO);
  public static final KnownCoefficient ONE = new KnownCoefficient(Fraction.ONE);
  public static final KnownCoefficient TWO = new KnownCoefficient(Fraction.TWO);
  public static final KnownCoefficient THREE = new KnownCoefficient(new Fraction(3));
  public static final KnownCoefficient MINUS_TWO = new KnownCoefficient(Fraction.TWO.negate());
  public static final KnownCoefficient MINUS_ONE = new KnownCoefficient(Fraction.ONE.negate());
  public static final KnownCoefficient ONE_BY_TWO = new KnownCoefficient(new Fraction(1, 2));
  public static final KnownCoefficient THREE_BY_TWO = new KnownCoefficient(new Fraction(3, 2));
  public static final KnownCoefficient FIVE_BY_TWO = new KnownCoefficient(new Fraction(5, 2));

  Fraction value;

  public KnownCoefficient(Fraction value) {
    this.value = value;
  }

  public KnownCoefficient(int value) {
    this(new Fraction(value));
  }

  public ArithExpr encode(
      Context context,
      Map<UnknownCoefficient, ArithExpr> coefficients,
      ConstraintSystemSolver.Domain domain) {
    if (isInteger(value) && ConstraintSystemSolver.Domain.INTEGER.equals(domain)) {
      return context.mkInt(value.getNumerator());
    }
    if (ConstraintSystemSolver.Domain.RATIONAL.equals(domain)) {
      return context.mkReal(value.getNumerator(), value.getDenominator());
    }
    throw bug("cannot encode constant");
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
