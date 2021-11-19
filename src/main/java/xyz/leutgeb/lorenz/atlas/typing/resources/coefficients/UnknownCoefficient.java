package xyz.leutgeb.lorenz.atlas.typing.resources.coefficients;

import static xyz.leutgeb.lorenz.atlas.util.Util.bug;
import static xyz.leutgeb.lorenz.atlas.util.Util.randomHex;

import com.microsoft.z3.ArithExpr;
import com.microsoft.z3.Context;
import com.microsoft.z3.RealExpr;
import com.microsoft.z3.RealSort;
import java.util.Map;
import lombok.AccessLevel;
import lombok.AllArgsConstructor;
import lombok.EqualsAndHashCode;
import lombok.Value;

@Value
@EqualsAndHashCode
@AllArgsConstructor(access = AccessLevel.PRIVATE)
public class UnknownCoefficient implements Coefficient {
  String name;
  boolean negated;
  boolean maybeNegative;

  public UnknownCoefficient(String name) {
    if (name == null || name.isBlank()) {
      throw new IllegalArgumentException("name cannot be null or blank");
    }
    this.name = name;
    this.negated = false;
    this.maybeNegative = false;
  }

  private UnknownCoefficient(String name, boolean negated) {
    this.name = name;
    this.negated = negated;
    this.maybeNegative = false;
  }

  public static UnknownCoefficient maybeNegative(String name) {
    return new UnknownCoefficient(name, false, true);
  }

  public static UnknownCoefficient maybeNegativeUnknown(String namePrefix) {
    return new UnknownCoefficient(namePrefix + randomHex(), false, true);
  }

  public static UnknownCoefficient unknown(String namePrefix) {
    return new UnknownCoefficient(namePrefix + randomHex());
  }

  @Override
  public UnknownCoefficient negate() {
    return new UnknownCoefficient(name, !negated, maybeNegative);
  }

  @Override
  public String toString() {
    return (negated ? "-" : "") + name;
  }

  public ArithExpr<RealSort> encode(Context ctx, Map<UnknownCoefficient, RealExpr> coefficients) {
    final var inner = coefficients.get(this.canonical());
    return negated ? ctx.mkUnaryMinus(inner) : inner;
  }

  @Override
  public Coefficient replace(Coefficient target, Coefficient replacement) {
    if (negated) {
      throw bug("don't know how to replace negated coefficient");
    }
    if (target instanceof final UnknownCoefficient unknownCoefficient) {
      if (unknownCoefficient.name.equals(name)) {
        return replacement;
      }
    }
    return this;
  }

  @Override
  public UnknownCoefficient canonical() {
    return negated ? negate() : this;
  }
}
