package xyz.leutgeb.lorenz.lac.typing.resources.coefficients;

import static xyz.leutgeb.lorenz.lac.Util.bug;
import static xyz.leutgeb.lorenz.lac.Util.randomHex;

import com.microsoft.z3.Context;
import com.microsoft.z3.RealExpr;
import java.util.Map;
import lombok.EqualsAndHashCode;
import lombok.Value;

@Value
@EqualsAndHashCode
public class UnknownCoefficient implements Coefficient {
  String name;
  boolean negated;

  public UnknownCoefficient(String name) {
    if (name == null || name.isBlank()) {
      throw new IllegalArgumentException("name cannot be null or blank");
    }
    this.name = name;
    this.negated = false;
  }

  private UnknownCoefficient(String name, boolean negated) {
    this.name = name;
    this.negated = negated;
  }

  public static UnknownCoefficient unknown(String name) {
    return new UnknownCoefficient(name + randomHex());
  }

  @Override
  public UnknownCoefficient negate() {
    return new UnknownCoefficient(name, !negated);
  }

  @Override
  public String toString() {
    return (negated ? "-" : "") + name;
  }

  public RealExpr encode(Context ctx, Map<Coefficient, RealExpr> coefficients) {
    final var inner = coefficients.get(this.canonical());
    return negated ? (RealExpr) ctx.mkUnaryMinus(inner) : inner;
  }

  @Override
  public Coefficient replace(Coefficient target, Coefficient replacement) {
    if (negated) {
      throw bug("don't know how to replace negated coefficient");
    }
    if (target instanceof UnknownCoefficient) {
      if (((UnknownCoefficient) target).name.equals(name)) {
        return replacement;
      }
    }
    return this;
  }

  @Override
  public Coefficient canonical() {
    return negated ? negate() : this;
  }
}
