package xyz.leutgeb.lorenz.lac.typing.resources.coefficients;

import static xyz.leutgeb.lorenz.lac.Util.bug;
import static xyz.leutgeb.lorenz.lac.Util.randomHex;

import com.microsoft.z3.Context;
import com.microsoft.z3.RealExpr;
import java.util.Map;
import lombok.EqualsAndHashCode;
import lombok.Value;
import xyz.leutgeb.lorenz.lac.Util;

@Value
@EqualsAndHashCode(callSuper = true)
public class UnknownCoefficient extends Coefficient {
  int id;
  String name;
  boolean negated;

  public UnknownCoefficient(int id) {
    this(id, "");
  }

  private UnknownCoefficient(int id, String name) {
    this.id = id;
    this.name = name;
    this.negated = false;
  }

  private UnknownCoefficient(int id, String name, boolean negated) {
    this.id = id;
    this.name = name;
    this.negated = negated;
  }

  public UnknownCoefficient negate() {
    return new UnknownCoefficient(id, name, !negated);
  }

  public static UnknownCoefficient unknown() {
    return unknown(randomHex());
  }

  public static UnknownCoefficient unknown(String name) {
    if (name.isBlank()) {
      throw new IllegalArgumentException("name cannot be blank");
    }
    return new UnknownCoefficient(0, name);
  }

  @Override
  public String toString() {
    final var prefix = negated ? "-" : "";
    if (name.isEmpty()) {
      return prefix + "∂" + Util.generateSubscript(id);
    } else {
      return prefix + name;
    }
  }

  public RealExpr encode(Context ctx, Map<Coefficient, RealExpr> coefficients) {
    final var inner = coefficients.get(this);
    return negated ? (RealExpr) ctx.mkUnaryMinus(inner) : inner;
  }

  @Override
  public Coefficient replace(Coefficient target, Coefficient replacement) {
    if (negated) {
      throw bug("don't know how to replace negated coefficient");
    }
    if (target instanceof UnknownCoefficient) {
      if (((UnknownCoefficient) target).id == id) {
        return replacement;
      }
    }
    return this;
  }
}
