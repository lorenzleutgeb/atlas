package xyz.leutgeb.lorenz.lac.typing.resources.coefficients;

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

  public UnknownCoefficient(int id) {
    this(id, "");
  }

  private UnknownCoefficient(int id, String name) {
    this.id = id;
    this.name = name;
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
    if (name.isEmpty()) {
      return "∂" + Util.generateSubscript(id);
    } else {
      return name;
    }
  }

  public RealExpr encode(Context ctx, Map<Coefficient, RealExpr> coefficients) {
    return coefficients.get(this);
  }

  @Override
  public Coefficient replace(Coefficient target, Coefficient replacement) {
    if (target instanceof UnknownCoefficient) {
      if (((UnknownCoefficient) target).id == id) {
        return replacement;
      }
    }
    return this;
  }
}
