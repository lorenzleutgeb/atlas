package xyz.leutgeb.lorenz.logs.resources.coefficients;

import com.microsoft.z3.Context;
import com.microsoft.z3.RealExpr;
import lombok.EqualsAndHashCode;
import lombok.Value;
import xyz.leutgeb.lorenz.logs.Util;

@Value
@EqualsAndHashCode(callSuper = true)
public class UnknownCoefficient extends Coefficient {
  int id;
  String name;

  public UnknownCoefficient(int id) {
    this(id, "");
  }

  public UnknownCoefficient(int id, String name) {
    this.id = id;
    this.name = name;
  }

  @Override
  public String toString() {
    if (name.isEmpty()) {
      return "∂" + Util.generateSubscript(id);
    } else {
      return name;
    }
  }

  public RealExpr encode(Context ctx, RealExpr[] coefficients) {
    return coefficients[id];
  }
}
