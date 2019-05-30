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

  @Override
  public String toString() {
    return "∂" + Util.generateSubscript(id);
  }

  public RealExpr encode(Context ctx, RealExpr[] coefficients) {
    return coefficients[id];
  }
}
