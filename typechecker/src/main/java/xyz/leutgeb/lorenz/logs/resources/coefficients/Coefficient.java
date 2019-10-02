package xyz.leutgeb.lorenz.logs.resources.coefficients;

import com.microsoft.z3.Context;
import com.microsoft.z3.RealExpr;
import xyz.leutgeb.lorenz.logs.resources.Constraints;

public abstract class Coefficient {
  public Coefficient add(Coefficient other, Constraints context) {
    throw new UnsupportedOperationException();
  }

  public abstract RealExpr encode(Context ctx, RealExpr[] coefficients);
}
