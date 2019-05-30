package xyz.leutgeb.lorenz.logs.resources.constraints;

import com.microsoft.z3.BoolExpr;
import com.microsoft.z3.Context;
import com.microsoft.z3.RealExpr;

public class LessThanOrEqualConstraint extends Constraint {
  @Override
  public BoolExpr encode(Context ctx, RealExpr[] coefficients) {
    return ctx.mkTrue();
  }
}
