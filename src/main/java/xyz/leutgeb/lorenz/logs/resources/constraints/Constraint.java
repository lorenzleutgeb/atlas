package xyz.leutgeb.lorenz.logs.resources.constraints;

import com.microsoft.z3.BoolExpr;
import com.microsoft.z3.Context;
import com.microsoft.z3.RealExpr;

public abstract class Constraint {
  public abstract BoolExpr encode(Context ctx, RealExpr[] coefficients);
}
