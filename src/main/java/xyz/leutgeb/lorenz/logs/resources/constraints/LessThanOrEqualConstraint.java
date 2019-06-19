package xyz.leutgeb.lorenz.logs.resources.constraints;

import com.microsoft.z3.BoolExpr;
import com.microsoft.z3.Context;
import com.microsoft.z3.RealExpr;
import lombok.Value;
import xyz.leutgeb.lorenz.logs.resources.coefficients.Coefficient;

@Value
public class LessThanOrEqualConstraint extends Constraint {
  protected final Coefficient left, right;

  @Override
  public BoolExpr encode(Context ctx, RealExpr[] coefficients) {
    return ctx.mkLe(left.encode(ctx, coefficients), right.encode(ctx, coefficients));
  }

  @Override
  public String toString() {
    return left + " <= " + right;
  }
}
