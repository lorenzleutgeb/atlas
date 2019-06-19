package xyz.leutgeb.lorenz.logs.resources.constraints;

import com.microsoft.z3.BoolExpr;
import com.microsoft.z3.Context;
import com.microsoft.z3.RealExpr;
import java.util.Objects;
import lombok.Data;
import lombok.EqualsAndHashCode;
import xyz.leutgeb.lorenz.logs.resources.coefficients.Coefficient;

@Data
@EqualsAndHashCode(callSuper = true)
public class EqualityConstraint extends Constraint {
  protected final Coefficient left, right;

  public EqualityConstraint(Coefficient left, Coefficient right) {
    Objects.requireNonNull(left);
    Objects.requireNonNull(right);
    this.left = left;
    this.right = right;
  }

  public BoolExpr encode(Context ctx, RealExpr[] coefficients) {
    return ctx.mkEq(left.encode(ctx, coefficients), right.encode(ctx, coefficients));
  }

  @Override
  public String toString() {
    return left + " = " + right;
  }
}
