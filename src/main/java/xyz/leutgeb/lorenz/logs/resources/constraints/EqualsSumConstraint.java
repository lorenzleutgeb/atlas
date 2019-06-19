package xyz.leutgeb.lorenz.logs.resources.constraints;

import com.microsoft.z3.ArithExpr;
import com.microsoft.z3.BoolExpr;
import com.microsoft.z3.Context;
import com.microsoft.z3.RealExpr;
import java.util.Collection;
import lombok.Value;
import xyz.leutgeb.lorenz.logs.resources.coefficients.Coefficient;

@Value
public class EqualsSumConstraint extends Constraint {
  Coefficient left;
  Collection<Coefficient> sum;

  @Override
  public String toString() {
    return left + " = " + sum;
  }

  @Override
  public BoolExpr encode(Context ctx, RealExpr[] coefficients) {
    final ArithExpr[] encodedSum =
        sum.stream().map(c -> c.encode(ctx, coefficients)).toArray(ArithExpr[]::new);

    return ctx.mkEq(left.encode(ctx, coefficients), ctx.mkAdd(encodedSum));
  }
}
