package xyz.leutgeb.lorenz.logs.resources.constraints;

import com.microsoft.z3.BoolExpr;
import com.microsoft.z3.Context;
import com.microsoft.z3.RealExpr;
import lombok.Data;
import lombok.EqualsAndHashCode;
import org.hipparchus.fraction.Fraction;
import xyz.leutgeb.lorenz.logs.ast.Expression;
import xyz.leutgeb.lorenz.logs.resources.coefficients.Coefficient;

@Data
@EqualsAndHashCode(callSuper = true)
public class OffsetConstraint extends EqualityConstraint {
  private final Fraction offset;

  public static OffsetConstraint increment(
      int id, Expression source, Coefficient left, Coefficient right) {
    return new OffsetConstraint(id, source, left, right, Fraction.ONE);
  }

  public OffsetConstraint(
      int id, Expression source, Coefficient left, Coefficient right, Fraction offset) {
    super(id, source, left, right);
    this.offset = offset;
  }

  @Override
  public String toString() {
    return prefixed(left + " = " + right + " + " + offset);
  }

  @Override
  public BoolExpr encode(Context ctx, RealExpr[] coefficients) {
    return ctx.mkEq(
        left.encode(ctx, coefficients),
        ctx.mkAdd(
            right.encode(ctx, coefficients),
            ctx.mkReal(offset.getNumerator(), offset.getDenominator())));
  }
}
