package xyz.leutgeb.lorenz.logs.resources.constraints;

import lombok.Data;
import lombok.EqualsAndHashCode;
import org.hipparchus.fraction.Fraction;
import xyz.leutgeb.lorenz.logs.resources.coefficients.Coefficient;

@Data
@EqualsAndHashCode(callSuper = true)
public class OffsetConstraint extends EqualityConstraint {
  private final Fraction offset;

  public static OffsetConstraint increment(Coefficient left, Coefficient right) {
    return new OffsetConstraint(left, right, Fraction.ONE);
  }

  public OffsetConstraint(Coefficient left, Coefficient right, Fraction offset) {
    super(left, right);
    this.offset = offset;
  }

  @Override
  public String toString() {
    return left + " = " + right + " + " + offset;
  }
}
