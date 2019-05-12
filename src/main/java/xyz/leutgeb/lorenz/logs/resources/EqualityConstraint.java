package xyz.leutgeb.lorenz.logs.resources;

import lombok.Data;
import lombok.EqualsAndHashCode;
import xyz.leutgeb.lorenz.logs.resources.coefficients.Coefficient;

@Data
@EqualsAndHashCode(callSuper = true)
public class EqualityConstraint extends Constraint {
  protected final Coefficient left, right;

  @Override
  public String toString() {
    return left + " = " + right;
  }
}
