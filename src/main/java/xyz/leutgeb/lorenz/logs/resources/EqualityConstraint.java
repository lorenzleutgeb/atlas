package xyz.leutgeb.lorenz.logs.resources;

import lombok.EqualsAndHashCode;
import lombok.Value;
import xyz.leutgeb.lorenz.logs.resources.coefficients.Coefficient;

@Value
@EqualsAndHashCode(callSuper = true)
public class EqualityConstraint extends Constraint {
  Coefficient left, right;

  @Override
  public String toString() {
    return left + " = " + right;
  }
}
