package xyz.leutgeb.lorenz.logs.resources;

import lombok.Value;
import xyz.leutgeb.lorenz.logs.resources.coefficients.Coefficient;

@Value
public class EqualityConstraint extends Constraint {
  Coefficient left, right;

  @Override
  public String toString() {
    return left + " = " + right;
  }
}
