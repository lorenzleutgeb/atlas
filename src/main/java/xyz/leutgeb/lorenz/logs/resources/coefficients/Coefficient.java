package xyz.leutgeb.lorenz.logs.resources.coefficients;

import xyz.leutgeb.lorenz.logs.resources.Constraints;

public abstract class Coefficient {
  public Coefficient add(Coefficient other, Constraints context) {
    throw new UnsupportedOperationException();
  }
}
