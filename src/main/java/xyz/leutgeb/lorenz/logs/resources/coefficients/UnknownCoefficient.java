package xyz.leutgeb.lorenz.logs.resources.coefficients;

import lombok.Value;
import xyz.leutgeb.lorenz.logs.Util;

@Value
public class UnknownCoefficient extends Coefficient {
  int id;

  @Override
  public String toString() {
    return "∂" + Util.generateSubscript(id);
  }
}
