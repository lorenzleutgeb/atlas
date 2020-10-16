package xyz.leutgeb.lorenz.lac.typing.resources;

import java.util.Map;
import xyz.leutgeb.lorenz.lac.typing.resources.coefficients.Coefficient;
import xyz.leutgeb.lorenz.lac.typing.resources.coefficients.KnownCoefficient;

public record FunctionAnnotation(Annotation from, Annotation to) {
  @Override
  public String toString() {
    return from.toString() + " → " + to.toString();
  }

  public boolean isUnknown() {
    return from.isUnknown() || to.isUnknown();
  }

  public FunctionAnnotation substitute(Map<Coefficient, KnownCoefficient> solution) {
    return new FunctionAnnotation(from.substitute(solution), to.substitute(solution));
  }

  public boolean isNonInteger() {
    return from.isNonInteger() || to.isNonInteger();
  }
}
