package xyz.leutgeb.lorenz.atlas.typing.resources;

import java.util.Map;
import lombok.AllArgsConstructor;
import lombok.Value;
import xyz.leutgeb.lorenz.atlas.typing.resources.coefficients.Coefficient;
import xyz.leutgeb.lorenz.atlas.typing.resources.coefficients.KnownCoefficient;

// TODO: Maybe refactor this to a record once Java 17 is out?
@Value
@AllArgsConstructor
public class FunctionAnnotation {
  public Annotation from;
  public Annotation to;

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

  public boolean isZero() {
    return from.isZero() && to.isZero();
  }

  public String getBound() {
    return Annotation.subtract(from, to).toLongString();
  }
}
