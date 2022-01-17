package xyz.leutgeb.lorenz.atlas.typing.resources;

import java.util.List;
import java.util.Map;
import lombok.AllArgsConstructor;
import lombok.Value;
import xyz.leutgeb.lorenz.atlas.typing.resources.coefficients.Coefficient;
import xyz.leutgeb.lorenz.atlas.typing.resources.coefficients.KnownCoefficient;

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

  public FunctionAnnotation(
      List<Coefficient> rankCoefficients, Map<List<Integer>, Coefficient> coefficients) {
    this(new Annotation(rankCoefficients, coefficients, "unknownfunctionannotation"));
  }

  public FunctionAnnotation substitute(Map<Coefficient, KnownCoefficient> solution) {
    return new FunctionAnnotation(from.substitute(solution), to.substitute(solution));
  }

  public FunctionAnnotation(Annotation annotation) {
    this(annotation, annotation);
  }

  public boolean isZero() {
    return from.isZero() && to.isZero();
  }

  public String getBound(List<String> arguments) {
    if (from.size() == to.size()) {
      return Annotation.subtract(from, to).toLongString(arguments);
    }

    final var right = to.toLongString();
    if ("0".equals(right)) {
      return from.toLongString(arguments);
    }
    return from.toLongString(arguments) + " - [" + to.toLongString() + "]";
  }
}
