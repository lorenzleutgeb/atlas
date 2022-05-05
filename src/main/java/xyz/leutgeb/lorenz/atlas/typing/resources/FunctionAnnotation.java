package xyz.leutgeb.lorenz.atlas.typing.resources;

import static xyz.leutgeb.lorenz.atlas.typing.resources.Annotation.isUnitIndex;

import java.util.List;
import java.util.Map;
import java.util.stream.Collectors;
import lombok.AllArgsConstructor;
import lombok.Value;
import xyz.leutgeb.lorenz.atlas.typing.resources.coefficients.Coefficient;
import xyz.leutgeb.lorenz.atlas.typing.resources.coefficients.KnownCoefficient;
import xyz.leutgeb.lorenz.atlas.util.Util;

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

    boolean combine =
        to.size() == 1
            && from.size() > 1
            && from.getUnitCoefficientOrZero() instanceof KnownCoefficient
            && to.getUnitCoefficientOrZero() instanceof KnownCoefficient;
    for (int i = 0; i < from.size() && combine; i++) {
      combine = from.getRankCoefficient(i).equals(to.getRankCoefficient());
    }

    if (combine) {
      final var left =
          new Annotation(
                  Util.repeat((Coefficient) KnownCoefficient.ZERO, from.size()).toList(),
                  from.streamNonRankCoefficients()
                      .filter(e -> !isUnitIndex(e.getKey()))
                      .collect(
                          Collectors.toUnmodifiableMap(Map.Entry::getKey, Map.Entry::getValue)),
                  "Qbound")
              .toLongString(arguments);

      final var right =
          new Annotation(
                  Util.repeat((Coefficient) KnownCoefficient.ZERO, to.size()).toList(),
                  to.streamNonRankCoefficients()
                      .filter(e -> !isUnitIndex(e.getKey()))
                      .collect(
                          Collectors.toUnmodifiableMap(Map.Entry::getKey, Map.Entry::getValue)),
                  "Qbound'")
              .toLongString();

      var c =
          ((KnownCoefficient) from.getUnitCoefficientOrZero())
              .getValue()
              .subtract(((KnownCoefficient) to.getUnitCoefficientOrZero()).getValue());

      if ("0".equals(right)) {
        if (c.signum() > 0) {
          return left + " + " + Coefficient.known(c);
        } else if (c.signum() < 0) {
          return left + " - " + Coefficient.known(c.abs());
        } else {
          return left;
        }
      }
      if (c.signum() > 0) {
        return left + " + " + Coefficient.known(c) + " - [" + right + "]";
      } else if (c.signum() < 0) {
        return left + " - [" + Coefficient.known(c.abs()) + " + " + right + "]";
      } else {
        return left + " - [" + right + "]";
      }
    } else {
      if (to.isZero()) {
        return from.toLongString(arguments);
      }
      return from.toLongString(arguments) + " - [" + to.toLongString() + "]";
    }
  }

  public boolean sameSize() {
    return from.size() == to.size();
  }
}
