package xyz.leutgeb.lorenz.atlas.typing.resources;

import static java.util.function.Predicate.not;

import java.util.Arrays;
import java.util.Collections;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.stream.Collectors;
import lombok.AllArgsConstructor;
import lombok.Value;
import xyz.leutgeb.lorenz.atlas.typing.resources.coefficients.Coefficient;
import xyz.leutgeb.lorenz.atlas.typing.resources.coefficients.KnownCoefficient;

// TODO: Maybe refactor this to a record once Java 17 is out?
@Value
@AllArgsConstructor
public class CombinedFunctionAnnotation {
  public FunctionAnnotation withCost;
  public Set<FunctionAnnotation> withoutCost;

  public static CombinedFunctionAnnotation of(
      FunctionAnnotation withCost, FunctionAnnotation... withoutCost) {
    final var withoutCostSet = new HashSet<>(Arrays.asList(withoutCost));
    return new CombinedFunctionAnnotation(withCost, Collections.unmodifiableSet(withoutCostSet));
  }

  public static CombinedFunctionAnnotation of(Annotation withCostFrom, Annotation withCostTo) {
    return new CombinedFunctionAnnotation(
        new FunctionAnnotation(withCostFrom, withCostTo), Set.of());
  }

  public static CombinedFunctionAnnotation of(
      Annotation withCostFrom, Annotation withCostTo, FunctionAnnotation... withoutCost) {
    final var withoutCostSet = new HashSet<>(Arrays.asList(withoutCost));
    return new CombinedFunctionAnnotation(
        new FunctionAnnotation(withCostFrom, withCostTo),
        Collections.unmodifiableSet(withoutCostSet));
  }

  public static CombinedFunctionAnnotation of(
      Annotation withCostFrom, Annotation withCostTo, Annotation... withoutCost) {
    if (withoutCost.length % 2 != 0) {
      throw new IllegalArgumentException(
          "even number of annotations without cost is required (pairs)");
    }
    final var withoutCostSet = new HashSet<FunctionAnnotation>();
    for (int i = 0; i < withoutCost.length - 1; i += 2) {
      withoutCostSet.add(new FunctionAnnotation(withoutCost[i], withoutCost[i + 1]));
    }
    return new CombinedFunctionAnnotation(
        new FunctionAnnotation(withCostFrom, withCostTo),
        Collections.unmodifiableSet(withoutCostSet));
  }

  @Override
  public String toString() {
    /*
    if (this.withoutCost.isEmpty()
        || (this.withoutCost.size() == 1 && Util.pick(this.withoutCost).isZero())) {
      // If there are no cf-Annotations, or there is only one and it is zero, we hide them.
      return withCost.toString();
    }
     */

    return "["
        + withCost
        + ", "
        + withoutCost.stream().map(Object::toString).collect(Collectors.joining(", ", "{", "}"))
        + "]";
  }

  public CombinedFunctionAnnotation substitute(Map<Coefficient, KnownCoefficient> solution) {
    return new CombinedFunctionAnnotation(
        withCost.substitute(solution),
        withoutCost.stream().map(x -> x.substitute(solution)).collect(Collectors.toSet()));
  }

  public boolean isUnknown() {
    return withCost.isUnknown()
        || (!withoutCost.isEmpty() && withoutCost.stream().anyMatch(FunctionAnnotation::isUnknown));
  }

  public String getBounds(List<String> arguments) {
    if (isUnknown()) {
      return "?";
    }

    return withCost.getBound(arguments);
  }

  public String getBoundsIncludingCf(List<String> arguments) {
    if (isUnknown()) {
      return "?";
    }
    final var withCost = this.withCost.getBound(arguments);
    final var withoutCost =
        this.withoutCost.stream()
            .map(x -> x.getBound(arguments))
            .filter(not("0"::equals))
            .collect(Collectors.joining(", ", "{", "}"));

    if ("{}".equals(withoutCost)) {
      return withCost;
    }

    return "[" + withCost + ", " + withoutCost + "]";
  }
}
