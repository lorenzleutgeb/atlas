package xyz.leutgeb.lorenz.lac.typing.resources;

import java.util.Collections;
import java.util.HashSet;
import java.util.Map;
import java.util.Set;
import java.util.stream.Collectors;

import lombok.AllArgsConstructor;
import lombok.Value;
import xyz.leutgeb.lorenz.lac.typing.resources.coefficients.Coefficient;
import xyz.leutgeb.lorenz.lac.typing.resources.coefficients.KnownCoefficient;

// TODO: Maybe refactor this to a record once Java 17 is out?
@Value
@AllArgsConstructor
public class CombinedFunctionAnnotation {
    public FunctionAnnotation withCost;
    public Set<FunctionAnnotation> withoutCost;

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

  public boolean isNonInteger() {
    return withCost.isNonInteger()
        || (!withoutCost.isEmpty()
            && withoutCost.stream().allMatch(FunctionAnnotation::isNonInteger));
  }
}
