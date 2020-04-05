package xyz.leutgeb.lorenz.lac.typing.resources.rules;

import static java.util.stream.Collectors.toList;
import static java.util.stream.IntStream.range;
import static java.util.stream.Stream.concat;
import static xyz.leutgeb.lorenz.lac.Util.append;
import static xyz.leutgeb.lorenz.lac.typing.resources.Annotation.isUnitIndex;

import java.util.List;
import lombok.extern.log4j.Log4j2;
import org.hipparchus.fraction.Fraction;
import xyz.leutgeb.lorenz.lac.ast.CallExpression;
import xyz.leutgeb.lorenz.lac.typing.resources.AnnotatingGlobals;
import xyz.leutgeb.lorenz.lac.typing.resources.Annotation;
import xyz.leutgeb.lorenz.lac.typing.resources.constraints.Constraint;
import xyz.leutgeb.lorenz.lac.typing.resources.constraints.EqualityConstraint;
import xyz.leutgeb.lorenz.lac.typing.resources.constraints.OffsetConstraint;
import xyz.leutgeb.lorenz.lac.typing.resources.proving.Obligation;

@Log4j2
public class App {
  private static List<Constraint> increment(Annotation left, Annotation right, int cost) {
    if (cost == 0) {
      return EqualityConstraint.eq(left, right);
    }

    if (left.size() != right.size()) {
      throw new IllegalArgumentException("annotations must be of same size");
    }

    return concat(
            range(0, left.size())
                .mapToObj(
                    i ->
                        new EqualityConstraint(
                            left.getRankCoefficient(i), right.getRankCoefficient(i))),
            left.streamCoefficients()
                .map(
                    leftEntry -> {
                      var leftCoefficient = leftEntry.getValue();
                      var rightCoefficient = right.getCoefficientOrZero(leftEntry.getKey());
                      return isUnitIndex(leftEntry.getKey())
                          ? new OffsetConstraint(
                              leftCoefficient, rightCoefficient, new Fraction(cost))
                          : new EqualityConstraint(leftCoefficient, rightCoefficient);
                    }))
        .collect(toList());
  }

  public static Rule.ApplicationResult apply(Obligation obligation, AnnotatingGlobals globals) {
    final var expression = (CallExpression) obligation.getExpression();

    // Look up global annotation for this function.
    final var annotation = globals.getDependingOnCost(expression.getName().getName());

    return Rule.ApplicationResult.onlyConstraints(
        append(
            increment(
                obligation.getContext().getAnnotation(),
                annotation.getFirst(),
                obligation.getCost()),
            EqualityConstraint.eq(annotation.getSecond(), obligation.getAnnotation())));
  }
}
