package xyz.leutgeb.lorenz.lac.typing.resources.rules;

import static java.util.stream.Collectors.toList;
import static java.util.stream.IntStream.range;
import static java.util.stream.Stream.concat;
import static xyz.leutgeb.lorenz.lac.Util.append;
import static xyz.leutgeb.lorenz.lac.typing.resources.Annotation.isUnitIndex;

import java.util.List;
import lombok.extern.slf4j.Slf4j;
import org.hipparchus.fraction.Fraction;
import xyz.leutgeb.lorenz.lac.ast.CallExpression;
import xyz.leutgeb.lorenz.lac.typing.resources.AnnotatingGlobals;
import xyz.leutgeb.lorenz.lac.typing.resources.Annotation;
import xyz.leutgeb.lorenz.lac.typing.resources.constraints.Constraint;
import xyz.leutgeb.lorenz.lac.typing.resources.constraints.EqualityConstraint;
import xyz.leutgeb.lorenz.lac.typing.resources.constraints.OffsetConstraint;
import xyz.leutgeb.lorenz.lac.typing.resources.proving.Obligation;

@Slf4j
public class App {
  private static String ruleName(int cost) {
    return "(app" + (cost == 0 ? ":cf" : "") + ")";
  }

  private static List<Constraint> increment(Annotation left, Annotation right, int cost) {
    if (cost == 0) {
      return EqualityConstraint.eq(
          "(app:cf) Q from signature = Q from context (equal since cost is zero)", left, right);
    }

    if (left.size() != right.size()) {
      throw new IllegalArgumentException("annotations must be of same size");
    }

    return concat(
            range(0, left.size())
                .mapToObj(
                    i ->
                        new EqualityConstraint(
                            left.getRankCoefficient(i),
                            right.getRankCoefficient(i),
                            ruleName(cost)
                                + " Q from signature = Q from context (rank coefficient stays the same)")),
            left.streamCoefficients()
                .map(
                    leftEntry -> {
                      var leftCoefficient = leftEntry.getValue();
                      var rightCoefficient = right.getCoefficientOrZero(leftEntry.getKey());
                      return isUnitIndex(leftEntry.getKey())
                          ? new OffsetConstraint(
                              leftCoefficient,
                              rightCoefficient,
                              new Fraction(cost),
                              ruleName(cost) + " Q from signature + 1 = Q from context")
                          : new EqualityConstraint(
                              leftCoefficient,
                              rightCoefficient,
                              ruleName(cost)
                                  + " Q from signature = Q from context (index "
                                  + leftEntry.getKey()
                                  + " stays the same, which means "
                                  + leftCoefficient
                                  + " = "
                                  + rightCoefficient
                                  + ")");
                    }))
        .collect(toList());
  }

  public static Rule.ApplicationResult apply(Obligation obligation, AnnotatingGlobals globals) {
    final var expression = (CallExpression) obligation.getExpression();

    // Look up global annotation for this function.
    final var annotation =
        globals.getDependingOnCost(expression.getName().getName(), obligation.getCost());

    return Rule.ApplicationResult.onlyConstraints(
        append(
            increment(
                obligation.getContext().getAnnotation(), annotation.from(), obligation.getCost()),
            EqualityConstraint.eq(
                ruleName(obligation.getCost()) + " Q'",
                annotation.to(),
                obligation.getAnnotation())));
  }
}
