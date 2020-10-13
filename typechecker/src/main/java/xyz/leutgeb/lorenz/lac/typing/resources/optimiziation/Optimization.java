package xyz.leutgeb.lorenz.lac.typing.resources.optimiziation;

import static java.util.Collections.emptySet;
import static xyz.leutgeb.lorenz.lac.typing.resources.coefficients.UnknownCoefficient.unknown;

import java.util.List;
import java.util.Set;
import xyz.leutgeb.lorenz.lac.typing.resources.FunctionAnnotation;
import xyz.leutgeb.lorenz.lac.typing.resources.coefficients.UnknownCoefficient;
import xyz.leutgeb.lorenz.lac.typing.resources.constraints.Constraint;
import xyz.leutgeb.lorenz.lac.typing.resources.constraints.EqualsSumConstraint;

public class Optimization {
  public record Target(UnknownCoefficient coefficient, Set<Constraint> constraints) {}

  public static Target sum(FunctionAnnotation annotation) {
    final var from = annotation.from();
    final var to = annotation.to();

    final var qRankCoefficientSum = unknown("Qranksum");
    final var qpRankCoefficientSum = unknown("Qpranksum");
    final var qCoefficientSum = unknown("Qothersum");
    final var qpCoefficientSum = unknown("Qpothersum");
    final var rankCoefficientSum = unknown("rankCoefficientSum");
    final var coefficientSum = unknown("coefficientSum");
    final var x = unknown("sum");

    return new Target(
        x,
        Set.of(
            from.sumRankCoefficients(qRankCoefficientSum),
            to.sumRankCoefficients(qpRankCoefficientSum),
            from.sumCoefficients(qCoefficientSum),
            to.sumCoefficients(qpCoefficientSum),
            new EqualsSumConstraint(
                rankCoefficientSum,
                List.of(qRankCoefficientSum, qpRankCoefficientSum),
                "(opt) rank coefficient sum"),
            new EqualsSumConstraint(
                coefficientSum,
                List.of(qCoefficientSum, qpCoefficientSum),
                "(opt) coefficient sum"),
            new EqualsSumConstraint(
                x, List.of(rankCoefficientSum, coefficientSum), "(opt) coefficient sum")));
  }

  public static Target setCounting(FunctionAnnotation annotation) {
    final var from = annotation.from();
    final var to = annotation.to();
    final var x = unknown("setDiffSum");
    return new Target(x, from.size() == to.size() ? Set.copyOf(from.diff(to, x)) : emptySet());
  }

  public static Target pairwiseDiff(FunctionAnnotation annotation) {
    final var from = annotation.from();
    final var to = annotation.to();
    final var x = unknown("pairwiseDiffSum");
    return new Target(
        x, from.size() == to.size() ? Set.copyOf(from.pairwiseDiff(to, x)) : emptySet());
  }
}
