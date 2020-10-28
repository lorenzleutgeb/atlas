package xyz.leutgeb.lorenz.lac.typing.resources.optimiziation;

import static xyz.leutgeb.lorenz.lac.typing.resources.coefficients.KnownCoefficient.ONE;
import static xyz.leutgeb.lorenz.lac.typing.resources.coefficients.KnownCoefficient.ZERO;

import java.util.ArrayList;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Optional;
import java.util.Set;
import lombok.AllArgsConstructor;
import lombok.Value;
import xyz.leutgeb.lorenz.lac.typing.resources.FunctionAnnotation;
import xyz.leutgeb.lorenz.lac.typing.resources.coefficients.UnknownCoefficient;
import xyz.leutgeb.lorenz.lac.typing.resources.constraints.Constraint;
import xyz.leutgeb.lorenz.lac.typing.resources.constraints.EqualityConstraint;
import xyz.leutgeb.lorenz.lac.typing.resources.constraints.EqualsSumConstraint;
import xyz.leutgeb.lorenz.lac.typing.resources.constraints.ExclusiveDisjunctiveConstraint;
import xyz.leutgeb.lorenz.lac.typing.resources.constraints.IfThenElseConstraint;

public class Optimization {
  @Value
  @AllArgsConstructor
  public static class MultiTarget {
    public List<UnknownCoefficient> rankCoefficients;
    public List<UnknownCoefficient> nonRankCoefficients;
    public Set<Constraint> constraints;
  }

  /*
  @Deprecated
  public static Target sum(FunctionAnnotation annotation) {
    final var from = annotation.from;
    final var to = annotation.to;

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
   */

  public static Optional<MultiTarget> setCounting(FunctionAnnotation annotation) {
    final var from = annotation.from;
    final var to = annotation.to;

    if (to.size() != from.size()) {
      return Optional.empty();
    }

    int size = to.size();

    final Set<Constraint> constraints = new HashSet<>();

    final List<UnknownCoefficient> rankCoefficients = new ArrayList<>(size);
    for (int i = 0; i < size; i++) {
      final var x = UnknownCoefficient.unknown("x");
      constraints.add(
          new IfThenElseConstraint(
              new ExclusiveDisjunctiveConstraint(
                  new EqualityConstraint(from.getRankCoefficient(i), ZERO, "(opt)"),
                  new EqualityConstraint(to.getRankCoefficient(i), ZERO, "(opt)"),
                  "(opt)"),
              ONE,
              ZERO,
              x,
              "(opt)"));
      rankCoefficients.add(x);
    }

    final List<UnknownCoefficient> nonRankCoefficients = new ArrayList<>();

    from.union(to)
        .map(Map.Entry::getValue)
        .forEach(
            pair -> {
              final var x = UnknownCoefficient.unknown("x");
              constraints.add(
                  new IfThenElseConstraint(
                      new ExclusiveDisjunctiveConstraint(
                          new EqualityConstraint(pair.getLeft(), ZERO, "(opt)"),
                          new EqualityConstraint(pair.getRight(), ZERO, "(opt)"),
                          "(opt)"),
                      ONE,
                      ZERO,
                      x,
                      "(opt)"));
              nonRankCoefficients.add(x);
            });

    return Optional.of(new MultiTarget(rankCoefficients, nonRankCoefficients, constraints));
  }

  public static Optional<MultiTarget> pairwiseDiff(FunctionAnnotation annotation) {
    final var from = annotation.from;
    final var to = annotation.to;

    if (to.size() != from.size()) {
      return Optional.empty();
    }

    int size = to.size();

    final Set<Constraint> constraints = new HashSet<>();

    final List<UnknownCoefficient> rankCoefficients = new ArrayList<>(size);
    for (int i = 0; i < size; i++) {
      final var x = UnknownCoefficient.unknown("x");
      constraints.add(
          new EqualsSumConstraint(
              x, List.of(from.getRankCoefficient(i), to.getRankCoefficient(i).negate()), "(opt)"));
      rankCoefficients.add(x);
    }

    final List<UnknownCoefficient> nonRankCoefficients = new ArrayList<>();

    from.union(to)
        .map(Map.Entry::getValue)
        .forEach(
            pair -> {
              final var x = UnknownCoefficient.unknown("x");
              constraints.add(
                  new EqualsSumConstraint(
                      x, List.of(pair.getLeft(), pair.getRight().negate()), "(opt)"));
              nonRankCoefficients.add(x);
            });

    return Optional.of(
        new Optimization.MultiTarget(rankCoefficients, nonRankCoefficients, constraints));
  }
}
