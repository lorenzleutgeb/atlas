package xyz.leutgeb.lorenz.lac.typing.resources.optimiziation;

import static xyz.leutgeb.lorenz.lac.typing.resources.coefficients.KnownCoefficient.ONE;
import static xyz.leutgeb.lorenz.lac.typing.resources.coefficients.KnownCoefficient.ZERO;
import static xyz.leutgeb.lorenz.lac.util.Util.append;

import java.util.ArrayList;
import java.util.Collection;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Optional;
import java.util.Set;
import java.util.function.Function;
import java.util.stream.Collectors;
import lombok.AllArgsConstructor;
import lombok.Value;
import xyz.leutgeb.lorenz.lac.ast.Program;
import xyz.leutgeb.lorenz.lac.typing.resources.Annotation;
import xyz.leutgeb.lorenz.lac.typing.resources.FunctionAnnotation;
import xyz.leutgeb.lorenz.lac.typing.resources.coefficients.Coefficient;
import xyz.leutgeb.lorenz.lac.typing.resources.coefficients.KnownCoefficient;
import xyz.leutgeb.lorenz.lac.typing.resources.coefficients.UnknownCoefficient;
import xyz.leutgeb.lorenz.lac.typing.resources.constraints.Constraint;
import xyz.leutgeb.lorenz.lac.typing.resources.constraints.EqualityConstraint;
import xyz.leutgeb.lorenz.lac.typing.resources.constraints.EqualsProductConstraint;
import xyz.leutgeb.lorenz.lac.typing.resources.constraints.EqualsSumConstraint;
import xyz.leutgeb.lorenz.lac.typing.resources.constraints.ExclusiveDisjunctiveConstraint;
import xyz.leutgeb.lorenz.lac.typing.resources.constraints.GreaterThanOrEqualConstraint;
import xyz.leutgeb.lorenz.lac.typing.resources.constraints.IfThenElseConstraint;

public class Optimization {
  @Value
  @AllArgsConstructor
  public static class MultiTarget {
    public List<UnknownCoefficient> targets;
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

    return Optional.of(new MultiTarget(append(rankCoefficients, nonRankCoefficients), constraints));
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
        new Optimization.MultiTarget(append(rankCoefficients, nonRankCoefficients), constraints));
  }

  public static Optional<MultiTarget> ng(FunctionAnnotation annotation) {
    final var from = annotation.from;
    final var to = annotation.to;

    if (to.size() != from.size()) {
      return Optional.empty();
    }

    int size = to.size();

    final Set<Constraint> constraints = new HashSet<>();

    final List<UnknownCoefficient> goals = new ArrayList<>();

    final var rankSlotSum = new ArrayList<Coefficient>();
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
      rankSlotSum.add(x);
    }

    final var rankSlots = UnknownCoefficient.unknown("slots");
    constraints.add(new EqualsSumConstraint(rankSlots, rankSlotSum, "(opt)"));

    final var slotSum = new ArrayList<Coefficient>();
    from.union(to)
        .forEach(
            e -> {
              var pair = e.getValue();

              int weight = 1;
              for (int i = 0; i < e.getKey().size() - 1; i++) {
                weight += Math.pow(e.getKey().get(i) + 1, 2);
              }
              weight += e.getKey().get(e.getKey().size() - 1);

              final var x = UnknownCoefficient.unknown("x");
              constraints.add(
                  new IfThenElseConstraint(
                      new ExclusiveDisjunctiveConstraint(
                          new EqualityConstraint(pair.getLeft(), ZERO, "(opt)"),
                          new EqualityConstraint(pair.getRight(), ZERO, "(opt)"),
                          "(opt)"),
                      Coefficient.of(weight),
                      ZERO,
                      x,
                      "(opt)"));
              slotSum.add(x);
            });

    final var slots = UnknownCoefficient.unknown("slots");
    constraints.add(new EqualsSumConstraint(slots, slotSum, "(opt)"));
    goals.add(slots);

    // Then we optimize for difference over those slots.
    for (int i = 0; i < size; i++) {
      final var x = UnknownCoefficient.unknown("x");
      constraints.add(
          new EqualsSumConstraint(
              x, List.of(from.getRankCoefficient(i), to.getRankCoefficient(i).negate()), "(opt)"));
      goals.add(x);
    }

    final var diffSum = new ArrayList<Coefficient>();
    from.union(to)
        .forEach(
            e -> {
              var pair = e.getValue();

              if (pair.getLeft() instanceof KnownCoefficient
                  && pair.getRight() instanceof KnownCoefficient) {
                return;
              }

              int weight = 1;
              for (int i = 0; i < e.getKey().size() - 1; i++) {
                weight += Math.pow(e.getKey().get(i) + 1, 2);
              }
              weight += e.getKey().get(e.getKey().size() - 1);

              final var diff = UnknownCoefficient.unknown("x");
              constraints.add(
                  new EqualsSumConstraint(
                      diff, List.of(pair.getLeft(), pair.getRight().negate()), "(opt)"));
              goals.add(diff);

              final var x = UnknownCoefficient.unknown("x");
              constraints.add(
                  new EqualsProductConstraint(x, List.of(diff, Coefficient.of(weight)), "(opt)"));
              diffSum.add(x);
            });

    UnknownCoefficient diffsumVar = UnknownCoefficient.unknown("x");
    constraints.add(new EqualsSumConstraint(diffsumVar, diffSum, "(opt)"));
    goals.add(diffsumVar);

    for (int i = 0; i < size; i++) {
      if (from.getRankCoefficient(i) instanceof UnknownCoefficient) {
        goals.add((UnknownCoefficient) from.getRankCoefficient(i));
      }
    }

    final var sum = new ArrayList<Coefficient>();
    from.streamNonRankCoefficients()
        .forEach(
            e -> {
              if (e.getValue() instanceof KnownCoefficient) {
                return;
              }

              int weight = 1;
              for (int i = 0; i < e.getKey().size() - 1; i++) {
                weight += Math.pow(e.getKey().get(i) + 1, 2);
              }
              weight += e.getKey().get(e.getKey().size() - 1);
              final var x = UnknownCoefficient.unknown("x");
              constraints.add(
                  new EqualsProductConstraint(
                      x, List.of(e.getValue(), Coefficient.of(weight)), "(opt)"));
              sum.add(x);
            });

    UnknownCoefficient sumVar = UnknownCoefficient.unknown("x");
    constraints.add(new EqualsSumConstraint(sumVar, sum, "(opt)"));
    goals.add(sumVar);

    return Optional.of(new Optimization.MultiTarget(goals, constraints));
  }

  private static boolean sameSize(FunctionAnnotation functionAnnotation) {
    return functionAnnotation.from.size() == functionAnnotation.to.size();
  }

  public static Optional<MultiTarget> weightedComponentWiseDifference(
      FunctionAnnotation annotation, Function<List<Integer>, Integer> weight) {
    if (!sameSize(annotation)) {
      return Optional.empty();
    }

    final Set<Constraint> constraints = new HashSet<>();
    final var diffSum = new ArrayList<Coefficient>();
    annotation
        .from
        .union(annotation.to)
        .forEach(
            e -> {
              var pair = e.getValue();

              if (pair.getLeft() instanceof KnownCoefficient
                  && pair.getRight() instanceof KnownCoefficient) {
                return;
              }

              final var diff = UnknownCoefficient.unknown("x");
              constraints.add(
                  new EqualsSumConstraint(
                      diff, List.of(pair.getLeft(), pair.getRight().negate()), "(opt)"));

              final var weightedDiff = UnknownCoefficient.unknown("x");
              constraints.add(
                  new EqualsProductConstraint(
                      weightedDiff,
                      List.of(diff, Coefficient.of(weight.apply(e.getKey()))),
                      "(opt) weightedComponentWiseDifference"));
              diffSum.add(weightedDiff);
            });

    UnknownCoefficient diffsumVar = UnknownCoefficient.unknown("x");
    constraints.add(
        new EqualsSumConstraint(diffsumVar, diffSum, "(opt) weightedComponentWiseDifference"));
    return Optional.of(new Optimization.MultiTarget(List.of(diffsumVar), constraints));
  }

  public static Optional<MultiTarget> squareWeightedComponentWiseDifference(
      FunctionAnnotation annotation) {
    return weightedComponentWiseDifference(annotation, Annotation::indexWeight);
  }

  public static Optional<MultiTarget> componentWiseDifference(FunctionAnnotation annotation) {
    if (!sameSize(annotation)) {
      return Optional.empty();
    }

    final Set<Constraint> constraints = new HashSet<>();
    final var diffSum = new ArrayList<Coefficient>();
    annotation
        .from
        .union(annotation.to)
        .forEach(
            e -> {
              var pair = e.getValue();

              if (pair.getLeft() instanceof KnownCoefficient
                  && pair.getRight() instanceof KnownCoefficient) {
                return;
              }

              final var diff = UnknownCoefficient.unknown("x");
              constraints.add(
                  new EqualsSumConstraint(
                      diff, List.of(pair.getLeft(), pair.getRight().negate()), "(opt)"));
              diffSum.add(diff);
            });

    UnknownCoefficient diffsumVar = UnknownCoefficient.unknown("x");
    constraints.add(new EqualsSumConstraint(diffsumVar, diffSum, "(opt)"));
    return Optional.of(new Optimization.MultiTarget(List.of(diffsumVar), constraints));
  }

  public static Optional<MultiTarget> componentWiseDifferenceAndSum(FunctionAnnotation annotation) {
    if (!sameSize(annotation)) {
      return Optional.empty();
    }

    final Set<Constraint> constraints = new HashSet<>();
    final var diffSum = new ArrayList<Coefficient>();
    final var sum = new ArrayList<Coefficient>();
    annotation
        .from
        .union(annotation.to)
        .forEach(
            e -> {
              var pair = e.getValue();

              if (pair.getLeft() instanceof KnownCoefficient
                  && pair.getRight() instanceof KnownCoefficient) {
                return;
              }

              final var diff = UnknownCoefficient.unknown("x");
              constraints.add(
                  new EqualsSumConstraint(
                      diff, List.of(pair.getLeft(), pair.getRight().negate()), "(opt)"));
              diffSum.add(diff);
              sum.add(pair.getLeft());
            });

    UnknownCoefficient diffsumVar = UnknownCoefficient.unknown("x");
    constraints.add(new EqualsSumConstraint(diffsumVar, diffSum, "(opt)"));

    UnknownCoefficient sumVar = UnknownCoefficient.unknown("x");
    constraints.add(new EqualsSumConstraint(sumVar, sum, "(opt)"));
    return Optional.of(new Optimization.MultiTarget(List.of(diffsumVar, sumVar), constraints));
  }

  public static Optional<MultiTarget> foo(FunctionAnnotation annotation) {
    final var from = annotation.from;
    final var to = annotation.to;

    if (to.size() != from.size()) {
      return Optional.empty();
    }

    int size = to.size();

    final Set<Constraint> constraints = new HashSet<>();

    final List<UnknownCoefficient> goals = new ArrayList<>();

    /*
    final var rankSlotSum = new ArrayList<Coefficient>();
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
      rankSlotSum.add(x);
    }
    final var rankSlots = UnknownCoefficient.unknown("slots");
    constraints.add(new EqualsSumConstraint(rankSlots, rankSlotSum, "(opt)"));
     */

    /*
    final var slotSum = new ArrayList<Coefficient>();
    from.union(to)
        .forEach(
            e -> {
              var pair = e.getValue();

              int weight = 1;
              for (int i = 0; i < e.getKey().size() - 1; i++) {
                weight += Math.pow(e.getKey().get(i) + 1, 2);
              }
              weight += e.getKey().get(e.getKey().size() - 1);

              final var diff = UnknownCoefficient.maybeNegativeUnknown("x");
              constraints.add(
                  new EqualsSumConstraint(
                      diff, List.of(pair.getLeft(), pair.getRight().negate()), "(opt)"));

              final var absDiff = UnknownCoefficient.unknown("x");
              constraints.add(abs(absDiff, diff));

              final var x = UnknownCoefficient.unknown("x");
              constraints.add(
                  new EqualsProductConstraint(
                      x, List.of(absDiff, Coefficient.of(weight)), "(opt)"));

              slotSum.add(x);
            });

    final var slots = UnknownCoefficient.unknown("slots");
    constraints.add(new EqualsSumConstraint(slots, slotSum, "(opt)"));
    goals.add(slots);
           */

    // Then we optimize for difference over those slots.
    /*
    for (int i = 0; i < size; i++) {
      final var x = UnknownCoefficient.unknown("x");
      constraints.add(
          new EqualsSumConstraint(
              x, List.of(from.getRankCoefficient(i), to.getRankCoefficient(i).negate()), "(opt)"));
      goals.add(x);
    }
    */

    final var diffSum = new ArrayList<Coefficient>();
    from.union(to)
        .forEach(
            e -> {
              var pair = e.getValue();

              if (pair.getLeft() instanceof KnownCoefficient
                  && pair.getRight() instanceof KnownCoefficient) {
                return;
              }

              int weight = 1;
              for (int i = 0; i < e.getKey().size() - 1; i++) {
                weight += Math.pow(e.getKey().get(i) + 1, 2);
                // weight += e.getKey().get(i);
              }
              weight += e.getKey().get(e.getKey().size() - 1);

              final var diff = UnknownCoefficient.unknown("x");
              constraints.add(
                  new EqualsSumConstraint(
                      diff, List.of(pair.getLeft(), pair.getRight().negate()), "(opt)"));
              // sum.add(diff);

              final var x = UnknownCoefficient.unknown("x");
              constraints.add(
                  new EqualsProductConstraint(x, List.of(diff, Coefficient.of(weight)), "(opt)"));
              diffSum.add(x);
            });

    UnknownCoefficient diffsumVar = UnknownCoefficient.unknown("x");
    constraints.add(new EqualsSumConstraint(diffsumVar, diffSum, "(opt)"));
    goals.add(diffsumVar);

    /*
    for (int i = 0; i < size; i++) {
      if (from.getRankCoefficient(i) instanceof UnknownCoefficient) {
        goals.add((UnknownCoefficient) from.getRankCoefficient(i));
      }
    }
     */

    /*
    final var absSum = new ArrayList<Coefficient>();
    from.streamNonRankCoefficients()
        .forEach(
            e -> {
              if (e.getValue() instanceof KnownCoefficient) {
                return;
              }

              int weight = 1;
              for (int i = 0; i < e.getKey().size() - 1; i++) {
                weight += Math.pow(e.getKey().get(i) + 1, 2);
              }
              weight += e.getKey().get(e.getKey().size() - 1);
              final var x = UnknownCoefficient.unknown("x");
              constraints.add(
                  new EqualsProductConstraint(
                      x, List.of(e.getValue(), Coefficient.of(weight)), "(opt)"));
              absSum.add(x);
            });

    UnknownCoefficient sumVar = UnknownCoefficient.unknown("x");
    constraints.add(new EqualsSumConstraint(sumVar, absSum, "(opt)"));
    goals.add(sumVar);
    */

    return Optional.of(new Optimization.MultiTarget(goals, constraints));
  }

  private static Constraint active(Coefficient x, Coefficient left, Coefficient right) {
    return new IfThenElseConstraint(
        new ExclusiveDisjunctiveConstraint(
            new EqualityConstraint(left, ZERO, "(opt)"),
            new EqualityConstraint(right, ZERO, "(opt)"),
            "(opt)"),
        ONE,
        ZERO,
        x,
        "(opt)");
  }

  private static Constraint abs(Coefficient y, Coefficient x) {
    return new IfThenElseConstraint(
        new GreaterThanOrEqualConstraint(x, ZERO, "(abs)"), x, x.negate(), y, "(abs)");
  }

  public static MultiTarget combine(
      Program program,
      Collection<String> fqns,
      Function<FunctionAnnotation, Optional<MultiTarget>> optimization,
      boolean forceRank) {

    return combine(
        fqns.stream()
            .map(
                fqn ->
                    program
                        .getFunctionDefinitions()
                        .get(fqn)
                        .getInferredSignature()
                        .getAnnotation()
                        .get()
                        .withCost)
            .collect(Collectors.toList()),
        optimization,
        forceRank);
  }

  public static MultiTarget combine(
      Collection<FunctionAnnotation> annotations,
      Function<FunctionAnnotation, Optional<MultiTarget>> optimization,
      boolean forceRank) {
    final List<List<Coefficient>> targetSums = new ArrayList<>();
    final Set<Constraint> constraints = new HashSet<>();

    for (final var annotation : annotations) {
      final var target = optimization.apply(annotation);
      if (target.isPresent()) {
        for (int i = 0; i < target.get().targets.size(); i++) {
          if (targetSums.size() >= i) {
            targetSums.add(new ArrayList<>());
          }

          final var sum = targetSums.get(i);
          sum.add(target.get().targets.get(i));
        }
        constraints.addAll(target.get().constraints);
      }

      if (forceRank
          && annotation.to.size() == 1
          && annotation.to.getRankCoefficientOrZero() instanceof UnknownCoefficient
          && annotation.from.size() == 1) {
        constraints.add(
            new EqualityConstraint(
                annotation.from.getRankCoefficient(),
                annotation.to.getRankCoefficient(),
                "(hack) force rank"));
      }
    }

    var targets = new ArrayList<UnknownCoefficient>(targetSums.size());

    for (List<Coefficient> sum : targetSums) {
      final var x = UnknownCoefficient.unknown("x");
      targets.add(x);
      constraints.add(new EqualsSumConstraint(x, sum, "(opt) combine"));
    }

    return new MultiTarget(targets, constraints);
  }
}
