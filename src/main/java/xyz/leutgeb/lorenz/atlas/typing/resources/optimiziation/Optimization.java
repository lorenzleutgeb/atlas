package xyz.leutgeb.lorenz.atlas.typing.resources.optimiziation;

import static xyz.leutgeb.lorenz.atlas.typing.resources.Annotation.indexWeight;
import static xyz.leutgeb.lorenz.atlas.typing.resources.Annotation.isUnitIndex;

import java.util.ArrayList;
import java.util.Collection;
import java.util.Collections;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Objects;
import java.util.Optional;
import java.util.Set;
import java.util.Spliterators;
import java.util.function.Function;
import java.util.stream.Collectors;
import java.util.stream.Stream;
import java.util.stream.StreamSupport;
import lombok.AllArgsConstructor;
import lombok.Value;
import xyz.leutgeb.lorenz.atlas.ast.FunctionDefinition;
import xyz.leutgeb.lorenz.atlas.ast.Program;
import xyz.leutgeb.lorenz.atlas.typing.resources.FunctionAnnotation;
import xyz.leutgeb.lorenz.atlas.typing.resources.coefficients.Coefficient;
import xyz.leutgeb.lorenz.atlas.typing.resources.coefficients.KnownCoefficient;
import xyz.leutgeb.lorenz.atlas.typing.resources.coefficients.UnknownCoefficient;
import xyz.leutgeb.lorenz.atlas.typing.resources.constraints.Constraint;
import xyz.leutgeb.lorenz.atlas.typing.resources.constraints.EqualsProductConstraint;
import xyz.leutgeb.lorenz.atlas.typing.resources.constraints.EqualsSumConstraint;

public class Optimization {
  @Value
  @AllArgsConstructor
  public static class MultiTarget {
    public List<UnknownCoefficient> targets;
    public Set<Constraint> constraints;
  }

  @Value
  @AllArgsConstructor
  public static class UniTarget {
    public UnknownCoefficient target;
    public Set<Constraint> constraints;
  }

  private static boolean sameSize(FunctionAnnotation functionAnnotation) {
    return functionAnnotation.from.size() == functionAnnotation.to.size();
  }

  public static Optional<UniTarget> weightedComponentWiseDifference(
      FunctionAnnotation annotation, Function<List<Integer>, Integer> weight) {
    if (annotation.to.size() == 0 || annotation.from.size() == 0) {
      return Optional.empty();
    }

    if (!sameSize(annotation)) {
      return notSameSizeWeightedComponentWiseDifference(annotation, weight);
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

              /*
              if (isConstantIndex(e.getKey()) && !isUnitIndex(e.getKey())) {
                constraints.add(new EqualityConstraint(ZERO, e.getValue().getLeft(), "(hack)"));
                return;
              }
               */

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
    return Optional.of(new UniTarget(diffsumVar, constraints));
  }

  private static Optional<UniTarget> notSameSizeWeightedComponentWiseDifference(
      FunctionAnnotation annotation, Function<List<Integer>, Integer> weight) {
    // First, care only about those that have shape (a, a, ..., a, c) and compare them to (a, c).
    // If that does not work, think about (a1, a2, ...., an, c) and what to compare them to.
    // Skip (0, 0, ..., 0, 2).
    // Skip rank coefficients.

    final Set<Constraint> constraints = new HashSet<>();
    final List<Coefficient> sum = new ArrayList<>();

    annotation
        .from
        .streamNonRankCoefficients()
        .forEach(
            e -> {
              final var a = e.getKey().get(0);
              for (int i = 1; i < e.getKey().size() - 1; i++) {
                if (!Objects.equals(e.getKey().get(i), a)) {
                  // constraints.add(new EqualityConstraint(e.getValue(), ZERO, "(opt) logs"));
                  return;
                }
              }
              final var c = e.getKey().get(e.getKey().size() - 1);

              final var diff = UnknownCoefficient.unknown("x");

              constraints.add(
                  new EqualsSumConstraint(
                      diff,
                      List.of(e.getValue(), annotation.to.getCoefficientOrZero(List.of(a, c))),
                      "(opt) logs"));

              final var weightedDiff = UnknownCoefficient.unknown("x");

              constraints.add(
                  new EqualsProductConstraint(
                      weightedDiff,
                      List.of(diff, Coefficient.of(weight.apply(List.of(a, c)))),
                      "(opt) logs"));

              sum.add(weightedDiff);
            });

    final var target = UnknownCoefficient.unknown("x");
    constraints.add(new EqualsSumConstraint(target, sum, "(opt) logs"));

    return Optional.of(new UniTarget(target, constraints));
  }

  public static Optional<UniTarget> rankDifference(FunctionAnnotation annotation) {
    if (annotation.to.size() == 0 || annotation.from.size() == 0) {
      return Optional.empty();
    }

    // At this point we know that a tree is returned and at least one tree is in the input.
    final Set<Constraint> constraints = new HashSet<>();
    final var sumElements = new ArrayList<Coefficient>();
    for (int i = 0; i < annotation.from.size(); i++) {
      final var diff = UnknownCoefficient.unknown("x");
      sumElements.add(diff);
      constraints.add(
          new EqualsSumConstraint(
              diff,
              List.of(
                  annotation.from.getRankCoefficientOrZero(i),
                  annotation.to.getRankCoefficientOrZero().negate()),
              "(opt) rank"));
    }
    final UnknownCoefficient diffsumVar = UnknownCoefficient.unknown("x");
    constraints.add(new EqualsSumConstraint(diffsumVar, sumElements, "(opt) rank"));
    return Optional.of(new UniTarget(diffsumVar, constraints));
  }

  public static Optional<UniTarget> constantDifference(FunctionAnnotation annotation) {
    if (annotation.to.size() == 0 || annotation.from.size() == 0) {
      return Optional.empty();
    }

    final var target = UnknownCoefficient.unknown("x");
    final var constraints =
        Collections.<Constraint>singleton(
            new EqualsSumConstraint(
                target,
                List.of(
                    annotation.from.getUnitCoefficientOrZero(),
                    annotation.from.getUnitCoefficientOrZero().negate()),
                "(opt) const"));

    return Optional.of(new UniTarget(target, constraints));
  }

  /*
  public static Optional<UniTarget> squareWeightedComponentWiseDifference(
      FunctionAnnotation annotation) {
    return weightedComponentWiseDifference(annotation, Annotation::indexWeight);
  }
   */

  public static Optional<UniTarget> customWeightedComponentWiseDifference(
      FunctionAnnotation annotation) {
    return weightedComponentWiseDifference(
        annotation,
        index -> {
          if (isUnitIndex(index)) {
            return 0;
          }
          if (List.of(1, 0).equals(index)) {
            return 1;
          }
          return indexWeight(index);
        });
  }

  /** Minimizes the left side. */
  public static Optional<UniTarget> abs(FunctionAnnotation annotation) {
    final Set<Constraint> constraints = new HashSet<>();
    final var sum = new ArrayList<Coefficient>();
    for (int i = 0; i < annotation.from.size(); i++) {
      sum.add(annotation.from.getRankCoefficientOrZero(i));
    }
    annotation.from.streamNonRankCoefficients().map(Map.Entry::getValue).forEach(sum::add);
    UnknownCoefficient sumVar = UnknownCoefficient.unknown("x");
    constraints.add(new EqualsSumConstraint(sumVar, sum, "(opt)"));
    return Optional.of(new UniTarget(sumVar, constraints));
  }

  public static UniTarget layeredCombo(
      Program program,
      Iterable<String> fqns,
      List<Integer> weights,
      Function<FunctionAnnotation, Optional<UniTarget>>... optimizations) {

    return layeredCombo(
        StreamSupport.stream(Spliterators.spliteratorUnknownSize(fqns.iterator(), 0), false)
            .map(program.getFunctionDefinitions()::get)
            .collect(Collectors.toSet()),
        weights,
        optimizations);
  }

  public static UniTarget layeredCombo(
      Set<FunctionDefinition> functionDefinitions,
      List<Integer> weights,
      Function<FunctionAnnotation, Optional<UniTarget>>... optimizations) {
    Set<Constraint> constraints = new HashSet<>();
    final var sumOverall = new ArrayList<Coefficient>();
    for (var i = 0; i < optimizations.length; i++) {
      final var opt = optimizations[i];

      final var sumAtLayer = new ArrayList<Coefficient>();
      for (var fd : functionDefinitions) {
        final var ann = fd.getInferredSignature().getAnnotation().get().withCost;
        // var it = combine(program, fqns, opt);
        var it = opt.apply(ann);
        if (it.isPresent()) {
          sumAtLayer.add(it.get().target);
          constraints.addAll(it.get().constraints);
        }
      }
      final var levelSum = UnknownCoefficient.unknown("levelsum");

      constraints.add(new EqualsSumConstraint(levelSum, sumAtLayer, "ssss"));
      final var multied = UnknownCoefficient.unknown("multied");
      constraints.add(
          new EqualsProductConstraint(
              multied, List.of(Coefficient.of(weights.get(i)), levelSum), "ooo"));
      sumOverall.add(multied);
    }
    final var overall = UnknownCoefficient.unknown("overall");
    constraints.add(new EqualsSumConstraint(overall, sumOverall, "(foo)"));

    return new UniTarget(overall, constraints);
  }

  /** Generates one target per criteria and function. First by criteria and then by function. */
  @SafeVarargs
  public static UniTarget layeredCombo(
      Program program,
      Iterable<String> fqns,
      Function<FunctionAnnotation, Optional<UniTarget>>... optimizations) {
    return layeredCombo(program, fqns, List.of(16127, 997, 97, 2), optimizations);
  }

  @SafeVarargs
  public static UniTarget layeredCombo(
      Set<FunctionDefinition> functionDefinitions,
      Function<FunctionAnnotation, Optional<UniTarget>>... optimizations) {
    return layeredCombo(functionDefinitions, List.of(16127, 997, 97, 2), optimizations);
  }

  @SafeVarargs
  public static MultiTarget layered(
      Program program,
      Collection<String> fqns,
      Function<FunctionAnnotation, Optional<UniTarget>>... optimizations) {
    List<UnknownCoefficient> targets = new ArrayList<>();
    Set<Constraint> constraints = new HashSet<>();
    for (var opt : optimizations) {
      var it = combine(program, fqns, opt);
      targets.add(it.target);
      constraints.addAll(it.constraints);
    }
    return new MultiTarget(targets, constraints);
  }

  public MultiTarget combine(UniTarget... targets) {
    return new MultiTarget(
        Stream.of(targets).map(UniTarget::getTarget).collect(Collectors.toList()),
        Stream.of(targets)
            .map(UniTarget::getConstraints)
            .flatMap(Collection::stream)
            .collect(Collectors.toSet()));
  }

  public static UniTarget combineRoots(
      Program program, Function<FunctionAnnotation, Optional<UniTarget>> optimization) {
    return combine(program, program.getRoots(), optimization);
  }

  public static UniTarget combine(
      Program program,
      Collection<String> fqns,
      Function<FunctionAnnotation, Optional<UniTarget>> optimization) {

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
        optimization);
  }

  public static UniTarget combine(
      Collection<FunctionAnnotation> annotations,
      Function<FunctionAnnotation, Optional<UniTarget>> optimization) {
    final List<Coefficient> targetSum = new ArrayList<>();
    final Set<Constraint> constraints = new HashSet<>();

    for (final var annotation : annotations) {
      final var target = optimization.apply(annotation);
      if (target.isPresent()) {
        targetSum.add(target.get().target);
        constraints.addAll(target.get().constraints);
      }
    }

    final var x = UnknownCoefficient.unknown("x");
    constraints.add(new EqualsSumConstraint(x, targetSum, "(opt) combine"));

    return new UniTarget(x, constraints);
  }

  public static UniTarget standard(Set<FunctionDefinition> functionDefinitions) {
    return Optimization.layeredCombo(
        functionDefinitions,
        Optimization::rankDifference,
        Optimization::customWeightedComponentWiseDifference,
        Optimization::constantDifference,
        Optimization::abs);
  }
}
