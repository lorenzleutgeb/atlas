package xyz.leutgeb.lorenz.lac.typing.resources.rules;

import java.util.ArrayList;
import java.util.Collections;
import java.util.List;
import java.util.stream.Collectors;
import org.apache.commons.lang3.tuple.Pair;
import xyz.leutgeb.lorenz.lac.ast.Identifier;
import xyz.leutgeb.lorenz.lac.ast.MatchExpression;
import xyz.leutgeb.lorenz.lac.typing.resources.AnnotatingContext;
import xyz.leutgeb.lorenz.lac.typing.resources.AnnotatingGlobals;
import xyz.leutgeb.lorenz.lac.typing.resources.coefficients.Coefficient;
import xyz.leutgeb.lorenz.lac.typing.resources.constraints.Constraint;
import xyz.leutgeb.lorenz.lac.typing.resources.constraints.EqualityConstraint;
import xyz.leutgeb.lorenz.lac.typing.resources.constraints.EqualsSumConstraint;
import xyz.leutgeb.lorenz.lac.typing.resources.heuristics.AnnotationHeuristic;
import xyz.leutgeb.lorenz.lac.typing.resources.proving.Obligation;

public class Match {
  public static AnnotatingContext pop(
      AnnotatingContext that, AnnotationHeuristic heuristic, String toPop) {
    final var newIds = new ArrayList<String>(that.size() - 1);
    for (final var id : that.getIds()) {
      if (toPop.equals(id)) {
        continue;
      }
      newIds.add(id);
    }
    return heuristic.generateContext("pop", newIds);
  }

  public static AnnotatingContext extend(
      AnnotatingContext that, AnnotationHeuristic heuristic, List<String> ids) {
    final var newIds = new ArrayList<String>(ids.size() + that.size());
    newIds.addAll(that.getIds());
    newIds.addAll(ids);
    return heuristic.generateContext("extend", newIds);
  }

  public static Rule.ApplicationResult apply(Obligation obligation, AnnotatingGlobals globals) {
    final var expression = (MatchExpression) obligation.getExpression();

    final var scrutinee = (Identifier) expression.getScrut();
    final var pattern = expression.getNodePattern();
    final var x1 = (Identifier) pattern.getLeft();
    final var x3 = (Identifier) pattern.getRight();
    final var leaf = expression.getLeaf();
    final var node = expression.getNode();
    final var qp = obligation.getAnnotation();

    final var gammaxq = obligation.getContext();
    final var gammap = pop(gammaxq, globals.getHeuristic(), scrutinee.getName());
    final var gammaxsr =
        extend(gammap, globals.getHeuristic(), List.of(x1.getName(), x3.getName()));

    final Pair<Obligation, List<Constraint>> p =
        Pair.of(obligation.keepCost(gammap, leaf, qp), new ArrayList<>());

    final Pair<Obligation, List<Constraint>> r =
        Pair.of(obligation.keepCost(gammaxsr, node, qp), new ArrayList<>());

    // m refers to the size of \Gamma
    r.getRight()
        .add(
            new EqualityConstraint(
                gammaxsr.getRankCoefficient(x1.getName()),
                gammaxsr.getRankCoefficient(x3.getName()),
                "(match) r_{m+1} = r_{m+2}"));

    r.getRight()
        .add(
            new EqualityConstraint(
                gammaxsr.getRankCoefficient(x3.getName()),
                gammaxq.getRankCoefficient(scrutinee.getName()),
                "(match) r_{m+2} = q_{m+1}"));

    // To implement this constraint we use two indices defined as function that set the
    // index for all variables except x_1/x_3 to zero. That way we get the above selection.
    // Again m refers to the size of \Gamma, thus q_{m+1} refers to the coefficient of the
    // scrutinee.
    r.getRight()
        .add(
            new EqualityConstraint(
                gammaxsr.getCoefficient(id -> id.equals(x1.getName()) ? 1 : 0, 0),
                gammaxsr.getCoefficient(id -> id.equals(x3.getName()) ? 1 : 0, 0),
                "r_{(\\vec{0}, 1, 0, 0)} = r_{(\\vec{0}, 0, 1, 0)}"));

    r.getRight()
        .add(
            new EqualityConstraint(
                gammaxsr.getCoefficient(id -> id.equals(x3.getName()) ? 1 : 0, 0),
                gammaxq.getRankCoefficient(scrutinee.getName()),
                "r_{(\\vec{0}, 0, 1, 0)} = q_{m+1}"));

    gammaxsr.stream()
        .filter(
            rEntry -> {
              final var a1 = rEntry.getAssociatedIndices().get(x1.getName());
              final var a2 = rEntry.getAssociatedIndices().get(x3.getName());
              return a1 != null && a1.equals(a2);
            })
        .forEach(
            rEntry -> {
              // Here we use the coefficient of x1, but could just as well use x3. The
              // previous filter ensures that they are the same.
              rEntry
                  .getAssociatedIndices()
                  .put(scrutinee.getName(), rEntry.getAssociatedIndices().get(x1.getName()));
              r.getRight()
                  .add(
                      new EqualityConstraint(
                          rEntry.getValue(),
                          gammaxq.getCoefficientOrZero(
                              rEntry.getAssociatedIndices(), rEntry.getOffsetIndex()),
                          "(match) r_{\\vec{a},a,a,b} = q_{\\vec{a}, a, b} for expression `"
                              + expression
                              + "`"));
            });

    // p_{\vec{a}, c} = \Sigma_{a+b=c} q_{\vec{a}, a, b}
    gammap.stream()
        .forEach(
            pEntry -> {
              final List<Coefficient> sum =
                  gammaxq.stream()
                      .filter(
                          qEntry ->
                              pEntry
                                  .getOffsetIndex()
                                  .equals(
                                      qEntry.getOffsetIndex()
                                          + qEntry.getAssociatedIndices().get(scrutinee.getName())))
                      .map(AnnotatingContext.Entry::getValue)
                      .collect(Collectors.toList());

              if (!sum.isEmpty()) {
                p.getRight().add(new EqualsSumConstraint(pEntry.getValue(), sum));
              }
            });

    for (var id : gammaxq.getIds()) {
      if (id.equals(scrutinee.getName())) {
        continue;
      }
      p.getRight()
          .add(
              new EqualityConstraint(
                  gammaxq.getRankCoefficient(id),
                  gammap.getRankCoefficient(id),
                  "(match) q_i = p_i"));
      p.getRight()
          .add(
              new EqualityConstraint(
                  gammaxq.getRankCoefficient(id),
                  gammaxsr.getRankCoefficient(id),
                  "(match) q_i = r_i"));
    }

    return new Rule.ApplicationResult(
        List.of(p.getLeft(), r.getLeft()),
        List.of(p.getRight(), r.getRight()),
        Collections.emptyList());
  }
}
