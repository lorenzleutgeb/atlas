package xyz.leutgeb.lorenz.lac.typing.resources.rules;

import static java.util.function.Predicate.not;
import static xyz.leutgeb.lorenz.lac.typing.resources.coefficients.KnownCoefficient.ZERO;

import java.util.ArrayList;
import java.util.HashSet;
import java.util.List;
import java.util.Set;
import java.util.stream.Collectors;
import java.util.stream.Stream;
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
import xyz.leutgeb.lorenz.lac.util.Pair;

public class Match implements Rule {
  public static final Match INSTANCE = new Match();

  public static AnnotatingContext pop(
      List<Identifier> that, AnnotationHeuristic heuristic, Identifier toPop) {
    final var newIds = new ArrayList<Identifier>(that.size() - 1);
    for (final var id : that) {
      if (toPop.equals(id)) {
        continue;
      }
      newIds.add(id);
    }
    return heuristic.generateContext("match " + toPop + " pop", newIds);
  }

  public static AnnotatingContext extend(
      List<Identifier> that,
      AnnotationHeuristic heuristic,
      Identifier scrutinee,
      List<Identifier> ids) {
    final var newIds = new ArrayList<Identifier>(ids.size() + that.size());
    newIds.addAll(that);
    newIds.addAll(ids);
    return heuristic.generateContext("match " + scrutinee + " extend " + ids, newIds);
    // return new AnnotatingContext(
    //   newIds, new Annotation(newIds.size(), "match " + scrutinee + " extend " + ids));
  }

  public Rule.ApplicationResult apply(Obligation obligation, AnnotatingGlobals globals) {
    final var expression = (MatchExpression) obligation.getExpression();

    final var x = (Identifier) expression.getScrut();
    final var pattern = expression.getNodePattern();
    final var x1 = (Identifier) pattern.getLeft();
    final var x3 = (Identifier) pattern.getRight();

    final var gammaxq = obligation.getContext();
    final var gammap = pop(gammaxq.getIds(), globals.getHeuristic(), x);
    final var ppqmp1 =
        globals
            .getHeuristic()
            .generate(gammap.getAnnotation().getName() + " + q_{m+1}", gammap.getAnnotation());
    final var gammappqmp1 = new AnnotatingContext(gammap.getIds(), ppqmp1);
    final var gammaxsr = extend(gammap.getIds(), globals.getHeuristic(), x, List.of(x1, x3));

    final Set<Coefficient> rOccurred = new HashSet<>();
    final Set<Coefficient> pOccurred = new HashSet<>();

    final Pair<Obligation, List<Constraint>> p =
        Pair.of(
            obligation.keepAnnotationAndCost(gammappqmp1, expression.getLeaf()), new ArrayList<>());
    p.getRight()
        .addAll(
            ppqmp1.increment(
                gammap.getAnnotation(), gammaxq.getRankCoefficient(x), "(match) P + q_{m+1}"));

    final Pair<Obligation, List<Constraint>> r =
        Pair.of(
            obligation.keepCost(gammaxsr, expression.getNode(), obligation.getAnnotation()),
            new ArrayList<>());

    // m refers to the size of Γ
    r.getRight()
        .add(
            new EqualityConstraint(
                gammaxsr.getRankCoefficient(x1),
                gammaxsr.getRankCoefficient(x3),
                "(match) r_{m+1} = r_{m+2} (for variables `" + x1 + "` and `" + x3 + "`)"));

    r.getRight()
        .add(
            new EqualityConstraint(
                gammaxsr.getRankCoefficient(x3),
                gammaxq.getRankCoefficient(x),
                "(match) r_{m+2} = q_{m+1} (for variables `" + x3 + "` and `" + x + "`)"));

    rOccurred.add(gammaxsr.getRankCoefficient(x1));
    rOccurred.add(gammaxsr.getRankCoefficient(x3));

    // r.getRight().addAll(gammaxsr.setRankCoefficient(x1, gammaxq.getRankCoefficient(x)));
    // r.getRight().addAll(gammaxsr.setRankCoefficient(x3, gammaxq.getRankCoefficient(x)));

    // To implement this constraint we use two indices defined as function that set the
    // index for all variables except x_1/x_3 to zero. That way we get the above selection.
    // Again m refers to the size of \Gamma, thus q_{m+1} refers to the coefficient of the
    // scrutinee.
    /*
    r.getRight()
        .addAll(
            gammaxsr.setCoefficient(id -> id.equals(x1) ? 1 : 0, 0, gammaxq.getRankCoefficient(x)));
    r.getRight()
        .addAll(
            gammaxsr.setCoefficient(id -> id.equals(x3) ? 1 : 0, 0, gammaxq.getRankCoefficient(x)));
     */

    r.getRight()
        .add(
            new EqualityConstraint(
                gammaxsr.getCoefficient(id -> id.equals(x1) ? 1 : 0, 0),
                gammaxsr.getCoefficient(id -> id.equals(x3) ? 1 : 0, 0),
                "r_{(\\vec{0}, 1, 0, 0)} = r_{(\\vec{0}, 0, 1, 0)}"));

    r.getRight()
        .add(
            new EqualityConstraint(
                gammaxsr.getCoefficient(id -> id.equals(x3) ? 1 : 0, 0),
                gammaxq.getRankCoefficient(x),
                "r_{(\\vec{0}, 0, 1, 0)} = q_{m+1}"));

    rOccurred.add(gammaxsr.getCoefficient(id -> id.equals(x3) ? 1 : 0, 0));
    rOccurred.add(gammaxsr.getCoefficient(id -> id.equals(x1) ? 1 : 0, 0));

    /*
    gammaxq.stream()
        .forEach(
            qEntry -> {
              final var a = qEntry.getAssociatedIndex(x);
              r.getRight()
                  .addAll(
                      gammaxsr.setCoefficient(qEntry.mask(x1, a).mask(x3, a), qEntry.getValue()));
            });
         */

    gammaxsr.stream()
        .filter(
            rEntry -> {
              final var a1 = rEntry.getAssociatedIndices().get(x1);
              final var a2 = rEntry.getAssociatedIndices().get(x3);
              // If a1 is null we are in big trouble. We just constructed r
              // and there definitely must be an index for x1!
              return a1.equals(a2);
            })
        .forEach(
            rEntry -> {
              rOccurred.add(rEntry.getValue());
              // Here we use the coefficient of x1, but could just as well use x3. The
              // previous filter ensures that they are the same.
              // rEntry.getAssociatedIndices().put(x, rEntry.getAssociatedIndices().get(x1));
              r.getRight()
                  .add(
                      new EqualityConstraint(
                          rEntry.getValue(),
                          gammaxq.getCoefficientOrZero(
                              rEntry.mask(x, rEntry.getAssociatedIndex(x1))),
                          "(match) r_{\\vec{a},a,a,b} = q_{\\vec{a},a,b} for expression `"
                              + expression
                              + "`"));
            });

    gammap.stream()
        .forEach(
            pEntry -> {
              final List<Coefficient> sum =
                  gammaxq.stream()
                      .filter(
                          qEntry ->
                              pEntry.getAssociatedIndices().entrySet().stream()
                                  .allMatch(
                                      entry ->
                                          qEntry
                                              .getAssociatedIndex(entry.getKey())
                                              .equals(entry.getValue())))
                      .filter(
                          qEntry ->
                              pEntry
                                  .getOffsetIndex()
                                  .equals(
                                      qEntry.getOffsetIndex()
                                          + qEntry.getAssociatedIndices().get(x)))
                      .map(AnnotatingContext.Entry::getValue)
                      .collect(Collectors.toList());

              if (!sum.isEmpty()) {
                pOccurred.add(pEntry.getValue());
                p.getRight()
                    .add(
                        new EqualsSumConstraint(
                            pEntry.getValue(),
                            sum,
                            "(match) p_{\\vec{a}, c} = Σ_{a+b=c} q_{\\vec{a}, a, b}"));
              }
            });

    for (var id : gammaxq.getIds()) {
      if (id.equals(x)) {
        continue;
      }
      p.getRight()
          .add(
              new EqualityConstraint(
                  gammaxq.getRankCoefficient(id),
                  gammap.getRankCoefficient(id),
                  "(match) q_i = p_i"));
      r.getRight()
          .add(
              new EqualityConstraint(
                  gammaxq.getRankCoefficient(id),
                  gammaxsr.getRankCoefficient(id),
                  "(match) q_i = r_i"));
      // r.getRight().addAll(gammaxsr.setRankCoefficient(id, gammaxq.getRankCoefficient(id)));
      // p.getRight().addAll(gammap.setRankCoefficient(id, gammaxq.getRankCoefficient(id)));
      rOccurred.add(gammaxsr.getRankCoefficient(id));
      rOccurred.add(gammap.getRankCoefficient(id));
    }

    final List<Constraint> setToZero =
        Stream.concat(
                gammaxsr.stream()
                    .map(AnnotatingContext.Entry::getValue)
                    .filter(not(rOccurred::contains)),
                gammap.stream()
                    .map(AnnotatingContext.Entry::getValue)
                    .filter(not(pOccurred::contains)))
            .map(
                coefficient ->
                    new EqualityConstraint(coefficient, ZERO, "(match) setToZero " + coefficient))
            .collect(Collectors.toUnmodifiableList());

    return new Rule.ApplicationResult(
        List.of(p.getLeft(), r.getLeft()), List.of(p.getRight(), r.getRight()), setToZero);
  }

  @Override
  public String getName() {
    return "match";
  }
}
