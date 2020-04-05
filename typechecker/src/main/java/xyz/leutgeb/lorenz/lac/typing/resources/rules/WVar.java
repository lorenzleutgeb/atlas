package xyz.leutgeb.lorenz.lac.typing.resources.rules;

import static com.google.common.collect.Sets.difference;
import static java.util.Collections.emptyList;
import static java.util.Collections.singletonList;
import static java.util.stream.Collectors.toList;
import static java.util.stream.Stream.concat;
import static xyz.leutgeb.lorenz.lac.Util.bug;
import static xyz.leutgeb.lorenz.lac.Util.pick;

import java.util.ArrayList;
import java.util.HashSet;
import java.util.List;
import java.util.Set;
import org.hipparchus.util.Pair;
import xyz.leutgeb.lorenz.lac.Util;
import xyz.leutgeb.lorenz.lac.ast.Expression;
import xyz.leutgeb.lorenz.lac.typing.resources.AnnotatingContext;
import xyz.leutgeb.lorenz.lac.typing.resources.AnnotatingGlobals;
import xyz.leutgeb.lorenz.lac.typing.resources.constraints.Constraint;
import xyz.leutgeb.lorenz.lac.typing.resources.constraints.EqualityConstraint;
import xyz.leutgeb.lorenz.lac.typing.resources.proving.Obligation;

public class WVar {
  /**
   * Preconditions:
   *
   * <ol>
   *   <li>x ∈ gammaxQ
   *   <li>NOT x ∈ gammaR
   *   <li>gammaxQ.size() == gammaR.size() + 1
   * </ol>
   *
   * @param gammaR
   * @param gammaxQ
   * @param x
   * @return
   */
  private static List<Constraint> weakenIdentifier(
      AnnotatingContext gammaR, AnnotatingContext gammaxQ, String x) {
    return concat(
            // r_{(\vec{a}, b)} = q_{(\vec{a}, 0, b)}
            gammaR.stream()
                .map(
                    rIndex -> {
                      final var qIndex = rIndex.mask(x, 0);

                      return new EqualityConstraint(
                          rIndex.getValue(), gammaxQ.getCoefficient(qIndex));
                    }),

            // r_i = q_i
            gammaR.getIds().stream()
                .map(
                    e ->
                        new EqualityConstraint(
                            gammaR.getRankCoefficient(e), gammaxQ.getRankCoefficient(e))))
        .collect(toList());
  }

  public static Set<String> redundantIds(Obligation obligation) {
    return redundantIds(obligation.getContext(), obligation.getExpression());
  }

  public static Set<String> redundantIds(AnnotatingContext context, Expression expression) {
    // Find out which variable to remove from the context.
    // In order to do that, take the set of identifiers in the
    // context, and subtract from it the set of identifiers in
    // the target expression. Any of the elements of this set
    // can be removed.
    final var idsInContext = new HashSet<>(context.getIds());
    final var idsInExpression = Util.setOfNames(expression.freeVariables());
    return difference(idsInContext, idsInExpression);
  }

  public static Rule.ApplicationResult apply(Obligation obligation, AnnotatingGlobals globals) {
    final var candidates = redundantIds(obligation);

    if (candidates.isEmpty()) {
      throw bug("chose to weaken, but cannot weaken");
    }

    final var idToWeaken = pick(candidates);
    final var context = obligation.getContext();
    final var remainingIds = new ArrayList<>(context.getIds());
    if (!remainingIds.remove(idToWeaken)) {
      throw bug("unknown identifier");
    }

    final var gammaR = globals.getHeuristic().generateContext("weaken", remainingIds);

    return new Rule.ApplicationResult(
        singletonList(
            Pair.create(
                new Obligation(
                    gammaR,
                    obligation.getExpression(),
                    obligation.getAnnotation(),
                    obligation.getCost()),
                weakenIdentifier(gammaR, context, idToWeaken))),
        emptyList());
  }
}
