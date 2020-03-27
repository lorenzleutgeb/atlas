package xyz.leutgeb.lorenz.lac.typing.resources.rules;

import static java.util.Collections.emptyList;
import static java.util.Collections.singletonList;
import static java.util.stream.Collectors.toList;
import static java.util.stream.Stream.concat;
import static xyz.leutgeb.lorenz.lac.Util.bug;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import org.hipparchus.util.Pair;
import xyz.leutgeb.lorenz.lac.ast.ShareExpression;
import xyz.leutgeb.lorenz.lac.typing.resources.AnnotatingGlobals;
import xyz.leutgeb.lorenz.lac.typing.resources.constraints.EqualityConstraint;
import xyz.leutgeb.lorenz.lac.typing.resources.constraints.EqualsSumConstraint;
import xyz.leutgeb.lorenz.lac.typing.resources.proving.Obligation;

public class Share implements Rule {
  @Override
  public RuleApplicationResult apply(Obligation obligation, AnnotatingGlobals globals) {
    // This is denoted by \/(Q) in the paper.
    final var gammazSQ = obligation.getContext();

    final ShareExpression expression = (ShareExpression) obligation.getExpression();

    // This is a pair, the elements are called x and y in the paper.
    final var xy = expression.getDown();
    final var x = xy.getFirst().getName();
    final var y = xy.getSecond().getName();

    // This is called z in the paper.
    final var z = expression.getUp().getName();

    if (gammazSQ.getIds().contains(xy.getFirst().getName())
        || gammazSQ.getIds().contains(xy.getSecond().getName())) {
      throw bug("an id that is introduced here is already in the context");
    }

    final var upIndex = gammazSQ.getIndex(z);
    if (upIndex < 0) {
      throw bug("up not contained");
    }

    final var gammaxyQIds = new ArrayList<>(gammazSQ.getIds());
    gammaxyQIds.remove(z);
    gammaxyQIds.add(xy.getFirst().getName());
    gammaxyQIds.add(xy.getSecond().getName());
    final var gammaxyQ = globals.getHeuristic().generateContext("share", gammaxyQIds);

    return new RuleApplicationResult(
        singletonList(
            Pair.create(
                obligation.keepCost(gammaxyQ, expression.getScope(), obligation.getAnnotation()),
                concat(
                        gammazSQ.getIds().stream()
                            .map(
                                id ->
                                    id.equals(z)
                                        ? new EqualsSumConstraint(
                                            gammazSQ.getRankCoefficient(id),
                                            List.of(
                                                gammaxyQ.getRankCoefficient(x),
                                                gammaxyQ.getRankCoefficient(y)))
                                        : new EqualityConstraint(
                                            gammaxyQ.getRankCoefficient(id),
                                            gammazSQ.getRankCoefficient(id))),

                        // Version 1:
                        gammaxyQ.stream()
                            .map(
                                qEntry -> {
                                  final var index = qEntry.getAssociatedIndices();
                                  final var unsharedIndex = new HashMap<>(index);
                                  unsharedIndex.put(z, index.get(x) + index.get(y));
                                  return new EqualityConstraint(
                                      gammaxyQ.getCoefficient(
                                          qEntry.getAssociatedIndices(), qEntry.getOffsetIndex()),
                                      gammazSQ.getCoefficientOrZero(
                                          unsharedIndex, qEntry.getOffsetIndex()));
                                }))

                    // Version 2:
                    /*
                                        gammazSQ
                                            .streamIndices()
                                            .flatMap(
                                                entry -> {
                                                  final var index = entry.getFirst();
                                                  final var a = index.get(z);
                                                  return rangeClosed(0, a)
                                                      .mapToObj(
                                                          a1 -> {
                                                            final var a2 = a - a1;

                                                            final var sharedIndex = new HashMap<>(index);
                                                            sharedIndex.put(x, a1);
                                                            sharedIndex.put(y, a2);

                                                            return new EqualityConstraint(
                                                                gammaxyQ.getCoefficient(
                                                                    sharedIndex, entry.getSecond()),
                                                                gammazSQ.getCoefficient(
                                                                    sharedIndex, entry.getSecond()));
                                                          });
                                                }))
                    */
                    .collect(toList()))),
        emptyList());

    /*
    for (Map.Entry<List<Integer>, Coefficient> entry : q.getCoefficients()) {
      final var index = entry.getKey();
      final var a1 = index.get(qSize - 2);
      final var a2 = index.get(qSize - 1);
      final List<Integer> unsharedIndex = new ArrayList<>(gammazSQ.size() + 1);
      if (qSize > 2) {
        unsharedIndex.addAll(index.subList(0, upIndex));
      }
      unsharedIndex.add(a1 + a2);
      if (qSize > 2) {
        unsharedIndex.addAll(index.subList(upIndex + 1, gammazSQ.size()));
      }
      unsharedIndex.add(index.get(index.size() - 1));
      generatedConstraints.addAll(
          EqualityConstraint.eq(
              entry.getValue(), gammazSQ.getAnnotation().getCoefficientOrZero(unsharedIndex)));
    }
     */

    /*
    sharedContext
        .streamIndices()
        .forEach(
            sharedEntry -> {
              context
                  .streamIndices()
                  .filter(entry -> entry.getSecond().equals(sharedEntry.getSecond()))
                  .filter(
                      entry ->
                          entry.getFirst().entrySet().stream()
                              .filter(entryToCheck -> !entryToCheck.getKey().equals(up.getName()))
                              .allMatch(
                                  entryToCheck ->
                                      entryToCheck
                                          .getValue()
                                          .equals(
                                              sharedEntry.getFirst().get(entryToCheck.getKey()))))
                  .filter(
                      entry ->
                          entry
                              .getFirst()
                              .get(up.getName())
                              .equals(
                                  sharedEntry.getFirst().get(down.getFirst().getName())
                                      + sharedEntry.getFirst().get(down.getSecond().getName())))
                  .forEach(
                      entry -> {
                        constraints.eq(
                            this,
                            context.getCoefficient(entry),
                            sharedContext.getCoefficient(sharedEntry));
                      });
            });
     */
  }
}
