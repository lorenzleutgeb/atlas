package xyz.leutgeb.lorenz.atlas.typing.resources.rules;

import static java.util.Collections.emptyList;
import static java.util.Collections.singletonList;
import static java.util.stream.Collectors.toList;
import static java.util.stream.Stream.concat;
import static xyz.leutgeb.lorenz.atlas.util.Util.bug;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import lombok.extern.slf4j.Slf4j;
import xyz.leutgeb.lorenz.atlas.ast.ShareExpression;
import xyz.leutgeb.lorenz.atlas.typing.resources.AnnotatingGlobals;
import xyz.leutgeb.lorenz.atlas.typing.resources.constraints.EqualityConstraint;
import xyz.leutgeb.lorenz.atlas.typing.resources.constraints.EqualsSumConstraint;
import xyz.leutgeb.lorenz.atlas.typing.resources.proving.Obligation;

@Slf4j
public class Share implements Rule {
  public static final Share INSTANCE = new Share();

  public Rule.ApplicationResult apply(
      Obligation obligation, AnnotatingGlobals globals, Map<String, String> arguments) {
    log.warn("This rule is not tested well.");

    // This is denoted by \/(Q) in the paper.
    final var gammazSQ = obligation.getContext();

    final ShareExpression expression = (ShareExpression) obligation.getExpression();

    // This is a pair, the elements are called x and y in the paper.
    final var xy = expression.getDown();
    final var x = xy.getLeft();
    final var y = xy.getRight();

    // This is called z in the paper.
    final var z = expression.getUp();

    if (gammazSQ.getIds().contains(xy.getLeft()) || gammazSQ.getIds().contains(xy.getRight())) {
      throw bug("an id that is introduced here is already in the context");
    }

    final var upIndex = gammazSQ.getIndex(z);
    if (upIndex < 0) {
      throw bug("up not contained");
    }

    final var gammaxyQIds = new ArrayList<>(gammazSQ.getIds());
    gammaxyQIds.remove(z);
    gammaxyQIds.add(xy.getLeft());
    gammaxyQIds.add(xy.getRight());
    final var gammaxyQ =
        globals.getHeuristic().generateContext("share " + z + " as " + xy, gammaxyQIds);

    return new Rule.ApplicationResult(
        singletonList(
            obligation.keepCost(gammaxyQ, expression.getScope(), obligation.getAnnotation())),
        singletonList(
            concat(
                    gammazSQ.getIds().stream()
                        .map(
                            id ->
                                id.equals(z)
                                    ? new EqualsSumConstraint(
                                        gammazSQ.getRankCoefficient(id),
                                        List.of(
                                            gammaxyQ.getRankCoefficient(x),
                                            gammaxyQ.getRankCoefficient(y)),
                                        "(share) rank coefficients are sum-equal when"
                                            + expression.getUp()
                                            + " as "
                                            + expression.getDown()
                                            + " in expression `"
                                            + expression.getScope()
                                            + "`")
                                    : new EqualityConstraint(
                                        gammaxyQ.getRankCoefficient(id),
                                        gammazSQ.getRankCoefficient(id),
                                        "(share) rank coefficients are equal when sharing "
                                            + expression.getUp()
                                            + " as "
                                            + expression.getDown()
                                            + " in expression `"
                                            + expression.getScope()
                                            + "`")),

                    // Version 1:
                    gammaxyQ
                        .streamNonRank()
                        .map(
                            qEntry -> {
                              final var index = qEntry.getAssociatedIndices();
                              final var unsharedIndex = new HashMap<>(index);
                              unsharedIndex.put(z, index.get(x) + index.get(y));
                              return new EqualityConstraint(
                                  gammaxyQ.getCoefficient(
                                      qEntry.getAssociatedIndices(), qEntry.getOffsetIndex()),
                                  gammazSQ.getCoefficientOrZero(
                                      unsharedIndex, qEntry.getOffsetIndex()),
                                  "(share) ? "
                                      + expression.getUp()
                                      + " as "
                                      + expression.getDown()
                                      + " in expression `"
                                      + expression.getScope()
                                      + "`");
                            }))

                // Version 2:
                /*
                                    gammazSQ
                                        .streamIndices()
                                        .flatMap(
                                            entry -> {
                                              final var index = entry.getLeft();
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
                                                                sharedIndex, entry.getRight()),
                                                            gammazSQ.getCoefficient(
                                                                sharedIndex, entry.getRight()));
                                                      });
                                            }))
                */
                .collect(toList())),
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
                  .filter(entry -> entry.getRight().equals(sharedEntry.getRight()))
                  .filter(
                      entry ->
                          entry.getLeft().entrySet().stream()
                              .filter(entryToCheck -> !entryToCheck.getKey().equals(up.getName()))
                              .allMatch(
                                  entryToCheck ->
                                      entryToCheck
                                          .getValue()
                                          .equals(
                                              sharedEntry.getLeft().get(entryToCheck.getKey()))))
                  .filter(
                      entry ->
                          entry
                              .getLeft()
                              .get(up.getName())
                              .equals(
                                  sharedEntry.getLeft().get(down.getLeft().getName())
                                      + sharedEntry.getLeft().get(down.getRight().getName())))
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
