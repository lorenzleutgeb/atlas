package xyz.leutgeb.lorenz.lac.typing.resources.rules;

import static xyz.leutgeb.lorenz.lac.Util.bug;

import java.util.List;
import java.util.Map;
import java.util.stream.Collectors;
import java.util.stream.Stream;
import xyz.leutgeb.lorenz.lac.ast.Identifier;
import xyz.leutgeb.lorenz.lac.ast.Tuple;
import xyz.leutgeb.lorenz.lac.typing.resources.AnnotatingGlobals;
import xyz.leutgeb.lorenz.lac.typing.resources.constraints.EqualityConstraint;
import xyz.leutgeb.lorenz.lac.typing.resources.proving.Obligation;

public class Node implements Rule {
  @Override
  public RuleApplicationResult apply(Obligation obligation, AnnotatingGlobals globals) {
    final var expression = (Tuple) obligation.getExpression();

    final var leftId = ((Identifier) expression.getLeft()).getName();
    final var rightId = ((Identifier) expression.getRight()).getName();

    final var q = obligation.getContext();

    final var expectedSize = leftId.equals(rightId) ? 1 : 2;
    if (q.size() != expectedSize) {
      throw bug("context was not weakened");
    }

    final var q1 = q.getRankCoefficient(leftId);
    final var q2 = q.getRankCoefficient(rightId);

    final var q100 = q.getCoefficientOrZero(Map.of(leftId, 1, rightId, 0), 0);
    final var q010 = q.getCoefficientOrZero(Map.of(leftId, 0, rightId, 1), 0);

    final var rankCoefficient = obligation.getAnnotation().getRankCoefficient(0);

    return RuleApplicationResult.onlyConstraints(
        Stream.concat(
                Stream.of(
                    // q_1 = q_2 = q_{*}'
                    new EqualityConstraint(q1, q2),
                    new EqualityConstraint(q2, rankCoefficient),

                    // q_{1,0,0} = q_{0,1,0} = q_{*}'
                    new EqualityConstraint(q100, q010),
                    new EqualityConstraint(q010, rankCoefficient)),

                // q_{a,a,b} = q'_{a,b}
                q.stream()
                    .filter(
                        qEntry ->
                            qEntry
                                .getAssociatedIndices()
                                .get(leftId)
                                .equals(qEntry.getAssociatedIndices().get(rightId)))
                    .map(
                        qEntry -> {
                          var a = qEntry.getAssociatedIndices().get(leftId);
                          var b = qEntry.getOffsetIndex();
                          return new EqualityConstraint(
                              obligation.getAnnotation().getCoefficient(List.of(a, b)),
                              q.getCoefficient(
                                  qEntry.getAssociatedIndices(), qEntry.getOffsetIndex()));
                        }))
            .collect(Collectors.toList()));
  }
}
