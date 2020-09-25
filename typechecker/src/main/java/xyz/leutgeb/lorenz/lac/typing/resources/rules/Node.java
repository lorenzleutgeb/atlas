package xyz.leutgeb.lorenz.lac.typing.resources.rules;

import static xyz.leutgeb.lorenz.lac.util.Util.bug;

import java.util.List;
import java.util.stream.Collectors;
import java.util.stream.Stream;
import xyz.leutgeb.lorenz.lac.ast.Identifier;
import xyz.leutgeb.lorenz.lac.ast.Tuple;
import xyz.leutgeb.lorenz.lac.typing.resources.AnnotatingGlobals;
import xyz.leutgeb.lorenz.lac.typing.resources.constraints.EqualityConstraint;
import xyz.leutgeb.lorenz.lac.typing.resources.proving.Obligation;

public class Node implements Rule {
  public static final Node INSTANCE = new Node();

  public Rule.ApplicationResult apply(Obligation obligation, AnnotatingGlobals globals) {
    final var expression = (Tuple) obligation.getExpression();

    final var leftId = ((Identifier) expression.getLeft());
    final var rightId = ((Identifier) expression.getRight());

    final var expectedSize = leftId.equals(rightId) ? 1 : 2;
    final var qplus1 = obligation.getContext();

    if (qplus1.size() != expectedSize) {
      throw bug("context was not weakened");
    }

    // We reorder the context such that it matches the syntax.
    final var qplus1reordered = qplus1.reorder(List.of(leftId, rightId)).getAnnotation();

    final var q = globals.getHeuristic().generate("qplus1minus1", qplus1reordered);

    final var q1 = q.getRankCoefficient(0);
    final var q2 = q.getRankCoefficient(1);

    final var q100 = q.getCoefficientOrZero(1, 0, 0);
    final var q010 = q.getCoefficientOrZero(0, 1, 0);

    final var rankCoefficient = obligation.getAnnotation().getRankCoefficient(0);

    return Rule.ApplicationResult.onlyConstraints(
        Stream.concat(
                qplus1reordered.increment(q, obligation.getCost(), "(node) Q + 1").stream(),
                Stream.concat(
                    Stream.of(
                        new EqualityConstraint(
                            q1, q2, "(node) q_1 = q_2 for expression `" + expression + "`"),
                        new EqualityConstraint(
                            q2,
                            rankCoefficient,
                            "(node) q_2 = q_{*}' for expression `" + expression + "`"),
                        new EqualityConstraint(
                            q100,
                            q010,
                            "(node) q_{1,0,0} = q_{0,1,0} for expression `" + expression + "`"),
                        new EqualityConstraint(
                            q010,
                            rankCoefficient,
                            "(node) q_{0,1,0} = q_{*}' for expression `" + expression + "`")),
                    q.streamCoefficients()
                        .filter(qEntry -> qEntry.getKey().get(0).equals(qEntry.getKey().get(1)))
                        .map(
                            qEntry -> {
                              var a = qEntry.getKey().get(0);
                              var c = qEntry.getKey().get(2);
                              return new EqualityConstraint(
                                  q.getCoefficientOrZero(a, a, c),
                                  obligation.getAnnotation().getCoefficientOrZero(List.of(a, c)),
                                  "(node) q_{a,a,c} = q'_{a,c} for expression `"
                                      + expression
                                      + "` with (a,c)="
                                      + qEntry);
                            })))
            .collect(Collectors.toList()));
  }

  @Override
  public String getName() {
    return "node";
  }
}
