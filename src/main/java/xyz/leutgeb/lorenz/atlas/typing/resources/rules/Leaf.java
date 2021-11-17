package xyz.leutgeb.lorenz.atlas.typing.resources.rules;

import static xyz.leutgeb.lorenz.atlas.ast.Identifier.LEAF_NAME;
import static xyz.leutgeb.lorenz.atlas.typing.resources.coefficients.KnownCoefficient.ZERO;
import static xyz.leutgeb.lorenz.atlas.util.Util.bug;

import java.util.ArrayList;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.function.Predicate;
import java.util.stream.Collectors;
import java.util.stream.Stream;
import lombok.extern.slf4j.Slf4j;
import xyz.leutgeb.lorenz.atlas.ast.Identifier;
import xyz.leutgeb.lorenz.atlas.typing.resources.AnnotatingGlobals;
import xyz.leutgeb.lorenz.atlas.typing.resources.coefficients.Coefficient;
import xyz.leutgeb.lorenz.atlas.typing.resources.constraints.Constraint;
import xyz.leutgeb.lorenz.atlas.typing.resources.constraints.EqualityConstraint;
import xyz.leutgeb.lorenz.atlas.typing.resources.constraints.EqualsSumConstraint;
import xyz.leutgeb.lorenz.atlas.typing.resources.proving.Obligation;

@Slf4j
public class Leaf implements Rule {
  public static final Leaf INSTANCE = new Leaf();

  public Rule.ApplicationResult apply(
      Obligation obligation, AnnotatingGlobals globals, Map<String, String> arguments) {
    final var expression = obligation.getExpression();
    if (!(expression instanceof final Identifier id)) {
      throw bug(
          "cannot apply (leaf) to identifier expression that is not identifier 'leaf' (it is '"
              + expression.terminalOrBox()
              + "')");
    }

    if (!id.getName().equals(LEAF_NAME)) {
      throw bug("cannot apply (leaf) to identifier 'leaf' (it is '" + id.getName() + "')");
    }

    final var context = obligation.getContext();
    if (!context.isEmpty()) {
      throw bug("have something in context for leaf: " + context.getIds());
    }

    var q = context.getAnnotation();
    var qp = obligation.getAnnotation();

    final var occurred = new HashSet<Coefficient>();

    /*
    IntStream.range(0, qp.size())
        .mapToObj(i -> new EqualityConstraint(qp.getRankCoefficient(i), ZERO, "(leaf) setToZero"))
        .forEach(constraints::add);
     */

    final var constraints =
        q.streamNonRankCoefficients()
            .filter(entry -> entry.getKey().get(0) >= 2)
            .flatMap(
                qEntry -> {
                  final var c = qEntry.getKey().get(0);

                  List<Coefficient> sum = new ArrayList<>();

                  if (c == 2) {
                    sum.add(qp.getRankCoefficientOrDefine());
                  }
                  for (int a = 0; a <= c; a++) {
                    int b = c - a;
                    Coefficient qpCoefficient = qp.getCoefficientOrZero(a, b);
                    if (ZERO.equals(qpCoefficient)) {
                      continue;
                    }
                    occurred.add(qpCoefficient);
                    sum.add(qpCoefficient);
                  }

                  if (!sum.isEmpty()) {
                    return Stream.of(
                        new EqualsSumConstraint(
                            qEntry.getValue(),
                            sum,
                            "(leaf from "
                                + (obligation.getExpression().getSource().getRoot())
                                + ") q_{(c)} = Σ_{a+b=c} q'_{(a, b)}"));
                  } else {
                    return Stream.of(
                        new EqualityConstraint(qEntry.getValue(), ZERO, "(leaf) setToZero 2"));
                  }
                })
            .collect(Collectors.<Constraint>toList());

    if (ZERO.equals(q.getCoefficientOrZero(2))) {
      constraints.add(
          new EqualityConstraint(qp.getRankCoefficientOrZero(), ZERO, "(leaf) cannot pay"));
    }

    // TODO: Check whether this is okay.
    // constraints.add(new EqualityConstraint(q.getCoefficientOrZero(1), ZERO, "(leaf) sanity"));

    qp.streamNonRankCoefficients()
        // TODO: Check whether this is okay.
        .filter(entry -> !(entry.getKey().get(0) == 1 && entry.getKey().get(1) == 0))
        .map(Map.Entry::getValue)
        .filter(Predicate.not(occurred::contains))
        .map(x -> new EqualityConstraint(x, ZERO, "(leaf) setToZero"))
        .forEach(constraints::add);

    return Rule.ApplicationResult.onlyConstraints(constraints);
  }

  @Override
  public String getName() {
    return "leaf";
  }
}
