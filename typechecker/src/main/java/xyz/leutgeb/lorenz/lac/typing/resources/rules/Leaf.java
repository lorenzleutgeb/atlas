package xyz.leutgeb.lorenz.lac.typing.resources.rules;

import static xyz.leutgeb.lorenz.lac.ast.Identifier.LEAF;
import static xyz.leutgeb.lorenz.lac.typing.resources.coefficients.KnownCoefficient.ZERO;
import static xyz.leutgeb.lorenz.lac.util.Util.bug;

import java.util.ArrayList;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.function.Predicate;
import java.util.stream.Collectors;
import java.util.stream.Stream;
import lombok.extern.slf4j.Slf4j;
import xyz.leutgeb.lorenz.lac.typing.resources.AnnotatingGlobals;
import xyz.leutgeb.lorenz.lac.typing.resources.coefficients.Coefficient;
import xyz.leutgeb.lorenz.lac.typing.resources.constraints.Constraint;
import xyz.leutgeb.lorenz.lac.typing.resources.constraints.EqualityConstraint;
import xyz.leutgeb.lorenz.lac.typing.resources.constraints.EqualsSumConstraint;
import xyz.leutgeb.lorenz.lac.typing.resources.proving.Obligation;

@Slf4j
public class Leaf implements Rule {
  public static final Leaf INSTANCE = new Leaf();

  public Rule.ApplicationResult apply(Obligation obligation, AnnotatingGlobals globals) {
    if (!LEAF.equals(obligation.getExpression())) {
      throw bug("cannot apply (leaf) to identifier that is not 'leaf'");
    }

    final var context = obligation.getContext();
    if (!context.isEmpty()) {
      throw bug("have something in context for leaf");
    }

    var q = context.getAnnotation();
    var qp = obligation.getAnnotation();

    final var occurred = new HashSet<Coefficient>();

    /*
    IntStream.range(0, qp.size())
        .mapToObj(i -> new EqualityConstraint(qp.getRankCoefficient(i), ZERO, "(leaf) setToZero"))
        .forEach(constraints::add);
     */

    if (ZERO.equals(q.getCoefficientOrZero(2))) {
      // throw bug("cannot pay leaf");
      log.error("cannot pay leaf");
    }

    final var constraints =
        q.streamNonRankCoefficients()
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
                            qEntry.getValue(), sum, "(leaf) q_{(c)} = Σ_{a+b=c} q'_{(a, b)}"));
                  } else {
                    return Stream.of(new EqualityConstraint(qEntry.getValue(), ZERO, "(leaf)"));
                  }
                })
            .collect(Collectors.<Constraint>toList());

    qp.streamNonRankCoefficients()
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
