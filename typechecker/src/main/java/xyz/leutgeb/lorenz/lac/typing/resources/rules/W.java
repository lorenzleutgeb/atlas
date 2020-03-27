package xyz.leutgeb.lorenz.lac.typing.resources.rules;

import static com.google.common.collect.Streams.concat;
import static java.util.Collections.emptyList;
import static java.util.Collections.singletonList;
import static java.util.stream.Collectors.toList;
import static xyz.leutgeb.lorenz.lac.Util.append;
import static xyz.leutgeb.lorenz.lac.Util.bug;

import java.util.List;
import java.util.function.BiFunction;
import java.util.stream.IntStream;
import org.hipparchus.util.Pair;
import xyz.leutgeb.lorenz.lac.typing.resources.AnnotatingContext;
import xyz.leutgeb.lorenz.lac.typing.resources.AnnotatingGlobals;
import xyz.leutgeb.lorenz.lac.typing.resources.Annotation;
import xyz.leutgeb.lorenz.lac.typing.resources.coefficients.Coefficient;
import xyz.leutgeb.lorenz.lac.typing.resources.constraints.Constraint;
import xyz.leutgeb.lorenz.lac.typing.resources.constraints.GreaterThanOrEqualConstraint;
import xyz.leutgeb.lorenz.lac.typing.resources.constraints.LessThanOrEqualConstraint;
import xyz.leutgeb.lorenz.lac.typing.resources.proving.Obligation;

public class W implements Rule {
  private static List<Constraint> compareCoefficientsLessOrEqual(
      Annotation left, Annotation right) {
    return compareCoefficients(left, right, LessThanOrEqualConstraint::new);
  }

  private static List<Constraint> compareCoefficientsGreaterOrEqual(
      Annotation left, Annotation right) {
    return compareCoefficients(left, right, GreaterThanOrEqualConstraint::new);
  }

  private static List<Constraint> compareCoefficients(
      Annotation left,
      Annotation right,
      BiFunction<Coefficient, Coefficient, Constraint> comparator) {
    if (left.size() != right.size()) {
      throw bug("cannot weaken annotations of different sizes");
    }
    return concat(
            left.streamCoefficients()
                .map(e -> comparator.apply(e.getValue(), right.getCoefficientOrZero(e.getKey()))),
            IntStream.range(0, left.size())
                .mapToObj(
                    i -> comparator.apply(left.getRankCoefficient(i), right.getRankCoefficient(i))))
        .collect(toList());
  }

  @Override
  public RuleApplicationResult apply(Obligation obligation, AnnotatingGlobals globals) {
    final var q = obligation.getContext().getAnnotation();
    final var qp = obligation.getAnnotation();

    final var p = globals.getHeuristic().generate("w", q);
    final var pp = globals.getHeuristic().generate("wp", qp);

    return new RuleApplicationResult(
        singletonList(
            Pair.create(
                obligation.keepCost(
                    new AnnotatingContext(obligation.getContext().getIds(), p),
                    obligation.getExpression(),
                    pp),
                append(
                    compareCoefficientsLessOrEqual(p, q),
                    compareCoefficientsGreaterOrEqual(pp, qp)))),
        emptyList());
  }
}
