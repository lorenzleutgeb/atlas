package xyz.leutgeb.lorenz.lac.typing.resources.rules;

import static java.util.Collections.singletonList;
import static java.util.function.Predicate.not;
import static java.util.stream.Collectors.toList;
import static java.util.stream.Stream.concat;
import static xyz.leutgeb.lorenz.lac.typing.resources.coefficients.KnownCoefficient.ZERO;
import static xyz.leutgeb.lorenz.lac.util.Util.bug;

import java.util.ArrayList;
import java.util.HashSet;
import java.util.List;
import java.util.Optional;
import java.util.Set;
import java.util.stream.Collectors;
import xyz.leutgeb.lorenz.lac.ast.Expression;
import xyz.leutgeb.lorenz.lac.ast.Identifier;
import xyz.leutgeb.lorenz.lac.typing.resources.AnnotatingContext;
import xyz.leutgeb.lorenz.lac.typing.resources.AnnotatingGlobals;
import xyz.leutgeb.lorenz.lac.typing.resources.coefficients.Coefficient;
import xyz.leutgeb.lorenz.lac.typing.resources.constraints.Constraint;
import xyz.leutgeb.lorenz.lac.typing.resources.constraints.EqualityConstraint;
import xyz.leutgeb.lorenz.lac.typing.resources.proving.Obligation;

public class WVar implements Rule {
  public static final WVar INSTANCE = new WVar();

  public static Optional<Identifier> redundantId(Obligation obligation) {
    return redundantId(obligation.getContext(), obligation.getExpression());
  }

  public static Optional<Identifier> redundantId(AnnotatingContext context, Expression expression) {
    return context.getIds().stream().filter(not(expression.freeVariables()::contains)).findFirst();
  }

  public Rule.ApplicationResult apply(Obligation obligation, AnnotatingGlobals globals) {
    final var candidate = redundantId(obligation);

    if (candidate.isEmpty()) {
      throw bug("chose to weaken, but cannot weaken");
    }

    final var idToWeaken = candidate.get();
    final var context = obligation.getContext();
    final var remainingIds = new ArrayList<>(context.getIds());
    if (!remainingIds.remove(idToWeaken)) {
      throw bug("unknown identifier");
    }

    final var gammaR = globals.getHeuristic().generateContext("wvar", remainingIds);

    final Set<Coefficient> occurred = new HashSet<>();

    final List<Constraint> constraints =
        concat(
                gammaR.stream()
                    .map(
                        rIndex -> {
                          occurred.add(rIndex.getValue());
                          return new EqualityConstraint(
                              rIndex.getValue(),
                              context.getCoefficientOrZero(rIndex.mask(idToWeaken, 0)),
                              "(w:var) r_{(\\vec{a}, b)} = q_{(\\vec{a}, 0, b)} when removing "
                                  + idToWeaken
                                  + " for expression `"
                                  + obligation.getExpression()
                                  + "` with (\\vec{a}, b) = "
                                  + rIndex);
                        }),
                gammaR.getIds().stream()
                    .map(
                        e ->
                            new EqualityConstraint(
                                gammaR.getRankCoefficient(e),
                                context.getRankCoefficient(e),
                                "(w:var) r_i = q_i when removing "
                                    + idToWeaken
                                    + " for expression `"
                                    + obligation.getExpression()
                                    + "` with rank coefficient of "
                                    + e)))
            .collect(toList());

    final List<Constraint> setToZeroR =
        gammaR.stream()
            .map(AnnotatingContext.Entry::getValue)
            .filter(not(occurred::contains))
            .map(
                coefficient ->
                    new EqualityConstraint(coefficient, ZERO, "(w:var) setToZero r " + coefficient))
            .collect(Collectors.toUnmodifiableList());

    return new Rule.ApplicationResult(
        singletonList(obligation.keepAnnotationAndCost(gammaR, obligation.getExpression())),
        singletonList(constraints),
        setToZeroR);
  }

  @Override
  public String getName() {
    return "w:var";
  }
}
