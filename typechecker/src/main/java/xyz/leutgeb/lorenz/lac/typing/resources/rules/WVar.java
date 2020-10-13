package xyz.leutgeb.lorenz.lac.typing.resources.rules;

import static java.util.Collections.emptyList;
import static java.util.Collections.singletonList;
import static java.util.function.Predicate.not;
import static xyz.leutgeb.lorenz.lac.util.Util.append;
import static xyz.leutgeb.lorenz.lac.util.Util.bug;

import java.util.ArrayList;
import java.util.Optional;
import java.util.stream.Collectors;
import xyz.leutgeb.lorenz.lac.ast.Expression;
import xyz.leutgeb.lorenz.lac.ast.Identifier;
import xyz.leutgeb.lorenz.lac.typing.resources.AnnotatingContext;
import xyz.leutgeb.lorenz.lac.typing.resources.AnnotatingGlobals;
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

    // final var gammaR = globals.getHeuristic().generateContext("wvar", remainingIds);
    final var gammaR = new AnnotatingContext(remainingIds, "wvar " + idToWeaken);

    return new Rule.ApplicationResult(
        singletonList(obligation.keepAnnotationAndCost(gammaR, obligation.getExpression())),
        singletonList(
            append(
                EqualityConstraint.eqRanksDefineFromLeft(
                    remainingIds,
                    context,
                    gammaR,
                    "(w:var) q_i = r_i when removing "
                        + idToWeaken
                        + " for expression `"
                        + obligation.getExpression()),
                context
                    .streamNonRank()
                    .filter(entry -> entry.getAssociatedIndex(idToWeaken) == 0)
                    .map(
                        entry ->
                            new EqualityConstraint(
                                entry.getValue(),
                                gammaR.getCoefficientOrDefine(entry),
                                "(w:var) r_{(a⃗⃗, b)} = q_{(a⃗⃗, 0, b)} when removing "
                                    + idToWeaken
                                    + " for expression `"
                                    + obligation.getExpression()
                                    + "`"
                                // + "` with (a⃗⃗, b) = "
                                // + rIndex
                                ))
                    .collect(Collectors.toUnmodifiableList()))),
        emptyList());
  }

  @Override
  public String getName() {
    return "w:var";
  }
}
