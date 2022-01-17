package xyz.leutgeb.lorenz.atlas.typing.resources.rules;

import static xyz.leutgeb.lorenz.atlas.typing.resources.rules.Rule.ApplicationResult.onlyObligations;

import java.util.ArrayList;
import java.util.List;
import java.util.Map;
import xyz.leutgeb.lorenz.atlas.ast.expressions.IdentifierExpression;
import xyz.leutgeb.lorenz.atlas.ast.expressions.MatchTupleExpression;
import xyz.leutgeb.lorenz.atlas.typing.resources.AnnotatingContext;
import xyz.leutgeb.lorenz.atlas.typing.resources.AnnotatingGlobals;
import xyz.leutgeb.lorenz.atlas.typing.resources.proving.Obligation;

public class MatchTuple implements Rule {
  public static final MatchTuple INSTANCE = new MatchTuple();

  public ApplicationResult apply(
      Obligation obligation, AnnotatingGlobals globals, Map<String, String> arguments) {
    final var expression = (MatchTupleExpression) obligation.getExpression();
    final var target = expression.getPattern().getTree();

    if (target.isEmpty()) {
      return onlyObligations(obligation.keepContextAndAnnotationAndCost(expression.getBody()));
    }

    final var context = obligation.getContext();
    final var ids = context.getIds();
    final List<IdentifierExpression> replaced = new ArrayList<>(ids.size());

    for (final var id : ids) {
      replaced.add(id.equals(expression.getScrut()) ? target.get() : id);
    }

    return onlyObligations(
        obligation.keepAnnotationAndCost(
            new AnnotatingContext(replaced, context.getAnnotation()), expression.getBody()));
  }
}
