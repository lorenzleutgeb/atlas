package xyz.leutgeb.lorenz.atlas.typing.resources.rules;

import java.util.List;
import java.util.Map;
import lombok.extern.slf4j.Slf4j;
import xyz.leutgeb.lorenz.atlas.ast.expressions.TickExpression;
import xyz.leutgeb.lorenz.atlas.typing.resources.AnnotatingContext;
import xyz.leutgeb.lorenz.atlas.typing.resources.AnnotatingGlobals;
import xyz.leutgeb.lorenz.atlas.typing.resources.proving.Obligation;

@Slf4j
public class TickAst implements Rule {
  public static final TickAst INSTANCE = new TickAst();

  public ApplicationResult apply(
      Obligation obligation, AnnotatingGlobals globals, Map<String, String> arguments) {
    final var expression = obligation.getExpression();

    if (expression instanceof TickExpression tickExpression) {
      if (!obligation.isCost()) {
        return ApplicationResult.onlyObligations(
            obligation.keepContextAndAnnotationAndCost(tickExpression.getBody()));
      }

      final var context = obligation.getContext();
      final var annotation = context.getAnnotation();
      final var q = globals.getHeuristic().generate("tickast" + annotation.getName(), annotation);

      return new ApplicationResult(
          List.of(
              obligation.keepCost(
                  new AnnotatingContext(context.getIds(), q),
                  tickExpression.getBody(),
                  obligation.getAnnotation())),
          List.of(annotation.increment(q, tickExpression.getCost(), "(tick:ast)")));
    }

    if (!obligation.isCost()) {
      return ApplicationResult.noop(obligation);
    }

    final var context = obligation.getContext();
    final var annotation = context.getAnnotation();
    final var q = globals.getHeuristic().generate("tickast" + annotation.getName(), annotation);
    log.warn(
        "Rule is not applied to a tick expression. Assuming a cost of 1 and continuing anyway.");
    return new ApplicationResult(
        List.of(
            obligation.keepCost(
                new AnnotatingContext(context.getIds(), q),
                expression,
                obligation.getAnnotation())),
        List.of(annotation.increment(q, 1, "(tick:ast)")));
  }
}
