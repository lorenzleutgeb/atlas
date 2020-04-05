package xyz.leutgeb.lorenz.lac.typing.resources.rules;

import java.util.List;
import xyz.leutgeb.lorenz.lac.ast.IfThenElseExpression;
import xyz.leutgeb.lorenz.lac.typing.resources.AnnotatingGlobals;
import xyz.leutgeb.lorenz.lac.typing.resources.proving.Obligation;

public class Ite {
  public static Rule.ApplicationResult apply(Obligation obligation, AnnotatingGlobals globals) {
    final var expression = (IfThenElseExpression) obligation.getExpression();

    // NOTE: We do not need to remove x from the context, since it must be a
    // boolean and therefore is not added to the context in the first place.
    return Rule.ApplicationResult.onlyObligations(
        List.of(
            obligation.keepCostAndContextAndAnnotation(expression.getTruthy()),
            obligation.keepCostAndContextAndAnnotation(expression.getFalsy())));
  }
}
