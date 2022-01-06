package xyz.leutgeb.lorenz.atlas.typing.resources.rules;

import java.util.List;
import java.util.Map;
import xyz.leutgeb.lorenz.atlas.ast.Identifier;
import xyz.leutgeb.lorenz.atlas.ast.IfThenElseExpression;
import xyz.leutgeb.lorenz.atlas.typing.resources.AnnotatingGlobals;
import xyz.leutgeb.lorenz.atlas.typing.resources.coefficients.KnownCoefficient;
import xyz.leutgeb.lorenz.atlas.typing.resources.proving.Obligation;

public class Ite implements Rule {
  public static final Ite INSTANCE = new Ite();

  public Rule.ApplicationResult apply(
      Obligation obligation, AnnotatingGlobals globals, Map<String, String> arguments) {
    final var expression = (IfThenElseExpression) obligation.getExpression();
    final var condition = expression.getCondition();
    final var isCoin =
        condition instanceof Identifier
            && Identifier.COIN_NAME.equals(((Identifier) condition).getName());

    if (isCoin) {
      // This is a coin flip.

      /*
      Γ | U1 |- e1 : α | Q
      Γ | U2 |- e2 : α | Q
      U = p * U1 + (1 - p) * U2
      --------------------------------------------- (ite:coin)
      Γ | U |- if coin p then e1 else e2 : α | Q
      */

      final var p = KnownCoefficient.ONE_BY_TWO;
      final var negp = KnownCoefficient.ONE_BY_TWO;

      obligation.getContext();

      return null;
    }

    // NOTE: We do not need to remove x from the context, since it must be a
    // boolean and therefore is not added to the context in the first place.
    return Rule.ApplicationResult.onlyObligations(
        List.of(
            obligation.keepContextAndAnnotationAndCost(expression.getTruthy()),
            obligation.keepContextAndAnnotationAndCost(expression.getFalsy())));
  }
}
