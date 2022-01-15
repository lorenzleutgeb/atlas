package xyz.leutgeb.lorenz.atlas.typing.resources.rules;

import static xyz.leutgeb.lorenz.atlas.util.Util.append;

import java.util.List;
import java.util.Map;
import xyz.leutgeb.lorenz.atlas.ast.Identifier;
import xyz.leutgeb.lorenz.atlas.ast.IfThenElseExpression;
import xyz.leutgeb.lorenz.atlas.typing.resources.AnnotatingContext;
import xyz.leutgeb.lorenz.atlas.typing.resources.AnnotatingGlobals;
import xyz.leutgeb.lorenz.atlas.typing.resources.Annotation;
import xyz.leutgeb.lorenz.atlas.typing.resources.coefficients.Coefficient;
import xyz.leutgeb.lorenz.atlas.typing.resources.coefficients.KnownCoefficient;
import xyz.leutgeb.lorenz.atlas.typing.resources.constraints.EqualityConstraint;
import xyz.leutgeb.lorenz.atlas.typing.resources.constraints.EqualsSumConstraint;
import xyz.leutgeb.lorenz.atlas.typing.resources.proving.Obligation;

public class Ite implements Rule {
  public static final Ite INSTANCE = new Ite();

  public Rule.ApplicationResult apply(
      Obligation obligation, AnnotatingGlobals globals, Map<String, String> arguments) {
    final var expression = (IfThenElseExpression) obligation.getExpression();
    final var condition = expression.getCondition();

    if (Identifier.isCoin(condition)) {
      final var u = obligation.getContext().getAnnotation();

      final var u1 = globals.getHeuristic().generate("u1", u);
      final var u2 = globals.getHeuristic().generate("u2", u);

      final var p = KnownCoefficient.ONE_BY_TWO;
      // final var p = KnownCoefficient.ONE;
      final var negp = Coefficient.unknownFromPrefix("negp");

      final var pu1 = u1.multiply(p);
      final var pu2 = u2.multiply(negp);

      final var sum = Annotation.add(pu1.getLeft(), pu2.getLeft());

      /*
      Γ | U1 |- e1 : α | Q
      Γ | U2 |- e2 : α | Q
      U = p * U1 + (1 - p) * U2
      --------------------------------------------- (ite:coin)
      Γ | U |- if coin p then e1 else e2 : α | Q
      */

      return new Rule.ApplicationResult(
          List.of(
              obligation.keepAnnotationAndCost(
                  new AnnotatingContext(obligation.getContext().getIds(), u1),
                  expression.getTruthy()),
              obligation.keepAnnotationAndCost(
                  new AnnotatingContext(obligation.getContext().getIds(), u2),
                  expression.getFalsy())),
          List.of(pu1.getRight(), pu2.getRight()),
          append(
              append(sum.getRight(), EqualityConstraint.eq(u, sum.getLeft(), "(ite:coin)")),
              List.of(
                  new EqualsSumConstraint(KnownCoefficient.ONE, List.of(p, negp), "(ite:coin)"))));
    }

    // NOTE: We do not need to remove x from the context, since it must be a
    // boolean and therefore is not added to the context in the first place.
    return Rule.ApplicationResult.onlyObligations(
        obligation.keepContextAndAnnotationAndCost(expression.getTruthy()),
        obligation.keepContextAndAnnotationAndCost(expression.getFalsy()));
  }
}
