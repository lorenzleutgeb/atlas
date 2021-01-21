package xyz.leutgeb.lorenz.lac.typing.resources.rules;

import java.util.ArrayList;
import java.util.List;
import java.util.Map;
import xyz.leutgeb.lorenz.lac.typing.resources.AnnotatingContext;
import xyz.leutgeb.lorenz.lac.typing.resources.AnnotatingGlobals;
import xyz.leutgeb.lorenz.lac.typing.resources.constraints.Constraint;
import xyz.leutgeb.lorenz.lac.typing.resources.constraints.EqualityConstraint;
import xyz.leutgeb.lorenz.lac.typing.resources.proving.Obligation;

/*
\infer[\ruleshift]{
  \tjudge{\Gamma}{Q+K}{e}{\alpha}{Q'+K}
}{%
  \tjudge{\Gamma}{Q}{e}{\alpha}{Q'}
  &
  K \geqslant 0
}
 */

public class Shift implements Rule {
  public static final Shift INSTANCE = new Shift();

  public ApplicationResult apply(
      Obligation obligation, AnnotatingGlobals globals, Map<String, String> arguments) {

    final var qk = obligation.getContext().getAnnotation();
    final var qpk = obligation.getAnnotation();

    final var k = qk.getUnitCoefficientOrZero();
    final var kp = qpk.getUnitCoefficientOrZero();

    final var constraints = new ArrayList<Constraint>();

    constraints.add(new EqualityConstraint(k, kp, getName()));

    final var q = globals.getHeuristic().generate("shiftedQ", qk.size());
    final var qp = globals.getHeuristic().generate("shiftedQp", qpk.size());

    constraints.addAll(obligation.getContext().getAnnotation().increment(q, k, "(shift)"));
    constraints.addAll(obligation.getAnnotation().increment(qp, k, "(shift)"));

    return new ApplicationResult(
        List.of(
            new Obligation(
                new AnnotatingContext(obligation.getContext().getIds(), q),
                obligation.getExpression(),
                qp,
                obligation.getCost())),
        List.of(constraints));
  }

  @Override
  public String getName() {
    return "shift";
  }
}
