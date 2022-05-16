package xyz.leutgeb.lorenz.atlas.typing.resources.rules;

import static xyz.leutgeb.lorenz.atlas.util.Util.append;

import java.util.List;
import java.util.Map;
import xyz.leutgeb.lorenz.atlas.typing.resources.AnnotatingContext;
import xyz.leutgeb.lorenz.atlas.typing.resources.AnnotatingGlobals;
import xyz.leutgeb.lorenz.atlas.typing.resources.coefficients.Coefficient;
import xyz.leutgeb.lorenz.atlas.typing.resources.proving.Obligation;

public class Shift implements Rule {
  public static final Shift INSTANCE = new Shift();

  public ApplicationResult apply(
      Obligation obligation, AnnotatingGlobals globals, Map<String, String> arguments) {

    final var qk = obligation.getContext().getAnnotation();
    final var qpk = obligation.getAnnotation();

    final var q = globals.getHeuristic().generate("s" + qk.getName(), qk.size());
    final var qp = globals.getHeuristic().generate("s" + qpk.getName(), qpk.size());

    final var k = Coefficient.unknownFromPrefix("k");

    return new ApplicationResult(
        List.of(
            new Obligation(
                new AnnotatingContext(obligation.getContext().getIds(), q),
                obligation.getExpression(),
                qp,
                obligation.isCost(),
                obligation.isCoin())),
        List.of(append(qk.increment(q, k, "(shift)"), qpk.increment(qp, k, "(shift)"))));
  }
}
