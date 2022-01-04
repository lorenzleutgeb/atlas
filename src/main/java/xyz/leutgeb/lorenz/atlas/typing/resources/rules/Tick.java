package xyz.leutgeb.lorenz.atlas.typing.resources.rules;

import java.util.List;
import java.util.Map;
import xyz.leutgeb.lorenz.atlas.typing.resources.AnnotatingGlobals;
import xyz.leutgeb.lorenz.atlas.typing.resources.proving.Obligation;

public class Tick implements Rule {
  public static final Tick INSTANCE = new Tick();

  public ApplicationResult apply(
      Obligation obligation, AnnotatingGlobals globals, Map<String, String> arguments) {
    if (!obligation.isCost()) {
      return ApplicationResult.noop(obligation);
    }

    final var annotation = obligation.getAnnotation();
    final var q = globals.getHeuristic().generate("qptick", annotation);

    return new ApplicationResult(
        List.of(obligation.keepCost(obligation.getContext(), obligation.getExpression(), q)),
        List.of(q.increment(annotation, 1, "(tick)")));
  }

  @Override
  public String getName() {
    return "tick";
  }
}
