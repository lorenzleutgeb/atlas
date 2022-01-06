package xyz.leutgeb.lorenz.atlas.typing.resources.rules;

import java.util.Map;
import xyz.leutgeb.lorenz.atlas.typing.resources.AnnotatingGlobals;
import xyz.leutgeb.lorenz.atlas.typing.resources.proving.Obligation;

public class Cmp implements Rule {
  public static final Cmp INSTANCE = new Cmp();

  public Rule.ApplicationResult apply(
      Obligation obligation, AnnotatingGlobals globals, Map<String, String> arguments) {
    return Rule.ApplicationResult.empty();
  }
}
