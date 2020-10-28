package xyz.leutgeb.lorenz.lac.typing.resources.rules;

import xyz.leutgeb.lorenz.lac.typing.resources.AnnotatingGlobals;
import xyz.leutgeb.lorenz.lac.typing.resources.proving.Obligation;

public class Cmp implements Rule {
  public static final Cmp INSTANCE = new Cmp();

  public Rule.ApplicationResult apply(Obligation obligation, AnnotatingGlobals globals) {
    return Rule.ApplicationResult.empty();
  }

  @Override
  public String getName() {
    return "cmp";
  }
}
