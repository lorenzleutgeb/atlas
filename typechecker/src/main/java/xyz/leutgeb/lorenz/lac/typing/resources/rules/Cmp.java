package xyz.leutgeb.lorenz.lac.typing.resources.rules;

import xyz.leutgeb.lorenz.lac.typing.resources.AnnotatingGlobals;
import xyz.leutgeb.lorenz.lac.typing.resources.proving.Obligation;

public class Cmp implements Rule {
  @Override
  public RuleApplicationResult apply(Obligation obligation, AnnotatingGlobals globals) {
    return RuleApplicationResult.empty();
  }
}
