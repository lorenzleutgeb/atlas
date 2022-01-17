package xyz.leutgeb.lorenz.atlas.unification;

import xyz.leutgeb.lorenz.atlas.ast.sources.Source;
import xyz.leutgeb.lorenz.atlas.typing.simple.TypeError;

public abstract class UnificationError extends TypeError {
  public UnificationError(String message, Source source) {
    super(message, source);
  }
}
