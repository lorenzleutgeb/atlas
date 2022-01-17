package xyz.leutgeb.lorenz.atlas.ast.sources;

import xyz.leutgeb.lorenz.atlas.ast.expressions.Expression;

/** Allows to track where an {@link Expression} came from. */
public abstract class Source {
  public Source getRoot() {
    return this;
  }
}
