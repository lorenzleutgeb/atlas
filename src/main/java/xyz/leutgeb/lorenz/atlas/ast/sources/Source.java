package xyz.leutgeb.lorenz.atlas.ast.sources;

/** Allows to track where an {@link xyz.leutgeb.lorenz.atlas.ast.Expression} came from. */
public abstract class Source {
  public Source getRoot() {
    return this;
  }
}
