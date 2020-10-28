package xyz.leutgeb.lorenz.lac.ast.sources;

/** Allows to track where an {@link xyz.leutgeb.lorenz.lac.ast.Expression} came from. */
public abstract class Source {
  public Source getRoot() {
    return this;
  }
}
