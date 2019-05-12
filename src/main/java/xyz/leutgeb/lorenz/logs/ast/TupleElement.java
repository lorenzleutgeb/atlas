package xyz.leutgeb.lorenz.logs.ast;

import xyz.leutgeb.lorenz.logs.ast.sources.Source;

public abstract class TupleElement extends Expression {
  public TupleElement(Source source) {
    super(source);
  }
}
