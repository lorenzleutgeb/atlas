package xyz.leutgeb.lorenz.lac.ast;

import xyz.leutgeb.lorenz.lac.ast.sources.Source;

abstract class TupleElement extends Expression {
  public TupleElement(Source source) {
    super(source);
  }
}
