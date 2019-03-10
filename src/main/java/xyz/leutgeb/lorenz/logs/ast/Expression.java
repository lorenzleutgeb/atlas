package xyz.leutgeb.lorenz.logs.ast;

import xyz.leutgeb.lorenz.logs.Context;
import xyz.leutgeb.lorenz.logs.type.Type;
import xyz.leutgeb.lorenz.logs.type.TypeError;
import xyz.leutgeb.lorenz.logs.unification.UnificationError;

public abstract class Expression {
  public Type infer(Context context) throws UnificationError, TypeError {
    throw new UnsupportedOperationException("not implemented");
  }
}
