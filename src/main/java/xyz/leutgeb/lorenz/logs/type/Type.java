package xyz.leutgeb.lorenz.logs.type;

import java.util.Collection;
import xyz.leutgeb.lorenz.logs.unification.Equivalence;
import xyz.leutgeb.lorenz.logs.unification.TypeMismatch;
import xyz.leutgeb.lorenz.logs.unification.UnificationVariable;

public abstract class Type {
  public Collection<Equivalence> decompose(Type b) throws TypeMismatch {
    throw new UnsupportedOperationException();
  }

  public boolean occurs(UnificationVariable b) {
    return false;
  }

  public Type substitute(UnificationVariable v, Type t) {
    return this;
  }

  public Type generalize(Generalizer g) {
    return this;
  }
}
