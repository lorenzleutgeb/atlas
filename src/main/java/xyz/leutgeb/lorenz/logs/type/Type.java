package xyz.leutgeb.lorenz.logs.type;

import java.util.Collection;
import java.util.Map;
import xyz.leutgeb.lorenz.logs.unification.Equivalence;
import xyz.leutgeb.lorenz.logs.unification.Problem;
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

  public Type wiggle(Map<TypeVar, UnificationVariable> wiggled, Problem context) {
    return this;
  }
}
