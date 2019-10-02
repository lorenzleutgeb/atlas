package xyz.leutgeb.lorenz.logs.typing.types;

import java.util.Collection;
import java.util.Map;
import xyz.leutgeb.lorenz.logs.typing.TypeVariable;
import xyz.leutgeb.lorenz.logs.unification.Equivalence;
import xyz.leutgeb.lorenz.logs.unification.Generalizer;
import xyz.leutgeb.lorenz.logs.unification.TypeMismatch;
import xyz.leutgeb.lorenz.logs.unification.UnificationProblem;
import xyz.leutgeb.lorenz.logs.unification.UnificationVariable;

public abstract class Type {
  public Collection<Equivalence> decompose(Type b) throws TypeMismatch {
    throw new UnsupportedOperationException();
  }

  public boolean occurs(UnificationVariable b) {
    return false;
  }

  public Type substitute(TypeVariable v, Type t) {
    return this;
  }

  public Type generalize(Generalizer g) {
    return this;
  }

  public Type wiggle(Map<TypeVariable, UnificationVariable> wiggled, UnificationProblem context) {
    return this;
  }
}
