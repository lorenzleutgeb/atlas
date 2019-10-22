package xyz.leutgeb.lorenz.logs.typing.types;

import java.util.Collection;
import xyz.leutgeb.lorenz.logs.Context;
import xyz.leutgeb.lorenz.logs.typing.TypeVariable;
import xyz.leutgeb.lorenz.logs.unification.Equivalence;
import xyz.leutgeb.lorenz.logs.unification.Generalizer;
import xyz.leutgeb.lorenz.logs.unification.Substitution;
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

  public Type wiggle(Substitution wiggled, UnificationProblem context) {
    return this;
  }

  public Type wiggle(UnificationProblem problem) {
    return wiggle(new Substitution(), problem);
  }

  public Type wiggle(Context context) {
    return this;
    // return wiggle(context.getProblem());
  }

  public abstract String toHaskell();
}
