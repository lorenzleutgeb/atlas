package xyz.leutgeb.lorenz.lac.typing.simple.types;

import java.util.Collection;
import xyz.leutgeb.lorenz.lac.typing.simple.TypeVariable;
import xyz.leutgeb.lorenz.lac.unification.Equivalence;
import xyz.leutgeb.lorenz.lac.unification.Generalizer;
import xyz.leutgeb.lorenz.lac.unification.Substitution;
import xyz.leutgeb.lorenz.lac.unification.TypeMismatch;
import xyz.leutgeb.lorenz.lac.unification.UnificationContext;
import xyz.leutgeb.lorenz.lac.unification.UnificationProblem;
import xyz.leutgeb.lorenz.lac.unification.UnificationVariable;

public abstract class Type {
  public Collection<Equivalence> decompose(Type b) throws TypeMismatch {
    throw new UnsupportedOperationException("not implemented");
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

  public Type wiggle(UnificationContext context) {
    return this;
    // return wiggle(context.getProblem());
  }

  public abstract String toHaskell();
}
