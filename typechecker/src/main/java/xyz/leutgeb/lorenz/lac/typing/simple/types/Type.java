package xyz.leutgeb.lorenz.lac.typing.simple.types;

import java.util.Collection;
import xyz.leutgeb.lorenz.lac.typing.simple.TypeVariable;
import xyz.leutgeb.lorenz.lac.unification.Equivalence;
import xyz.leutgeb.lorenz.lac.unification.Generalizer;
import xyz.leutgeb.lorenz.lac.unification.Substitution;
import xyz.leutgeb.lorenz.lac.unification.TypeMismatch;
import xyz.leutgeb.lorenz.lac.unification.UnificationContext;

public interface Type {
  Collection<Equivalence> decompose(Type b) throws TypeMismatch;

  boolean occurs(TypeVariable b);

  Type substitute(TypeVariable v, Type t);

  Type generalize(Generalizer g);

  Type wiggle(Substitution wiggled, UnificationContext context);

  default Type wiggle(UnificationContext context) {
    return wiggle(new Substitution(), context);
  }

  String toHaskell();
}
