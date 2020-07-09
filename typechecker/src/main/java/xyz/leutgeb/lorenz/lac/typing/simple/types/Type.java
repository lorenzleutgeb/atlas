package xyz.leutgeb.lorenz.lac.typing.simple.types;

import java.util.Collection;
import java.util.Set;
import xyz.leutgeb.lorenz.lac.typing.simple.TypeVariable;
import xyz.leutgeb.lorenz.lac.unification.*;

public interface Type {
  Collection<Equivalence> decompose(Type b) throws TypeMismatch;

  boolean occurs(TypeVariable b);

  Type substitute(TypeVariable v, Type t);

  Type generalize(Generalizer g);

  Type wiggle(Substitution wiggled, UnificationContext context);

  Set<TypeVariable> variables();

  default Type wiggle(UnificationContext context) {
    return wiggle(new Substitution(), context);
  }

  String toHaskell();
}
