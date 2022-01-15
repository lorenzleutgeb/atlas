package xyz.leutgeb.lorenz.atlas.typing.simple.types;

import jakarta.json.JsonValue;
import java.util.Collection;
import java.util.Optional;
import java.util.Set;
import xyz.leutgeb.lorenz.atlas.typing.simple.TypeVariable;
import xyz.leutgeb.lorenz.atlas.unification.*;

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

  String toJava();

  JsonValue toJson();

  default Optional<Integer> countTrees() {
    return Optional.empty();
  }
}
