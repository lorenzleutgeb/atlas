package xyz.leutgeb.lorenz.atlas.typing.simple.types;

import java.util.*;

import jakarta.json.JsonValue;
import xyz.leutgeb.lorenz.atlas.ast.sources.Source;
import xyz.leutgeb.lorenz.atlas.typing.simple.TypeVariable;
import xyz.leutgeb.lorenz.atlas.unification.*;

import static jakarta.json.Json.*;

public class Int64Type implements Type {
  public static final Int64Type INSTANCE = new Int64Type();

  // @Override
  public Collection<Equivalence> decompose(Type b) throws TypeMismatch {
    throw new UnsupportedOperationException("not implemented");
  }

  @Override
  public Collection<Equivalence> decompose(Type b, Source source) throws TypeMismatch {
    if (!(b instanceof Int64Type)) {
      throw new TypeMismatch(this, b, source);
    }
    return Collections.emptyList();
  }

  @Override
  public boolean occurs(TypeVariable b) {
    return false;
  }

  @Override
  public Type substitute(TypeVariable v, Type t) {
    return this;
  }

  @Override
  public Type generalize(Generalizer g) {
    return this;
  }

  @Override
  public Type wiggle(Substitution wiggled, UnificationContext context) {
    return this;
  }

  @Override
  public Set<TypeVariable> variables() {
    return Collections.emptySet();
  }

  // @Override
  public String toHaskell() {
    return "Int64";
  }

  @Override
  public String toJava() {
    return "Long";
  }

  @Override
  public JsonValue toJson() {
    return createObjectBuilder(Map.of("name", "Int64")).build();
  }
  @Override
  public Optional<Integer> countTrees() {
    return Optional.of(0);
  }

  @Override
  public String toString() {
    return "Int64";
  }
}
