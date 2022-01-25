package xyz.leutgeb.lorenz.atlas.typing.simple.types;

import jakarta.json.Json;
import jakarta.json.JsonValue;
import java.util.Collection;
import java.util.Collections;
import java.util.Optional;
import java.util.Set;
import lombok.EqualsAndHashCode;
import lombok.Value;
import xyz.leutgeb.lorenz.atlas.ast.sources.Source;
import xyz.leutgeb.lorenz.atlas.typing.simple.TypeVariable;
import xyz.leutgeb.lorenz.atlas.unification.Equivalence;
import xyz.leutgeb.lorenz.atlas.unification.Generalizer;
import xyz.leutgeb.lorenz.atlas.unification.Substitution;
import xyz.leutgeb.lorenz.atlas.unification.TypeMismatch;
import xyz.leutgeb.lorenz.atlas.unification.UnificationContext;

@Value
@EqualsAndHashCode(callSuper = false)
public class TreeType implements Type {
  TypeVariable elementType;

  public TreeType(TypeVariable elementType) {
    this.elementType = elementType;
  }

  @Override
  public String toString() {
    return "Tree " + elementType;
  }

  @Override
  public Collection<Equivalence> decompose(Type b, Source source) throws TypeMismatch {
    if (!(b instanceof final TreeType tree)) {
      throw new TypeMismatch(this, b, source);
    }

    if (!elementType.equals(tree.elementType)) {
      return Collections.singletonList(new Equivalence(elementType, tree.elementType, source));
    }
    return Collections.emptyList();
  }

  @Override
  public Type substitute(TypeVariable v, Type t) {
    var substitute = elementType.substitute(v, t);
    if (substitute instanceof TypeVariable) {
      return new TreeType((TypeVariable) substitute);
    }
    throw new RuntimeException("this type of tree cannot be constructed");
  }

  @Override
  public Type wiggle(Substitution wiggled, UnificationContext context) {
    return new TreeType(elementType.wiggle(wiggled, context));
  }

  @Override
  public Set<TypeVariable> variables() {
    return elementType.variables();
  }

  @Override
  public String toHaskell() {
    return "Tree " + elementType.toHaskell();
  }

  @Override
  public String toJava() {
    return "Tree<" + elementType.toJava() + ">";
  }

  @Override
  public JsonValue toJson() {
    final var builder = Json.createObjectBuilder();
    builder.add("name", "Tree");

    final var argumentsBuilder = Json.createArrayBuilder();
    argumentsBuilder.add(elementType.toJson());
    builder.add("arguments", argumentsBuilder.build());

    return builder.build();
  }

  @Override
  public Optional<Integer> countTrees() {
    return Optional.of(1);
  }

  @Override
  public Type generalize(Generalizer g) {
    return new TreeType(elementType.generalize(g));
  }

  @Override
  public boolean occurs(TypeVariable b) {
    return elementType.equals(b);
  }
}
