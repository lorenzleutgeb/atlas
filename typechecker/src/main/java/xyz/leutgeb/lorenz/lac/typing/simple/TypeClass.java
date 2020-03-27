package xyz.leutgeb.lorenz.lac.typing.simple;

import static java.util.Collections.emptySet;
import static java.util.Collections.singleton;

import java.util.Set;
import lombok.Value;

@Value
public class TypeClass {
  public static TypeClass EQ = new TypeClass("Eq", 1, emptySet());
  public static TypeClass ORD = new TypeClass("Ord", 1, singleton(TypeClass.EQ));

  String name;
  int arity;

  /**
   * This encoding of preconditions is very primitive. We do not account for differences in arity of
   * preconditions and this, and also reodering of arguments is not possible.
   */
  Set<TypeClass> preconditions;

  private TypeClass(String name, int arity, Set<TypeClass> preconditions) {
    this.name = name;
    this.arity = arity;
    if (preconditions.stream().anyMatch(precondition -> precondition.arity != this.arity)) {
      throw new IllegalArgumentException("arity mismatch");
    }
    this.preconditions = preconditions;
  }
}
