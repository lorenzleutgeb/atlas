package xyz.leutgeb.lorenz.logs.typing;

import static java.util.Collections.singleton;
import static java.util.Collections.singletonList;

import java.util.Collections;
import java.util.List;
import java.util.Set;
import lombok.Value;
import xyz.leutgeb.lorenz.logs.unification.Substitution;

@Value
public class TypeClass {
  public static TypeClass EQ =
      new TypeClass("Eq", singletonList(TypeVariable.GAMMA), Collections.emptySet());

  public static TypeClass ORD =
      new TypeClass(
          "Ord",
          singletonList(TypeVariable.GAMMA),
          singleton(new TypeConstraint(TypeClass.EQ, Substitution.identity())));

  String name;
  List<TypeVariable> variables;
  Set<TypeConstraint> preconditions;
}
