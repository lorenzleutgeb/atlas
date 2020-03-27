package xyz.leutgeb.lorenz.lac.typing.simple.types;

import java.util.Collection;
import java.util.Collections;
import xyz.leutgeb.lorenz.lac.unification.Equivalence;
import xyz.leutgeb.lorenz.lac.unification.TypeMismatch;

public class BoolType extends Type {
  public static final BoolType INSTANCE = new BoolType();

  @Override
  public Collection<Equivalence> decompose(Type b) throws TypeMismatch {
    if (!(b instanceof BoolType)) {
      throw new TypeMismatch(this, b);
    }
    return Collections.emptyList();
  }

  @Override
  public String toHaskell() {
    return "Bool";
  }

  @Override
  public String toString() {
    return "Bool";
  }
}
