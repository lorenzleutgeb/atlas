package xyz.leutgeb.lorenz.lac.typing.simple.types;

import java.util.Collection;
import java.util.Collections;
import xyz.leutgeb.lorenz.lac.unification.Equivalence;
import xyz.leutgeb.lorenz.lac.unification.TypeMismatch;

@Deprecated
public class BaseType extends Type {
  public static final BaseType INSTANCE = new BaseType();

  @Override
  public Collection<Equivalence> decompose(Type b) throws TypeMismatch {
    if (!(b instanceof BaseType)) {
      throw new TypeMismatch(this, b);
    }
    return Collections.emptyList();
  }

  @Override
  public String toHaskell() {
    return "Integer";
  }
}
