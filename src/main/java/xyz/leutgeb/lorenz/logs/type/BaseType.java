package xyz.leutgeb.lorenz.logs.type;

import java.util.Collection;
import java.util.Collections;
import xyz.leutgeb.lorenz.logs.unification.Equivalence;
import xyz.leutgeb.lorenz.logs.unification.TypeMismatch;

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
}
