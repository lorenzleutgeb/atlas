package xyz.leutgeb.lorenz.atlas.typing.simple.types;

import java.util.Collection;
import xyz.leutgeb.lorenz.atlas.unification.Equivalence;
import xyz.leutgeb.lorenz.atlas.unification.TypeMismatch;

@Deprecated
public class NumType /* implements Type */ {
  public static final NumType INSTANCE = new NumType();

  // @Override
  public Collection<Equivalence> decompose(Type b) throws TypeMismatch {
    throw new UnsupportedOperationException("not implemented");
  }

  // @Override
  public String toHaskell() {
    return "Num";
  }
}
