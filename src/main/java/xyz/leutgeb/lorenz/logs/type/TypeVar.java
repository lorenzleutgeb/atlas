package xyz.leutgeb.lorenz.logs.type;

import java.util.Collection;
import xyz.leutgeb.lorenz.logs.unification.Equivalence;
import xyz.leutgeb.lorenz.logs.unification.TypeMismatch;
import xyz.leutgeb.lorenz.logs.unification.UnificationVariable;

public class TypeVar extends Type {
  private String name;

  public TypeVar(String name) {
    this.name = name;
  }

  public int hashCode() {
    return this.name.hashCode();
  }

  public boolean equals(Object o) {
    if (o instanceof xyz.leutgeb.lorenz.logs.type.TypeVar) {
      xyz.leutgeb.lorenz.logs.type.TypeVar t = (xyz.leutgeb.lorenz.logs.type.TypeVar) o;
      return this.name.equals(t.name);
    } else {
      return false;
    }
  }

  public Type generalize(Generalizer g) {
    return this;
  }

  public String toString() {
    return this.name;
  }

  public Collection<Equivalence> decompose(Type b) throws TypeMismatch {
    throw new UnsupportedOperationException();
  }

  public Type substitute(UnificationVariable v, Type t) {
    return this;
  }

  public boolean occurs(UnificationVariable v) {
    return false;
  }
}
