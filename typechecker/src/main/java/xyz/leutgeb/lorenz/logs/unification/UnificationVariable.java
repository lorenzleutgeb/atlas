package xyz.leutgeb.lorenz.logs.unification;

import java.util.Collection;
import java.util.Collections;
import lombok.Data;
import xyz.leutgeb.lorenz.logs.Util;
import xyz.leutgeb.lorenz.logs.typing.TypeVariable;
import xyz.leutgeb.lorenz.logs.typing.types.Type;

@Data
public class UnificationVariable extends TypeVariable {
  public UnificationVariable(int id) {
    super(id);
  }

  public TypeVariable generalize(Generalizer g) {
    return g.generalize(this);
  }

  @Override
  public Collection<Equivalence> decompose(Type b) throws TypeMismatch {
    return Collections.emptyList();
  }

  public Type substitute(TypeVariable v, Type t) {
    return v.equals(this) ? t : this;
  }

  @Override
  public UnificationVariable wiggle(Substitution wiggled, UnificationProblem context) {
    return this;
  }

  @Override
  public String toString() {
    return "?" + Util.generateSubscript(getIndex());
  }

  @Override
  public boolean equals(Object other) {
    if (this == other) {
      return true;
    }

    if (!(other instanceof UnificationVariable)) {
      return false;
    }

    return ((UnificationVariable) other).getIndex() == getIndex();
  }
}
