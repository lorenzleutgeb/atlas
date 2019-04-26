package xyz.leutgeb.lorenz.logs.unification;

import java.util.Collection;
import java.util.Collections;
import lombok.Data;
import lombok.EqualsAndHashCode;
import xyz.leutgeb.lorenz.logs.type.Generalizer;
import xyz.leutgeb.lorenz.logs.type.Type;

@Data
@EqualsAndHashCode(callSuper = true)
public class UnificationVariable extends Type {
  private final int id;

  public String toString() {
    return "u" + this.id;
  }

  public Type generalize(Generalizer g) {
    return g.generalize(this);
  }

  @Override
  public Collection<Equivalence> decompose(Type b) throws TypeMismatch {
    return Collections.emptyList();
  }

  public Type substitute(UnificationVariable v, Type t) {
    return v.equals(this) ? t : this;
  }
}
