package xyz.leutgeb.lorenz.logs.unification;

import java.util.Collection;
import java.util.Collections;
import java.util.Map;
import lombok.Data;
import lombok.EqualsAndHashCode;
import xyz.leutgeb.lorenz.logs.Util;
import xyz.leutgeb.lorenz.logs.typing.TypeVariable;
import xyz.leutgeb.lorenz.logs.typing.types.Type;

@Data
@EqualsAndHashCode(callSuper = true)
public class UnificationVariable extends TypeVariable {
  public UnificationVariable(int id) {
    super("?" + Util.generateSubscript(id));
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
  public UnificationVariable wiggle(
      Map<TypeVariable, UnificationVariable> wiggled, UnificationProblem context) {
    return this;
  }

  @Override
  public String toString() {
    return super.toString();
  }
}
