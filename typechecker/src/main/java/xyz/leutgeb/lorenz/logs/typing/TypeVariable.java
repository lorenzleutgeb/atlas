package xyz.leutgeb.lorenz.logs.typing;

import lombok.Data;
import lombok.EqualsAndHashCode;
import xyz.leutgeb.lorenz.logs.typing.types.Type;
import xyz.leutgeb.lorenz.logs.unification.Generalizer;
import xyz.leutgeb.lorenz.logs.unification.Substitution;
import xyz.leutgeb.lorenz.logs.unification.UnificationProblem;
import xyz.leutgeb.lorenz.logs.unification.UnificationVariable;

@Data
@EqualsAndHashCode(callSuper = false)
public class TypeVariable extends Type {
  private static final String[] GREEK = new String[] {"α", "β", "γ", "δ", "ε"};
  private static final String[] HASKELL = new String[] {"a", "b", "c", "d", "e"};

  public static final TypeVariable ALPHA = new TypeVariable(0);
  public static final TypeVariable BETA = new TypeVariable(1);
  public static final TypeVariable GAMMA = new TypeVariable(2);
  public static final TypeVariable DELTA = new TypeVariable(3);
  public static final TypeVariable EPSILON = new TypeVariable(4);

  private final int index;

  @Override
  public UnificationVariable wiggle(Substitution wiggled, UnificationProblem context) {
    if (wiggled.isInDomain(this)) {
      return (UnificationVariable) wiggled.apply(this);
    }

    var result = context.fresh();
    wiggled.substitute(this, result);
    return result;
  }

  @Override
  public String toHaskell() {
    if (index < HASKELL.length) {
      return HASKELL[index];
    }
    return "ty" + index;
  }

  @Override
  public Type substitute(TypeVariable v, Type t) {
    return v.equals(this) ? t : this;
  }

  @Override
  public String toString() {
    if (index < GREEK.length) {
      return GREEK[index];
    }
    return "ty" + index;
  }

  @Override
  public TypeVariable generalize(Generalizer generalizer) {
    return this;
  }

  @Override
  public boolean equals(Object o) {
    if (this == o) {
      return true;
    }
    if (o == null || getClass() != o.getClass()) {
      return false;
    }

    TypeVariable that = (TypeVariable) o;

    return index == that.index;
  }

  @Override
  public int hashCode() {
    return index;
  }
}
