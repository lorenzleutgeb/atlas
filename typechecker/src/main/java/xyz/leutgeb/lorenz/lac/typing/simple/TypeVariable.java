package xyz.leutgeb.lorenz.lac.typing.simple;

import java.util.Collection;
import xyz.leutgeb.lorenz.lac.typing.simple.types.Type;
import xyz.leutgeb.lorenz.lac.unification.Equivalence;
import xyz.leutgeb.lorenz.lac.unification.Generalizer;
import xyz.leutgeb.lorenz.lac.unification.Substitution;
import xyz.leutgeb.lorenz.lac.unification.TypeMismatch;
import xyz.leutgeb.lorenz.lac.unification.UnificationProblem;
import xyz.leutgeb.lorenz.lac.unification.UnificationVariable;

public class TypeVariable extends Type {
  public static final TypeVariable ALPHA = new TypeVariable("α");
  public static final TypeVariable BETA = new TypeVariable("β");
  public static final TypeVariable GAMMA = new TypeVariable("γ");
  public static final TypeVariable DELTA = new TypeVariable("δ");
  public static final TypeVariable EPSILON = new TypeVariable("ε");
  private static final String[] GREEK = new String[] {"α", "β", "γ", "δ", "ε"};
  private final String name;

  public TypeVariable(String name) {
    this.name = name;
  }

  public TypeVariable(int index) {
    if (index < GREEK.length) {
      name = GREEK[index];
    } else {
      name = "ty" + index;
    }
  }

  protected String getName() {
    return name;
  }

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
    return name;
  }

  @Override
  public Collection<Equivalence> decompose(Type b) throws TypeMismatch {
    throw new UnsupportedOperationException("cannot decompose type variable");
  }

  @Override
  public Type substitute(TypeVariable v, Type t) {
    return v.equals(this) ? t : this;
  }

  @Override
  public String toString() {
    return name;
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

    return name.equals(that.name);
  }

  @Override
  public int hashCode() {
    return name.hashCode();
  }
}
