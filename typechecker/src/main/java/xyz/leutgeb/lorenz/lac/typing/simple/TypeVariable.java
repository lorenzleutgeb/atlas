package xyz.leutgeb.lorenz.lac.typing.simple;

import static java.util.Collections.singleton;

import java.util.Collection;
import java.util.Set;
import xyz.leutgeb.lorenz.lac.typing.simple.types.Type;
import xyz.leutgeb.lorenz.lac.unification.Equivalence;
import xyz.leutgeb.lorenz.lac.unification.Generalizer;
import xyz.leutgeb.lorenz.lac.unification.Substitution;
import xyz.leutgeb.lorenz.lac.unification.TypeMismatch;
import xyz.leutgeb.lorenz.lac.unification.UnificationContext;
import xyz.leutgeb.lorenz.lac.unification.UnificationVariable;

// NOTE: Do not use @Value here, since have other classes inherit from TypeVariable.
public class TypeVariable implements Type {
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
  public UnificationVariable wiggle(Substitution wiggled, UnificationContext problem) {
    if (wiggled.isInDomain(this)) {
      return (UnificationVariable) wiggled.apply(this);
    }

    var result = problem.fresh();
    wiggled.substitute(this, result);
    return result;
  }

  @Override
  public Set<TypeVariable> variables() {
    return singleton(this);
  }

  @Override
  public String toHaskell() {
    return name;
  }

  @Override
  public String toJava() {
    return name;
  }

  @Override
  public Collection<Equivalence> decompose(Type b) throws TypeMismatch {
    throw new TypeMismatch(this, b);
  }

  @Override
  public boolean occurs(TypeVariable b) {
    return equals(b);
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
