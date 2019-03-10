package xyz.leutgeb.lorenz.logs.type;

import java.util.Collection;
import java.util.List;
import lombok.Data;
import lombok.RequiredArgsConstructor;
import xyz.leutgeb.lorenz.logs.unification.Equivalence;
import xyz.leutgeb.lorenz.logs.unification.TypeMismatch;
import xyz.leutgeb.lorenz.logs.unification.UnificationVariable;

@Data
@RequiredArgsConstructor
public class FunctionType extends Type {
  private final Type from;
  private final Type to;

  public FunctionType(List<Type> from, Type to) {
    this.from = new ProductType(from);
    this.to = to;
  }

  public Type generalize(Generalizer g) {
    return new FunctionType(from.generalize(g), to.generalize(g));
  }

  public Collection<Equivalence> decompose(Type b) throws TypeMismatch {
    if (!(b instanceof FunctionType)) {
      throw new TypeMismatch(this, b);
    }
    var ft = (FunctionType) b;
    return List.of(new Equivalence(from, ft.from), new Equivalence(to, ft.to));
  }

  public Type substitute(UnificationVariable v, Type t) {
    return new FunctionType(this.from.substitute(v, t), this.to.substitute(v, t));
  }

  public boolean occurs(UnificationVariable v) {
    return this.from.occurs(v) || this.to.occurs(v);
  }

  @Override
  public String toString() {
    return "(" + from + " → " + to + ")";
  }
}
