package xyz.leutgeb.lorenz.logs.unification;

import java.util.Collection;
import lombok.Data;
import xyz.leutgeb.lorenz.logs.type.Type;

@Data
public class Equivalence {
  private final Type left;
  private final Type right;

  public Equivalence(UnificationVariable left, Type right) {
    this.left = left;
    this.right = right;
  }

  public Equivalence(Type left, Type right) {
    if (!(left instanceof UnificationVariable) && (right instanceof UnificationVariable)) {
      this.left = right;
      this.right = left;
    } else {
      this.left = left;
      this.right = right;
    }
  }

  public Equivalence substitute(UnificationVariable variable, Type result) {
    return new Equivalence(left.substitute(variable, result), right.substitute(variable, result));
  }

  public void occurs() throws OccursError {
    if (left instanceof UnificationVariable && right.occurs((UnificationVariable) left)) {
      throw new OccursError(left, right);
    }
  }

  public Collection<Equivalence> unify() throws UnificationError {
    return left.decompose(right);
  }

  @Override
  public String toString() {
    return left + " ≈ " + right;
  }
}
