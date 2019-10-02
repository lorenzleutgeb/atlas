package xyz.leutgeb.lorenz.logs.unification;

import static java.util.Collections.emptyList;
import static java.util.Objects.requireNonNull;

import java.util.Collection;
import javax.annotation.Nullable;
import lombok.Value;
import xyz.leutgeb.lorenz.logs.ast.Expression;
import xyz.leutgeb.lorenz.logs.typing.types.Type;

@Value
public class Equivalence {
  Type left;
  Type right;

  /** Optionally, keep the expression that justifies this equivalence. */
  @Nullable Expression justification;

  public Equivalence(Type left, Type right, @Nullable Expression justification) {
    requireNonNull(left);
    requireNonNull(right);

    /* The following is handy for debugging issues in type inference:
    if (Stream.of(TypeVariable.GREEK).anyMatch(x -> x.equals(left) || x.equals(right))) {
      throw new IllegalArgumentException("no type variables when unifying!");
    }

    for (Type t : List.of(left, right)) {
      if (t instanceof TreeType) {
        if (TypeVariable.isGreek(((TreeType) t).getElementType())) {
          throw new IllegalArgumentException("no type variables when unifying!");
        }
      }
    }
    */

    if (!(left instanceof UnificationVariable) && (right instanceof UnificationVariable)) {
      this.left = right;
      this.right = left;
    } else {
      this.left = left;
      this.right = right;
    }
    this.justification = justification;
  }

  /** Constructs an instance without justification. Use with care. */
  public Equivalence(Type left, Type right) {
    this(left, right, null);
  }

  public Equivalence substitute(UnificationVariable variable, Type result) {
    return new Equivalence(
        left.substitute(variable, result), right.substitute(variable, result), justification);
  }

  public void occurs() throws OccursError {
    if (left instanceof UnificationVariable && right.occurs((UnificationVariable) left)) {
      throw new OccursError(left, right);
    }
  }

  public Collection<Equivalence> unify() throws UnificationError {
    if (left == right) {
      return emptyList();
    }
    return left.decompose(right);
  }

  @Override
  public String toString() {
    return left + " ≈ " + right;
  }
}
