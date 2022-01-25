package xyz.leutgeb.lorenz.atlas.unification;

import static java.util.Collections.emptyList;
import static java.util.Objects.requireNonNull;

import java.util.Collection;
import java.util.LinkedList;
import java.util.ListIterator;
import java.util.Optional;
import lombok.Value;
import xyz.leutgeb.lorenz.atlas.ast.sources.Source;
import xyz.leutgeb.lorenz.atlas.typing.simple.types.Type;

@Value
public class Equivalence {
  Type left;
  Type right;

  Source source;

  public Equivalence(Type left, Type right, Source source) {
    requireNonNull(left);
    requireNonNull(right);
    this.source = source;

    if (left.equals(right)) {
      throw new IllegalArgumentException(
          "two types that are equal should not be explicitly said to be equivalent");
    }

    if (!(left instanceof UnificationVariable) && (right instanceof UnificationVariable)) {
      this.left = right;
      this.right = left;
    } else {
      this.left = left;
      this.right = right;
    }
  }

  public static Substitution solve(LinkedList<Equivalence> equivalences) throws UnificationError {
    final var solution = new Substitution();
    while (!equivalences.isEmpty()) {
      Equivalence e = equivalences.poll();
      var more = e.unify();

      if (!more.isEmpty()) {
        equivalences.addAll(more);
        continue;
      }

      if (!(e.getLeft() instanceof UnificationVariable left)) {
        continue;
      }

      e.occurs();

      // Substitute
      ListIterator<Equivalence> iterator = equivalences.listIterator();
      while (iterator.hasNext()) {
        Optional<Equivalence> substitute = iterator.next().substitute(left, e.getRight());
        substitute.ifPresent(iterator::set);
      }

      solution.substitute(left, e.getRight());
    }
    return solution;
  }

  public Optional<Equivalence> substitute(UnificationVariable variable, Type result) {
    var leftSubstitute = left.substitute(variable, result);
    var rightSubstitute = right.substitute(variable, result);
    if (!leftSubstitute.equals(rightSubstitute)) {
      return Optional.of(new Equivalence(leftSubstitute, rightSubstitute, source));
    }
    return Optional.empty();
  }

  public void occurs() throws OccursError {
    if (left instanceof UnificationVariable && right.occurs((UnificationVariable) left)) {
      throw new OccursError(left, right, source);
    }
  }

  public Collection<Equivalence> unify() throws UnificationError {
    if (left == right || left.equals(right)) {
      return emptyList();
    }
    return left.decompose(right, source);
  }

  @Override
  public String toString() {
    return left + " ≈ " + right;
  }
}
