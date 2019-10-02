package xyz.leutgeb.lorenz.logs.unification;

import java.util.HashSet;
import java.util.LinkedList;
import java.util.ListIterator;
import java.util.Set;
import java.util.stream.Collectors;
import lombok.Getter;
import xyz.leutgeb.lorenz.logs.ast.Expression;
import xyz.leutgeb.lorenz.logs.typing.TypeConstraint;
import xyz.leutgeb.lorenz.logs.typing.types.Type;

public class UnificationProblem {
  private final LinkedList<Equivalence> equivalences = new LinkedList<>();

  @Getter private Set<TypeConstraint> constraints = new HashSet<>();
  private Substitution solution = new Substitution();

  private int freshness = 0;

  public void add(Equivalence equivalence) {
    equivalences.add(equivalence);
  }

  /**
   * @see #add(Equivalence)
   * @see Equivalence
   */
  public void add(Expression justification, Type left, Type right) {
    add(new Equivalence(left, right, justification));
  }

  @Override
  public String toString() {
    return "{"
        + this.equivalences.stream().map(Object::toString).collect(Collectors.joining(", "))
        + "}";
  }

  public Substitution solve() throws UnificationError {
    while (!equivalences.isEmpty()) {
      Equivalence e = equivalences.poll();
      var more = e.unify();

      if (!more.isEmpty()) {
        equivalences.addAll(more);
        continue;
      }

      if (!(e.getLeft() instanceof UnificationVariable)) {
        continue;
      }

      var left = (UnificationVariable) e.getLeft();
      e.occurs();
      substitute(left, e.getRight());
      solution.substitute(left, e.getRight());
      // solution = solution.compose(left, e.getRight());
      constraints =
          constraints
              .stream()
              .map(x -> x.apply(solution))
              .collect(Collectors.toCollection(HashSet::new));
    }
    var minimizedConstraints = new HashSet<TypeConstraint>();
    for (var constraint : constraints) {
      minimizedConstraints.removeIf(x -> !x.equals(constraint) && constraint.implies(x));
      if (minimizedConstraints.stream().noneMatch(x -> x.implies(constraint))) {
        minimizedConstraints.add(constraint);
      }
    }
    constraints = minimizedConstraints;
    return solution;
  }

  public Substitution solveAndGeneralize(Type generalizationBase) throws UnificationError {
    final var result = solve();
    var subsGenBase = result.apply(generalizationBase);
    var generalizer = new Generalizer();
    subsGenBase.generalize(generalizer);
    constraints =
        constraints
            .stream()
            .map(x -> x.apply(result))
            .collect(Collectors.toCollection(HashSet::new));
    return result.compose(generalizer.toSubstitution());
  }

  private void substitute(UnificationVariable variable, Type result) {
    ListIterator<Equivalence> iterator = equivalences.listIterator();
    while (iterator.hasNext()) {
      iterator.set(iterator.next().substitute(variable, result));
    }
  }

  public UnificationVariable fresh() {
    return new UnificationVariable(freshness++);
  }

  public void addConstraint(TypeConstraint tc) {
    constraints.add(tc);
  }
}
