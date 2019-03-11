package xyz.leutgeb.lorenz.logs.unification;

import java.util.LinkedList;
import java.util.ListIterator;
import java.util.stream.Collectors;
import xyz.leutgeb.lorenz.logs.type.Type;

public class Problem {
  private final LinkedList<Equivalence> equivalences = new LinkedList<>();
  private final Substitution solution = new Substitution();

  private int freshness = 0;

  public void add(Equivalence equivalence) {
    equivalences.add(equivalence);
  }

  /**
   * @see #add(Equivalence)
   * @see Equivalence
   */
  public void add(Type left, Type right) {
    add(new Equivalence(left, right));
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
      solution.add(left, e.getRight());
    }
    return solution;
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
}
