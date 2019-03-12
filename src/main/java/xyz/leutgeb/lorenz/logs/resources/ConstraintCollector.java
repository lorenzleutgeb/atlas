package xyz.leutgeb.lorenz.logs.resources;

import java.util.LinkedList;

public class ConstraintCollector {
  private int freshness = 0;

  private final LinkedList<Constraint> constraints = new LinkedList<>();

  public void add(Constraint constraint) {
    constraints.add(constraint);
  }

  public UnknownCoefficient fresh() {
    return new UnknownCoefficient(freshness++);
  }

  public void solve() {
    // TODO(lorenzleutgeb): Pass all constraints to some constraint solver, then read back results.
    throw new UnsupportedOperationException();
  }
}
