package xyz.leutgeb.lorenz.logs.resources;

import java.util.LinkedList;
import java.util.stream.Collectors;

import xyz.leutgeb.lorenz.logs.resources.coefficients.Coefficient;
import xyz.leutgeb.lorenz.logs.resources.coefficients.UnknownCoefficient;

public class Constraints {
  private AnnotationHeuristic annotationHeuristic = RangeHeuristic.DEFAULT;
  private int freshness = 0;

  private final LinkedList<Constraint> constraints = new LinkedList<>();

  public void add(Constraint constraint) {
    constraints.add(constraint);
  }

  public void eq(Coefficient... coefficients) {
    for (int i = 0; i < coefficients.length - 1; i++) {
      for (int j = i + 1; j < coefficients.length; j++) {
        add(new EqualityConstraint(coefficients[i], coefficients[j]));
      }
    }
  }

  public UnknownCoefficient unknown() {
    return new UnknownCoefficient(freshness++);
  }

  public TypingContextAnnotation heuristic(int size) {
    return annotationHeuristic.generate(size, this);
  }

  public void solve() {
    // TODO(lorenzleutgeb): Pass all constraints to some constraint solver, then read back results.
    throw new UnsupportedOperationException();
  }

  @Override
  public String toString() {
    return "{ " + constraints.stream().map(Object::toString).collect(Collectors.joining(", ")) + " }";
  }
}
