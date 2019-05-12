package xyz.leutgeb.lorenz.logs.resources;

import java.util.LinkedList;
import java.util.List;
import java.util.Map;
import java.util.stream.Collectors;
import lombok.extern.log4j.Log4j2;
import xyz.leutgeb.lorenz.logs.resources.coefficients.Coefficient;
import xyz.leutgeb.lorenz.logs.resources.coefficients.UnknownCoefficient;

@Log4j2
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

  public void eq(Annotation... annotations) {
    for (int i = 0; i < annotations.length - 1; i++) {
      for (int j = i + 1; j < annotations.length; j++) {
        if (annotations[i].size() != annotations[j].size()) {
          throw new IllegalArgumentException("annotations of different sizes cannot be equal");
        }
        final int size = annotations[i].size();
        for (int x = 0; x < size; x++) {
          eq(
              annotations[i].getRankCoefficients().get(x),
              annotations[j].getRankCoefficients().get(x));
        }

        if (annotations[i].getCoefficients().size() != annotations[j].getCoefficients().size()) {
          throw new UnsupportedOperationException(
              "annotations have different number of coefficients");
        }

        for (Map.Entry<List<Integer>, Coefficient> entry :
            annotations[i].getCoefficients().entrySet()) {
          var other = annotations[j].getCoefficients().get(entry.getKey());

          if (other == null) {
            throw new UnsupportedOperationException("some coefficient is missing");
          }

          eq(entry.getValue(), other);
        }
      }
    }
  }

  /** Adds constraints such that left equals right + 1. */
  public void increment(Annotation left, Annotation right) {
    if (left.size() != right.size()) {
      throw new IllegalArgumentException("annotations of different sizes cannot be equal");
    }
    final int size = left.size();
    for (int x = 0; x < size; x++) {
      eq(left.getRankCoefficients().get(x), right.getRankCoefficients().get(x));
    }

    if (left.getCoefficients().size() != right.getCoefficients().size()) {
      throw new UnsupportedOperationException("annotations have different number of coefficients");
    }

    for (Map.Entry<List<Integer>, Coefficient> entry : left.getCoefficients().entrySet()) {
      var other = right.getCoefficients().get(entry.getKey());

      if (other == null) {
        throw new UnsupportedOperationException("some coefficient is missing");
      }

      var vec = entry.getKey();

      // NOTE: Special case for increments!
      if (vec.subList(0, vec.size() - 2).stream().allMatch(x -> x == 0)
          && entry.getKey().get(vec.size() - 1) == 2) {
        add(OffsetConstraint.increment(entry.getValue(), other));
      } else {
        eq(entry.getValue(), other);
      }
    }
  }

  public UnknownCoefficient unknown() {
    return new UnknownCoefficient(freshness++);
  }

  public Annotation heuristic(int size) {
    return annotationHeuristic.generate(size, this);
  }

  public void solve() {
    // TODO(lorenzleutgeb): Pass all constraints to some constraint solver, then read back results.
    throw new UnsupportedOperationException();
  }

  @Override
  public String toString() {
    return "{ "
        + constraints.stream().map(Object::toString).collect(Collectors.joining(", "))
        + " }";
  }
}
