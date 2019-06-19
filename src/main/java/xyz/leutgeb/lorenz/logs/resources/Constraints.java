package xyz.leutgeb.lorenz.logs.resources;

import static com.microsoft.z3.Status.SATISFIABLE;
import static com.microsoft.z3.Status.UNKNOWN;
import static xyz.leutgeb.lorenz.logs.Util.ensureLibrary;

import com.microsoft.z3.Context;
import com.microsoft.z3.Model;
import com.microsoft.z3.RatNum;
import com.microsoft.z3.RealExpr;
import com.microsoft.z3.Solver;
import java.util.Arrays;
import java.util.Collection;
import java.util.LinkedList;
import java.util.List;
import java.util.Map;
import java.util.Objects;
import java.util.stream.Collectors;
import lombok.extern.log4j.Log4j2;
import xyz.leutgeb.lorenz.logs.Util;
import xyz.leutgeb.lorenz.logs.resources.coefficients.Coefficient;
import xyz.leutgeb.lorenz.logs.resources.coefficients.KnownCoefficient;
import xyz.leutgeb.lorenz.logs.resources.coefficients.UnknownCoefficient;
import xyz.leutgeb.lorenz.logs.resources.constraints.Constraint;
import xyz.leutgeb.lorenz.logs.resources.constraints.EqualityConstraint;
import xyz.leutgeb.lorenz.logs.resources.constraints.EqualsSumConstraint;
import xyz.leutgeb.lorenz.logs.resources.constraints.LessThanOrEqualConstraint;
import xyz.leutgeb.lorenz.logs.resources.constraints.OffsetConstraint;

@Log4j2
public class Constraints {
  private static final Map<String, String> Z3_CONFIG = Map.of("model", "true");

  static {
    ensureLibrary("z3java");
  }

  private final LinkedList<Constraint> constraints = new LinkedList<>();
  private AnnotationHeuristic annotationHeuristic = RangeHeuristic.DEFAULT;
  private int freshness = 0;
  private KnownCoefficient[] solution;

  public void add(Constraint constraint) {
    constraints.add(constraint);
  }

  public void le(Coefficient left, Coefficient right) {
    add(new LessThanOrEqualConstraint(left, right));
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

  public KnownCoefficient substitute(Coefficient x) {
    if (x instanceof KnownCoefficient) {
      log.warn("You are trying to substitute a coefficient that is already known.");
      return (KnownCoefficient) x;
    } else if (x instanceof UnknownCoefficient) {
      return solution[((UnknownCoefficient) x).getId()];
    }
    throw new UnsupportedOperationException();
  }

  public void solve() {
    if (solution != null) {
      return;
    }
    final var ctx = new Context(Z3_CONFIG);
    final var solver = ctx.mkSolver();

    // Encode all coefficients as constants.
    RealExpr[] coefficients = new RealExpr[freshness];
    // Also encode that they are rational numbers.
    // IntExpr[] numerators = new IntExpr[freshness];
    // IntExpr[] denominators = new IntExpr[freshness];
    for (int i = 0; i < freshness; i++) {
      coefficients[i] = ctx.mkRealConst("q" + i);
      // numerators[i] = ctx.mkIntConst("q" + i + "n");
      // denominators[i] = ctx.mkIntConst("q" + i + "d");
      // s.add(ctx.mkEq(coefficients[i], ctx.mkDiv(numerators[i], denominators[i])));
      // s.add(ctx.mkNot(ctx.mkEq(denominators[i], ctx.mkInt(0))));
      solver.add(ctx.mkGe(coefficients[i], ctx.mkReal(0)));
    }

    // Then encode all their dependencies.
    for (Constraint c : constraints) {
      solver.add(c.encode(ctx, coefficients));
    }

    log.debug(solver.toString());

    final var model = check(solver);
    solution = new KnownCoefficient[coefficients.length];
    for (int i = 0; i < coefficients.length; i++) {
      var x = model.getConstInterp(coefficients[i]);
      if (!x.isRatNum()) {
        throw new RuntimeException("solution for q" + i + " is not a rational number");
      }
      final var xr = (RatNum) x;
      var num = xr.getNumerator();
      if (num.getBigInteger().intValueExact() == 0) {
        solution[i] = KnownCoefficient.ZERO;
      } else {
        log.debug("q" + i + " = " + xr.getNumerator() + "/" + xr.getDenominator());
        solution[i] = new KnownCoefficient(Util.toFraction(xr));
      }
    }
  }

  private Model check(Solver solver) {
    var status = solver.check();
    if (SATISFIABLE.equals(status)) {
      return solver.getModel();
    } else if (UNKNOWN.equals(status)) {
      log.error("Attempt to solve constraint system yielded unknown result.");
      throw new RuntimeException("satisfiability of constraints unknown");
    }
    log.error("Constraint system is unsatisfiable!");
    log.error(
        "Unsatisfiable core:\n\t"
            + Arrays.stream(solver.getUnsatCore())
                .map(Objects::toString)
                .collect(Collectors.joining("\n\t")));
    throw new RuntimeException("constraint system is unsatisfiable");
  }

  @Override
  public String toString() {
    return "{ "
        + constraints.stream().map(Object::toString).collect(Collectors.joining(", "))
        + " }";
  }

  public void eqSum(Coefficient left, Collection<Coefficient> sum) {
    add(new EqualsSumConstraint(left, sum));
  }
}
