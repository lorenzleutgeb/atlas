package xyz.leutgeb.lorenz.logs.resources;

import static com.microsoft.z3.Status.SATISFIABLE;
import static com.microsoft.z3.Status.UNKNOWN;
import static xyz.leutgeb.lorenz.logs.Util.ensureLibrary;

import com.google.common.collect.Sets;
import com.microsoft.z3.Context;
import com.microsoft.z3.Model;
import com.microsoft.z3.RatNum;
import com.microsoft.z3.RealExpr;
import com.microsoft.z3.Solver;
import java.io.File;
import java.io.FileNotFoundException;
import java.io.PrintWriter;
import java.time.Duration;
import java.time.Instant;
import java.util.Arrays;
import java.util.Collection;
import java.util.HashMap;
import java.util.LinkedList;
import java.util.List;
import java.util.Map;
import java.util.Objects;
import java.util.Set;
import java.util.stream.Collectors;
import lombok.extern.log4j.Log4j2;
import org.hipparchus.fraction.Fraction;
import xyz.leutgeb.lorenz.logs.Util;
import xyz.leutgeb.lorenz.logs.ast.Expression;
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
  static {
    ensureLibrary("z3java");
  }

  private final LinkedList<Constraint> constraints = new LinkedList<>();
  private final String name;
  private AnnotationHeuristic annotationHeuristic = RangeHeuristic.DEFAULT;
  private int freshness = 0;
  private KnownCoefficient[] solution;
  private Map<Integer, String> coefficientNames = new HashMap<>();

  public Constraints(String name) {
    this.name = name;
  }

  public Constraints() {
    this.name = "unknown";
  }

  private static Map<String, String> z3Config(boolean unsatCore) {
    final var config = new HashMap<String, String>();
    /*
     Legal parameters are:
    auto_config (bool) (default: true)
    debug_ref_count (bool) (default: false)
    dot_proof_file (string) (default: proof.dot)
    dump_models (bool) (default: false)
    memory_high_watermark (unsigned int) (default: 0)
    memory_max_alloc_count (unsigned int) (default: 0)
    memory_max_size (unsigned int) (default: 0)
    model (bool) (default: true)
    model_compress (bool) (default: true)
    model_validate (bool) (default: false)
    proof (bool) (default: false)
    rlimit (unsigned int) (default: 0)
    smtlib2_compliant (bool) (default: false)
    stats (bool) (default: false)
    timeout (unsigned int) (default: 4294967295)
    trace (bool) (default: false)
    trace_file_name (string) (default: z3.log)
    type_check (bool) (default: true)
    unsat_core (bool) (default: false)
    verbose (unsigned int) (default: 0)
    warning (bool) (default: true)
    well_sorted_check (bool) (default: false)
       */
    config.put("model", "true");
    config.put("timeout", String.valueOf(Duration.ofSeconds(100).toMillis()));
    if (unsatCore) {
      config.put("unsat_core", "true");
      // Global.setParameter("smt.core.minimize", "true");
      // Global.setParameter("produce-unsat-cores", "true");
    }
    return config;
  }

  public void add(Constraint constraint) {
    constraints.add(constraint);
  }

  public void le(Expression source, Coefficient left, Coefficient right) {
    add(new LessThanOrEqualConstraint(constraints.size(), source, left, right));
  }

  public void eq(Expression source, Coefficient... coefficients) {
    for (int i = 0; i < coefficients.length - 1; i++) {
      for (int j = i + 1; j < coefficients.length; j++) {
        add(new EqualityConstraint(constraints.size(), source, coefficients[i], coefficients[j]));
      }
    }
  }

  public void eq(Expression source, Annotation... annotations) {
    for (int i = 0; i < annotations.length - 1; i++) {
      for (int j = i + 1; j < annotations.length; j++) {
        if (annotations[i].size() != annotations[j].size()) {
          throw new IllegalArgumentException("annotations of different sizes cannot be equal");
        }
        final int size = annotations[i].size();
        for (int x = 0; x < size; x++) {
          eq(source, annotations[i].getRankCoefficient(x), annotations[j].getRankCoefficient(x));
        }

        // if (annotations[i].getCoefficients().size() != annotations[j].getCoefficients().size()) {
        //  throw new UnsupportedOperationException(
        //      "annotations have different number of coefficients");
        // }

        for (Map.Entry<List<Integer>, Coefficient> entry : annotations[i].getCoefficients()) {
          var other = annotations[j].getCoefficient(entry.getKey());

          if (other == null) {
            throw new UnsupportedOperationException("some coefficient is missing");
          }

          eq(source, entry.getValue(), other);
        }
      }
    }
  }

  public void eq(Expression source, AnnotatingContext... annotations) {
    for (int i = 0; i < annotations.length - 1; i++) {
      for (int j = i + 1; j < annotations.length; j++) {
        if (annotations[i].size() != annotations[j].size()) {
          throw new IllegalArgumentException("annotations of different sizes cannot be equal");
        }
        if (!Sets.difference(Set.of(annotations[i].getIds()), Set.of(annotations[j].getIds()))
            .isEmpty()) {
          throw new IllegalArgumentException(
              "annotations over different variables cannot be equal");
        }
        final int size = annotations[i].size();
        for (int x = 0; x < size; x++) {
          final String id = annotations[j].getIds().get(x);
          eq(source, annotations[i].getRankCoefficient(id), annotations[j].getRankCoefficient(id));
        }

        // if (annotations[i].getCoefficients().size() != annotations[j].getCoefficients().size()) {
        //  throw new UnsupportedOperationException(
        //      "annotations have different number of coefficients");
        // }

        final var indices = annotations[i].streamIndices().collect(Collectors.toList());
        for (var index : indices) {
          eq(source, annotations[i].getCoefficient(index), annotations[j].getCoefficient(index));
        }
      }
    }
  }

  /** Adds constraints such that left equals right + 1. */
  public void increment(Expression source, Annotation left, Annotation right, int cost) {
    if (left.size() != right.size()) {
      throw new IllegalArgumentException("annotations of different sizes cannot be equal");
    }
    final int size = left.size();
    for (int x = 0; x < size; x++) {
      eq(source, left.getRankCoefficient(x), right.getRankCoefficient(x));
    }

    // if (left.getCoefficients().size() != right.getCoefficients().size()) {
    //  throw new UnsupportedOperationException("annotations have different number of
    // coefficients");
    // }

    for (Map.Entry<List<Integer>, Coefficient> entry : left.getCoefficients()) {
      var other = right.getCoefficient(entry.getKey());

      if (other == null) {
        throw new UnsupportedOperationException("some coefficient is missing");
      }

      var vec = entry.getKey();

      // NOTE: Special case for increments!
      if (vec.subList(0, vec.size() - 2).stream().allMatch(x -> x == 0)
          && entry.getKey().get(vec.size() - 1) == 2) {
        add(
            new OffsetConstraint(
                constraints.size(), source, entry.getValue(), other, new Fraction(cost)));
        // add(OffsetConstraint.increment(constraints.size(), source, entry.getValue(), other));
      } else {
        eq(source, entry.getValue(), other);
      }
    }
  }

  public UnknownCoefficient unknown() {
    return unknown("");
  }

  public UnknownCoefficient unknown(String name) {
    coefficientNames.put(freshness, name);
    return new UnknownCoefficient(freshness++, name);
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
    final var unsatCore = true;
    final var ctx = new Context(z3Config(unsatCore));
    final var solver = ctx.mkSolver();

    // Encode all coefficients as constants.
    RealExpr[] coefficients = new RealExpr[freshness];
    // Also encode that they are rational numbers.
    // IntExpr[] numerators = new IntExpr[freshness];
    // IntExpr[] denominators = new IntExpr[freshness];
    for (int i = 0; i < freshness; i++) {
      coefficients[i] = ctx.mkRealConst("q" + coefficientNames.getOrDefault(i, "q_unknown_" + i));
      // coefficients[i] = ctx.mkRealConst("q" + i);
      // numerators[i] = ctx.mkIntConst("q" + i + "n");
      // denominators[i] = ctx.mkIntConst("q" + i + "d");
      // s.add(ctx.mkEq(coefficients[i], ctx.mkDiv(numerators[i], denominators[i])));
      // s.add(ctx.mkNot(ctx.mkEq(denominators[i], ctx.mkInt(0))));
      solver.add(ctx.mkGe(coefficients[i], ctx.mkReal(0)));
    }

    // Then encode all their dependencies.
    for (Constraint c : constraints) {
      if (unsatCore) {
        solver.assertAndTrack(
            c.encode(ctx, coefficients),
            ctx.mkBoolConst(
                c.getClass().getSimpleName().toLowerCase().replace("constraint", "") + c.getId()));
      } else {
        solver.add(c.encode(ctx, coefficients));
      }
    }

    // log.debug(solver.toString());
    try (PrintWriter out = new PrintWriter(new File("out", name + ".smt"))) {
      out.println(solver);
    } catch (FileNotFoundException e) {
      // This is not essential.
    }

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
        /*
        log.debug(
            "q"
                + coefficientNames.getOrDefault(i, "q_unknown_" + i)
                + " = "
                + xr.getNumerator()
                + "/"
                + xr.getDenominator());
         */
        solution[i] = new KnownCoefficient(Util.toFraction(xr));
      }
    }
  }

  private Model check(Solver solver) {
    final var start = Instant.now();
    var status = solver.check();
    final var stop = Instant.now();
    log.debug("Solving time: " + (Duration.between(start, stop)));
    if (SATISFIABLE.equals(status)) {
      return solver.getModel();
    } else if (UNKNOWN.equals(status)) {
      log.error("Attempt to solve constraint system yielded unknown result.");
      throw new RuntimeException("satisfiability of constraints unknown");
    }
    log.error("Constraint system is unsatisfiable!");
    log.error(solver.toString());
    final var core = solver.getUnsatCore();
    if (core.length > 0) {
      log.error(
          "Unsatisfiable core:\n\t"
              + Arrays.stream(solver.getUnsatCore())
                  .map(Objects::toString)
                  .collect(Collectors.joining("\n\t")));
    } else {
      log.error("Unsatisfiable core is empty!");
    }
    throw new RuntimeException("constraint system is unsatisfiable");
  }

  @Override
  public String toString() {
    return "{ "
        + constraints.stream().map(Object::toString).collect(Collectors.joining(", "))
        + " }";
  }

  public void eqSum(Expression source, Coefficient left, Collection<Coefficient> sum) {
    add(new EqualsSumConstraint(constraints.size(), source, left, sum));
  }
}
