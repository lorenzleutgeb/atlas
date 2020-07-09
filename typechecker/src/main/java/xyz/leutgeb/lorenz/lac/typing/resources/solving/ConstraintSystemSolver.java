package xyz.leutgeb.lorenz.lac.typing.resources.solving;

import static com.microsoft.z3.Status.SATISFIABLE;
import static com.microsoft.z3.Status.UNKNOWN;
import static java.util.Optional.empty;
import static xyz.leutgeb.lorenz.lac.Util.*;

import com.google.common.collect.HashBiMap;
import com.microsoft.z3.*;
import java.io.File;
import java.io.FileNotFoundException;
import java.io.PrintWriter;
import java.time.Duration;
import java.time.Instant;
import java.util.*;
import java.util.concurrent.TimeoutException;
import java.util.function.Supplier;
import java.util.stream.Collectors;
import lombok.extern.slf4j.Slf4j;
import org.hipparchus.fraction.Fraction;
import xyz.leutgeb.lorenz.lac.Util;
import xyz.leutgeb.lorenz.lac.typing.resources.coefficients.Coefficient;
import xyz.leutgeb.lorenz.lac.typing.resources.coefficients.KnownCoefficient;
import xyz.leutgeb.lorenz.lac.typing.resources.coefficients.UnknownCoefficient;
import xyz.leutgeb.lorenz.lac.typing.resources.constraints.Constraint;

@Slf4j
public class ConstraintSystemSolver {
  static {
    ensureLibrary("z3java");
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

  public static Optional<Map<Coefficient, KnownCoefficient>> solve(
      Set<Constraint> constraints, String name) {
    final var unsatCore = true;
    final var ctx = new Context(z3Config(unsatCore));
    final var solver = ctx.mkSolver();

    var optimize = false;
    Optimize opt = null;

    if (optimize) {
      opt = ctx.mkOptimize();
    }

    final var generatedCoefficients = HashBiMap.<RealExpr, Coefficient>create();

    // final var coefficients =
    // constraints.stream().map(Constraint::occurringCoefficients).reduce(emptySet(), Sets::union);
    final var coefficients = new HashSet<Coefficient>();
    for (Constraint constraint : constraints) {
      coefficients.addAll(constraint.occurringCoefficients());
    }

    for (var coefficient : coefficients) {
      if (!(coefficient instanceof UnknownCoefficient)) {
        continue;
      }
      if (generatedCoefficients.inverse().containsKey(coefficient)) {
        continue;
      }
      generatedCoefficients
          .inverse()
          .computeIfAbsent(
              coefficient,
              (Coefficient c) ->
                  // ctx.mkRealConst("q" + "q_unknown_" + generatedCoefficients.size()
                  ctx.mkRealConst(((UnknownCoefficient) c).getName()));
    }

    // Encode all coefficients as constants.
    // Also encode that they are rational numbers.
    // IntExpr[] numerators = new IntExpr[freshness];
    // IntExpr[] denominators = new IntExpr[freshness];
    for (final var coefficientExpr : generatedCoefficients.keySet()) {
      // coefficients[i] = ctx.mkRealConst("q" + i);
      // numerators[i] = ctx.mkIntConst("q" + i + "n");
      // denominators[i] = ctx.mkIntConst("q" + i + "d");
      // s.add(ctx.mkEq(coefficients[i], ctx.mkDiv(numerators[i], denominators[i])));
      // s.add(ctx.mkNot(ctx.mkEq(denominators[i], ctx.mkInt(0))));
      final var positive = ctx.mkGe(coefficientExpr, ctx.mkReal(0));
      if (optimize) {
        opt.Add(positive);
        opt.MkMinimize(coefficientExpr);
      } else {
        solver.add(positive);
      }
    }

    /*
    if (optimize) {
      var sum = ctx.mkRealConst("sum");
      opt.Add(ctx.mkEq(sum, ctx.mkAdd(coefficients)));
      opt.MkMinimize(sum);
    }
     */

    // Then encode all their dependencies.
    for (Constraint c : constraints) {
      if (optimize) {
        opt.Add(c.encode(ctx, generatedCoefficients.inverse()));
      } else {
        if (unsatCore) {
          solver.assertAndTrack(
              c.encode(ctx, generatedCoefficients.inverse()), ctx.mkBoolConst(c.getTracking()));
          // c.getId() + " " + c.getClass().getSimpleName().replace("Constraint", "")));
        } else {
          solver.add(c.encode(ctx, generatedCoefficients.inverse()));
        }
      }
    }

    log.info("Coefficients: " + generatedCoefficients.keySet().size());
    log.info("Constraints: " + constraints.size());

    // log.debug(solver.toString());
    // TODO: Parameterize location.
    File smtFile = new File("out", name + ".smt");
    try (PrintWriter out = new PrintWriter(smtFile)) {
      out.println(optimize ? opt : solver);
      log.info("Wrote SMT instance to {}.", smtFile);
    } catch (FileNotFoundException e) {
      // This is not essential.
    }

    final Optional<Model> optionalModel;
    if (optimize) {
      optionalModel = check(opt::Check, opt::getModel, opt::getUnsatCore, constraints);
    } else {
      // System.out.println(solver);
      optionalModel = check(solver::check, solver::getModel, solver::getUnsatCore, constraints);
    }
    if (optionalModel.isEmpty()) {
      return empty();
    }
    final Model model = optionalModel.get();
    final var solution = new HashMap<Coefficient, KnownCoefficient>();
    for (final var e : generatedCoefficients.entrySet()) {
      var x = model.getConstInterp(e.getKey());
      if (!x.isRatNum()) {
        throw new RuntimeException("solution for " + e.getValue() + " is not a rational number");
      }
      final var xr = (RatNum) x;
      var num = xr.getNumerator();
      if (num.getBigInteger().intValueExact() == 0) {
        solution.put(e.getValue(), KnownCoefficient.ZERO);
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
        Fraction v = Util.toFraction(xr);
        if (signum(v) < 0) {
          log.warn("Got negative coefficient");
        }
        solution.put(e.getValue(), new KnownCoefficient(v));
      }
    }

    if (solution.size() != generatedCoefficients.size()) {
      log.warn("Partial solution!");
    }

    return Optional.of(solution);
  }

  private static Optional<Model> check(
      Supplier<Status> check,
      Supplier<Model> getModel,
      Supplier<BoolExpr[]> getUnsatCore,
      Set<Constraint> constraints) {
    final var start = Instant.now();
    log.info("Invoking z3 at " + start);
    var status = check.get();
    final var stop = Instant.now();
    log.debug("Solving time: " + (Duration.between(start, stop)));
    if (SATISFIABLE.equals(status)) {
      return Optional.of(getModel.get());
    } else if (UNKNOWN.equals(status)) {
      log.error("Attempt to solve constraint system yielded unknown result.");
      throw new RuntimeException(new TimeoutException("satisfiability of constraints unknown"));
    }
    log.error("Constraint system is unsatisfiable!");
    final var core = getUnsatCore.get();
    if (core.length > 0) {
      Set<String> coreSet =
          Arrays.stream(getUnsatCore.get())
              // .map(x -> x.toString().substring(1).split(" ")[0])
              .map(Object::toString)
              .map(x -> x.substring(1, x.length() - 1))
              .collect(Collectors.toSet());

      log.info(
          "Unsatisfiable core (raw from Z3):\n{}",
          coreSet.stream().collect(Collectors.joining("\n")));

      constraints.stream()
          // .filter(c -> coreSet.contains(c.getTracking()))
          // .forEach(c -> c.setCore(true));
          .forEach(c -> c.markCoreByTrackings(coreSet));

      /*
      final String unsatCoreLog =
          constraints.stream()
              .filter(Constraint::isCore)
              .map(c -> c.getTracking() + "\t" + c + "\t" + c.getReason())
              .collect(Collectors.joining("\n"));
      log.info("Unsatisfiable core:\n{}", unsatCoreLog);
       */
    } else {
      throw bug("Unsatisfiable core is empty, even though constraint system is unsatisfiable!");
    }
    return empty();
  }
}
