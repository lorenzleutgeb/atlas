package xyz.leutgeb.lorenz.lac.typing.resources.solving;

import static com.microsoft.z3.Status.SATISFIABLE;
import static com.microsoft.z3.Status.UNKNOWN;
import static java.util.Collections.emptyList;
import static java.util.Collections.emptyMap;
import static java.util.Optional.empty;
import static xyz.leutgeb.lorenz.lac.util.Util.bug;
import static xyz.leutgeb.lorenz.lac.util.Util.output;
import static xyz.leutgeb.lorenz.lac.util.Util.randomHex;
import static xyz.leutgeb.lorenz.lac.util.Util.signum;
import static xyz.leutgeb.lorenz.lac.util.Z3Support.load;

import com.google.common.collect.HashBiMap;
import com.microsoft.z3.ArithExpr;
import com.microsoft.z3.BoolExpr;
import com.microsoft.z3.Context;
import com.microsoft.z3.Expr;
import com.microsoft.z3.IntNum;
import com.microsoft.z3.Model;
import com.microsoft.z3.Optimize;
import com.microsoft.z3.RatNum;
import com.microsoft.z3.Solver;
import com.microsoft.z3.Statistics;
import com.microsoft.z3.Status;
import com.microsoft.z3.Z3Exception;
import java.io.IOException;
import java.io.PrintStream;
import java.io.PrintWriter;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.time.Duration;
import java.time.Instant;
import java.util.Arrays;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Optional;
import java.util.Set;
import java.util.function.Supplier;
import java.util.stream.Collectors;
import java.util.stream.Stream;
import lombok.Value;
import lombok.extern.slf4j.Slf4j;
import xyz.leutgeb.lorenz.lac.typing.resources.coefficients.Coefficient;
import xyz.leutgeb.lorenz.lac.typing.resources.coefficients.KnownCoefficient;
import xyz.leutgeb.lorenz.lac.typing.resources.coefficients.UnknownCoefficient;
import xyz.leutgeb.lorenz.lac.typing.resources.constraints.Constraint;
import xyz.leutgeb.lorenz.lac.util.Fraction;
import xyz.leutgeb.lorenz.lac.util.Pair;
import xyz.leutgeb.lorenz.lac.util.Util;

@Slf4j
public class ConstraintSystemSolver {

  @Value
  public static class Result {
    Status status;
    Optional<Map<Coefficient, KnownCoefficient>> solution;
    Map<String, String> statistics;
    Optional<Path> smtFile;

    public Result(
        Status status,
        Optional<Map<Coefficient, KnownCoefficient>> solution,
        Map<String, String> statistics,
        Optional<Path> smtFile) {
      if (status.equals(SATISFIABLE) != solution.isPresent()) {
        throw new IllegalArgumentException();
      }
      this.status = status;
      this.solution = solution;
      this.statistics = statistics;
      this.smtFile = smtFile;
    }

    public boolean hasSolution() {
      return status.equals(SATISFIABLE);
    }

    public int toExitCode() {
      switch (status) {
        case SATISFIABLE:
          return 0;
        case UNSATISFIABLE:
          return 1;
        case UNKNOWN:
          return 2;
        default:
          throw bug("unexpected status: " + status);
      }
    }

    public static Result unsat() {
      return new Result(Status.UNSATISFIABLE, empty(), emptyMap(), empty());
    }

    public static Result unknown() {
      return new Result(UNKNOWN, empty(), emptyMap(), empty());
    }
  }

  private static Map<String, String> z3Config(boolean unsatCore) {
    // Execute `z3 -p` to get a list of parameters.
    // return emptyMap();
    return Map.of("unsat_core", String.valueOf(unsatCore));
    // return Map.of("parallel.enable", "true");
  }

  public static Result solve(Set<Constraint> constraints) {
    return solve(constraints, Paths.get("out", randomHex()), emptyList(), Domain.INTEGER);
  }

  public static Result solve(Set<Constraint> constraints, Path outPath) {
    return solve(constraints, outPath, emptyList(), Domain.INTEGER);
  }

  public static Result solve(
      Set<Constraint> constraints, Path outPath, List<UnknownCoefficient> target) {
    return solve(constraints, outPath, target, Domain.RATIONAL);
  }

  public static Result solve(
      Set<Constraint> constraints, Path outPath, List<UnknownCoefficient> target, Domain domain) {
    load(outPath.resolve("z3.log"));

    final var unsatCore = target.isEmpty();

    try (final var ctx = new Context(z3Config(unsatCore))) {
      final Solver solver = ctx.mkSolver();
      // final Solver solver = ctx.mkTactic("qflia").getSolver();
      // final Solver solver =
      // /*domain.getLogic()*/Optional.of("LIA").map(ctx::mkSolver).orElseGet(ctx::mkSolver);

      // final var params = ctx.mkParams();
      // params.add("?", false);
      // params.add("threads", Runtime.getRuntime().availableProcessors());

      // PT2M34.378834S

      // params.add("opt.priority", "pareto");
      // params.add("opt.enable_sat", "true");
      // params.add("smt.pb.enable_simplex", "true");
      // solver.setParameters(params);

      var optimize = !target.isEmpty();
      final Optimize opt = optimize ? ctx.mkOptimize() : null;

      final var generatedCoefficients = HashBiMap.<ArithExpr, UnknownCoefficient>create();

      final var coefficients = new HashSet<Coefficient>();
      for (Constraint constraint : constraints) {
        coefficients.addAll(constraint.occurringCoefficients());
      }

      for (var coefficient : coefficients) {
        if (!(coefficient instanceof UnknownCoefficient)) {
          continue;
        }
        final var unknownCoefficient = ((UnknownCoefficient) coefficient).canonical();
        if (generatedCoefficients.inverse().containsKey(unknownCoefficient)) {
          continue;
        }
        if (!generatedCoefficients.inverse().containsKey(unknownCoefficient)) {
          final var it =
              Domain.INTEGER.equals(domain)
                  ? ctx.mkIntConst(unknownCoefficient.getName())
                  : ctx.mkRealConst(unknownCoefficient.getName());
          if (!unknownCoefficient.isMaybeNegative()) {
            final var positive = ctx.mkGe(it, ctx.mkInt(0));
            if (optimize) {
              opt.Add(positive);
            } else {
              if (unsatCore && false) {
                solver.assertAndTrack(
                    positive, ctx.mkBoolConst("non negative " + unknownCoefficient));
              } else {
                solver.add(positive);
              }
            }
          }
          generatedCoefficients.inverse().put(unknownCoefficient, it);
        }
      }

      if (optimize) {
        target.forEach(
            x -> {
              if (!generatedCoefficients.inverse().containsKey(x)) {
                log.warn("Could not find generated coefficient for optimization target '{}'", x);
              } else {
                opt.MkMinimize(generatedCoefficients.inverse().get(x));
              }
            });
      }

      for (Constraint c : constraints) {
        if (optimize) {
          opt.Add(c.encode(ctx, generatedCoefficients.inverse()));
        } else {
          if (unsatCore) {
            solver.assertAndTrack(
                c.encode(ctx, generatedCoefficients.inverse()), ctx.mkBoolConst(c.getTracking()));
          } else {
            solver.add(c.encode(ctx, generatedCoefficients.inverse()));
          }
        }
      }

      System.out.println("lac Coefficients: " + generatedCoefficients.keySet().size());
      System.out.println("lac Constraints:  " + constraints.size());
      System.out.println("Z3  Scopes:       " + (optimize ? "?" : solver.getNumScopes()));
      System.out.println("Z3  Assertions:   " + (optimize ? "?" : solver.getNumAssertions()));

      final Path smtFile = outPath.resolve("instance.smt");

      try (final var out = new PrintWriter(output(smtFile))) {
        out.println(optimize ? opt : solver);
        log.debug("Wrote SMT instance to {}.", smtFile);
        System.out.println("Wrote SMT instance to " + smtFile);
      } catch (IOException ioException) {
        log.warn("Failed to write SMT instance to {}.", smtFile, ioException);
        ioException.printStackTrace();
      }

      var result =
          optimize
              ? check(opt::Check, opt::getModel, opt::getUnsatCore, unsatCore, opt::toString)
              : check(
                  solver::check,
                  solver::getModel,
                  solver::getUnsatCore,
                  unsatCore,
                  solver::toString);

      if (optimize && result.getLeft().equals(SATISFIABLE)) {
        System.out.println("Objectives:");
        for (Expr objective : opt.getObjectives()) {
          System.out.println(objective + " = " + opt.getModel().getConstInterp(objective));
        }
      }

      var stats =
          statisticsToMapAndFile(optimize ? opt.getStatistics() : solver.getStatistics(), outPath);

      if (!optimize) {
        stats.put("num scopes", String.valueOf(solver.getNumScopes()));
        stats.put("num assertions", String.valueOf(solver.getNumAssertions()));
      }

      if (!result.getLeft().equals(SATISFIABLE)) {
        return new Result(result.getLeft(), empty(), stats, Optional.of(smtFile));
      }
      final Model model = result.getRight().get();
      final var solution = new HashMap<Coefficient, KnownCoefficient>();
      for (final var e : generatedCoefficients.entrySet()) {
        var x = model.getConstInterp(e.getKey());
        if (Domain.RATIONAL.equals(domain) && !x.isRatNum()) {
          log.warn("solution for " + e.getValue() + " is not a rational number, it is " + x);
        }
        KnownCoefficient v;
        if (x instanceof RatNum && Domain.RATIONAL.equals(domain)) {
          final var xr = (RatNum) x;
          var num = xr.getNumerator();
          if (num.getBigInteger().intValueExact() == 0) {
            v = KnownCoefficient.ZERO;
          } else {
            v = new KnownCoefficient(Util.toFraction(xr));
          }
        } else if (x instanceof IntNum && Domain.INTEGER.equals(domain)) {
          final var xr = (IntNum) x;
          if (xr.getBigInteger().intValueExact() == 0) {
            v = KnownCoefficient.ZERO;
          } else {
            v = new KnownCoefficient(new Fraction(xr.getInt()));
          }
        } else {
          throw bug("interpretation contains constant of unknown or unexpected type");
        }
        if (signum(v.getValue()) < 0 && !e.getValue().isMaybeNegative()) {
          log.warn("Got negative coefficient");
        }
        solution.put(e.getValue(), v);
      }

      if (solution.size() != generatedCoefficients.size()) {
        log.warn("Partial solution!");
      }

      return new Result(result.getLeft(), Optional.of(solution), stats, Optional.of(smtFile));
    }
  }

  private static Map<String, String> statisticsToMapAndFile(Statistics statistics, Path outPath) {
    final Map<String, String> result = new HashMap<>();
    try {
      try (final var out = output(outPath.resolve("z3-statistics.txt"))) {
        final var printer = new PrintStream(out);
        for (var entry : statistics.getEntries()) {
          final var value = entry.getValueString();
          result.put(entry.Key, value);
          log.trace("{}={}", entry.Key, value);
          printer.println(entry.Key + "=" + value);
        }
      }
    } catch (Exception exception) {
      // ignored
    }
    return result;
  }

  private static Pair<Status, Optional<Model>> check(
      Supplier<Status> check,
      Supplier<Model> getModel,
      Supplier<BoolExpr[]> getUnsatCore,
      boolean unsatCore,
      Supplier<String> program) {
    final var start = Instant.now();
    log.debug("Solving start: " + start);
    Status status = UNKNOWN;
    try {
      status = check.get();
    } catch (Z3Exception e) {
      if (e.getMessage().equals("maximization suspended")) {
        return Pair.of(UNKNOWN, empty());
      }
      throw e;
    }
    final var stop = Instant.now();
    System.out.println("Solving duration: " + (Duration.between(start, stop)));
    log.debug("Solving duration: " + (Duration.between(start, stop)));
    if (SATISFIABLE.equals(status)) {
      return Pair.of(status, Optional.of(getModel.get()));
    } else if (UNKNOWN.equals(status)) {
      log.error("Attempt to solve constraint system yielded unknown result.");
      return Pair.of(status, empty());
    }
    log.error("Constraint system is unsatisfiable!");
    if (!unsatCore) {
      log.error("Got no unsat core");
      return Pair.of(status, Optional.empty());
    }
    Set<String> coreSet =
        Arrays.stream(getUnsatCore.get())
            .map(Object::toString)
            .map(x -> x.substring(1, x.length() - 1))
            .collect(Collectors.toSet());

    log.info(
        "Unsatisfiable core (raw from Z3):\n{}",
        Stream.of(program.get().replaceAll("\\n\\s+", " ").split("\n"))
            .filter(line -> line.contains("assert"))
            .filter(line -> coreSet.stream().anyMatch(line::contains))
            .collect(Collectors.joining("\n")));

    return Pair.of(status, empty());
  }

  public enum Domain {
    RATIONAL(empty()),
    INTEGER(Optional.of("QF_LIA"));

    private final Optional<String> logic;

    Domain(Optional<String> logic) {
      this.logic = logic;
    }

    public Optional<String> getLogic() {
      return logic;
    }
  }
}
