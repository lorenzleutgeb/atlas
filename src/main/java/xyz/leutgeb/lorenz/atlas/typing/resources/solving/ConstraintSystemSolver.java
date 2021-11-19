package xyz.leutgeb.lorenz.atlas.typing.resources.solving;

import static com.microsoft.z3.Status.SATISFIABLE;
import static com.microsoft.z3.Status.UNKNOWN;
import static com.microsoft.z3.Status.UNSATISFIABLE;
import static java.util.Collections.emptyList;
import static java.util.Collections.emptyMap;
import static java.util.Optional.empty;
import static xyz.leutgeb.lorenz.atlas.util.Util.bug;
import static xyz.leutgeb.lorenz.atlas.util.Util.flag;
import static xyz.leutgeb.lorenz.atlas.util.Util.output;
import static xyz.leutgeb.lorenz.atlas.util.Util.randomHex;
import static xyz.leutgeb.lorenz.atlas.util.Z3Support.load;

import com.google.common.collect.HashBiMap;
import com.microsoft.z3.BoolExpr;
import com.microsoft.z3.Context;
import com.microsoft.z3.Expr;
import com.microsoft.z3.Model;
import com.microsoft.z3.Optimize;
import com.microsoft.z3.RatNum;
import com.microsoft.z3.RealExpr;
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
import xyz.leutgeb.lorenz.atlas.typing.resources.coefficients.Coefficient;
import xyz.leutgeb.lorenz.atlas.typing.resources.coefficients.KnownCoefficient;
import xyz.leutgeb.lorenz.atlas.typing.resources.coefficients.UnknownCoefficient;
import xyz.leutgeb.lorenz.atlas.typing.resources.constraints.Constraint;
import xyz.leutgeb.lorenz.atlas.util.Pair;
import xyz.leutgeb.lorenz.atlas.util.Util;

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

    public boolean isSatisfiable() {
      return status.equals(SATISFIABLE);
    }

    public int toExitCode() {
      return switch (status) {
        case SATISFIABLE -> 0;
        case UNSATISFIABLE -> 1;
        case UNKNOWN -> 2;
        default -> throw bug("unexpected status: " + status);
      };
    }

    public static Result unsat() {
      return new Result(Status.UNSATISFIABLE, empty(), emptyMap(), empty());
    }

    public static Result unknown() {
      return new Result(UNKNOWN, empty(), emptyMap(), empty());
    }

    public static Result merge(Result a, Result b) {
      final Status status = a.status.equals(SATISFIABLE) ? b.status : a.status;
      return new Result(
          status,
          UNSATISFIABLE.equals(status)
              ? empty()
              : Optional.of(
                  Stream.concat(
                          a.getSolution().orElse(emptyMap()).entrySet().stream(),
                          b.getSolution().orElse(emptyMap()).entrySet().stream())
                      .collect(
                          Collectors.toMap(
                              Map.Entry::getKey,
                              Map.Entry::getValue,
                              (xa, xb) -> {
                                if (xa.equals(xb)) {
                                  return xa;
                                }
                                log.error("Cannot merge {} and {}!", xa, xb);
                                return null;
                              }))),
          emptyMap(),
          empty());
    }
  }

  private static Map<String, String> z3Config(boolean unsatCore) {
    // Execute `z3 -p` to get a list of parameters.
    // return emptyMap();
    return Map.of("unsat_core", String.valueOf(unsatCore));
  }

  public static Result solve(Set<Constraint> constraints) {
    return solve(constraints, Paths.get("out", randomHex()), emptyList());
  }

  public static Result solve(Set<Constraint> constraints, Path outPath) {
    return solve(constraints, outPath, emptyList());
  }

  public static Result solve(
      Set<Constraint> constraints, Path outPath, List<UnknownCoefficient> target) {
    load(outPath.resolve("z3.log"));

    final var dump = flag(ConstraintSystemSolver.class, emptyMap(), "dump");
    final var optimize = !target.isEmpty() && !dump;
    final var unsatCore = !optimize && !dump;

    // This allows more accurate memory measurements, but is dangerous when running in parallel.
    // Native.resetMemory();

    try (final var ctx = new Context(z3Config(unsatCore))) {
      final Solver solver = ctx.mkSolver();
      // final Solver solver = ctx.mkTactic("qflia").getSolver();
      // /*domain.getLogic()*/Optional.of("LIA").map(ctx::mkSolver).orElseGet(ctx::mkSolver);

      final Optimize opt = optimize ? ctx.mkOptimize() : null;

      final var generatedCoefficients = HashBiMap.<RealExpr, UnknownCoefficient>create();

      final var coefficients = new HashSet<Coefficient>();
      for (Constraint constraint : constraints) {
        coefficients.addAll(constraint.occurringCoefficients());
      }

      final var trackNonNegative =
          flag(ConstraintSystemSolver.class, emptyMap(), "trackNonNegative");

      for (var coefficient : coefficients) {
        if (!(coefficient instanceof UnknownCoefficient)) {
          continue;
        }
        final var unknownCoefficient = ((UnknownCoefficient) coefficient).canonical();
        if (generatedCoefficients.inverse().containsKey(unknownCoefficient)) {
          continue;
        }
        if (!generatedCoefficients.inverse().containsKey(unknownCoefficient)) {
          final var it = ctx.mkRealConst(unknownCoefficient.getName());
          if (!unknownCoefficient.isMaybeNegative()) {
            final var positive = ctx.mkGe(it, ctx.mkReal(0));
            if (optimize) {
              opt.Add(positive);
            } else {
              if (unsatCore && trackNonNegative) {
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

      log.info("atlas Coefficients: " + generatedCoefficients.keySet().size());
      log.info("atlas Constraints:  " + constraints.size());
      log.info("Z3  Scopes:       " + (optimize ? "?" : solver.getNumScopes()));
      log.info("Z3  Assertions:   " + (optimize ? "?" : solver.getNumAssertions()));

      final Path smtFile = outPath.resolve("instance.smt");

      try (final var out = new PrintWriter(output(smtFile))) {
        out.println(optimize ? opt : solver);
        log.debug("Wrote SMT instance to {}.", smtFile);
        log.info("Wrote SMT instance to " + smtFile);
      } catch (IOException ioException) {
        log.warn("Failed to write SMT instance to {}.", smtFile, ioException);
        ioException.printStackTrace();
      }

      if (dump) {
        log.info("Exiting because dump was requested.");
        System.exit(0);
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
        for (Expr objective : opt.getObjectives()) {
          log.debug("Objective: " + objective + " = " + opt.getModel().getConstInterp(objective));
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
        if (!x.isRatNum()) {
          log.warn("solution for " + e.getValue() + " is not a rational number, it is " + x);
        }
        KnownCoefficient v;
        if (x instanceof final RatNum xr) {
          var num = xr.getNumerator();
          if (num.getBigInteger().intValueExact() == 0) {
            v = KnownCoefficient.ZERO;
          } else {
            v = new KnownCoefficient(Util.toFraction(xr));
          }
        } else {
          throw bug("interpretation contains constant of unknown or unexpected type");
        }
        if (v.getValue().signum() < 0 && !e.getValue().isMaybeNegative()) {
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
          if ("max memory".equals(entry.Key)) {
            log.info("Max. Memory: " + entry.getValueString() + " MiB");
          }
          if ("memory".equals(entry.Key)) {
            log.info("     Memory: " + entry.getValueString() + " MiB");
          }
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
    log.info("Solving duration: " + (Duration.between(start, stop)));
    log.debug("Solving duratio: " + (Duration.between(start, stop)));
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
}
