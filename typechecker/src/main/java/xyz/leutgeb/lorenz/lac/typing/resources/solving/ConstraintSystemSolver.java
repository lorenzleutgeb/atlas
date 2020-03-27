package xyz.leutgeb.lorenz.lac.typing.resources.solving;

import static com.microsoft.z3.Status.SATISFIABLE;
import static com.microsoft.z3.Status.UNKNOWN;
import static java.util.Optional.empty;
import static xyz.leutgeb.lorenz.lac.Util.bug;
import static xyz.leutgeb.lorenz.lac.Util.ensureLibrary;
import static xyz.leutgeb.lorenz.lac.Util.rawObjectNode;
import static xyz.leutgeb.lorenz.lac.Util.signum;

import com.google.common.collect.HashBiMap;
import com.microsoft.z3.BoolExpr;
import com.microsoft.z3.Context;
import com.microsoft.z3.Model;
import com.microsoft.z3.Optimize;
import com.microsoft.z3.RatNum;
import com.microsoft.z3.RealExpr;
import com.microsoft.z3.Status;
import guru.nidi.graphviz.attribute.Label;
import guru.nidi.graphviz.attribute.Records;
import guru.nidi.graphviz.model.Graph;
import guru.nidi.graphviz.model.Node;
import java.io.File;
import java.io.FileNotFoundException;
import java.io.PrintWriter;
import java.time.Duration;
import java.time.Instant;
import java.util.Arrays;
import java.util.HashMap;
import java.util.Map;
import java.util.Optional;
import java.util.Set;
import java.util.function.Supplier;
import java.util.stream.Collectors;
import lombok.extern.log4j.Log4j2;
import org.hipparchus.fraction.Fraction;
import xyz.leutgeb.lorenz.lac.Util;
import xyz.leutgeb.lorenz.lac.typing.resources.coefficients.Coefficient;
import xyz.leutgeb.lorenz.lac.typing.resources.coefficients.KnownCoefficient;
import xyz.leutgeb.lorenz.lac.typing.resources.constraints.Constraint;

@Log4j2
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
      Set<Constraint> constraints, String name) throws ConstraintSystemException {
    final var unsatCore = true;
    final var ctx = new Context(z3Config(unsatCore));
    final var solver = ctx.mkSolver();

    var optimize = false;
    Optimize opt = null;

    if (optimize) {
      opt = ctx.mkOptimize();
    }

    final var generatedCoefficients = HashBiMap.<RealExpr, Coefficient>create();

    for (var constraint : constraints) {
      for (var coefficient : constraint.occurringCoefficients()) {
        generatedCoefficients
            .inverse()
            .computeIfAbsent(
                coefficient,
                (Coefficient c) ->
                    ctx.mkRealConst("q" + "q_unknown_" + generatedCoefficients.size()));
      }
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
              c.encode(ctx, generatedCoefficients.inverse()),
              ctx.mkBoolConst(
                  c.getId() + " " + c.getClass().getSimpleName().replace("constraint", "")));
        } else {
          solver.add(c.encode(ctx, generatedCoefficients.inverse()));
        }
      }
    }

    // log.debug(solver.toString());
    // TODO: Parameterize location.
    try (PrintWriter out = new PrintWriter(new File("out", name + ".smt"))) {
      out.println(optimize ? opt : solver);
    } catch (FileNotFoundException e) {
      // This is not essential.
    }

    final Optional<Model> optionalModel;
    if (optimize) {
      optionalModel = check(opt::Check, opt::getModel, opt::getUnsatCore, constraints);
    } else {
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
      Set<Constraint> constraints)
      throws ConstraintSystemTimeoutException {
    final var start = Instant.now();
    System.out.println("Invoking z3 at " + start);
    var status = check.get();
    final var stop = Instant.now();
    log.debug("Solving time: " + (Duration.between(start, stop)));
    if (SATISFIABLE.equals(status)) {
      return Optional.of(getModel.get());
    } else if (UNKNOWN.equals(status)) {
      log.error("Attempt to solve constraint system yielded unknown result.");
      throw new ConstraintSystemTimeoutException("satisfiability of constraints unknown");
    }
    // log.error("Constraint system is unsatisfiable!");
    final var core = getUnsatCore.get();
    if (core.length > 0) {
      Set<String> coreSet =
          Arrays.stream(getUnsatCore.get())
              .map(x -> x.toString().substring(1).split(" ")[0])
              .collect(Collectors.toSet());
      constraints.forEach(c -> c.setCore(coreSet.contains(c.getId())));
      /*
      log.error(
          "Unsatisfiable core:\n\t"
              + Arrays.stream(getUnsatCore.get())
                  .map(Objects::toString)
                  .collect(Collectors.joining("\n\t")));
       */
    } else {
      throw bug("Unsatisfiable core is empty, even though constraint system is unsatisfiable!");
    }
    return empty();
  }

  public static Graph toGraph(Graph graph, Set<Constraint> constraints) {
    var nodes = new HashMap<Coefficient, Node>();

    // var clusters = new HashMap<String, Graph>();

    for (var constraint : constraints) {
      for (var it : constraint.occurringCoefficients()) {
        var s = it.toString();
        var label = s.startsWith("h") ? Records.of(s.substring(1).split(",")) : Label.of(s);
        nodes.put(it, rawObjectNode(it).with(label));
        // if (it instanceof  UnknownCoefficient) {
        //	var name = ((UnknownCoefficient) it).getName().split(" ")[0];
        //	var cluster = clusters.getOrDefault(name, graph(name).cluster());
        //  var node = objectNode(it);
        //	cluster = cluster.with(node);
        //	clusters.put(name, cluster);
        // }
        // graph = graph.with(objectNode(it));
      }
    }

    // for (var cluster : clusters.values()) {
    //  graph = graph.with(cluster);
    // }

    for (var it : constraints) {
      graph = it.toGraph(graph, nodes);
    }

    return graph;
  }
}
