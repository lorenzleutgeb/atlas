package xyz.leutgeb.lorenz.atlas.typing.resources.constraints;

import static guru.nidi.graphviz.model.Factory.graph;
import static guru.nidi.graphviz.model.Factory.node;
import static xyz.leutgeb.lorenz.atlas.util.Util.append;
import static xyz.leutgeb.lorenz.atlas.util.Util.output;
import static xyz.leutgeb.lorenz.atlas.util.Util.rawObjectNode;

import com.google.common.collect.BiMap;
import com.microsoft.z3.BoolExpr;
import com.microsoft.z3.Context;
import com.microsoft.z3.RealExpr;
import guru.nidi.graphviz.attribute.Label;
import guru.nidi.graphviz.attribute.Records;
import guru.nidi.graphviz.engine.Engine;
import guru.nidi.graphviz.engine.Format;
import guru.nidi.graphviz.engine.Graphviz;
import guru.nidi.graphviz.engine.GraphvizCmdLineEngine;
import guru.nidi.graphviz.model.Graph;
import guru.nidi.graphviz.model.Link;
import guru.nidi.graphviz.model.Node;
import java.nio.file.Path;
import java.util.Collections;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.concurrent.TimeUnit;
import java.util.stream.Stream;
import lombok.Getter;
import lombok.Setter;
import lombok.extern.slf4j.Slf4j;
import xyz.leutgeb.lorenz.atlas.typing.resources.coefficients.Coefficient;
import xyz.leutgeb.lorenz.atlas.typing.resources.coefficients.KnownCoefficient;
import xyz.leutgeb.lorenz.atlas.typing.resources.coefficients.UnknownCoefficient;

@Slf4j
public abstract class Constraint {
  private final String reason;

  /**
   * For debugging purposes. This flag will be set to {@code true} if this constraint is part of the
   * unsatisfiable core of the constraint system it appears in.
   */
  @Getter @Setter protected boolean core = false;

  public Constraint(String reason) {
    this.reason = reason;
  }

  public static void plot(String name, Set<Constraint> constraints, Path path) {
    try {
      Graph graph =
          graph(name)
              .directed()
              .graphAttr()
              .with("ranksep", "2.5")
              .graphAttr()
              .with("splines", "ortho") /*.graphAttr().with(Rank.RankDir.BOTTOM_TO_TOP)*/;

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
      var lel = new GraphvizCmdLineEngine();
      lel.timeout(2, TimeUnit.MINUTES);
      Graphviz.useEngine(lel);
      var viz = Graphviz.fromGraph(graph);
      Path target = path.resolve(name + "-constraints.svg");
      try (final var out = output(target)) {
        viz.engine(Engine.DOT).render(Format.SVG).toOutputStream(out);
        log.info("Wrote plot to {}", target);
      }
    } catch (Exception e) {
      log.warn("Non-critical exception thrown.", e);
    }
  }

  Link highlight(Link link) {
    link = link.with(Label.of(String.valueOf(getTracking())).head());
    if (core) {
      return link.with("arrowsize", "1.5").with("penwidth", "4.5");
    }
    return link.with("style", "dotted");
  }

  public String getReason() {
    return reason;
  }

  public abstract BoolExpr encode(Context ctx, BiMap<UnknownCoefficient, RealExpr> coefficients);

  /**
   * Edge colors:
   *
   * <p>blue equal green less or equal cyan sum (with intermediate node) red offset (specialization
   * of equal)
   */
  public abstract Graph toGraph(Graph graph, Map<Coefficient, Node> nodes);

  public abstract Constraint replace(Coefficient target, Coefficient replacement);

  Node toNode(Coefficient coefficient, Map<Coefficient, Node> map) {
    if (map.containsKey(coefficient)) {
      return map.get(coefficient);
    }
    if (coefficient instanceof KnownCoefficient) {
      return node(coefficient.toString());
    }
    return null;
  }

  public abstract Set<Coefficient> occurringCoefficients();

  public String getTracking() {
    return String.format("%010d", System.identityHashCode(this)) + ": " + reason;
    // return Util.stamp(this);
  }

  public Stream<Constraint> children() {
    return Stream.empty();
  }

  public boolean known() {
    return false;
  }

  public boolean satisfied() {
    if (!known()) {
      throw new IllegalStateException("unknown");
    }
    return satisfiedInternal();
  }

  protected boolean satisfiedInternal() {
    return false;
  }

  public String toStringWithReason() {
    return toString() + " ∵ " + reason;
  }

  public String toRecord() {
    return toStringWithReason();
  }
}
