package xyz.leutgeb.lorenz.lac.typing.resources.constraints;

import static guru.nidi.graphviz.model.Factory.node;

import com.google.common.collect.BiMap;
import com.microsoft.z3.BoolExpr;
import com.microsoft.z3.Context;
import com.microsoft.z3.RealExpr;
import guru.nidi.graphviz.attribute.Label;
import guru.nidi.graphviz.model.Graph;
import guru.nidi.graphviz.model.Link;
import guru.nidi.graphviz.model.Node;
import java.util.Map;
import java.util.Set;
import lombok.Setter;
import xyz.leutgeb.lorenz.lac.Util;
import xyz.leutgeb.lorenz.lac.typing.resources.coefficients.Coefficient;
import xyz.leutgeb.lorenz.lac.typing.resources.coefficients.KnownCoefficient;

public abstract class Constraint {
  private final String id;

  /**
   * For debugging purposes. This flag will be set to {@code true} if this constraint is part of the
   * unsatisfiable core of the constraint system it appears in.
   */
  @Setter protected boolean core = false;

  Constraint() {
    this.id = Util.randomHex();
  }

  Link highlight(Link link) {
    link = link.with(Label.of(String.valueOf(id)).head());
    if (core) {
      return link.with("arrowsize", "1.5").with("penwidth", "4.5");
    }
    return link.with("style", "dotted");
  }

  public String getId() {
    return id;
  }

  public abstract BoolExpr encode(Context ctx, BiMap<Coefficient, RealExpr> coefficients);

  String prefixed(String suffix) {
    return "(" + id + ": " + suffix + ")";
  }

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
}
