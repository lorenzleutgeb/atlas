package xyz.leutgeb.lorenz.lac.typing.resources.constraints;

import static guru.nidi.graphviz.model.Link.to;
import static xyz.leutgeb.lorenz.lac.Util.bug;

import com.google.common.collect.BiMap;
import com.microsoft.z3.BoolExpr;
import com.microsoft.z3.Context;
import com.microsoft.z3.RealExpr;
import guru.nidi.graphviz.attribute.Color;
import guru.nidi.graphviz.model.Graph;
import guru.nidi.graphviz.model.Node;
import java.util.ArrayList;
import java.util.List;
import java.util.Map;
import java.util.Objects;
import java.util.Set;
import lombok.Data;
import lombok.EqualsAndHashCode;
import lombok.NonNull;
import lombok.extern.log4j.Log4j2;
import xyz.leutgeb.lorenz.lac.typing.resources.Annotation;
import xyz.leutgeb.lorenz.lac.typing.resources.coefficients.Coefficient;

@Data
@Log4j2
@EqualsAndHashCode(callSuper = true)
public class EqualityConstraint extends Constraint {
  @NonNull protected final Coefficient left, right;

  public EqualityConstraint(Coefficient left, Coefficient right) {
    Objects.requireNonNull(left);
    Objects.requireNonNull(right);
    if (left.equals(right)) {
      throw new IllegalArgumentException(
          "cannot create equality constraint for equal coefficients");
    }
    this.left = left;
    this.right = right;
  }

  public static List<Constraint> eq(Coefficient... coefficients) {
    final var result = new ArrayList<Constraint>();
    for (int i = 0; i < coefficients.length - 1; i++) {
      for (int j = i + 1; j < coefficients.length; j++) {
        if (!coefficients[i].equals(coefficients[j])) {
          result.add(new EqualityConstraint(coefficients[i], coefficients[j]));
        }
      }
    }
    return result;
  }

  public static List<Constraint> eq(Annotation... annotations) {
    final var result = new ArrayList<Constraint>();
    for (int i = 0; i < annotations.length - 1; i++) {
      for (int j = i + 1; j < annotations.length; j++) {
        if (annotations[i].size() != annotations[j].size()) {
          throw new IllegalArgumentException("annotations of different sizes cannot be equal");
        }
        if (annotations[i].size() > 1) {
          log.warn(
              "you are equating annotations of size larger than one directly, that could turn out badly");
        }
        final int size = annotations[i].size();
        for (int x = 0; x < size; x++) {
          result.addAll(
              eq(annotations[i].getRankCoefficient(x), annotations[j].getRankCoefficient(x)));
        }

        // if (annotations[i].getCoefficients().size() != annotations[j].getCoefficients().size()) {
        //  throw new UnsupportedOperationException(
        //      "annotations have different number of coefficients");
        // }

        for (Map.Entry<List<Integer>, Coefficient> entry : annotations[i].getCoefficients()) {
          var other = annotations[j].getCoefficient(entry.getKey());

          if (other == null) {
            throw bug("some coefficient is missing");
          }

          result.addAll(eq(entry.getValue(), other));
        }
      }
    }
    return result;
  }

  public BoolExpr encode(Context ctx, BiMap<Coefficient, RealExpr> coefficients) {
    return ctx.mkEq(left.encode(ctx, coefficients), right.encode(ctx, coefficients));
  }

  @Override
  public Graph toGraph(Graph graph, Map<Coefficient, Node> nodes) {
    var leftNode = toNode(left, nodes);
    var rightNode = toNode(right, nodes);
    Objects.requireNonNull(leftNode);
    Objects.requireNonNull(rightNode);
    return graph.with(
        leftNode.link(
            highlight(
                to(rightNode).with(Color.BLUE4).with("dir", "none").with("penwidth", "1.5"))));
  }

  @Override
  public Constraint replace(Coefficient target, Coefficient replacement) {
    return new EqualityConstraint(
        left.replace(target, replacement), right.replace(target, replacement));
  }

  @Override
  public Set<Coefficient> occurringCoefficients() {
    return Set.of(left, right);
  }

  @Override
  public String toString() {
    return left + " = " + right;
  }
}
