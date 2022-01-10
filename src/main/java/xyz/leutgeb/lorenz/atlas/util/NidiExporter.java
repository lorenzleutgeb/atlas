package xyz.leutgeb.lorenz.atlas.util;

import static guru.nidi.graphviz.model.Factory.to;

import guru.nidi.graphviz.attribute.Attributed;
import guru.nidi.graphviz.attribute.Attributes;
import guru.nidi.graphviz.attribute.For;
import guru.nidi.graphviz.attribute.Label;
import guru.nidi.graphviz.model.Factory;
import guru.nidi.graphviz.model.Graph;
import guru.nidi.graphviz.model.Node;
import java.util.Collections;
import java.util.Map;
import java.util.Optional;
import java.util.function.Function;
import org.jgrapht.nio.Attribute;
import org.jgrapht.nio.AttributeType;
import org.jgrapht.nio.BaseExporter;
import org.jgrapht.nio.IntegerIdProvider;

public class NidiExporter<V, E> extends BaseExporter<V, E> {
  private static final String DEFAULT_GRAPH_ID = "G";

  public NidiExporter() {
    this(new IntegerIdProvider<>());
  }

  /**
   * Constructor
   *
   * @param vertexIdProvider the vertex id provider to use. Cannot be null.
   */
  public NidiExporter(Function<V, String> vertexIdProvider) {
    super(vertexIdProvider);
  }

  private static <T extends Attributed<T, F>, F extends For> T renderAttributes(
      T attributed, Optional<Map<String, Attribute>> attributes) {
    for (Map.Entry<String, Attribute> attr : attributes.orElse(Collections.emptyMap()).entrySet()) {
      String name = attr.getKey();
      if (attr.getValue() instanceof final NidiAttribute attribute) {
        // if (name.equals("label")) {
        attributed = attributed.with((Attributes<? extends F>) attribute.getNidi());
        /*} else {
          attributed = attributed.with(attr.getKey(), attr.getValue().getValue());
        }*/
      } else if (name.equals("label")) {
        // Special case for labels
        if (attr.getValue().getType().equals(AttributeType.HTML)) {
          attributed =
              attributed.with((Attributes<? extends F>) Label.html(attr.getValue().getValue()));
        } else {
          attributed =
              attributed.with((Attributes<? extends F>) Label.of(attr.getValue().getValue()));
        }
      } else {
        attributed = attributed.with(name, attr.getValue().getValue());
      }
    }
    return attributed;
  }

  public Graph transform(org.jgrapht.Graph<V, E> g) {
    Graph result = Factory.graph();
    if (g.getType().isAllowingMultipleEdges()) {
      result = result.strict();
    }
    if (g.getType().isDirected()) {
      result = result.directed();
    }
    result = result.named(getGraphId().orElse(DEFAULT_GRAPH_ID));

    for (Map.Entry<String, Attribute> attr :
        graphAttributeProvider.orElse(Collections::emptyMap).get().entrySet()) {
      result = result.graphAttr().with(attr.getKey(), attr.getValue());
    }

    for (V v : g.vertexSet()) {
      boolean skipEdges = false;

      /*
      if (v instanceof Obligation obligation) {
        if (!obligation.isCost()) {
          skipEdges = true;
        }
      }
       */

      Node node = renderAttributes(Factory.node(getVertexId(v)), getVertexAttributes(v));

      // TODO(lorenzleutgeb): Support ports, see https://graphviz.org/docs/attr-types/portPos/

      if (!skipEdges) {
        for (E e : g.outgoingEdgesOf(v)) {
          var target = getVertexId(g.getEdgeTarget(e));

          node = node.link(renderAttributes(to(Factory.node(target)), getEdgeAttributes(e)));
        }
      }

      result = result.with(node);
    }

    return result;
  }
}
