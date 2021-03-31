package xyz.leutgeb.lorenz.lac.util;

import java.util.Objects;
import org.jgrapht.graph.DefaultEdge;

public class KindedEdge<K> extends DefaultEdge {
  protected K kind;

  protected KindedEdge(K kind) {
    this.kind = kind;
  }

  public K getKind() {
    return kind;
  }

  @Override
  public boolean equals(Object o) {
    if (this == o) {
      return true;
    }
    if (o == null || getClass() != o.getClass()) {
      return false;
    }

    KindedEdge sizeEdge = (KindedEdge) o;

    return Objects.equals(kind, sizeEdge.kind)
        && Objects.equals(getSource(), sizeEdge.getSource())
        && Objects.equals(getTarget(), sizeEdge.getTarget());
  }

  @Override
  public int hashCode() {
    return Objects.hash(kind, getSource(), getTarget());
  }
}
