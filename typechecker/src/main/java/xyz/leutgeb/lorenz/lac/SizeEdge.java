package xyz.leutgeb.lorenz.lac;

import java.util.Objects;
import lombok.Value;
import org.jgrapht.graph.DefaultEdge;

@Value
public class SizeEdge extends DefaultEdge {
  Kind kind;

  public static SizeEdge eq() {
    return new SizeEdge(Kind.EQ);
  }

  public static SizeEdge gt() {
    return new SizeEdge(Kind.GT);
  }

  @Override
  public boolean equals(Object o) {
    if (this == o) {
      return true;
    }
    if (o == null || getClass() != o.getClass()) {
      return false;
    }

    SizeEdge sizeEdge = (SizeEdge) o;

    return Objects.equals(kind, sizeEdge.kind)
        && Objects.equals(getSource(), sizeEdge.getSource())
        && Objects.equals(getTarget(), sizeEdge.getTarget());
  }

  @Override
  public int hashCode() {
    return Objects.hash(kind, getSource(), getTarget());
  }

  public static enum Kind {
    EQ,
    GT
  }
}
