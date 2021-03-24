package xyz.leutgeb.lorenz.lac.util;

import lombok.EqualsAndHashCode;
import lombok.Value;

@Value
@EqualsAndHashCode(callSuper = true)
public class DependencyEdge extends KindedEdge<DependencyEdge.Kind> {
  private DependencyEdge(Kind kind) {
    super(kind);
  }

  public static DependencyEdge source() {
    return new DependencyEdge(Kind.SOURCE);
  }

  public static DependencyEdge synthetic() {
    return new DependencyEdge(Kind.SYNTHETIC);
  }

  public enum Kind {
    /** This dependency edge reflects a direct dependency in the code. */
    SOURCE,

    /** This dependency edge was added. */
    SYNTHETIC
  }
}
