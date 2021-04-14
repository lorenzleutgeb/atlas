package xyz.leutgeb.lorenz.atlas.util;

import lombok.EqualsAndHashCode;
import lombok.Value;

@Value
@EqualsAndHashCode(callSuper = true)
public class SizeEdge extends KindedEdge<SizeEdge.Kind> {
  private SizeEdge(Kind kind) {
    super(kind);
  }

  public static SizeEdge eq() {
    return new SizeEdge(Kind.EQ);
  }

  public static SizeEdge gt() {
    return new SizeEdge(Kind.GT);
  }

  public enum Kind {
    /** Source is "equal to" target. */
    EQ,

    /** Source is "greater than" target. */
    GT
  }
}
