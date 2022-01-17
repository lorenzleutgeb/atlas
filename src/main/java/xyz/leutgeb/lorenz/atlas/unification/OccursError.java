package xyz.leutgeb.lorenz.atlas.unification;

import xyz.leutgeb.lorenz.atlas.ast.sources.Source;
import xyz.leutgeb.lorenz.atlas.typing.simple.types.Type;

public class OccursError extends UnificationError {
  private final Type a;
  private final Type b;

  public OccursError(Type a, Type b, Source source) {
    super(getMessage(a, b), source);
    this.a = a;
    this.b = b;
  }

  public static String getMessage(Type a, Type b) {
    return "Cannot create infinite signature " + a.toString() + " = " + b.toString();
  }
}
