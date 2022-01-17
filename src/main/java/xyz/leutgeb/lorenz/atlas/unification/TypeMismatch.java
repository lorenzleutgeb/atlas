package xyz.leutgeb.lorenz.atlas.unification;

import xyz.leutgeb.lorenz.atlas.ast.sources.Source;
import xyz.leutgeb.lorenz.atlas.typing.simple.types.Type;

public class TypeMismatch extends UnificationError {
  private final Type a;
  private final Type b;

  public TypeMismatch(Type a, Type b, Source source) {
    super(getMessage(a, b), source);
    this.a = a;
    this.b = b;
  }

  public static String getMessage(Type a, Type b) {
    return "Cannot decompose " + a.toString() + " with " + b.toString();
  }
}
