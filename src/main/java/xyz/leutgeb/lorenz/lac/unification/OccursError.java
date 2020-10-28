package xyz.leutgeb.lorenz.lac.unification;

import lombok.RequiredArgsConstructor;
import xyz.leutgeb.lorenz.lac.typing.simple.types.Type;

@RequiredArgsConstructor
public class OccursError extends UnificationError {
  private final Type a;
  private final Type b;

  public String getMessage() {
    return "Cannot create infinite signature " + this.a.toString() + " = " + this.b.toString();
  }
}
