package xyz.leutgeb.lorenz.logs.unification;

import lombok.RequiredArgsConstructor;
import xyz.leutgeb.lorenz.logs.typing.types.Type;

@RequiredArgsConstructor
public class OccursError extends UnificationError {
  private final Type a;
  private final Type b;

  public String getMessage() {
    return "Cannot create infinite signature " + this.a.toString() + " = " + this.b.toString();
  }
}
