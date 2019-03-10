package xyz.leutgeb.lorenz.logs.unification;

import lombok.RequiredArgsConstructor;
import xyz.leutgeb.lorenz.logs.type.Type;

@RequiredArgsConstructor
public class OccursError extends UnificationError {
  private final Type a;
  private final Type b;

  public String getMessage() {
    return "Cannot create infinite type " + this.a.toString() + " = " + this.b.toString();
  }
}
