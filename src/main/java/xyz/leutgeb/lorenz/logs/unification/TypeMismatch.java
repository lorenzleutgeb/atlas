package xyz.leutgeb.lorenz.logs.unification;

import lombok.RequiredArgsConstructor;
import xyz.leutgeb.lorenz.logs.type.Type;

@RequiredArgsConstructor
public class TypeMismatch extends UnificationError {
  private final Type a;
  private final Type b;

  public String getMessage() {
    return "Cannot decompose " + this.a.toString() + " with " + this.b.toString();
  }
}
