package xyz.leutgeb.lorenz.logs.type;

import lombok.Value;

@Value
public class TypeVar extends Type {
  String name;

  public String toString() {
    return name;
  }
}
