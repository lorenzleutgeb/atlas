package xyz.leutgeb.lorenz.logs.resources;

import lombok.Value;
import xyz.leutgeb.lorenz.logs.type.Type;

@Value
public class AnnotatedType {
  Type type;
  Annotation annotation;

  @Override
  public String toString() {
    return type + " | " + annotation;
  }
}
