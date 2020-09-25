package xyz.leutgeb.lorenz.lac.typing.resources;

public record FunctionAnnotation(Annotation from, Annotation to) {
  @Override
  public String toString() {
    return from.toString() + " → " + to.toString();
  }

  public boolean isUnknown() {
    return from.isUnknown() || to.isUnknown();
  }
}
