package xyz.leutgeb.lorenz.atlas.typing.simple;

import xyz.leutgeb.lorenz.atlas.ast.sources.Source;

public class TypeError extends Exception {
  private Source source;

  @Deprecated
  public TypeError() {}

  @Deprecated
  public TypeError(String message) {
    this(message, null);
  }

  @Override
  public String getMessage() {
    final var result = source == null ? "" : ("At " + source.getRoot() + ": ");
    return result + super.getMessage();
  }

  public TypeError(String message, Source source) {
    super(message);
    this.source = source;
  }

  public static class NotInContext extends TypeError {
    private final String name;

    public NotInContext(String name) {
      this.name = name;
    }

    @Override
    public String getMessage() {
      return this.name + " was not found in the context";
    }
  }

  public static class AnnotationMismatch extends TypeError {
    private final String fqn;
    private final FunctionSignature annotated;
    private final FunctionSignature inferred;

    public AnnotationMismatch(String fqn, FunctionSignature annotated, FunctionSignature inferred) {
      this.fqn = fqn;
      this.annotated = annotated;
      this.inferred = inferred;
    }

    @Override
    public String getMessage() {
      return "The signature for "
          + fqn
          + " was annotated as "
          + annotated
          + " but inferred as "
          + inferred;
    }
  }
}
