package xyz.leutgeb.lorenz.lac.typing.simple;

public class TypeError extends Exception {
  public TypeError() {}

  public TypeError(String message) {
    super(message);
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
