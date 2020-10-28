package xyz.leutgeb.lorenz.lac.typing.resources.solving;

public abstract class ConstraintSystemException extends Exception {
  ConstraintSystemException(String message) {
    super(message);
  }
}
