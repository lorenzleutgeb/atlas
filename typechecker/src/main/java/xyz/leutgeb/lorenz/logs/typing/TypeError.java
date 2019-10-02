package xyz.leutgeb.lorenz.logs.typing;

public class TypeError extends Exception {
  public static class NotInContext extends TypeError {
    private String name;

    public NotInContext(String name) {
      this.name = name;
    }

    public String getMessage() {
      return this.name + " was not found in the context";
    }
  }
}
