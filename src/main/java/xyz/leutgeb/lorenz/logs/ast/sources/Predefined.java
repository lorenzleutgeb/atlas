package xyz.leutgeb.lorenz.logs.ast.sources;

public class Predefined extends Source {
  public static final Predefined INSTANCE = new Predefined();

  @Override
  public String toString() {
    return "<predefined>";
  }
}
