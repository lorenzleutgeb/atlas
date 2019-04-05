package xyz.leutgeb.lorenz.logs.ast;

public class Predefined extends Source {
  public static final Predefined INSTANCE = new Predefined();

  @Override
  public String toString() {
    return "<predefined>";
  }
}
