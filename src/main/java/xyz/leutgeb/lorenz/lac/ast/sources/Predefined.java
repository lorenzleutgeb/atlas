package xyz.leutgeb.lorenz.lac.ast.sources;

public class Predefined extends Source {
  public static final Predefined INSTANCE = new Predefined();

  @Override
  public String toString() {
    return "_PREDEFINED_";
  }
}
