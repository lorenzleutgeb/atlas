package xyz.leutgeb.lorenz.lac.typing.simple.types;

public class NumType extends Type {
  public static final NumType INSTANCE = new NumType();

  @Override
  public String toHaskell() {
    return "Num";
  }
}
