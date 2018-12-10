package xyz.leutgeb.lorenz.logs.ast;

public class Nil extends ConstantExpression {
  private static final Nil INSTANCE = new Nil();

  public static Nil getInstance() {
    return INSTANCE;
  }

  private Nil() {}
}
