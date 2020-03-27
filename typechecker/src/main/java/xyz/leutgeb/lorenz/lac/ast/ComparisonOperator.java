package xyz.leutgeb.lorenz.lac.ast;

import java.io.PrintStream;

public enum ComparisonOperator {
  EQ("=="),
  NE("!="),
  LT("<"),
  LE("<="),
  GT(">"),
  GE(">=");

  private final String token;

  ComparisonOperator(String token) {
    this.token = token;
  }

  public static ComparisonOperator fromToken(String token) {
    for (ComparisonOperator op : ComparisonOperator.values()) {
      if (op.token.equals(token)) {
        return op;
      }
    }
    throw new IllegalArgumentException();
  }

  public void printTo(PrintStream out) {
    out.print(token);
  }

  @Override
  public String toString() {
    return token;
  }
}
