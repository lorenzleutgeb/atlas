package xyz.leutgeb.lorenz.logs.ast;

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
}
