package xyz.leutgeb.lorenz.lac.ast;

import static java.util.function.Predicate.isEqual;

import java.io.PrintStream;
import java.util.Collections;
import java.util.List;

public enum ComparisonOperator {
  EQ(List.of("⩵", "==")),
  NE(List.of("≠", "!=")),
  LT(List.of("⪱", "<")),
  LE(List.of("⪯", "<=")),
  GT(List.of("⪲", ">")),
  GE(List.of("⪰", ">="));

  private final List<String> tokens;

  ComparisonOperator(String token) {
    this(Collections.singletonList(token));
  }

  ComparisonOperator(List<String> tokens) {
    if (tokens.isEmpty()) {
      throw new IllegalArgumentException();
    }
    this.tokens = Collections.unmodifiableList(tokens);
  }

  public static ComparisonOperator fromToken(String token) {
    for (ComparisonOperator op : ComparisonOperator.values()) {
      if (op.tokens.stream().anyMatch(isEqual(token))) {
        return op;
      }
    }
    throw new IllegalArgumentException();
  }

  public void printTo(PrintStream out) {
    out.print(tokens.get(0));
  }

  @Override
  public String toString() {
    return tokens.get(0);
  }
}
