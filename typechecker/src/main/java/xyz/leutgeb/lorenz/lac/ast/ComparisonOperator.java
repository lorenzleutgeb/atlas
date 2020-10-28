package xyz.leutgeb.lorenz.lac.ast;

import static java.util.function.Predicate.isEqual;

import java.io.PrintStream;
import java.util.Collections;
import java.util.List;
import java.util.function.BiFunction;

public enum ComparisonOperator {
  EQ(List.of("⩵", "=="), (x, y) -> "Objects.equals(" + x + ", " + y + ")"),
  NE(List.of("≠", "!="), (x, y) -> "!Objects.equals(" + x + ", " + y + ")"),
  LT(List.of("⪱", "<"), comparable("<")),
  LE(List.of("⪯", "<="), comparable("<=")),
  GT(List.of("⪲", ">"), comparable(">")),
  GE(List.of("⪰", ">="), comparable(">="));

  private final List<String> tokens;
  private final BiFunction<String, String, String> toJava;

  private static BiFunction<String, String, String> comparable(String operator) {
    return (x, y) -> x + ".compareTo(" + y + ") " + operator + " 0";
  }

  ComparisonOperator(List<String> tokens, BiFunction<String, String, String> toJava) {
    this.toJava = toJava;
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

  public void printHaskellTo(PrintStream out) {
    out.print(tokens.get(1));
  }

  public void printJavaTo(String x, String y, PrintStream out) {
    out.print(toJava.apply(x, y));
  }

  @Override
  public String toString() {
    return tokens.get(0);
  }
}
