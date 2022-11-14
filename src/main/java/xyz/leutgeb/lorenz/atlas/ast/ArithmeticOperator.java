package xyz.leutgeb.lorenz.atlas.ast;

import java.io.PrintStream;
import java.util.Collections;
import java.util.List;
import java.util.function.BiFunction;

import static java.util.function.Predicate.isEqual;

public enum ArithmeticOperator {
  PLUS(List.of("+")),
  MINUS(List.of("-")),
  TIMES(List.of("*", "⨯")),
  DIV(List.of("/"));

  private final List<String> tokens;

  ArithmeticOperator(List<String> tokens) {
    if (tokens.isEmpty()) {
      throw new IllegalArgumentException();
    }
    this.tokens = Collections.unmodifiableList(tokens);
  }

  public static ArithmeticOperator fromToken(String token) {
    for (ArithmeticOperator op : ArithmeticOperator.values()) {
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
    printTo(out);
  }

  public void printJavaTo(String x, String y, PrintStream out) {
    out.print(x);
    out.print(" ");
    printTo(out);
    out.print(" ");
    out.print(y);
  }

  @Override
  public String toString() {
    return tokens.get(0);
  }
}
