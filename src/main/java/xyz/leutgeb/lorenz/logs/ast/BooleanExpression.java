package xyz.leutgeb.lorenz.logs.ast;

import lombok.Data;

@Data
public class BooleanExpression extends Expression {
  private final Expression left;
  private final ComparisonOperator operator;
  private final Expression right;
}
