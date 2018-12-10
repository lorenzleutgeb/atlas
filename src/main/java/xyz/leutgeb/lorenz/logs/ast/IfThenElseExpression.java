package xyz.leutgeb.lorenz.logs.ast;

import lombok.Data;

@Data
public class IfThenElseExpression extends Expression {
  private final BooleanExpression condition;
  private final Expression truthy;
  private final Expression falsy;
}
