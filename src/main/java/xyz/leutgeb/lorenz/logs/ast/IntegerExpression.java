package xyz.leutgeb.lorenz.logs.ast;

import lombok.Data;

@Data
public class IntegerExpression extends ConstantExpression {
  private final Integer value;
}
