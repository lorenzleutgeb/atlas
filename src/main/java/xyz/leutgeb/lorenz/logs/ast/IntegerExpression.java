package xyz.leutgeb.lorenz.logs.ast;

import lombok.Data;

@Data
@Deprecated
public class IntegerExpression extends Expression {
  private final Integer value;
}
