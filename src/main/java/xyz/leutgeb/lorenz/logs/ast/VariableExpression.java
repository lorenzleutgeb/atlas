package xyz.leutgeb.lorenz.logs.ast;

import lombok.Data;

@Data
public class VariableExpression extends Expression {
  private final String name;
}
