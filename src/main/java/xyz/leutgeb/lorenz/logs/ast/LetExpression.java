package xyz.leutgeb.lorenz.logs.ast;

import lombok.Data;

@Data
public class LetExpression extends Expression {
  private final String declared;
  private final Expression body;
}
