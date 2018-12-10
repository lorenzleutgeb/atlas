package xyz.leutgeb.lorenz.logs.ast;

import java.util.List;
import lombok.Data;

@Data
public class CallExpression extends Expression {
  private final String name;
  private final List<Expression> parameters;
}
