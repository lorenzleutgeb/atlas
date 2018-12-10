package xyz.leutgeb.lorenz.logs.ast;

import java.util.List;
import lombok.Data;

@Data
public class FunctionDefinition {
  private final String name;
  private final List<String> arguments;
  private final Expression body;
}
