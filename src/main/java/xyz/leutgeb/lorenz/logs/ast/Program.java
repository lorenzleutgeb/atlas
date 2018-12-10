package xyz.leutgeb.lorenz.logs.ast;

import java.util.List;
import lombok.Data;

@Data
public class Program {
  private final List<FunctionDefinition> functionDefinitions;
}
