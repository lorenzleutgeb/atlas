package xyz.leutgeb.lorenz.logs.visitor;

import java.nio.file.Path;
import java.util.List;
import java.util.stream.Collectors;
import xyz.leutgeb.lorenz.logs.antlr.SplayParser;
import xyz.leutgeb.lorenz.logs.ast.FunctionDefinition;

public class ProgramVisitor extends SourceNameAwareVisitor<List<FunctionDefinition>> {
  public ProgramVisitor(String moduleName, Path path) {
    super(moduleName, path);
  }

  @Override
  public List<FunctionDefinition> visitProgram(SplayParser.ProgramContext ctx) {
    return ctx.func().stream()
        .map((new FunctionDefinitionVisitor(getModuleName(), getPath()))::visit)
        .collect(Collectors.toList());
  }
}
