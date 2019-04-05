package xyz.leutgeb.lorenz.logs.visitor;

import java.util.stream.Collectors;
import lombok.RequiredArgsConstructor;
import xyz.leutgeb.lorenz.logs.antlr.SplayBaseVisitor;
import xyz.leutgeb.lorenz.logs.antlr.SplayParser;
import xyz.leutgeb.lorenz.logs.ast.Program;

@RequiredArgsConstructor
public class ProgramVisitor extends SplayBaseVisitor<Program> {
  private final String sourceName;

  @Override
  public Program visitProgram(SplayParser.ProgramContext ctx) {
    return new Program(
        ctx.func()
            .stream()
            .map((new FunctionDefinitionVisitor(sourceName))::visit)
            .collect(Collectors.toList()));
  }
}
