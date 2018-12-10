package xyz.leutgeb.lorenz.logs.visitors;

import java.util.stream.Collectors;
import xyz.leutgeb.lorenz.logs.antlr.SplayBaseVisitor;
import xyz.leutgeb.lorenz.logs.antlr.SplayParser;
import xyz.leutgeb.lorenz.logs.ast.Program;

public class ProgramVisitor extends SplayBaseVisitor<Program> {
  @Override
  public Program visitProgram(SplayParser.ProgramContext ctx) {
    return new Program(
        ctx.func()
            .stream()
            .map((new FunctionDefinitionVisitor())::visit)
            .collect(Collectors.toList()));
  }
}
