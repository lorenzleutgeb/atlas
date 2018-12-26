package xyz.leutgeb.lorenz.logs.visitor;

import java.util.stream.Collectors;
import xyz.leutgeb.lorenz.logs.SymbolTable;
import xyz.leutgeb.lorenz.logs.antlr.SplayBaseVisitor;
import xyz.leutgeb.lorenz.logs.antlr.SplayParser;
import xyz.leutgeb.lorenz.logs.ast.Program;

public class ProgramVisitor extends SplayBaseVisitor<Program> {
  private SymbolTable symbolTable = SymbolTable.root();

  @Override
  public Program visitProgram(SplayParser.ProgramContext ctx) {
    return new Program(
        ctx.func()
            .stream()
            .map((new FunctionDefinitionVisitor(symbolTable))::visit)
            .collect(Collectors.toList()));
  }
}
