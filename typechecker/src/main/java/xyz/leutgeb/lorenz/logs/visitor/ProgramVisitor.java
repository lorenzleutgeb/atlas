package xyz.leutgeb.lorenz.logs.visitor;

import static java.util.stream.Collectors.toMap;

import java.util.LinkedHashMap;
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
            .collect(
                toMap(
                    (SplayParser.FuncContext x) -> x.name.getText(),
                    (new FunctionDefinitionVisitor(sourceName))::visit,
                    (a, b) -> {
                      throw new RuntimeException("clashing function definitions");
                    },
                    LinkedHashMap::new)));
  }
}
