package xyz.leutgeb.lorenz.logs.visitor;

import java.nio.file.Path;
import lombok.Getter;
import lombok.RequiredArgsConstructor;
import org.antlr.v4.runtime.ParserRuleContext;
import xyz.leutgeb.lorenz.logs.antlr.SplayBaseVisitor;
import xyz.leutgeb.lorenz.logs.ast.sources.Parsed;
import xyz.leutgeb.lorenz.logs.ast.sources.Source;

@RequiredArgsConstructor
public class SourceNameAwareVisitor<T> extends SplayBaseVisitor<T> {
  @Getter private final String moduleName;
  @Getter private final Path path;

  protected Source getSource(ParserRuleContext context) {
    return new Parsed(context, moduleName);
  }
}
