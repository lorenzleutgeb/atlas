package xyz.leutgeb.lorenz.logs.visitor;

import lombok.Getter;
import lombok.RequiredArgsConstructor;
import org.antlr.v4.runtime.ParserRuleContext;
import xyz.leutgeb.lorenz.logs.antlr.SplayBaseVisitor;
import xyz.leutgeb.lorenz.logs.ast.Parsed;
import xyz.leutgeb.lorenz.logs.ast.Source;

@RequiredArgsConstructor
public class SourceNameAwareVisitor<T> extends SplayBaseVisitor<T> {
  @Getter private final String sourceName;

  protected Source getSource(ParserRuleContext context) {
    return new Parsed(context, sourceName);
  }
}
