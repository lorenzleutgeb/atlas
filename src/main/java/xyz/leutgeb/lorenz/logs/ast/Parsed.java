package xyz.leutgeb.lorenz.logs.ast;

import lombok.EqualsAndHashCode;
import lombok.Value;
import org.antlr.v4.runtime.ParserRuleContext;
import org.antlr.v4.runtime.Token;

@Value
@EqualsAndHashCode(callSuper = true)
public class Parsed extends Source {
  ParserRuleContext tree;
  String file;

  /** @link https://www.gnu.org/prep/standards/standards.html#Errors */
  @Override
  public String toString() {
    return file + ":" + tokenToString(tree.getStart()) + "-" + tokenToString(tree.getStop());
  }

  private String tokenToString(Token token) {
    return token.getLine() + "." + token.getCharPositionInLine();
  }
}
