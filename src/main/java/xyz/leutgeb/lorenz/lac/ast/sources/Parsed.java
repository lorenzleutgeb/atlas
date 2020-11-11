package xyz.leutgeb.lorenz.lac.ast.sources;

import java.nio.file.Path;
import lombok.EqualsAndHashCode;
import lombok.Value;
import org.antlr.v4.runtime.ParserRuleContext;
import org.antlr.v4.runtime.Token;

@Value
@EqualsAndHashCode(callSuper = true)
public class Parsed extends Source {
  ParserRuleContext tree;
  Path path;

  /** @link https://www.gnu.org/prep/standards/standards.html#Errors */
  @Override
  public String toString() {
    return path + ":" + tokenToString(tree.getStart()) + "-" + tokenToString(tree.getStop());
  }

  private String tokenToString(Token token) {
    return token.getLine() + "." + token.getCharPositionInLine();
  }

  public Parsed relativize(Path other) {
    return new Parsed(tree, path.relativize(other));
  }
}
