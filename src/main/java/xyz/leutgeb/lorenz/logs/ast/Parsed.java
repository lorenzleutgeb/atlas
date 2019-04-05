package xyz.leutgeb.lorenz.logs.ast;

import lombok.Value;
import org.antlr.v4.runtime.ParserRuleContext;

@Value
public class Parsed extends Source {
  ParserRuleContext tree;
  String file;
}
