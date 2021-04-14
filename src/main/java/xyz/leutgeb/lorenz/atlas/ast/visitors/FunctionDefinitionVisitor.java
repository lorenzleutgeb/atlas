package xyz.leutgeb.lorenz.atlas.ast.visitors;

import java.nio.file.Path;
import java.util.ArrayList;
import java.util.List;
import org.antlr.v4.runtime.Token;
import xyz.leutgeb.lorenz.atlas.antlr.SplayParser;
import xyz.leutgeb.lorenz.atlas.ast.FunctionDefinition;
import xyz.leutgeb.lorenz.atlas.typing.simple.FunctionSignature;

class FunctionDefinitionVisitor extends SourceNameAwareVisitor<FunctionDefinition> {
  private final ExpressionVisitor expressionVisitor;
  private final FunctionSignatureVisitor functionSignatureVisitor;

  public FunctionDefinitionVisitor(String moduleName, Path path) {
    super(moduleName, path);
    expressionVisitor = new ExpressionVisitor(getModuleName(), getPath());
    functionSignatureVisitor = new FunctionSignatureVisitor(getModuleName(), getPath());
  }

  @Override
  public FunctionDefinition visitFunc(SplayParser.FuncContext ctx) {
    List<String> args = new ArrayList<>(ctx.args.size());
    for (Token t : ctx.args) {
      String arg = t.getText();
      args.add(arg);
    }
    FunctionSignature functionSignature = null;
    if (ctx.signature() != null) {
      if (!ctx.signature().name.getText().equals(ctx.name.getText())) {
        throw new RuntimeException(
            "mismatching name in type annotation and definition (\""
                + ctx.signature().name.getText()
                + "\" vs. \""
                + ctx.name.getText()
                + "\"");
      }
      functionSignature = functionSignatureVisitor.visitSignature(ctx.signature());
    }
    return new FunctionDefinition(
        getModuleName(),
        ctx.name.getText(),
        args,
        expressionVisitor.visit(ctx.body),
        functionSignature);
  }
}
