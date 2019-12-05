package xyz.leutgeb.lorenz.logs.visitor;

import org.antlr.v4.runtime.Token;
import xyz.leutgeb.lorenz.logs.antlr.SplayParser;
import xyz.leutgeb.lorenz.logs.ast.FunctionDefinition;
import xyz.leutgeb.lorenz.logs.typing.FunctionSignature;

import java.nio.file.Path;
import java.util.ArrayList;
import java.util.List;

public class FunctionDefinitionVisitor extends SourceNameAwareVisitor<FunctionDefinition> {
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
        throw new RuntimeException("mismatching name in type annotation and definition");
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
