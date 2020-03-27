package xyz.leutgeb.lorenz.lac.ast.visitors;

import static xyz.leutgeb.lorenz.lac.Util.bug;

import java.nio.file.Path;
import java.util.stream.Collectors;
import xyz.leutgeb.lorenz.lac.antlr.SplayParser;
import xyz.leutgeb.lorenz.lac.typing.simple.TypeVariable;
import xyz.leutgeb.lorenz.lac.typing.simple.types.BoolType;
import xyz.leutgeb.lorenz.lac.typing.simple.types.ProductType;
import xyz.leutgeb.lorenz.lac.typing.simple.types.TreeType;
import xyz.leutgeb.lorenz.lac.typing.simple.types.Type;

class TypeVisitor extends SourceNameAwareVisitor<Type> {
  public TypeVisitor(String moduleName, Path path) {
    super(moduleName, path);
  }

  @Override
  public Type visitPredefinedType(SplayParser.PredefinedTypeContext ctx) {
    if (ctx.BOOL() != null) {
      return BoolType.INSTANCE;
    }
    throw bug("unkown predefined type");
  }

  @Override
  public Type visitProductType(SplayParser.ProductTypeContext ctx) {
    // TODO: Namings.
    return new ProductType(ctx.items.stream().map(this::visit).collect(Collectors.toList()));
  }

  @Override
  public Type visitVariableType(SplayParser.VariableTypeContext ctx) {
    return new TypeVariable(ctx.IDENTIFIER().getText());
  }

  @Override
  public Type visitTreeType(SplayParser.TreeTypeContext ctx) {
    return new TreeType((TypeVariable) visitVariableType(ctx.variableType()));
  }
}
