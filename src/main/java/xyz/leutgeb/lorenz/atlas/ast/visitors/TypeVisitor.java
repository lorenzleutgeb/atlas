package xyz.leutgeb.lorenz.atlas.ast.visitors;

import java.nio.file.Path;
import java.util.stream.Collectors;
import xyz.leutgeb.lorenz.atlas.antlr.SplayParser;
import xyz.leutgeb.lorenz.atlas.typing.simple.TypeVariable;
import xyz.leutgeb.lorenz.atlas.typing.simple.types.BoolType;
import xyz.leutgeb.lorenz.atlas.typing.simple.types.ProductType;
import xyz.leutgeb.lorenz.atlas.typing.simple.types.TreeType;
import xyz.leutgeb.lorenz.atlas.typing.simple.types.Type;

class TypeVisitor extends SourceNameAwareVisitor<Type> {
  public TypeVisitor(String moduleName, Path path) {
    super(moduleName, path);
  }

  @Override
  public Type visitPredefinedType(SplayParser.PredefinedTypeContext ctx) {
    if (ctx.BOOL() != null) {
      return BoolType.INSTANCE;
    }
    return visitTreeType(ctx.treeType());
  }

  @Override
  public Type visitType(SplayParser.TypeContext ctx) {
    if (ctx.items != null && !ctx.items.isEmpty()) {
      return new ProductType(ctx.items.stream().map(this::visit).collect(Collectors.toList()));
    }
    if (ctx.flat != null) {
      return visitFlatType(ctx.flat);
    }
    throw new UnsupportedOperationException();
  }

  @Override
  public Type visitFlatType(SplayParser.FlatTypeContext ctx) {
    if (ctx.IDENTIFIER() != null) {
      return new TypeVariable(ctx.IDENTIFIER().getText());
    }
    if (ctx.predefinedType() != null) {
      return visitPredefinedType(ctx.predefinedType());
    }
    throw new UnsupportedOperationException();
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
