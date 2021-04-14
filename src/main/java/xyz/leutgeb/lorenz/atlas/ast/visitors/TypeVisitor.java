package xyz.leutgeb.lorenz.atlas.ast.visitors;

import static xyz.leutgeb.lorenz.atlas.util.Util.bug;

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
    throw bug("unkown predefined type");
  }

  @Override
  public Type visitProductType(SplayParser.ProductTypeContext ctx) {
    // TODO(lorenz.leutgeb): Namings.
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
