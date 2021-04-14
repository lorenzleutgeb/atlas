package xyz.leutgeb.lorenz.atlas.ast.visitors;

import java.nio.file.Path;
import xyz.leutgeb.lorenz.atlas.antlr.SplayParser;
import xyz.leutgeb.lorenz.atlas.typing.simple.TypeClass;
import xyz.leutgeb.lorenz.atlas.typing.simple.TypeConstraint;

class TypeConstraintVisitor extends SourceNameAwareVisitor<TypeConstraint> {
  private final TypeVisitor typeVisitor;

  public TypeConstraintVisitor(String moduleName, Path path, TypeVisitor typeVisitor) {
    super(moduleName, path);
    this.typeVisitor = typeVisitor;
  }

  @Override
  public TypeConstraint visitConstraint(SplayParser.ConstraintContext ctx) {
    var typeVariable =
        ctx.treeType() != null
            ? typeVisitor.visitTreeType(ctx.treeType())
            : typeVisitor.visitVariableType(ctx.variableType());
    return new TypeConstraint(
        ctx.typeClass().TYC_EQ() != null ? TypeClass.EQ : TypeClass.ORD, typeVariable);
  }
}
