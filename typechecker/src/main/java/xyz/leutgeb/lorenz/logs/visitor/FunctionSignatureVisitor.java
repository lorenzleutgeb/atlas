package xyz.leutgeb.lorenz.logs.visitor;

import xyz.leutgeb.lorenz.logs.antlr.SplayParser;
import xyz.leutgeb.lorenz.logs.typing.FunctionSignature;
import xyz.leutgeb.lorenz.logs.typing.TypeConstraint;
import xyz.leutgeb.lorenz.logs.typing.types.FunctionType;
import xyz.leutgeb.lorenz.logs.typing.types.ProductType;
import xyz.leutgeb.lorenz.logs.typing.types.Type;

import java.nio.file.Path;
import java.util.Collections;
import java.util.Set;
import java.util.stream.Collectors;

public class FunctionSignatureVisitor extends SourceNameAwareVisitor<FunctionSignature> {
  private TypeVisitor typeVisitor;
  private TypeConstraintVisitor typeConstraintVisitor;

  public FunctionSignatureVisitor(String moduleName, Path path) {
    super(moduleName, path);
    this.typeVisitor = new TypeVisitor(moduleName, path);
    this.typeConstraintVisitor = new TypeConstraintVisitor(moduleName, path, typeVisitor);
  }

  @Override
  public FunctionSignature visitSignature(SplayParser.SignatureContext ctx) {
    if (ctx == null) {
      return null;
    }
    Set<TypeConstraint> typeConstraints =
        ctx.constraints() != null
            ? ctx.constraints().items.stream()
                .map(typeConstraintVisitor::visitConstraint)
                .collect(Collectors.toSet())
            : Collections.emptySet();
    ProductType from = (ProductType) typeVisitor.visitProductType(ctx.from);
    Type to = typeVisitor.visit(ctx.to);
    return new FunctionSignature(typeConstraints, new FunctionType(from, to));
  }
}
