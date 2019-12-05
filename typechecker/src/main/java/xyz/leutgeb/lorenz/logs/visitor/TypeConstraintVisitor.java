package xyz.leutgeb.lorenz.logs.visitor;

import xyz.leutgeb.lorenz.logs.antlr.SplayParser;
import xyz.leutgeb.lorenz.logs.typing.TypeClass;
import xyz.leutgeb.lorenz.logs.typing.TypeConstraint;

import java.nio.file.Path;

public class TypeConstraintVisitor extends SourceNameAwareVisitor<TypeConstraint> {
	private final TypeVisitor typeVisitor;

	public TypeConstraintVisitor(String moduleName, Path path, TypeVisitor typeVisitor) {
		super(moduleName, path);
		this.typeVisitor = typeVisitor;
	}

	@Override
	public TypeConstraint visitConstraint(SplayParser.ConstraintContext ctx) {
		var typeVariable = ctx.treeType() != null ? typeVisitor.visitTreeType(ctx.treeType()) : typeVisitor.visitVariableType(ctx.variableType());
		return new TypeConstraint(ctx.typeClass().TYC_EQ() != null ? TypeClass.EQ : TypeClass.ORD, typeVariable);
	}
}