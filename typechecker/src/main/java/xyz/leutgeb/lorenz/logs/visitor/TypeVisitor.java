package xyz.leutgeb.lorenz.logs.visitor;

import xyz.leutgeb.lorenz.logs.antlr.SplayParser;
import xyz.leutgeb.lorenz.logs.typing.TypeVariable;
import xyz.leutgeb.lorenz.logs.typing.types.BoolType;
import xyz.leutgeb.lorenz.logs.typing.types.ProductType;
import xyz.leutgeb.lorenz.logs.typing.types.TreeType;
import xyz.leutgeb.lorenz.logs.typing.types.Type;

import java.nio.file.Path;
import java.util.stream.Collectors;

import static xyz.leutgeb.lorenz.logs.Util.bug;

public class TypeVisitor extends SourceNameAwareVisitor<Type> {
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