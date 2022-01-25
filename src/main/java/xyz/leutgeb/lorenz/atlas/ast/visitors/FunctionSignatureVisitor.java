package xyz.leutgeb.lorenz.atlas.ast.visitors;

import java.nio.file.Path;
import java.util.ArrayList;
import java.util.Collections;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Optional;
import java.util.Set;
import java.util.stream.Collectors;
import org.antlr.v4.runtime.Token;
import org.hipparchus.fraction.Fraction;
import xyz.leutgeb.lorenz.atlas.antlr.SplayParser;
import xyz.leutgeb.lorenz.atlas.typing.resources.Annotation;
import xyz.leutgeb.lorenz.atlas.typing.resources.CombinedFunctionAnnotation;
import xyz.leutgeb.lorenz.atlas.typing.resources.FunctionAnnotation;
import xyz.leutgeb.lorenz.atlas.typing.resources.coefficients.Coefficient;
import xyz.leutgeb.lorenz.atlas.typing.resources.coefficients.KnownCoefficient;
import xyz.leutgeb.lorenz.atlas.typing.simple.FunctionSignature;
import xyz.leutgeb.lorenz.atlas.typing.simple.TypeConstraint;
import xyz.leutgeb.lorenz.atlas.typing.simple.types.FunctionType;
import xyz.leutgeb.lorenz.atlas.typing.simple.types.ProductType;
import xyz.leutgeb.lorenz.atlas.typing.simple.types.Type;

class FunctionSignatureVisitor extends SourceNameAwareVisitor<FunctionSignature> {
  private final TypeVisitor typeVisitor;
  private final TypeConstraintVisitor typeConstraintVisitor;

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
    Optional<CombinedFunctionAnnotation> annotation = Optional.empty();

    final ProductType from = ProductType.wrap(typeVisitor.visit(ctx.from));
    final Type to = typeVisitor.visit(ctx.to);

    if (ctx.annotatedAnnotation != null) {
      annotation =
          Optional.of(
              new CombinedFunctionAnnotation(
                  new FunctionAnnotation(
                      convert(from.countTrees().get(), ctx.annotatedAnnotation.with.from),
                      convert(to.countTrees().get(), ctx.annotatedAnnotation.with.to)),

                  // TODO: Parse cf-annotations.
                  ctx.annotatedAnnotation.without.stream()
                      .map(
                          cf ->
                              new FunctionAnnotation(
                                  convert(from.countTrees().get(), cf.from),
                                  convert(to.countTrees().get(), cf.to)))
                      .collect(Collectors.toSet())));
    }

    return new FunctionSignature(typeConstraints, new FunctionType(from, to), annotation);
  }

  public static Annotation convert(int size, SplayParser.AnnotationContext annotationContext) {
    // if (annotationContext instanceof SplayParser.DontCareAnnotationContext) {
    //	return Optional.empty();
    // }
    if (annotationContext instanceof SplayParser.ZeroAnnotationContext) {
      final var start = annotationContext.getStart();
      return Annotation.zero(
          size,
          "fixed in source code at position "
              + start.getLine()
              + ":"
              + start.getCharPositionInLine());
    }
    if (annotationContext instanceof final SplayParser.NonEmptyAnnotationContext context) {
      List<Coefficient> rankCoefficients = new ArrayList<>(size);
      for (int i = 0; i < size; i++) {
        rankCoefficients.add(KnownCoefficient.ZERO);
      }
      Map<List<Integer>, Coefficient> coeffiecients = new HashMap<>();
      for (var entry : context.entries) {
        final var index = entry.index();
        final var value = convert(entry.coefficient);
        if (index instanceof SplayParser.RankIndexContext) {
          final var rankIndex = (SplayParser.RankIndexContext) index;
          rankCoefficients.set(Integer.parseInt(rankIndex.NUMBER().getText()), value);
        } else if (index instanceof SplayParser.OtherIndexContext) {
          final var otherIndex = (SplayParser.OtherIndexContext) index;
          coeffiecients.put(
              otherIndex.elements.stream()
                  .map(Token::getText)
                  .map(Integer::parseInt)
                  .collect(Collectors.toUnmodifiableList()),
              value);
        }
      }
      final var start = context.getStart();
      return new Annotation(
          rankCoefficients,
          coeffiecients,
          "fixed at position " + start.getLine() + ":" + start.getCharPositionInLine());
    }
    throw new IllegalArgumentException("cannot convert context");
  }

  private static KnownCoefficient convert(SplayParser.NumberContext context) {
    if (context instanceof SplayParser.NatContext) {
      return new KnownCoefficient(new Fraction(Integer.parseInt(context.getText())));
    }
    if (context instanceof final SplayParser.RatContext ratContext) {
      return new KnownCoefficient(
          new Fraction(
              Integer.parseInt(ratContext.numerator.getText()),
              Integer.parseInt(ratContext.denominator.getText())));
    }
    throw new IllegalArgumentException("cannot convert context");
  }
}
