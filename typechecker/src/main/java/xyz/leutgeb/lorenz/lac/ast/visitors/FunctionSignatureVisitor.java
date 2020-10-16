package xyz.leutgeb.lorenz.lac.ast.visitors;

import static xyz.leutgeb.lorenz.lac.util.Util.bug;

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
import xyz.leutgeb.lorenz.lac.antlr.SplayParser;
import xyz.leutgeb.lorenz.lac.typing.resources.Annotation;
import xyz.leutgeb.lorenz.lac.typing.resources.FunctionAnnotation;
import xyz.leutgeb.lorenz.lac.typing.resources.coefficients.Coefficient;
import xyz.leutgeb.lorenz.lac.typing.resources.coefficients.KnownCoefficient;
import xyz.leutgeb.lorenz.lac.typing.simple.FunctionSignature;
import xyz.leutgeb.lorenz.lac.typing.simple.TypeConstraint;
import xyz.leutgeb.lorenz.lac.typing.simple.types.FunctionType;
import xyz.leutgeb.lorenz.lac.typing.simple.types.ProductType;
import xyz.leutgeb.lorenz.lac.typing.simple.types.TreeType;
import xyz.leutgeb.lorenz.lac.typing.simple.types.Type;
import xyz.leutgeb.lorenz.lac.util.Fraction;

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
    final var arrow = ctx.arrow();
    Set<TypeConstraint> typeConstraints =
        ctx.constraints() != null
            ? ctx.constraints().items.stream()
                .map(typeConstraintVisitor::visitConstraint)
                .collect(Collectors.toSet())
            : Collections.emptySet();
    ProductType from = null;
    Type to;
    Optional<FunctionAnnotation> annotation = Optional.empty();
    if (arrow instanceof SplayParser.SimpleArrowContext simpleArrow) {
      from = (ProductType) typeVisitor.visitProductType(simpleArrow.from);
      to = typeVisitor.visit(simpleArrow.to);
    } else if (arrow instanceof SplayParser.AnnotatedArrowContext annotatedArrow) {
      from = (ProductType) typeVisitor.visitProductType(annotatedArrow.from);
      to = typeVisitor.visit(annotatedArrow.to);
      annotation =
          Optional.of(
              new FunctionAnnotation(
                  convert((int) from.treeSize(), annotatedArrow.fromAnnotation),
                  convert(to instanceof TreeType ? 1 : 0, annotatedArrow.toAnnotation)));
    } else {
      throw bug("unknown arrow");
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
    if (annotationContext instanceof SplayParser.NonEmptyAnnotationContext context) {
      List<Coefficient> rankCoefficients = new ArrayList<>(size);
      for (int i = 0; i < size; i++) {
        rankCoefficients.add(KnownCoefficient.ZERO);
      }
      Map<List<Integer>, Coefficient> coeffiecients = new HashMap<>();
      for (var entry : context.entries) {
        final var index = entry.index();
        final var value = convert(entry.coefficient);
        if (index instanceof SplayParser.RankIndexContext rankIndex) {
          rankCoefficients.set(Integer.parseInt(rankIndex.NUMBER().getText()), value);
        } else if (index instanceof SplayParser.OtherIndexContext otherIndex) {
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
    if (context instanceof SplayParser.RatContext ratContext) {
      return new KnownCoefficient(
          new Fraction(
              Integer.parseInt(ratContext.numerator.getText()),
              Integer.parseInt(ratContext.denominator.getText())));
    }
    throw new IllegalArgumentException("cannot convert context");
  }
}
