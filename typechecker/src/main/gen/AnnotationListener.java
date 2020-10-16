// Generated from
// /home/lorenz/src/github.com/lorenzleutgeb/lac/typechecker/src/main/antlr/xyz/leutgeb/lorenz/lac/antlr/Annotation.g4 by ANTLR 4.8
import org.antlr.v4.runtime.tree.ParseTreeListener;

/**
 * This interface defines a complete listener for a parse tree produced by {@link AnnotationParser}.
 */
public interface AnnotationListener extends ParseTreeListener {
  /**
   * Enter a parse tree produced by the {@code nonEmptyAnnotation} labeled alternative in {@link
   * AnnotationParser#annotation}.
   *
   * @param ctx the parse tree
   */
  void enterNonEmptyAnnotation(AnnotationParser.NonEmptyAnnotationContext ctx);
  /**
   * Exit a parse tree produced by the {@code nonEmptyAnnotation} labeled alternative in {@link
   * AnnotationParser#annotation}.
   *
   * @param ctx the parse tree
   */
  void exitNonEmptyAnnotation(AnnotationParser.NonEmptyAnnotationContext ctx);
  /**
   * Enter a parse tree produced by the {@code zeroAnnotation} labeled alternative in {@link
   * AnnotationParser#annotation}.
   *
   * @param ctx the parse tree
   */
  void enterZeroAnnotation(AnnotationParser.ZeroAnnotationContext ctx);
  /**
   * Exit a parse tree produced by the {@code zeroAnnotation} labeled alternative in {@link
   * AnnotationParser#annotation}.
   *
   * @param ctx the parse tree
   */
  void exitZeroAnnotation(AnnotationParser.ZeroAnnotationContext ctx);
  /**
   * Enter a parse tree produced by {@link AnnotationParser#annotationEntry}.
   *
   * @param ctx the parse tree
   */
  void enterAnnotationEntry(AnnotationParser.AnnotationEntryContext ctx);
  /**
   * Exit a parse tree produced by {@link AnnotationParser#annotationEntry}.
   *
   * @param ctx the parse tree
   */
  void exitAnnotationEntry(AnnotationParser.AnnotationEntryContext ctx);
  /**
   * Enter a parse tree produced by the {@code nat} labeled alternative in {@link
   * AnnotationParser#number}.
   *
   * @param ctx the parse tree
   */
  void enterNat(AnnotationParser.NatContext ctx);
  /**
   * Exit a parse tree produced by the {@code nat} labeled alternative in {@link
   * AnnotationParser#number}.
   *
   * @param ctx the parse tree
   */
  void exitNat(AnnotationParser.NatContext ctx);
  /**
   * Enter a parse tree produced by the {@code rat} labeled alternative in {@link
   * AnnotationParser#number}.
   *
   * @param ctx the parse tree
   */
  void enterRat(AnnotationParser.RatContext ctx);
  /**
   * Exit a parse tree produced by the {@code rat} labeled alternative in {@link
   * AnnotationParser#number}.
   *
   * @param ctx the parse tree
   */
  void exitRat(AnnotationParser.RatContext ctx);
  /**
   * Enter a parse tree produced by the {@code rankIndex} labeled alternative in {@link
   * AnnotationParser#index}.
   *
   * @param ctx the parse tree
   */
  void enterRankIndex(AnnotationParser.RankIndexContext ctx);
  /**
   * Exit a parse tree produced by the {@code rankIndex} labeled alternative in {@link
   * AnnotationParser#index}.
   *
   * @param ctx the parse tree
   */
  void exitRankIndex(AnnotationParser.RankIndexContext ctx);
  /**
   * Enter a parse tree produced by the {@code otherIndex} labeled alternative in {@link
   * AnnotationParser#index}.
   *
   * @param ctx the parse tree
   */
  void enterOtherIndex(AnnotationParser.OtherIndexContext ctx);
  /**
   * Exit a parse tree produced by the {@code otherIndex} labeled alternative in {@link
   * AnnotationParser#index}.
   *
   * @param ctx the parse tree
   */
  void exitOtherIndex(AnnotationParser.OtherIndexContext ctx);
}
