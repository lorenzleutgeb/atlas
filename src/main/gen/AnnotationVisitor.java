// Generated from /home/lorenz/src/github.com/lorenzleutgeb/lac/src/main/antlr/xyz/leutgeb/lorenz/lac/antlr/Annotation.g4 by ANTLR 4.8
import org.antlr.v4.runtime.tree.ParseTreeVisitor;

/**
 * This interface defines a complete generic visitor for a parse tree produced
 * by {@link AnnotationParser}.
 *
 * @param <T> The return type of the visit operation. Use {@link Void} for
 * operations with no return type.
 */
public interface AnnotationVisitor<T> extends ParseTreeVisitor<T> {
	/**
	 * Visit a parse tree produced by the {@code nonEmptyAnnotation}
	 * labeled alternative in {@link AnnotationParser#annotation}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	T visitNonEmptyAnnotation(AnnotationParser.NonEmptyAnnotationContext ctx);
	/**
	 * Visit a parse tree produced by the {@code zeroAnnotation}
	 * labeled alternative in {@link AnnotationParser#annotation}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	T visitZeroAnnotation(AnnotationParser.ZeroAnnotationContext ctx);
	/**
	 * Visit a parse tree produced by {@link AnnotationParser#annotationEntry}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	T visitAnnotationEntry(AnnotationParser.AnnotationEntryContext ctx);
	/**
	 * Visit a parse tree produced by the {@code nat}
	 * labeled alternative in {@link AnnotationParser#number}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	T visitNat(AnnotationParser.NatContext ctx);
	/**
	 * Visit a parse tree produced by the {@code rat}
	 * labeled alternative in {@link AnnotationParser#number}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	T visitRat(AnnotationParser.RatContext ctx);
	/**
	 * Visit a parse tree produced by the {@code rankIndex}
	 * labeled alternative in {@link AnnotationParser#index}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	T visitRankIndex(AnnotationParser.RankIndexContext ctx);
	/**
	 * Visit a parse tree produced by the {@code otherIndex}
	 * labeled alternative in {@link AnnotationParser#index}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	T visitOtherIndex(AnnotationParser.OtherIndexContext ctx);
}