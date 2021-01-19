// Generated from /home/lorenz/src/github.com/lorenzleutgeb/lac/src/main/antlr/xyz/leutgeb/lorenz/lac/antlr/Annotation.g4 by ANTLR 4.8
import org.antlr.v4.runtime.atn.*;
import org.antlr.v4.runtime.dfa.DFA;
import org.antlr.v4.runtime.*;
import org.antlr.v4.runtime.misc.*;
import org.antlr.v4.runtime.tree.*;
import java.util.List;
import java.util.Iterator;
import java.util.ArrayList;

@SuppressWarnings({"all", "warnings", "unchecked", "unused", "cast"})
public class AnnotationParser extends Parser {
	static { RuntimeMetaData.checkVersion("4.8", RuntimeMetaData.VERSION); }

	protected static final DFA[] _decisionToDFA;
	protected static final PredictionContextCache _sharedContextCache =
		new PredictionContextCache();
	public static final int
		NUMBER=1, SLASH=2, TIMES=3, CURLY_OPEN=4, CURLY_CLOSE=5, SQUARE_OPEN=6, 
		SQUARE_CLOSE=7, SEMICOLON=8, COMMA=9, EXCLAMATION_MARK=10, ARROW=11, MAPSTO=12, 
		IDENTIFIER=13, AT=14, PAREN_OPEN=15, PAREN_CLOSE=16, COMMENT=17, BLANK=18;
	public static final int
		RULE_annotation = 0, RULE_annotationEntry = 1, RULE_number = 2, RULE_index = 3;
	private static String[] makeRuleNames() {
		return new String[] {
			"annotation", "annotationEntry", "number", "index"
		};
	}
	public static final String[] ruleNames = makeRuleNames();

	private static String[] makeLiteralNames() {
		return new String[] {
			null, null, "'/'", "'*'", "'{'", "'}'", "'['", "']'", "';'", "','", "'!'", 
			"'->'", null, null, "'@'", "'('", "')'"
		};
	}
	private static final String[] _LITERAL_NAMES = makeLiteralNames();
	private static String[] makeSymbolicNames() {
		return new String[] {
			null, "NUMBER", "SLASH", "TIMES", "CURLY_OPEN", "CURLY_CLOSE", "SQUARE_OPEN", 
			"SQUARE_CLOSE", "SEMICOLON", "COMMA", "EXCLAMATION_MARK", "ARROW", "MAPSTO", 
			"IDENTIFIER", "AT", "PAREN_OPEN", "PAREN_CLOSE", "COMMENT", "BLANK"
		};
	}
	private static final String[] _SYMBOLIC_NAMES = makeSymbolicNames();
	public static final Vocabulary VOCABULARY = new VocabularyImpl(_LITERAL_NAMES, _SYMBOLIC_NAMES);

	/**
	 * @deprecated Use {@link #VOCABULARY} instead.
	 */
	@Deprecated
	public static final String[] tokenNames;
	static {
		tokenNames = new String[_SYMBOLIC_NAMES.length];
		for (int i = 0; i < tokenNames.length; i++) {
			tokenNames[i] = VOCABULARY.getLiteralName(i);
			if (tokenNames[i] == null) {
				tokenNames[i] = VOCABULARY.getSymbolicName(i);
			}

			if (tokenNames[i] == null) {
				tokenNames[i] = "<INVALID>";
			}
		}
	}

	@Override
	@Deprecated
	public String[] getTokenNames() {
		return tokenNames;
	}

	@Override

	public Vocabulary getVocabulary() {
		return VOCABULARY;
	}

	@Override
	public String getGrammarFileName() { return "Annotation.g4"; }

	@Override
	public String[] getRuleNames() { return ruleNames; }

	@Override
	public String getSerializedATN() { return _serializedATN; }

	@Override
	public ATN getATN() { return _ATN; }

	public AnnotationParser(TokenStream input) {
		super(input);
		_interp = new ParserATNSimulator(this,_ATN,_decisionToDFA,_sharedContextCache);
	}

	public static class AnnotationContext extends ParserRuleContext {
		public AnnotationContext(ParserRuleContext parent, int invokingState) {
			super(parent, invokingState);
		}
		@Override public int getRuleIndex() { return RULE_annotation; }
	 
		public AnnotationContext() { }
		public void copyFrom(AnnotationContext ctx) {
			super.copyFrom(ctx);
		}
	}
	public static class NonEmptyAnnotationContext extends AnnotationContext {
		public AnnotationEntryContext annotationEntry;
		public List<AnnotationEntryContext> entries = new ArrayList<AnnotationEntryContext>();
		public TerminalNode SQUARE_OPEN() { return getToken(AnnotationParser.SQUARE_OPEN, 0); }
		public TerminalNode SQUARE_CLOSE() { return getToken(AnnotationParser.SQUARE_CLOSE, 0); }
		public List<AnnotationEntryContext> annotationEntry() {
			return getRuleContexts(AnnotationEntryContext.class);
		}
		public AnnotationEntryContext annotationEntry(int i) {
			return getRuleContext(AnnotationEntryContext.class,i);
		}
		public List<TerminalNode> COMMA() { return getTokens(AnnotationParser.COMMA); }
		public TerminalNode COMMA(int i) {
			return getToken(AnnotationParser.COMMA, i);
		}
		public NonEmptyAnnotationContext(AnnotationContext ctx) { copyFrom(ctx); }
		@Override
		public void enterRule(ParseTreeListener listener) {
			if ( listener instanceof AnnotationListener ) ((AnnotationListener)listener).enterNonEmptyAnnotation(this);
		}
		@Override
		public void exitRule(ParseTreeListener listener) {
			if ( listener instanceof AnnotationListener ) ((AnnotationListener)listener).exitNonEmptyAnnotation(this);
		}
		@Override
		public <T> T accept(ParseTreeVisitor<? extends T> visitor) {
			if ( visitor instanceof AnnotationVisitor ) return ((AnnotationVisitor<? extends T>)visitor).visitNonEmptyAnnotation(this);
			else return visitor.visitChildren(this);
		}
	}
	public static class ZeroAnnotationContext extends AnnotationContext {
		public TerminalNode SQUARE_OPEN() { return getToken(AnnotationParser.SQUARE_OPEN, 0); }
		public TerminalNode SQUARE_CLOSE() { return getToken(AnnotationParser.SQUARE_CLOSE, 0); }
		public ZeroAnnotationContext(AnnotationContext ctx) { copyFrom(ctx); }
		@Override
		public void enterRule(ParseTreeListener listener) {
			if ( listener instanceof AnnotationListener ) ((AnnotationListener)listener).enterZeroAnnotation(this);
		}
		@Override
		public void exitRule(ParseTreeListener listener) {
			if ( listener instanceof AnnotationListener ) ((AnnotationListener)listener).exitZeroAnnotation(this);
		}
		@Override
		public <T> T accept(ParseTreeVisitor<? extends T> visitor) {
			if ( visitor instanceof AnnotationVisitor ) return ((AnnotationVisitor<? extends T>)visitor).visitZeroAnnotation(this);
			else return visitor.visitChildren(this);
		}
	}

	public final AnnotationContext annotation() throws RecognitionException {
		AnnotationContext _localctx = new AnnotationContext(_ctx, getState());
		enterRule(_localctx, 0, RULE_annotation);
		int _la;
		try {
			setState(21);
			_errHandler.sync(this);
			switch ( getInterpreter().adaptivePredict(_input,1,_ctx) ) {
			case 1:
				_localctx = new NonEmptyAnnotationContext(_localctx);
				enterOuterAlt(_localctx, 1);
				{
				setState(8);
				match(SQUARE_OPEN);
				setState(9);
				((NonEmptyAnnotationContext)_localctx).annotationEntry = annotationEntry();
				((NonEmptyAnnotationContext)_localctx).entries.add(((NonEmptyAnnotationContext)_localctx).annotationEntry);
				setState(14);
				_errHandler.sync(this);
				_la = _input.LA(1);
				while (_la==COMMA) {
					{
					{
					setState(10);
					match(COMMA);
					setState(11);
					((NonEmptyAnnotationContext)_localctx).annotationEntry = annotationEntry();
					((NonEmptyAnnotationContext)_localctx).entries.add(((NonEmptyAnnotationContext)_localctx).annotationEntry);
					}
					}
					setState(16);
					_errHandler.sync(this);
					_la = _input.LA(1);
				}
				setState(17);
				match(SQUARE_CLOSE);
				}
				break;
			case 2:
				_localctx = new ZeroAnnotationContext(_localctx);
				enterOuterAlt(_localctx, 2);
				{
				setState(19);
				match(SQUARE_OPEN);
				setState(20);
				match(SQUARE_CLOSE);
				}
				break;
			}
		}
		catch (RecognitionException re) {
			_localctx.exception = re;
			_errHandler.reportError(this, re);
			_errHandler.recover(this, re);
		}
		finally {
			exitRule();
		}
		return _localctx;
	}

	public static class AnnotationEntryContext extends ParserRuleContext {
		public NumberContext coefficient;
		public IndexContext index() {
			return getRuleContext(IndexContext.class,0);
		}
		public TerminalNode MAPSTO() { return getToken(AnnotationParser.MAPSTO, 0); }
		public NumberContext number() {
			return getRuleContext(NumberContext.class,0);
		}
		public AnnotationEntryContext(ParserRuleContext parent, int invokingState) {
			super(parent, invokingState);
		}
		@Override public int getRuleIndex() { return RULE_annotationEntry; }
		@Override
		public void enterRule(ParseTreeListener listener) {
			if ( listener instanceof AnnotationListener ) ((AnnotationListener)listener).enterAnnotationEntry(this);
		}
		@Override
		public void exitRule(ParseTreeListener listener) {
			if ( listener instanceof AnnotationListener ) ((AnnotationListener)listener).exitAnnotationEntry(this);
		}
		@Override
		public <T> T accept(ParseTreeVisitor<? extends T> visitor) {
			if ( visitor instanceof AnnotationVisitor ) return ((AnnotationVisitor<? extends T>)visitor).visitAnnotationEntry(this);
			else return visitor.visitChildren(this);
		}
	}

	public final AnnotationEntryContext annotationEntry() throws RecognitionException {
		AnnotationEntryContext _localctx = new AnnotationEntryContext(_ctx, getState());
		enterRule(_localctx, 2, RULE_annotationEntry);
		try {
			enterOuterAlt(_localctx, 1);
			{
			setState(23);
			index();
			setState(24);
			match(MAPSTO);
			setState(25);
			((AnnotationEntryContext)_localctx).coefficient = number();
			}
		}
		catch (RecognitionException re) {
			_localctx.exception = re;
			_errHandler.reportError(this, re);
			_errHandler.recover(this, re);
		}
		finally {
			exitRule();
		}
		return _localctx;
	}

	public static class NumberContext extends ParserRuleContext {
		public NumberContext(ParserRuleContext parent, int invokingState) {
			super(parent, invokingState);
		}
		@Override public int getRuleIndex() { return RULE_number; }
	 
		public NumberContext() { }
		public void copyFrom(NumberContext ctx) {
			super.copyFrom(ctx);
		}
	}
	public static class NatContext extends NumberContext {
		public TerminalNode NUMBER() { return getToken(AnnotationParser.NUMBER, 0); }
		public NatContext(NumberContext ctx) { copyFrom(ctx); }
		@Override
		public void enterRule(ParseTreeListener listener) {
			if ( listener instanceof AnnotationListener ) ((AnnotationListener)listener).enterNat(this);
		}
		@Override
		public void exitRule(ParseTreeListener listener) {
			if ( listener instanceof AnnotationListener ) ((AnnotationListener)listener).exitNat(this);
		}
		@Override
		public <T> T accept(ParseTreeVisitor<? extends T> visitor) {
			if ( visitor instanceof AnnotationVisitor ) return ((AnnotationVisitor<? extends T>)visitor).visitNat(this);
			else return visitor.visitChildren(this);
		}
	}
	public static class RatContext extends NumberContext {
		public Token numerator;
		public Token denominator;
		public TerminalNode SLASH() { return getToken(AnnotationParser.SLASH, 0); }
		public List<TerminalNode> NUMBER() { return getTokens(AnnotationParser.NUMBER); }
		public TerminalNode NUMBER(int i) {
			return getToken(AnnotationParser.NUMBER, i);
		}
		public RatContext(NumberContext ctx) { copyFrom(ctx); }
		@Override
		public void enterRule(ParseTreeListener listener) {
			if ( listener instanceof AnnotationListener ) ((AnnotationListener)listener).enterRat(this);
		}
		@Override
		public void exitRule(ParseTreeListener listener) {
			if ( listener instanceof AnnotationListener ) ((AnnotationListener)listener).exitRat(this);
		}
		@Override
		public <T> T accept(ParseTreeVisitor<? extends T> visitor) {
			if ( visitor instanceof AnnotationVisitor ) return ((AnnotationVisitor<? extends T>)visitor).visitRat(this);
			else return visitor.visitChildren(this);
		}
	}

	public final NumberContext number() throws RecognitionException {
		NumberContext _localctx = new NumberContext(_ctx, getState());
		enterRule(_localctx, 4, RULE_number);
		try {
			setState(31);
			_errHandler.sync(this);
			switch ( getInterpreter().adaptivePredict(_input,2,_ctx) ) {
			case 1:
				_localctx = new NatContext(_localctx);
				enterOuterAlt(_localctx, 1);
				{
				setState(27);
				match(NUMBER);
				}
				break;
			case 2:
				_localctx = new RatContext(_localctx);
				enterOuterAlt(_localctx, 2);
				{
				setState(28);
				((RatContext)_localctx).numerator = match(NUMBER);
				setState(29);
				match(SLASH);
				setState(30);
				((RatContext)_localctx).denominator = match(NUMBER);
				}
				break;
			}
		}
		catch (RecognitionException re) {
			_localctx.exception = re;
			_errHandler.reportError(this, re);
			_errHandler.recover(this, re);
		}
		finally {
			exitRule();
		}
		return _localctx;
	}

	public static class IndexContext extends ParserRuleContext {
		public IndexContext(ParserRuleContext parent, int invokingState) {
			super(parent, invokingState);
		}
		@Override public int getRuleIndex() { return RULE_index; }
	 
		public IndexContext() { }
		public void copyFrom(IndexContext ctx) {
			super.copyFrom(ctx);
		}
	}
	public static class RankIndexContext extends IndexContext {
		public TerminalNode NUMBER() { return getToken(AnnotationParser.NUMBER, 0); }
		public RankIndexContext(IndexContext ctx) { copyFrom(ctx); }
		@Override
		public void enterRule(ParseTreeListener listener) {
			if ( listener instanceof AnnotationListener ) ((AnnotationListener)listener).enterRankIndex(this);
		}
		@Override
		public void exitRule(ParseTreeListener listener) {
			if ( listener instanceof AnnotationListener ) ((AnnotationListener)listener).exitRankIndex(this);
		}
		@Override
		public <T> T accept(ParseTreeVisitor<? extends T> visitor) {
			if ( visitor instanceof AnnotationVisitor ) return ((AnnotationVisitor<? extends T>)visitor).visitRankIndex(this);
			else return visitor.visitChildren(this);
		}
	}
	public static class OtherIndexContext extends IndexContext {
		public Token NUMBER;
		public List<Token> elements = new ArrayList<Token>();
		public TerminalNode PAREN_OPEN() { return getToken(AnnotationParser.PAREN_OPEN, 0); }
		public TerminalNode PAREN_CLOSE() { return getToken(AnnotationParser.PAREN_CLOSE, 0); }
		public List<TerminalNode> NUMBER() { return getTokens(AnnotationParser.NUMBER); }
		public TerminalNode NUMBER(int i) {
			return getToken(AnnotationParser.NUMBER, i);
		}
		public OtherIndexContext(IndexContext ctx) { copyFrom(ctx); }
		@Override
		public void enterRule(ParseTreeListener listener) {
			if ( listener instanceof AnnotationListener ) ((AnnotationListener)listener).enterOtherIndex(this);
		}
		@Override
		public void exitRule(ParseTreeListener listener) {
			if ( listener instanceof AnnotationListener ) ((AnnotationListener)listener).exitOtherIndex(this);
		}
		@Override
		public <T> T accept(ParseTreeVisitor<? extends T> visitor) {
			if ( visitor instanceof AnnotationVisitor ) return ((AnnotationVisitor<? extends T>)visitor).visitOtherIndex(this);
			else return visitor.visitChildren(this);
		}
	}

	public final IndexContext index() throws RecognitionException {
		IndexContext _localctx = new IndexContext(_ctx, getState());
		enterRule(_localctx, 6, RULE_index);
		int _la;
		try {
			setState(42);
			_errHandler.sync(this);
			switch (_input.LA(1)) {
			case NUMBER:
				_localctx = new RankIndexContext(_localctx);
				enterOuterAlt(_localctx, 1);
				{
				setState(33);
				match(NUMBER);
				}
				break;
			case PAREN_OPEN:
				_localctx = new OtherIndexContext(_localctx);
				enterOuterAlt(_localctx, 2);
				{
				setState(34);
				match(PAREN_OPEN);
				setState(38);
				_errHandler.sync(this);
				_la = _input.LA(1);
				while (_la==NUMBER) {
					{
					{
					setState(35);
					((OtherIndexContext)_localctx).NUMBER = match(NUMBER);
					((OtherIndexContext)_localctx).elements.add(((OtherIndexContext)_localctx).NUMBER);
					}
					}
					setState(40);
					_errHandler.sync(this);
					_la = _input.LA(1);
				}
				setState(41);
				match(PAREN_CLOSE);
				}
				break;
			default:
				throw new NoViableAltException(this);
			}
		}
		catch (RecognitionException re) {
			_localctx.exception = re;
			_errHandler.reportError(this, re);
			_errHandler.recover(this, re);
		}
		finally {
			exitRule();
		}
		return _localctx;
	}

	public static final String _serializedATN =
		"\3\u608b\ua72a\u8133\ub9ed\u417c\u3be7\u7786\u5964\3\24/\4\2\t\2\4\3\t"+
		"\3\4\4\t\4\4\5\t\5\3\2\3\2\3\2\3\2\7\2\17\n\2\f\2\16\2\22\13\2\3\2\3\2"+
		"\3\2\3\2\5\2\30\n\2\3\3\3\3\3\3\3\3\3\4\3\4\3\4\3\4\5\4\"\n\4\3\5\3\5"+
		"\3\5\7\5\'\n\5\f\5\16\5*\13\5\3\5\5\5-\n\5\3\5\2\2\6\2\4\6\b\2\2\2/\2"+
		"\27\3\2\2\2\4\31\3\2\2\2\6!\3\2\2\2\b,\3\2\2\2\n\13\7\b\2\2\13\20\5\4"+
		"\3\2\f\r\7\13\2\2\r\17\5\4\3\2\16\f\3\2\2\2\17\22\3\2\2\2\20\16\3\2\2"+
		"\2\20\21\3\2\2\2\21\23\3\2\2\2\22\20\3\2\2\2\23\24\7\t\2\2\24\30\3\2\2"+
		"\2\25\26\7\b\2\2\26\30\7\t\2\2\27\n\3\2\2\2\27\25\3\2\2\2\30\3\3\2\2\2"+
		"\31\32\5\b\5\2\32\33\7\16\2\2\33\34\5\6\4\2\34\5\3\2\2\2\35\"\7\3\2\2"+
		"\36\37\7\3\2\2\37 \7\4\2\2 \"\7\3\2\2!\35\3\2\2\2!\36\3\2\2\2\"\7\3\2"+
		"\2\2#-\7\3\2\2$(\7\21\2\2%\'\7\3\2\2&%\3\2\2\2\'*\3\2\2\2(&\3\2\2\2()"+
		"\3\2\2\2)+\3\2\2\2*(\3\2\2\2+-\7\22\2\2,#\3\2\2\2,$\3\2\2\2-\t\3\2\2\2"+
		"\7\20\27!(,";
	public static final ATN _ATN =
		new ATNDeserializer().deserialize(_serializedATN.toCharArray());
	static {
		_decisionToDFA = new DFA[_ATN.getNumberOfDecisions()];
		for (int i = 0; i < _ATN.getNumberOfDecisions(); i++) {
			_decisionToDFA[i] = new DFA(_ATN.getDecisionState(i), i);
		}
	}
}