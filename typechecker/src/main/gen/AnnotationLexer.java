// Generated from
// /home/lorenz/src/github.com/lorenzleutgeb/lac/typechecker/src/main/antlr/xyz/leutgeb/lorenz/lac/antlr/Annotation.g4 by ANTLR 4.8
import org.antlr.v4.runtime.*;
import org.antlr.v4.runtime.CharStream;
import org.antlr.v4.runtime.Lexer;
import org.antlr.v4.runtime.atn.*;
import org.antlr.v4.runtime.dfa.DFA;
import org.antlr.v4.runtime.misc.*;

@SuppressWarnings({"all", "warnings", "unchecked", "unused", "cast"})
public class AnnotationLexer extends Lexer {
  static {
    RuntimeMetaData.checkVersion("4.8", RuntimeMetaData.VERSION);
  }

  protected static final DFA[] _decisionToDFA;
  protected static final PredictionContextCache _sharedContextCache = new PredictionContextCache();
  public static final int NUMBER = 1,
      SLASH = 2,
      TIMES = 3,
      CURLY_OPEN = 4,
      CURLY_CLOSE = 5,
      SQUARE_OPEN = 6,
      SQUARE_CLOSE = 7,
      SEMICOLON = 8,
      COMMA = 9,
      EXCLAMATION_MARK = 10,
      ARROW = 11,
      MAPSTO = 12,
      IDENTIFIER = 13,
      AT = 14,
      PAREN_OPEN = 15,
      PAREN_CLOSE = 16,
      COMMENT = 17,
      BLANK = 18;
  public static String[] channelNames = {"DEFAULT_TOKEN_CHANNEL", "HIDDEN"};

  public static String[] modeNames = {"DEFAULT_MODE"};

  private static String[] makeRuleNames() {
    return new String[] {
      "NUMBER", "SLASH", "TIMES", "CURLY_OPEN", "CURLY_CLOSE", "SQUARE_OPEN",
      "SQUARE_CLOSE", "SEMICOLON", "COMMA", "EXCLAMATION_MARK", "ARROW", "MAPSTO",
      "IDENTIFIER", "AT", "PAREN_OPEN", "PAREN_CLOSE", "COMMENT", "BLANK"
    };
  }

  public static final String[] ruleNames = makeRuleNames();

  private static String[] makeLiteralNames() {
    return new String[] {
      null, null, "'/'", "'*'", "'{'", "'}'", "'['", "']'", "';'", "','", "'!'", "'->'", null, null,
      "'@'", "'('", "')'"
    };
  }

  private static final String[] _LITERAL_NAMES = makeLiteralNames();

  private static String[] makeSymbolicNames() {
    return new String[] {
      null,
      "NUMBER",
      "SLASH",
      "TIMES",
      "CURLY_OPEN",
      "CURLY_CLOSE",
      "SQUARE_OPEN",
      "SQUARE_CLOSE",
      "SEMICOLON",
      "COMMA",
      "EXCLAMATION_MARK",
      "ARROW",
      "MAPSTO",
      "IDENTIFIER",
      "AT",
      "PAREN_OPEN",
      "PAREN_CLOSE",
      "COMMENT",
      "BLANK"
    };
  }

  private static final String[] _SYMBOLIC_NAMES = makeSymbolicNames();
  public static final Vocabulary VOCABULARY = new VocabularyImpl(_LITERAL_NAMES, _SYMBOLIC_NAMES);

  /** @deprecated Use {@link #VOCABULARY} instead. */
  @Deprecated public static final String[] tokenNames;

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

  public AnnotationLexer(CharStream input) {
    super(input);
    _interp = new LexerATNSimulator(this, _ATN, _decisionToDFA, _sharedContextCache);
  }

  @Override
  public String getGrammarFileName() {
    return "Annotation.g4";
  }

  @Override
  public String[] getRuleNames() {
    return ruleNames;
  }

  @Override
  public String getSerializedATN() {
    return _serializedATN;
  }

  @Override
  public String[] getChannelNames() {
    return channelNames;
  }

  @Override
  public String[] getModeNames() {
    return modeNames;
  }

  @Override
  public ATN getATN() {
    return _ATN;
  }

  public static final String _serializedATN =
      "\3\u608b\ua72a\u8133\ub9ed\u417c\u3be7\u7786\u5964\2\24g\b\1\4\2\t\2\4"
          + "\3\t\3\4\4\t\4\4\5\t\5\4\6\t\6\4\7\t\7\4\b\t\b\4\t\t\t\4\n\t\n\4\13\t"
          + "\13\4\f\t\f\4\r\t\r\4\16\t\16\4\17\t\17\4\20\t\20\4\21\t\21\4\22\t\22"
          + "\4\23\t\23\3\2\3\2\3\3\3\3\3\4\3\4\3\5\3\5\3\6\3\6\3\7\3\7\3\b\3\b\3\t"
          + "\3\t\3\n\3\n\3\13\3\13\3\f\3\f\3\f\3\r\3\r\3\r\3\r\5\rC\n\r\3\16\3\16"
          + "\7\16G\n\16\f\16\16\16J\13\16\3\17\3\17\3\20\3\20\3\21\3\21\3\22\3\22"
          + "\3\22\3\22\3\22\7\22W\n\22\f\22\16\22Z\13\22\3\22\3\22\3\22\3\22\3\22"
          + "\3\23\6\23b\n\23\r\23\16\23c\3\23\3\23\3X\2\24\3\3\5\4\7\5\t\6\13\7\r"
          + "\b\17\t\21\n\23\13\25\f\27\r\31\16\33\17\35\20\37\21!\22#\23%\24\3\2\5"
          + "\6\2AAC\\aac|\b\2..\62<C\\aac}\177\177\5\2\13\f\16\17\"\"\2k\2\3\3\2\2"
          + "\2\2\5\3\2\2\2\2\7\3\2\2\2\2\t\3\2\2\2\2\13\3\2\2\2\2\r\3\2\2\2\2\17\3"
          + "\2\2\2\2\21\3\2\2\2\2\23\3\2\2\2\2\25\3\2\2\2\2\27\3\2\2\2\2\31\3\2\2"
          + "\2\2\33\3\2\2\2\2\35\3\2\2\2\2\37\3\2\2\2\2!\3\2\2\2\2#\3\2\2\2\2%\3\2"
          + "\2\2\3\'\3\2\2\2\5)\3\2\2\2\7+\3\2\2\2\t-\3\2\2\2\13/\3\2\2\2\r\61\3\2"
          + "\2\2\17\63\3\2\2\2\21\65\3\2\2\2\23\67\3\2\2\2\259\3\2\2\2\27;\3\2\2\2"
          + "\31B\3\2\2\2\33D\3\2\2\2\35K\3\2\2\2\37M\3\2\2\2!O\3\2\2\2#Q\3\2\2\2%"
          + "a\3\2\2\2\'(\4\62;\2(\4\3\2\2\2)*\7\61\2\2*\6\3\2\2\2+,\7,\2\2,\b\3\2"
          + "\2\2-.\7}\2\2.\n\3\2\2\2/\60\7\177\2\2\60\f\3\2\2\2\61\62\7]\2\2\62\16"
          + "\3\2\2\2\63\64\7_\2\2\64\20\3\2\2\2\65\66\7=\2\2\66\22\3\2\2\2\678\7."
          + "\2\28\24\3\2\2\29:\7#\2\2:\26\3\2\2\2;<\7/\2\2<=\7@\2\2=\30\3\2\2\2>C"
          + "\7\u21a8\2\2?@\7~\2\2@A\7/\2\2AC\7@\2\2B>\3\2\2\2B?\3\2\2\2C\32\3\2\2"
          + "\2DH\t\2\2\2EG\t\3\2\2FE\3\2\2\2GJ\3\2\2\2HF\3\2\2\2HI\3\2\2\2I\34\3\2"
          + "\2\2JH\3\2\2\2KL\7B\2\2L\36\3\2\2\2MN\7*\2\2N \3\2\2\2OP\7+\2\2P\"\3\2"
          + "\2\2QR\7*\2\2RS\7,\2\2SX\3\2\2\2TW\5#\22\2UW\13\2\2\2VT\3\2\2\2VU\3\2"
          + "\2\2WZ\3\2\2\2XY\3\2\2\2XV\3\2\2\2Y[\3\2\2\2ZX\3\2\2\2[\\\7,\2\2\\]\7"
          + "+\2\2]^\3\2\2\2^_\b\22\2\2_$\3\2\2\2`b\t\4\2\2a`\3\2\2\2bc\3\2\2\2ca\3"
          + "\2\2\2cd\3\2\2\2de\3\2\2\2ef\b\23\2\2f&\3\2\2\2\b\2BHVXc\3\2\3\2";
  public static final ATN _ATN = new ATNDeserializer().deserialize(_serializedATN.toCharArray());

  static {
    _decisionToDFA = new DFA[_ATN.getNumberOfDecisions()];
    for (int i = 0; i < _ATN.getNumberOfDecisions(); i++) {
      _decisionToDFA[i] = new DFA(_ATN.getDecisionState(i), i);
    }
  }
}
