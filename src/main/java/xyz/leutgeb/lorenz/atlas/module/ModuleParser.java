package xyz.leutgeb.lorenz.atlas.module;

import java.nio.file.Path;
import java.util.List;
import lombok.extern.slf4j.Slf4j;
import org.antlr.v4.runtime.BailErrorStrategy;
import org.antlr.v4.runtime.CharStream;
import org.antlr.v4.runtime.CharStreams;
import org.antlr.v4.runtime.CommonTokenStream;
import org.antlr.v4.runtime.DefaultErrorStrategy;
import org.antlr.v4.runtime.RecognitionException;
import org.antlr.v4.runtime.atn.PredictionMode;
import org.antlr.v4.runtime.misc.ParseCancellationException;
import xyz.leutgeb.lorenz.atlas.antlr.SplayLexer;
import xyz.leutgeb.lorenz.atlas.antlr.SplayParser;
import xyz.leutgeb.lorenz.atlas.ast.FunctionDefinition;
import xyz.leutgeb.lorenz.atlas.ast.visitors.ProgramVisitor;

@Slf4j
class ModuleParser {
  public static List<FunctionDefinition> parse(String s, String moduleName) {
    try {
      return parse(CharStreams.fromString(s, "inline"), moduleName);
      // } catch (IOException e) {
      // In this case we assume that something went fundamentally
      // wrong when using a String as input. The caller probably
      // assumes that I/O on a String should always be fine.
      // throw new RuntimeException("Encountered I/O-related exception while parsing a String.", e);
    } catch (RecognitionException | ParseCancellationException e) {
      // If there were issues parsing the given string, we
      // throw something that suggests that the input string
      // is malformed.
      throw new IllegalArgumentException("Could not parse input program.", e);
    }
  }

  public static List<FunctionDefinition> parse(CharStream stream, String moduleName) {
    log.debug("Loading module " + moduleName);
    /*
    // In order to require less memory: use unbuffered streams and avoid constructing a full parse tree.
    ASPCore2Lexer lexer = new ASPCore2Lexer(new UnbufferedCharStream(is));
    lexer.setTokenFactory(new CommonTokenFactory(true));
    final ASPCore2Parser parser = new ASPCore2Parser(new UnbufferedTokenStream<>(lexer));
    parser.setBuildParseTree(false);
    */
    CommonTokenStream tokens = new CommonTokenStream(new SplayLexer(stream));
    final SplayParser parser = new SplayParser(tokens);

    // Try SLL parsing mode (faster but may terminate incorrectly).
    parser.getInterpreter().setPredictionMode(PredictionMode.SLL);
    parser.removeErrorListeners();
    parser.setErrorHandler(new BailErrorStrategy());

    final CustomErrorListener errorListener = new CustomErrorListener(stream.getSourceName());

    SplayParser.ProgramContext programContext;
    try {
      // Parse program
      programContext = parser.program();
    } catch (ParseCancellationException e) {
      // Recognition exception may be caused simply by SLL parsing failing,
      // retry with LL parser and DefaultErrorStrategy printing errors to console.
      if (e.getCause() instanceof RecognitionException) {
        tokens.seek(0);
        parser.addErrorListener(errorListener);
        parser.setErrorHandler(new DefaultErrorStrategy());
        parser.getInterpreter().setPredictionMode(PredictionMode.LL);
        // Re-run parse.
        programContext = parser.program();
      } else {
        throw e;
      }
    }

    // If the our SwallowingErrorListener has handled some exception during parsing
    // just re-throw that exception.
    // At this time, error messages will be already printed out to standard error
    // because ANTLR by default adds an org.antlr.v4.runtime.ConsoleErrorListener
    // to every parser.
    // That ConsoleErrorListener will print useful messages, but not report back to
    // our code.
    // org.antlr.v4.runtime.BailErrorStrategy cannot be used here, because it would
    // abruptly stop parsing as soon as the first error is reached (i.e. no recovery
    // is attempted) and the user will only see the first error encountered.
    if (errorListener.getRecognitionException() != null) {
      throw errorListener.getRecognitionException();
    }

    // Abort parsing if there were some (recoverable) syntax errors.
    if (parser.getNumberOfSyntaxErrors() != 0) {
      throw new ParseCancellationException();
    }

    // Construct internal program representation.
    ProgramVisitor visitor = new ProgramVisitor(moduleName, Path.of(stream.getSourceName()));
    return visitor.visitProgram(programContext);
  }
}
