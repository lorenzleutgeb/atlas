package xyz.leutgeb.lorenz.lac;

import org.antlr.v4.runtime.BaseErrorListener;
import org.antlr.v4.runtime.RecognitionException;
import org.antlr.v4.runtime.Recognizer;

class CustomErrorListener extends BaseErrorListener {
  private final String fileName;
  private RecognitionException recognitionException;

  public CustomErrorListener(String fileName) {
    this.fileName = fileName;
  }

  @Override
  public void syntaxError(
      Recognizer<?, ?> recognizer,
      Object offendingSymbol,
      int line,
      int charPositionInLine,
      String msg,
      RecognitionException e) {
    super.syntaxError(recognizer, offendingSymbol, line, charPositionInLine, msg, e);

    System.err.println(fileName + ":" + line + ":" + charPositionInLine + ": " + msg);

    this.recognitionException = e;
  }

  public RecognitionException getRecognitionException() {
    return recognitionException;
  }
}
