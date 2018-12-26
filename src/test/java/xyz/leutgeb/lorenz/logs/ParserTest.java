package xyz.leutgeb.lorenz.logs;

import java.io.IOException;
import org.antlr.v4.runtime.CharStreams;
import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.ValueSource;

public class ParserTest {
  @ParameterizedTest
  @ValueSource(strings = {"insert", "splay", "splay_max", "delete"})
  void suite(final String fileName) throws IOException {
    ProgramParser.parse(CharStreams.fromStream(getClass().getResourceAsStream("/" + fileName)));
  }
}
