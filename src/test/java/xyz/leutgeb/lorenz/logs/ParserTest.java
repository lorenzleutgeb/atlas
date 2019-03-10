package xyz.leutgeb.lorenz.logs;

import java.io.IOException;
import org.antlr.v4.runtime.CharStreams;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.ValueSource;
import xyz.leutgeb.lorenz.logs.ast.Program;
import xyz.leutgeb.lorenz.logs.type.TypeError;
import xyz.leutgeb.lorenz.logs.unification.UnificationError;

public class ParserTest {
  @ParameterizedTest
  @ValueSource(strings = {"splay", "insert", "splay_max", "delete"})
  void suite(final String fileName) throws IOException, UnificationError, TypeError {
    Program program =
        ProgramParser.parse(CharStreams.fromStream(getClass().getResourceAsStream("/" + fileName)));
    var signature = program.getSignature();
    Assertions.assertNotNull(program);
  }

  @Test
  void fiddle() throws Exception {
    Program program = ProgramParser.parse("foo t = match t with | (a, b, c) -> t");
    var signature = program.getSignature();
    // Type result = program.getFunctionDefinitions().get(0).getBody().infer(Context.root());
    Assertions.assertNotNull(program);
  }
}
