package xyz.leutgeb.lorenz.logs;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static xyz.leutgeb.lorenz.logs.ProgramParser.parse;
import static xyz.leutgeb.lorenz.logs.type.TypeVar.ALPHA;
import static xyz.leutgeb.lorenz.logs.type.TypeVar.BETA;

import java.io.IOException;
import java.util.Arrays;
import java.util.stream.Stream;
import org.antlr.v4.runtime.CharStreams;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.Arguments;
import org.junit.jupiter.params.provider.MethodSource;
import org.junit.jupiter.params.provider.ValueSource;
import xyz.leutgeb.lorenz.logs.ast.Program;
import xyz.leutgeb.lorenz.logs.type.BoolType;
import xyz.leutgeb.lorenz.logs.type.FunctionType;
import xyz.leutgeb.lorenz.logs.type.TreeType;
import xyz.leutgeb.lorenz.logs.type.Type;
import xyz.leutgeb.lorenz.logs.type.TypeError;
import xyz.leutgeb.lorenz.logs.unification.UnificationError;

public class ParserTest {
  @ParameterizedTest
  @ValueSource(strings = {"splay", "insert", "splay_max", "delete"})
  void suite(final String fileName) throws IOException, UnificationError, TypeError {
    Program program = parse(CharStreams.fromStream(getClass().getResourceAsStream("/" + fileName)));
    var signature = program.getSignature();
    assertNotNull(program);
  }

  private static FunctionType ft(Type to, Type... from) {
    return new FunctionType(Arrays.asList(from), to);
  }

  private static final TreeType ATREE = new TreeType(ALPHA);

  static Stream<Arguments> generateSnippets() {
    return Stream.of(
        Arguments.of("f t = match t with | (a, b, c) -> t", ft(ATREE, ATREE)),
        Arguments.of("f x = x", ft(ALPHA, ALPHA)),
        Arguments.of("f = nil", ft(ATREE)),
        Arguments.of("f x y = x", ft(ALPHA, ALPHA, BETA)),
        Arguments.of("f x y = y", ft(BETA, ALPHA, BETA)),
        Arguments.of("f x y = if x < y then false else true", ft(BoolType.INSTANCE, ALPHA, ALPHA))
        // The following requires identifiers as conditions (grammar change):
        // Arguments.of("f x = if x then false else true", ft(BoolType.INSTANCE, BoolType.INSTANCE))
        );
  }

  @ParameterizedTest
  @MethodSource("generateSnippets")
  void snippets(String program, Type expected) throws Exception {
    assertEquals(expected, parse(program).getSignature().values().iterator().next());
  }

  @Test
  void fiddle() throws Exception {
    Program program = parse("foo t = match t with | (a, b, c) -> t");
    var signature = program.getSignature();
    assertNotNull(program);
  }
}
