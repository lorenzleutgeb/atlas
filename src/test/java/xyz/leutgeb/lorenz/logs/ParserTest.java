package xyz.leutgeb.lorenz.logs;

import static java.util.Collections.enumeration;
import static java.util.stream.Collectors.toUnmodifiableList;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static xyz.leutgeb.lorenz.logs.ProgramParser.parse;
import static xyz.leutgeb.lorenz.logs.type.TypeVar.ALPHA;
import static xyz.leutgeb.lorenz.logs.type.TypeVar.BETA;

import java.io.IOException;
import java.io.InputStream;
import java.io.SequenceInputStream;
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
  private static final String COLON = ":";

  @ParameterizedTest
  @ValueSource(
    strings = {
      "splay",
      "splay" + COLON + "insert",
      "splay_max",
      "splay" + COLON + "splay_max" + COLON + "delete"
    }
  )
  void suite(final String fileNames) throws IOException, UnificationError, TypeError {
    Stream<String> fileNameStream = Stream.of(fileNames.split(COLON));
    Program program =
        parse(
            CharStreams.fromStream(
                new SequenceInputStream(
                    enumeration(fileNameStream.map(this::open).collect(toUnmodifiableList())))));
    var signature = program.getSignature();
    assertNotNull(program);
  }

  private InputStream open(String fileName) {
    return getClass().getResourceAsStream("/" + fileName);
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
    // Program program = parse("foo t = match t with | (a, b, c) -> t");
    Program program =
        parse(
            "f x y = x \n\ng x = x \n\nh x y z = match g (f x y) with | (a, b, c) -> let foo = g (f a b) in (foo, b, c)");
    var signature = program.getSignature();
    assertNotNull(program);
  }
}
