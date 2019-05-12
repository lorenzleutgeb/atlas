package xyz.leutgeb.lorenz.logs;

import static java.util.Collections.enumeration;
import static java.util.stream.Collectors.toUnmodifiableList;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static xyz.leutgeb.lorenz.logs.ProgramParser.parse;
import static xyz.leutgeb.lorenz.logs.typing.TypeVariable.ALPHA;
import static xyz.leutgeb.lorenz.logs.typing.TypeVariable.BETA;

import java.io.ByteArrayOutputStream;
import java.io.IOException;
import java.io.InputStream;
import java.io.InputStreamReader;
import java.io.PrintStream;
import java.io.SequenceInputStream;
import java.nio.charset.StandardCharsets;
import java.util.stream.Stream;
import org.antlr.v4.runtime.CharStreams;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.Arguments;
import org.junit.jupiter.params.provider.MethodSource;
import org.junit.jupiter.params.provider.ValueSource;
import xyz.leutgeb.lorenz.logs.ast.Program;
import xyz.leutgeb.lorenz.logs.typing.BoolType;
import xyz.leutgeb.lorenz.logs.typing.FunctionSignature;
import xyz.leutgeb.lorenz.logs.typing.FunctionType;
import xyz.leutgeb.lorenz.logs.typing.TreeType;
import xyz.leutgeb.lorenz.logs.typing.Type;
import xyz.leutgeb.lorenz.logs.typing.TypeClass;
import xyz.leutgeb.lorenz.logs.typing.TypeConstraint;
import xyz.leutgeb.lorenz.logs.typing.TypeError;
import xyz.leutgeb.lorenz.logs.unification.Substitution;
import xyz.leutgeb.lorenz.logs.unification.UnificationError;

public class ParserTest {
  private static final String PLUS = "~";
  private static final TreeType ATREE = new TreeType(ALPHA);
  private static final BoolType BOOL = BoolType.INSTANCE;

  private static InputStream open(String fileName) {
    if (!fileName.endsWith(".ml")) {
      fileName += ".ml";
    }
    return ParserTest.class.getResourceAsStream("/" + fileName);
  }

  private static String read(String fileName) throws IOException {
    return new String(open(fileName).readAllBytes(), StandardCharsets.UTF_8);
  }

  private static FunctionType ft(Type to, Type... from) {
    return new FunctionType(to, from);
  }

  private static Stream<Arguments> generateSnippets() throws IOException {
    return Stream.of(
        Arguments.of("id t = match t with | (_, _, _) -> t", ft(ATREE, ATREE)),
        Arguments.of("id t = match t with | (a, b, c) -> (a, b, c)", ft(ATREE, ATREE)),
        Arguments.of("id x = x", ft(ALPHA, ALPHA)),
        Arguments.of("const = nil", ft(ATREE)),
        Arguments.of("left x y = x", ft(ALPHA, ALPHA, BETA)),
        Arguments.of("right x y = y", ft(BETA, ALPHA, BETA)),
        Arguments.of("less x y = if x < y then false else true", ft(BOOL, ALPHA, ALPHA)),
        Arguments.of("left x = match x with | (y, _, _) -> y", ft(ATREE, ATREE)),
        Arguments.of("neg x = if x then false else true", ft(BOOL, BOOL)),
        Arguments.of("empty t = match x with | nil -> true | (_, _, _) -> false", ft(BOOL, ATREE)),
        Arguments.of("flip t = match t with | (a, b, c) -> (c, b, a)", ft(ATREE, ATREE)),
        Arguments.of(read("contains_unordered"), ft(BOOL, ATREE)));
  }

  @ParameterizedTest
  @ValueSource(
    strings = {
      "flip",
      "walk",
      "traversals",
      "splay",
      "splay" + PLUS + "insert",
      "splay_max",
      "splay" + PLUS + "splay_max" + PLUS + "delete",
      "contains_unordered"
    }
  )
  void suite(final String fileNames) throws IOException, UnificationError, TypeError {
    Stream<String> fileNameStream = Stream.of(fileNames.split(PLUS));
    Program program =
        parse(
            CharStreams.fromReader(
                new InputStreamReader(
                    new SequenceInputStream(
                        enumeration(
                            fileNameStream.map(ParserTest::open).collect(toUnmodifiableList())))),
                fileNames));
    program.normalize();
    assertNotNull(program);

    // Check that prettyprinting emits syntactically correct programs.
    var output = new ByteArrayOutputStream();
    program.printTo(new PrintStream(output), false);
    System.out.println(output.toString());
    program = parse(CharStreams.fromString(output.toString()));
    assertNotNull(program);
  }

  @Test
  void inferSplay() throws IOException, UnificationError, TypeError {
    Program program = parse(CharStreams.fromReader(new InputStreamReader(open("splay")), "splay"));
    program.infer();
    FunctionSignature signature = program.getFunctionDefinitions().get("splay").getSignature();
    assertEquals(ft(ATREE, ALPHA, ATREE), signature.getType());
    assertTrue(
        signature
            .getConstraints()
            .contains(new TypeConstraint(TypeClass.ORD, Substitution.identity())));
    assertTrue(
        signature
            .getConstraints()
            .contains(new TypeConstraint(TypeClass.EQ, new Substitution(ALPHA, ATREE))));
    assertEquals(2, signature.getConstraints().size());
  }

  @ParameterizedTest
  @MethodSource("generateSnippets")
  void snippets(String source, Type expected) throws Exception {
    var program = parse(source);
    program.infer();
    assertEquals(
        expected,
        program.getFunctionDefinitions().values().iterator().next().getSignature().getType());
  }

  @Test
  void fiddle() throws Exception {
    Program program = parse("swp t = match t with | (l, m, r) -> (r, m, l)");
    program.infer();
    program.solve();
    program.printTo(System.out);
    assertNotNull(program);
  }
}
