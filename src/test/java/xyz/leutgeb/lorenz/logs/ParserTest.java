package xyz.leutgeb.lorenz.logs;

import static java.util.Collections.enumeration;
import static java.util.stream.Collectors.toUnmodifiableList;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static xyz.leutgeb.lorenz.logs.ProgramParser.parse;
import static xyz.leutgeb.lorenz.logs.type.TypeVariable.ALPHA;
import static xyz.leutgeb.lorenz.logs.type.TypeVariable.BETA;

import java.io.ByteArrayOutputStream;
import java.io.IOException;
import java.io.InputStream;
import java.io.InputStreamReader;
import java.io.PrintStream;
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
import xyz.leutgeb.lorenz.logs.type.FunctionSignature;
import xyz.leutgeb.lorenz.logs.type.FunctionType;
import xyz.leutgeb.lorenz.logs.type.TreeType;
import xyz.leutgeb.lorenz.logs.type.Type;
import xyz.leutgeb.lorenz.logs.type.TypeClass;
import xyz.leutgeb.lorenz.logs.type.TypeConstraint;
import xyz.leutgeb.lorenz.logs.type.TypeError;
import xyz.leutgeb.lorenz.logs.unification.Substitution;
import xyz.leutgeb.lorenz.logs.unification.UnificationError;

/** TODO: Depth First Search using a List as stack. Plainly walking a tree. */
public class ParserTest {
  private static final String PLUS = "~";

  @ParameterizedTest
  @ValueSource(
    strings = {
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
                        enumeration(fileNameStream.map(this::open).collect(toUnmodifiableList())))),
                fileNames));
    program.normalize();
    assertNotNull(program);

    // Check that prettyprinting emits syntactically correct programs.
    var output = new ByteArrayOutputStream();
    program.printTo(new PrintStream(output), false);
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

  private InputStream open(String fileName) {
    if (!fileName.endsWith(".ml")) {
      fileName += ".ml";
    }
    return getClass().getResourceAsStream("/" + fileName);
  }

  private static FunctionType ft(Type to, Type... from) {
    return new FunctionType(Arrays.asList(from), to);
  }

  private static final TreeType ATREE = new TreeType(ALPHA);

  static Stream<Arguments> generateSnippets() {
    return Stream.of(
        Arguments.of("f t = match t with | (a, b, c) -> t", ft(ATREE, ATREE)),
        Arguments.of("f t = match t with | (a, b, c) -> (a, b, c)", ft(ATREE, ATREE)),
        Arguments.of("f x = x", ft(ALPHA, ALPHA)),
        Arguments.of("f = nil", ft(ATREE)),
        Arguments.of("f x y = x", ft(ALPHA, ALPHA, BETA)),
        Arguments.of("f x y = y", ft(BETA, ALPHA, BETA)),
        Arguments.of("f x y = if x < y then false else true", ft(BoolType.INSTANCE, ALPHA, ALPHA)),
        Arguments.of("f x = match x with | (y, _, _) -> y", ft(ATREE, ATREE))
        // The following requires identifiers as conditions (grammar change):
        // Arguments.of("f x = if x then false else true", ft(BoolType.INSTANCE, BoolType.INSTANCE))
        );
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
    Program program =
        parse(
            "f x y = x \n\ng x = x \n\nh x y z = match g (f x y) with | (a, b, c) -> let foo = g (f a b) in (foo, b, c)");
    program.infer();
    program.printTo(System.out);
    assertNotNull(program);
  }
}
