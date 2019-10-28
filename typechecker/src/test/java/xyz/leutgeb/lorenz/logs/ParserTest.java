package xyz.leutgeb.lorenz.logs;

import static java.util.Collections.emptySet;
import static java.util.Collections.singleton;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.junit.jupiter.params.provider.Arguments.arguments;
import static xyz.leutgeb.lorenz.logs.typing.TypeVariable.ALPHA;
import static xyz.leutgeb.lorenz.logs.typing.TypeVariable.BETA;

import java.io.File;
import java.io.FileOutputStream;
import java.io.PrintStream;
import java.nio.file.Path;
import java.util.Set;
import java.util.stream.Stream;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.Arguments;
import org.junit.jupiter.params.provider.MethodSource;
import xyz.leutgeb.lorenz.logs.ast.Program;
import xyz.leutgeb.lorenz.logs.typing.FunctionSignature;
import xyz.leutgeb.lorenz.logs.typing.TypeClass;
import xyz.leutgeb.lorenz.logs.typing.TypeConstraint;
import xyz.leutgeb.lorenz.logs.typing.types.BoolType;
import xyz.leutgeb.lorenz.logs.typing.types.FunctionType;
import xyz.leutgeb.lorenz.logs.typing.types.TreeType;
import xyz.leutgeb.lorenz.logs.typing.types.Type;

public class ParserTest {
  private static final String PLUS = "~";
  private static final TreeType ATREE = new TreeType(ALPHA);
  private static final TreeType BTREE = new TreeType(BETA);
  private static final BoolType BOOL = BoolType.INSTANCE;
  private static final File OUT = new File("..", "out");

  private static Loader loader() {
    return new Loader(Path.of(".", "src", "test", "resources"));
  }

  private static Stream<Arguments> fqns() {
    return Stream.of(
        arguments("PairingHeap.merge", sig(Set.of(ord(ALPHA)), ATREE, ATREE, ATREE)),
        arguments("LeftList.postorder", sig(ATREE, ATREE, ATREE)),
        arguments("LeftList.rev_append", sig(ATREE, ATREE, ATREE)),
        arguments("LeftList.append", sig(ATREE, ATREE, ATREE)),
        arguments("RightList.append", sig(ATREE, ATREE, ATREE)),
        arguments("RightList.rev_append", sig(ATREE, ATREE, ATREE)),
        arguments("LeftList.inorder", sig(ATREE, ATREE, ATREE)),
        arguments("LeftList.preorder", sig(ATREE, ATREE, ATREE)),
        arguments("Tree.contains_unordered", sig(singleton(eq(ALPHA)), ALPHA, ATREE, BOOL)),
        arguments("LeftList.postorder", sig(ATREE, ATREE, ATREE)),
        arguments("SplayTree.splay_max", sig(Set.of(eq(ATREE)), ATREE, ATREE)),
        arguments("LeftList.cons", sig(ALPHA, ATREE, ATREE)),
        arguments("LeftList.tl", sig(ATREE, ATREE)));
    /*
    arguments("SplayTree.splay", sig(Set.of(eq(ATREE), ord(ALPHA)), ALPHA, ATREE, ATREE)),
    arguments("SplayTree.delete", sig(Set.of(eq(ATREE), ord(ALPHA)), ALPHA, ATREE, ATREE)),
    arguments("SkewHeap.merge", sig(Set.of(ord(ALPHA)), ATREE, ATREE, ATREE)),
    arguments("SkewHeap.insert", sig(Set.of(ord(ALPHA)), ALPHA, ATREE, ATREE)));
         */
  }

  private static FunctionType ft(Type... types) {
    return new FunctionType(types);
  }

  private static TypeConstraint eq(Type alpha) {
    return new TypeConstraint(TypeClass.EQ, alpha);
  }

  private static TypeConstraint ord(Type alpha) {
    return new TypeConstraint(TypeClass.ORD, alpha);
  }

  private static FunctionSignature sig(Set<TypeConstraint> constraints, Type... types) {
    return new FunctionSignature(constraints, types);
  }

  private static FunctionSignature sig(Type... types) {
    return new FunctionSignature(emptySet(), types);
  }

  @ParameterizedTest(name = "{0} : {1}")
  @MethodSource("fqns")
  void snippets(String fqn, FunctionSignature expectedSignature) throws Exception {
    final var loader = loader();
    final Program program = loader.load(fqn);
    program.infer();
    for (var e : program.getFunctionDefinitions()) {
      e.printTo(new PrintStream(new File(OUT, e.getName() + ".ml").getAbsoluteFile()));
      e.printHaskellTo(new PrintStream(new File(OUT, e.getName() + ".hs").getAbsoluteFile()));
    }

    final var definition =
        program.getFunctionDefinitions().stream()
            .filter(x -> x.getFullyQualifiedName().equals(fqn))
            .findFirst();

    // final var definition =
    // program.getFunctionDefinitions().get(program.getFunctionDefinitions().size() - 1);

    assertTrue(definition.isPresent());
    assertEquals(expectedSignature, definition.get().getSignature());

    program.solve();
  }

  @Test
  void pairingHeapMerge() throws Exception {
    final String fqn = "PairingHeap.merge";
    final var expectedSignature = sig(Set.of(ord(ALPHA)), ATREE, ATREE, ATREE);
    final var loader = loader();
    final Program program = loader.load(fqn);
    program.infer();

    final var definition =
        program.getFunctionDefinitions().stream()
            .filter(x -> x.getFullyQualifiedName().equals(fqn))
            .findFirst();

    // final var definition =
    // program.getFunctionDefinitions().get(program.getFunctionDefinitions().size() - 1);

    assertTrue(definition.isPresent());
    assertEquals(expectedSignature, definition.get().getSignature());

    program.solve();
  }

  @Test
  void all() throws Exception {
    final var loader = loader();
    loader.autoload();
    final var program = loader.all();
    loader.exportGraph(new FileOutputStream(new File(OUT, "all.dot")));
    program.infer();
    for (var fd : program.getFunctionDefinitions()) {
      System.out.println(fd.getFullyQualifiedName() + " : " + fd.getSignature());
    }
  }

  @Test
  void fiddle() throws Exception {
    Loader loader = Loader.atCurrentWorkingDirectory();
    Program program = loader.loadInline("swp t = match t with | (l, m, r) -> (r, m, l)");
    program.infer();
    program.solve();
    program.printTo(System.out);
    assertNotNull(program);
  }
}
