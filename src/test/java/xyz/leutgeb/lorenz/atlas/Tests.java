package xyz.leutgeb.lorenz.atlas;

import static java.util.Collections.emptyList;
import static java.util.Collections.emptyMap;
import static java.util.Collections.emptySet;
import static java.util.Collections.singleton;
import static java.util.Collections.singletonList;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.junit.jupiter.params.provider.Arguments.arguments;
import static xyz.leutgeb.lorenz.atlas.typing.resources.Annotation.unitIndex;
import static xyz.leutgeb.lorenz.atlas.typing.resources.coefficients.KnownCoefficient.ZERO;
import static xyz.leutgeb.lorenz.atlas.typing.simple.TypeConstraint.ord;
import static xyz.leutgeb.lorenz.atlas.typing.simple.TypeVariable.alpha;
import static xyz.leutgeb.lorenz.atlas.typing.simple.TypeVariable.beta;
import static xyz.leutgeb.lorenz.atlas.util.Util.fqnToFlatFilename;
import static xyz.leutgeb.lorenz.atlas.util.Z3Support.load;

import java.io.File;
import java.io.FileOutputStream;
import java.io.PrintStream;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.stream.Collectors;
import java.util.stream.Stream;
import org.hipparchus.fraction.Fraction;
import org.jgrapht.nio.AttributeType;
import org.jgrapht.nio.DefaultAttribute;
import org.junit.jupiter.api.BeforeAll;
import org.junit.jupiter.api.Disabled;
import org.junit.jupiter.api.DisplayName;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.Arguments;
import org.junit.jupiter.params.provider.CsvSource;
import org.junit.jupiter.params.provider.MethodSource;
import xyz.leutgeb.lorenz.atlas.ast.Program;
import xyz.leutgeb.lorenz.atlas.ast.expressions.IdentifierExpression;
import xyz.leutgeb.lorenz.atlas.typing.resources.Annotation;
import xyz.leutgeb.lorenz.atlas.typing.resources.CombinedFunctionAnnotation;
import xyz.leutgeb.lorenz.atlas.typing.resources.FunctionAnnotation;
import xyz.leutgeb.lorenz.atlas.typing.resources.coefficients.Coefficient;
import xyz.leutgeb.lorenz.atlas.typing.resources.coefficients.KnownCoefficient;
import xyz.leutgeb.lorenz.atlas.typing.resources.constraints.InequalityConstraint;
import xyz.leutgeb.lorenz.atlas.typing.resources.constraints.LessThanOrEqualConstraint;
import xyz.leutgeb.lorenz.atlas.typing.resources.constraints.OffsetConstraint;
import xyz.leutgeb.lorenz.atlas.typing.resources.proving.Prover;
import xyz.leutgeb.lorenz.atlas.typing.resources.solving.Solver;
import xyz.leutgeb.lorenz.atlas.typing.simple.FunctionSignature;
import xyz.leutgeb.lorenz.atlas.typing.simple.TypeConstraint;
import xyz.leutgeb.lorenz.atlas.typing.simple.types.BoolType;
import xyz.leutgeb.lorenz.atlas.typing.simple.types.TreeType;
import xyz.leutgeb.lorenz.atlas.typing.simple.types.Type;
import xyz.leutgeb.lorenz.atlas.util.NidiExporter;
import xyz.leutgeb.lorenz.atlas.util.SizeEdge;
import xyz.leutgeb.lorenz.atlas.util.Util;

public class Tests {
  static final TreeType ATREE = new TreeType(alpha());
  private static final TreeType BTREE = new TreeType(beta());
  private static final BoolType BOOL = BoolType.INSTANCE;
  private static final File OUT = new File("out");

  private static Stream<Arguments> nonConstantCostDefinitions() {
    return Stream.of(
        arguments("LeftList.postorder", sig(ATREE, ATREE, ATREE), ExpectedResult.UNKNOWN),
        arguments("LeftList.rev_append", sig(ATREE, ATREE, ATREE), ExpectedResult.UNKNOWN),
        arguments("LeftList.append", sig(ATREE, ATREE, ATREE), ExpectedResult.UNKNOWN),
        arguments("RightList.append", sig(ATREE, ATREE, ATREE), ExpectedResult.UNKNOWN),
        arguments("RightList.rev_append", sig(ATREE, ATREE, ATREE), ExpectedResult.UNKNOWN),
        /*
        arguments("LeftList.inorder", sig(ATREE, ATREE, ATREE), ExpectedResult.UNKNOWN),
        arguments("LeftList.preorder", sig(ATREE, ATREE, ATREE), ExpectedResult.UNKNOWN),
        arguments(
            "Scratch.contains_unordered",
            sig(singleton(eq(alpha())), alpha(), ATREE, BOOL),
            ExpectedResult.UNKNOWN),
         */
        arguments("LeftList.postorder", sig(ATREE, ATREE, ATREE), ExpectedResult.UNKNOWN),
        arguments("SplayTree.splay_max", sig(ATREE, ATREE), ExpectedResult.SAT),
        /*
        arguments(
            "SplayTree.splay",
            sig(Set.of(ord(alpha())), alpha(), ATREE, ATREE),
            ExpectedResult.SAT),
        arguments(
            "SplayTree.splay_eq",
            sig(Set.of(ord(alpha()), eq(ATREE)), alpha(), ATREE, ATREE),
            ExpectedResult.SAT),
        arguments(
            "SplayTree.delete",
            sig(Set.of(ord(alpha())), alpha(), ATREE, ATREE),
            ExpectedResult.UNKNOWN),
         */
        arguments(
            "SkewHeap.merge",
            sig(singleton(ord(alpha())), ATREE, ATREE, ATREE),
            ExpectedResult.UNKNOWN),
        arguments(
            "SkewHeap.insert",
            sig(singleton(ord(alpha())), alpha(), ATREE, ATREE),
            ExpectedResult.UNKNOWN),
        // arguments("PairingHeap.del_min", sig(singleton(ord(alpha())), ATREE, ATREE)),
        arguments("PairingHeap.merge_pairs", sig(singleton(ord(alpha())), ATREE, ATREE)));
  }

  private static Stream<Arguments> constantCostDefinitions() {
    return Stream.of(
        arguments("LeftList.cons", sig(alpha(), ATREE, ATREE), 0),
        arguments("LeftList.cons_cons", sig(alpha(), alpha(), ATREE, ATREE), 0),
        arguments("RightList.cons", sig(alpha(), ATREE, ATREE), 0),
        arguments("RightList.cons_cons", sig(alpha(), alpha(), ATREE, ATREE), 0),
        arguments("Tree.singleton", sig(alpha(), ATREE), 0),
        arguments("Bool.not", sig(BOOL, BOOL), 0),
        arguments("Bool.or", sig(BOOL, BOOL, BOOL), 0),
        arguments("Bool.and", sig(BOOL, BOOL, BOOL), 0),
        arguments("RightList.tl", sig(ATREE, ATREE), 0),
        arguments("LeftList.tl", sig(ATREE, ATREE), 0),
        arguments("Tree.right", sig(ATREE, ATREE), 0),
        arguments("Tree.left", sig(ATREE, ATREE), 0),
        arguments("Tree.flip", sig(ATREE, ATREE), 0),
        arguments("Tree.empty", sig(ATREE, BOOL), 0),
        arguments("Tree.clone", sig(alpha(), ATREE, ATREE), 0),
        arguments("PairingHeap.is_root", sig(ATREE, BOOL), 0),
        arguments("PairingHeap.link", sig(singleton(ord(alpha())), ATREE, ATREE), 0),
        // arguments("PairingHeap.merge", sig(singleton(ord(alpha())), ATREE, ATREE, ATREE), 1),
        // arguments("PairingHeap.insert", sig(singleton(ord(alpha())), alpha(), ATREE, ATREE), 3),
        // arguments("Scratch.empty_1", sig(ATREE, BOOL), 1),
        // arguments("Scratch.empty_2", sig(ATREE, BOOL), 1),
        // arguments("Scratch.empty_3", sig(ATREE, BOOL), 0),
        arguments("Prelude.id", sig(alpha(), alpha()), 0),
        arguments("Pair.fst", sig(alpha(), beta(), alpha()), 0),
        arguments("Pair.snd", sig(alpha(), beta(), beta()), 0)
        // arguments("Scratch.first_nonempty_and_second_empty", sig(ATREE, BTREE, BOOL), 1)
        );
  }

  private static FunctionSignature sig(Set<TypeConstraint> constraints, Type... types) {
    return new FunctionSignature(constraints, types);
  }

  private static FunctionSignature sig(Type... types) {
    return new FunctionSignature(emptySet(), types);
  }

  private static Stream<Arguments> infiniteCostDefinitions() {
    return Stream.of(
        arguments("Infinite.infinite_1"),
        arguments("Infinite.infinite_2"),
        arguments("Infinite.infinite_3"),
        arguments("Infinite.infinite_4"),
        arguments("Infinite.infinite_5"),
        arguments("Infinite.infinite_6"),
        arguments("Infinite.infinite_7"),
        arguments("Infinite.infinite_8"),
        arguments("Infinite.infinite_9"),
        arguments("Infinite.infinite_10"),
        arguments("Infinite.infinite_11"),
        arguments("Infinite.infinite_12"),
        arguments("Infinite.infinite_13"),
        arguments("Infinite.infinite_14"),
        arguments("Infinite.infinite_15"),
        arguments("Infinite.infinite_16"),
        arguments("Infinite.infinite_17"),
        arguments("Infinite.infinite_18"),
        arguments("Infinite.infinite_19a"),
        arguments("Infinite.infinite_19b"));
  }

  static NidiExporter<IdentifierExpression, SizeEdge> exporter() {
    final var exporter =
        new NidiExporter<IdentifierExpression, SizeEdge>(
            identifier ->
                identifier.getName() + "_" + (identifier.getIntro() == null ? "null" : identifier));
    exporter.setVertexAttributeProvider(
        v -> Map.of("label", new DefaultAttribute<>(v.getName(), AttributeType.STRING)));
    exporter.setEdgeAttributeProvider(
        e ->
            Map.of(
                // "label",
                // new DefaultAttribute<>(e.getKind().toString(), AttributeType.STRING),
                "color",
                new DefaultAttribute<>(
                    e.getKind().equals(SizeEdge.Kind.EQ) ? "blue4" : "red", AttributeType.STRING)));
    return exporter;
  }

  @BeforeAll
  public static void beforeAll() {
    System.setProperty("z3.skipLibraryLoad", "true");
    //load();
  }

  @Disabled
  @ParameterizedTest
  @MethodSource("nonConstantCostDefinitions")
  @DisplayName("Non-Constant Cost")
  void nonConstantCost(String fqn, FunctionSignature expectedSignature) throws Exception {
    final var program = TestUtil.loadAndNormalizeAndInferAndUnshare(fqn);
    for (var e : program.getFunctionDefinitions().values()) {
      e.printHaskellTo(new PrintStream(new File(OUT, e.getName() + ".hs").getAbsoluteFile()));
    }

    final var definition = program.getFunctionDefinitions().get(fqn);

    assertNotNull(definition);
    assertEquals(expectedSignature, definition.getAnnotatedSignature(), "annotated signature");
    assertEquals(expectedSignature, definition.getInferredSignature(), "inferred signature");

    final Solver.Result result =
        program.solve(new HashMap<>(), emptyMap(), true, false, false, new HashSet<>());
    assertTrue(result.isSatisfiable());
    program.printAllInferredSignaturesInOrder(System.out);
  }

  @ParameterizedTest
  // @Disabled("assume finiteness")
  @MethodSource("infiniteCostDefinitions")
  void infiniteCost(String fqn) throws Exception {
    System.setProperty(Util.getPropertyName(Prover.class, "tickAst"), "true");
    final var program = TestUtil.loadAndNormalizeAndInferAndUnshare(fqn);
    final var solution =
        program.solve(new HashMap<>(), emptyMap(), true, false, false, new HashSet<>());
    program.printAllInferredSignaturesInOrder(System.out);
    assertTrue(solution.getSolution().isEmpty());
  }

  @Test
  @Disabled("linear")
  void revAppend() throws Exception {
    final var fqn = "LeftList.rev_append";
    final var expectedSignature = sig(ATREE, ATREE, ATREE);
    final var program = TestUtil.loadAndNormalizeAndInferAndUnshare(fqn);
    final var definition = program.getFunctionDefinitions().get(fqn);

    assertEquals(expectedSignature, definition.getInferredSignature());

    final var solution =
        program.solve(new HashMap<>(), emptyMap(), true, false, false, new HashSet<>());
    program.printAllInferredSignaturesInOrder(System.out);
    assertTrue(solution.getSolution().isEmpty());
  }

  @ParameterizedTest
  @MethodSource("constantCostDefinitions")
  @DisplayName("Constant Cost")
  // @Timeout(value = 15)
  void constantCost(final String fqn, FunctionSignature expectedSignature, int constantCost)
      throws Exception {
    final var program = TestUtil.loadAndNormalizeAndInferAndUnshare(fqn);
    final var definition = program.getFunctionDefinitions().get(fqn);

    assertNotNull(definition);
    assertEquals(expectedSignature, definition.getInferredSignature());

    final var returnsTree = expectedSignature.getType().getTo().countTrees().get() > 0;

    final List<Coefficient> args =
        Stream.generate(() -> ZERO)
            .limit(expectedSignature.getType().getFrom().countTrees().get())
            .collect(Collectors.toList());

    // If there are problems with one test case, use this snippet to check for satisfiability.

    final var inferredInput = Coefficient.unknown("inferredInput");
    final var inferredResult = Coefficient.unknown("inferredResult");
    final var inferred = new HashMap<String, CombinedFunctionAnnotation>();
    inferred.put(
        fqn,
        new CombinedFunctionAnnotation(
            new FunctionAnnotation(
                Annotation.constant(
                    expectedSignature.getType().getFrom().countTrees().get(),
                    "inferredArgs",
                    inferredInput),
                new Annotation(
                    returnsTree ? singletonList(ZERO) : emptyList(),
                    returnsTree ? Map.of(unitIndex(1), inferredResult) : emptyMap(),
                    "inferredReturn")),
            emptySet()));
    final var inferredSolution =
        program.solve(inferred, emptyMap(), true, false, false, emptySet());
    assertTrue(inferredSolution.getSolution().isPresent());
    program.printAllInferredSignaturesInOrder(System.out);

    // We show that it is possible to type the function in such a way that the difference between
    // the potential of the arguments and the potential of the result is exactly the cost that we
    // expect.
    final var tightInput = Coefficient.unknown("tightInput");
    final var tightResult = Coefficient.unknown("tightResult");
    final var tight = new HashMap<String, CombinedFunctionAnnotation>();
    tight.put(
        fqn,
        new CombinedFunctionAnnotation(
            new FunctionAnnotation(
                Annotation.constant(
                    expectedSignature.getType().getFrom().countTrees().get(),
                    "expectedArgs",
                    tightInput),
                new Annotation(
                    returnsTree ? singletonList(ZERO) : emptyList(),
                    returnsTree ? Map.of(unitIndex(1), tightResult) : emptyMap(),
                    "expectedReturn")),
            emptySet()));

    assertTrue(
        program
            .solve(
                tight,
                emptyMap(),
                true,
                false,
                false,
                singleton(
                    new OffsetConstraint(
                        tightInput, tightResult, new Fraction(constantCost), "outside")))
            .getSolution()
            .isPresent());

    if (constantCost > 0) {
      // We show that it is impossible to type the function in such a way that the the potential of
      // the arguments is less than we expect.
      final var tooSmallInput = Coefficient.unknown("tightInput");
      final var tooSmall = new HashMap<String, CombinedFunctionAnnotation>();
      final var costKnownCoefficient = new KnownCoefficient(new Fraction(constantCost - 1));
      tooSmall.put(
          fqn,
          new CombinedFunctionAnnotation(
              new FunctionAnnotation(
                  new Annotation(
                      args,
                      Map.of(
                          unitIndex(expectedSignature.getType().getFrom().countTrees().get()),
                          tooSmallInput),
                      "expectedArgs"),
                  new Annotation(
                      returnsTree ? singletonList(ZERO) : emptyList(),
                      returnsTree ? Map.of(unitIndex(1), ZERO) : emptyMap(),
                      "expectedReturn")),
              emptySet()));

      assertTrue(
          program
              .solve(
                  tooSmall,
                  emptyMap(),
                  true,
                  false,
                  false,
                  singleton(
                      new LessThanOrEqualConstraint(
                          tooSmallInput, costKnownCoefficient, "outside constraint")))
              .getSolution()
              .isEmpty(),
          "No solution is expected, since it should not be possible to type the program with a cost of "
              + (constantCost - 1));
    }

    if (!returnsTree) {
      return;
    }

    // We show that it is impossible to type the function in such a way that the the potential of
    // the arguments is less than the potential of the result.
    final var generatorInput = Coefficient.unknown("generatorInput");
    final var generatorResult = Coefficient.unknown("generatorResult");
    final var symbolicGenerator = new HashMap<String, CombinedFunctionAnnotation>();
    symbolicGenerator.put(
        fqn,
        new CombinedFunctionAnnotation(
            new FunctionAnnotation(
                new Annotation(
                    args,
                    Map.of(
                        unitIndex(expectedSignature.getType().getFrom().countTrees().get()),
                        generatorInput),
                    "expectedGeneratorArgs"),
                new Annotation(
                    singletonList(ZERO),
                    Map.of(unitIndex(1), generatorResult),
                    "expectedGeneratorReturn")),
            emptySet()));

    final var perpetuumMobile =
        program.solve(
            symbolicGenerator,
            emptyMap(),
            true,
            false,
            false,
            Set.of(
                new LessThanOrEqualConstraint(
                    generatorInput, generatorResult, "outside constraint"),
                new InequalityConstraint(generatorInput, generatorResult, "outside constraint")));

    if (perpetuumMobile.getSolution().isPresent()) {
      program.printAllInferredSignaturesInOrder(System.out);
    }
    assertTrue(perpetuumMobile.getSolution().isEmpty(), "Perpetuum mobile!");
  }

  @ParameterizedTest
  @CsvSource({"true,lazy", "false,eager"})
  @Disabled
  void dumps(boolean lazy, String suffix) throws Exception {
    final var loader = TestUtil.loader();
    loader.autoload();
    final var program = loader.all();
    loader.exportGraph(new FileOutputStream(new File(OUT, "all.dot")));
    for (var fd : program.getFunctionDefinitions().values()) {
      fd.printTo(
          new PrintStream(
              new File(OUT, fqnToFlatFilename(fd.getFullyQualifiedName()) + "-loaded.ml")
                  .getAbsoluteFile()));
    }
    program.normalize();
    for (var fd : program.getFunctionDefinitions().values()) {
      fd.printTo(
          new PrintStream(
              new File(OUT, fqnToFlatFilename(fd.getFullyQualifiedName()) + "-normalized.ml")
                  .getAbsoluteFile()));
    }
    program.infer();
    program.printAllSimpleSignaturesInOrder(System.out);

    for (var fd : program.getFunctionDefinitions().values()) {
      fd.printJavaTo(System.out, true);
    }
    /*
    program.unshare(lazy);
    for (var fd : program.getFunctionDefinitions().values()) {
      fd.printTo(
          new PrintStream(
              new File(
                      OUT,
                      fqnToFlatFilename(fd.getFullyQualifiedName()) + "-" + suffix + "-unshared.ml")
                  .getAbsoluteFile()));
    }

    final var exporter = exporter();

    for (var fd : program.getFunctionDefinitions().values()) {
      fd.analyzeSizes();
      final var exp = exporter.transform(fd.getSizeAnalysis());
      final var viz = Graphviz.fromGraph(exp);
      try {
        viz.render(Format.SVG)
            .toOutputStream(
                new PrintStream(
                    new File(
                        OUT,
                        fqnToFlatFilename(fd.getFullyQualifiedName())
                            + "-"
                            + suffix
                            + "-sizes.svg")));
      } catch (IOException e) {
        e.printStackTrace();
      }
    }
    */
  }

  @Test
  void fiddle() throws Exception {
    var loader = TestUtil.loader();
    loader.autoload();
    Program program = loader.loadInline("foo t a = (t, a)");
    program.normalize();
    program.infer();

    String x = "";
    // program.dumpToJsh(new File(OUT, "Dump.jsh").toPath());
  }

  private enum ExpectedResult {
    SAT,
    UNSAT,
    UNKNOWN
  }
}
