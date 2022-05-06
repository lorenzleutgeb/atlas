package xyz.leutgeb.lorenz.atlas;

import static org.hipparchus.fraction.Fraction.ONE_HALF;
import static org.hipparchus.fraction.Fraction.ONE_THIRD;
import static org.hipparchus.fraction.Fraction.TWO_THIRDS;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.junit.jupiter.api.Assertions.fail;
import static xyz.leutgeb.lorenz.atlas.Tactics.logToLog;
import static xyz.leutgeb.lorenz.atlas.typing.resources.Annotation.unitIndex;
import static xyz.leutgeb.lorenz.atlas.typing.resources.coefficients.Coefficient.known;
import static xyz.leutgeb.lorenz.atlas.typing.resources.coefficients.KnownCoefficient.TWO;

import com.google.common.collect.Lists;
import java.util.Collections;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.stream.Stream;
import org.hipparchus.fraction.Fraction;
import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.Arguments;
import org.junit.jupiter.params.provider.MethodSource;
import xyz.leutgeb.lorenz.atlas.ast.Program;
import xyz.leutgeb.lorenz.atlas.typing.resources.Annotation;
import xyz.leutgeb.lorenz.atlas.typing.resources.CombinedFunctionAnnotation;
import xyz.leutgeb.lorenz.atlas.typing.resources.coefficients.Coefficient;
import xyz.leutgeb.lorenz.atlas.typing.resources.coefficients.KnownCoefficient;
import xyz.leutgeb.lorenz.atlas.typing.resources.constraints.EqualsProductConstraint;
import xyz.leutgeb.lorenz.atlas.util.Pair;

public class Matrix {
  private static List<Fraction> fracs = List.of(ONE_HALF, ONE_THIRD, TWO_THIRDS);

  private static String moduleName(Fraction p, Fraction rec) {
    final var rot = Fraction.ONE.subtract(rec);

    return "RandSplayTreeMatrix.P"
        + p.getNumerator()
        + p.getDenominator()
        + "C"
        + rec.getNumerator()
        + rec.getDenominator();
  }

  private static final Map<Pair<Fraction, Fraction>, CombinedFunctionAnnotation> RESULTS =
      Map.of(
          Pair.of(ONE_THIRD, ONE_THIRD),
          shape(known(5, 6), known(5, 6), known(ONE_THIRD)),
          Pair.of(ONE_THIRD, ONE_HALF),
          shape(known(1), known(1), known(ONE_HALF)),
          Pair.of(ONE_THIRD, TWO_THIRDS),
          shape(known(7, 6), known(7, 6), known(TWO_THIRDS)),
          Pair.of(ONE_HALF, ONE_THIRD),
          shape(known(2, 3), known(1), known(ONE_THIRD)),
          Pair.of(ONE_HALF, ONE_HALF),
          shape(known(3, 4), known(9, 8), known(ONE_HALF)),
          Pair.of(ONE_HALF, TWO_THIRDS),
          shape(known(5, 6), known(5, 4), known(TWO_THIRDS)),
          Pair.of(TWO_THIRDS, ONE_THIRD),
          shape(known(7, 9), known(77, 54), known(ONE_THIRD)),
          Pair.of(TWO_THIRDS, ONE_HALF),
          shape(known(5, 6), known(55, 36), known(ONE_HALF)),
          Pair.of(TWO_THIRDS, TWO_THIRDS),
          shape(known(8, 9), known(44, 27), known(TWO_THIRDS)));

  private static CombinedFunctionAnnotation shape(
      KnownCoefficient rank, KnownCoefficient log, KnownCoefficient c) {
    return CombinedFunctionAnnotation.of(
        new Annotation(List.of(rank), Map.of(List.of(1, 0), log, unitIndex(1), c), "Q"),
        new Annotation(List.of(rank), Map.of(unitIndex(1), c), "Q'"),
        logToLog(Coefficient.known(rank.getValue().divide(2))));
  }

  public static Stream<Arguments> generate() {
    return Lists.cartesianProduct(fracs, fracs).stream().map(x -> Arguments.of(x.get(0), x.get(1)));
  }

  @ParameterizedTest
  @MethodSource("generate")
  public void verify(Fraction p, Fraction rec) {
    String moduleName = moduleName(p, rec);
    final String fqn = moduleName + ".splay";
    Program program = TestUtil.loadAndNormalizeAndInferAndUnshare(fqn);

    if (!RESULTS.containsKey(Pair.of(p, rec))) {
      fail("No annotation to check.");
    }

    CombinedFunctionAnnotation dummy = RESULTS.get(Pair.of(p, rec));

    final var result =
        program.solve(
            new HashMap<>(Map.of(fqn, dummy)),
            Collections.emptyMap(),
            false,
            false,
            false,
            Collections.emptySet());

    assertTrue(result.isSatisfiable());
    program.printAllInferredSignaturesInOrder(System.out);
    program.printAllInferredBoundsInOrder(System.out);
  }

  @ParameterizedTest
  @MethodSource("generate")
  public void infer(Fraction p, Fraction rec) {
    String moduleName = moduleName(p, rec);
    final String fqn = moduleName + ".splay";
    Program program = TestUtil.loadAndNormalizeAndInferAndUnshare(fqn);

    final var rk = Coefficient.unknown("rankCoeff");
    final var rkBy2 = Coefficient.unknown("rankCoeffBy2");
    CombinedFunctionAnnotation dummy =
        CombinedFunctionAnnotation.of(
            new Annotation(
                List.of(rk),
                Map.of(
                    List.of(1, 0), Coefficient.unknown("logIn"),
                    unitIndex(1), Coefficient.unknown("constIn")),
                "Q"),
            new Annotation(
                List.of(rk), Map.of(unitIndex(1), Coefficient.unknown("constReturn")), "Q'"),
            logToLog(rkBy2));

    final var externalConstraints =
        Set.of(new EqualsProductConstraint(rk, List.of(TWO, rkBy2), "(fix) cf"));

    System.out.println(dummy);

    final var result =
        program.solve(
            new HashMap<>(Map.of(fqn, dummy)),
            Collections.emptyMap(),
            Program.InferenceMode.DIRECT,
            false,
            false,
            false,
            false,
            new HashSet<>(externalConstraints));

    assertTrue(result.isSatisfiable());
    program.printAllInferredSignaturesInOrder(System.out);
    program.printAllInferredBoundsInOrder(System.out);
  }
}
