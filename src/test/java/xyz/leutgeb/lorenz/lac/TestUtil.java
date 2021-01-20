package xyz.leutgeb.lorenz.lac;

import static java.util.Collections.emptyList;
import static java.util.stream.Collectors.toUnmodifiableList;
import static org.junit.jupiter.api.Assertions.fail;

import com.google.common.collect.Streams;
import java.io.IOException;
import java.nio.file.Path;
import java.util.ArrayList;
import java.util.Collection;
import java.util.List;
import java.util.Map;
import java.util.Optional;
import java.util.Set;
import java.util.function.Predicate;
import java.util.stream.Collectors;
import java.util.stream.IntStream;
import java.util.stream.Stream;
import java.util.stream.StreamSupport;
import tech.tablesaw.api.StringColumn;
import tech.tablesaw.api.Table;
import tech.tablesaw.columns.Column;
import xyz.leutgeb.lorenz.lac.ast.Program;
import xyz.leutgeb.lorenz.lac.module.Loader;
import xyz.leutgeb.lorenz.lac.typing.resources.AnnotatingContext;
import xyz.leutgeb.lorenz.lac.typing.resources.Annotation;
import xyz.leutgeb.lorenz.lac.typing.resources.coefficients.Coefficient;
import xyz.leutgeb.lorenz.lac.typing.resources.coefficients.KnownCoefficient;
import xyz.leutgeb.lorenz.lac.typing.resources.proving.Obligation;
import xyz.leutgeb.lorenz.lac.typing.resources.proving.Prover;
import xyz.leutgeb.lorenz.lac.typing.simple.TypeError;
import xyz.leutgeb.lorenz.lac.unification.UnificationError;

public class TestUtil {
  public static Obligation fromProver(Prover prover, Predicate<Obligation> predicate) {
    return StreamSupport.stream(prover.getProof().spliterator(), false)
        .filter(predicate)
        .findFirst()
        .orElseGet(() -> fail("missing obligation"));
  }

  public static String printTable(List<AnnotatingContext> ac) {
    return printTable(ac, emptyList());
  }

  public static String printTable(List<AnnotatingContext> ac, List<Annotation> a) {
    var ranks =
        Stream.concat(ac.stream().map(AnnotatingContext::size), a.stream().map(Annotation::size))
            .mapToInt(x -> x)
            .max()
            .orElse(0);

    var potentialFunctions =
        Stream.concat(ac.stream().map(AnnotatingContext::getAnnotation), a.stream())
            .flatMap(Annotation::streamNonRankCoefficients)
            .map(Map.Entry::getKey)
            .sorted(Annotation.INDEX_COMPARATOR)
            .distinct()
            .collect(toUnmodifiableList());

    Stream<Column<?>> columns =
        Stream.concat(
            Stream.of(
                StringColumn.create(
                    "Index",
                    Streams.concat(
                            IntStream.range(0, ranks).mapToObj(String::valueOf),
                            potentialFunctions.stream())
                        .map(Object::toString)
                        .collect(Collectors.toList()))),
            Streams.zip(
                Stream.concat(a.stream(), ac.stream().map(AnnotatingContext::getAnnotation)),
                Stream.concat(
                    a.stream().map(Annotation::getName),
                    ac.stream().map(AnnotatingContext::toString)),
                (annotation, name) ->
                    StringColumn.create(
                        name,
                        Stream.concat(
                            IntStream.range(0, ranks)
                                .mapToObj(
                                    rank ->
                                        annotation.size() > rank
                                            ? annotation.getRankCoefficient(rank).toString()
                                            : "0"),
                            potentialFunctions.stream()
                                .map(
                                    potentialFunction ->
                                        annotation.size() + 1 == potentialFunction.size()
                                            ? annotation
                                                .getCoefficientOrZero(potentialFunction)
                                                .toString()
                                            : "·")))));

    return Table.create("Overview", columns).print(Integer.MAX_VALUE / 2);
  }

  public static String printTable(
      Prover prover, Optional<Map<Coefficient, KnownCoefficient>> solution) {
    if (solution.isEmpty()) {
      return "UNSAT";
    }

    var acs = new ArrayList<AnnotatingContext>(prover.getNamed().size());
    var as = new ArrayList<Annotation>(prover.getNamed().size());

    if (as.isEmpty() && acs.isEmpty()) {
      return "";
    }

    for (var entry : prover.getNamed().entrySet()) {
      acs.add(entry.getValue().getContext().substitute(solution.get()).rename(entry.getKey()));
      as.add(
          entry.getValue().getAnnotation().substitute(solution.get()).rename(entry.getKey() + "'"));
    }

    return printTable(acs, as);
  }

  public static Program loadAndNormalizeAndInferAndUnshare(String... fqns)
      throws UnificationError, TypeError, IOException {
    return loadAndNormalizeAndInferAndUnshare(Set.of(fqns));
  }

  public static Program loadAndNormalizeAndInferAndUnshare(Collection<String> fqns)
      throws UnificationError, TypeError, IOException {
    final var result = loader().load(Set.copyOf(fqns));
    result.normalize();
    result.infer();
    result.unshare(true);
    result.analyzeSizes();
    return result;
  }

  public static Loader loader() {
    return new Loader(Path.of(".", "src", "test", "resources", "examples"));
  }
}
