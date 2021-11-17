package xyz.leutgeb.lorenz.atlas.typing.resources;

import static com.google.common.collect.Comparators.lexicographical;
import static java.util.Collections.emptyList;
import static java.util.Collections.emptyMap;
import static java.util.Collections.singletonList;
import static java.util.function.Function.identity;
import static java.util.function.Predicate.not;
import static java.util.stream.Collectors.joining;
import static java.util.stream.Collectors.toList;
import static java.util.stream.Collectors.toMap;
import static java.util.stream.IntStream.range;
import static java.util.stream.Stream.concat;
import static xyz.leutgeb.lorenz.atlas.typing.resources.coefficients.KnownCoefficient.ONE;
import static xyz.leutgeb.lorenz.atlas.typing.resources.coefficients.KnownCoefficient.ZERO;
import static xyz.leutgeb.lorenz.atlas.util.Util.bug;
import static xyz.leutgeb.lorenz.atlas.util.Util.generateSubscript;
import static xyz.leutgeb.lorenz.atlas.util.Util.generateSubscriptIndex;
import static xyz.leutgeb.lorenz.atlas.util.Util.isAllZeroes;
import static xyz.leutgeb.lorenz.atlas.util.Util.randomHex;
import static xyz.leutgeb.lorenz.atlas.util.Util.repeat;

import com.google.common.collect.Streams;
import com.google.common.primitives.Ints;
import java.util.AbstractMap;
import java.util.ArrayList;
import java.util.Comparator;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Objects;
import java.util.Set;
import java.util.concurrent.atomic.AtomicInteger;
import java.util.stream.Collectors;
import java.util.stream.IntStream;
import java.util.stream.Stream;
import lombok.Getter;
import lombok.extern.slf4j.Slf4j;
import org.hipparchus.fraction.Fraction;
import xyz.leutgeb.lorenz.atlas.typing.resources.coefficients.Coefficient;
import xyz.leutgeb.lorenz.atlas.typing.resources.coefficients.KnownCoefficient;
import xyz.leutgeb.lorenz.atlas.typing.resources.coefficients.UnknownCoefficient;
import xyz.leutgeb.lorenz.atlas.typing.resources.constraints.Constraint;
import xyz.leutgeb.lorenz.atlas.typing.resources.constraints.EqualityConstraint;
import xyz.leutgeb.lorenz.atlas.typing.resources.constraints.EqualsProductConstraint;
import xyz.leutgeb.lorenz.atlas.typing.resources.constraints.EqualsSumConstraint;
import xyz.leutgeb.lorenz.atlas.typing.resources.constraints.OffsetConstraint;
import xyz.leutgeb.lorenz.atlas.util.Pair;
import xyz.leutgeb.lorenz.atlas.util.Util;

@Slf4j
public class Annotation {
  public static final Comparator<Iterable<Integer>> INDEX_COMPARATOR =
      lexicographical(Integer::compareTo);

  private final List<Coefficient> rankCoefficients;
  private final Map<List<Integer>, Coefficient> coefficients;

  @Getter String name;

  private static final AtomicInteger COUNT = new AtomicInteger();

  @Getter private final int id;

  public Annotation(int size, String name) {
    this.name = name;
    this.id = COUNT.getAndIncrement();
    rankCoefficients = new ArrayList<>(size);
    for (int i = 0; i < size; i++) {
      rankCoefficients.add(null);
    }
    coefficients = new HashMap<>();
  }

  public Annotation(
      int size,
      List<Integer> rankCoefficients,
      List<List<Integer>> nonRankCoefficients,
      String name) {
    this(size, name);
    for (var i : rankCoefficients) {
      if (i >= size) {
        throw new IllegalArgumentException();
      }
    }
    for (var l : nonRankCoefficients) {
      if (l.size() != this.size() + 1) {
        throw new IllegalArgumentException();
      }
    }
    for (var l : nonRankCoefficients) {
      this.getCoefficientOrDefine(l);
    }
    for (int i : rankCoefficients) {
      this.getRankCoefficientOrDefine(i);
    }
  }

  public Annotation(Coefficient rankCoefficient, Map<List<Integer>, Coefficient> coefficients) {
    this(singletonList(rankCoefficient), coefficients, randomHex());
  }

  public Annotation(
      List<Coefficient> rankCoefficients,
      Map<List<Integer>, Coefficient> coefficients,
      String name) {
    this.name = name;
    this.id = COUNT.getAndIncrement();
    this.rankCoefficients = new ArrayList<>(rankCoefficients);

    for (var l : coefficients.keySet()) {
      if (l.size() != this.size() + 1) {
        throw new IllegalArgumentException();
      }
    }
    this.coefficients =
        coefficients.entrySet().stream()
            .filter(entry -> !ZERO.equals(entry.getValue()))
            .collect(toMap(Map.Entry::getKey, Map.Entry::getValue));
  }

  public static Annotation knownConstant(int size, String name, int potential) {
    return constant(size, name, new KnownCoefficient(new Fraction(potential)));
  }

  /**
   * Constructs an annotation with given size that has given constant potential. Note that the given
   * potential might be an unknown coefficient.
   */
  public static Annotation constant(int size, String name, Coefficient potential) {
    return new Annotation(
        repeat(ZERO, size).collect(Collectors.toList()), Map.of(unitIndex(size), potential), name);
  }

  /** Constructs an annotation of given size that has a constant but unknown potential. */
  public static Annotation constant(int size, String name) {
    return constant(size, name, UnknownCoefficient.unknown("constant"));
  }

  /** Constructs an annotation of given size that has zero potential. */
  public static Annotation zero(int size, String name) {
    return new Annotation(repeat(ZERO, size).collect(toList()), emptyMap(), name);
  }

  public static Annotation zero(int size) {
    return zero(size, "zero-" + size);
  }

  /** Constructs an annotation of zero size that has zero potential. */
  public static Annotation empty() {
    return zero(0);
  }

  public static boolean isZero(List<Integer> index) {
    for (int e : index) {
      if (e != 0) {
        return false;
      }
    }
    return true;
  }

  public Annotation copyShape(String name) {
    return new Annotation(
        IntStream.range(0, size()).mapToObj(i -> unknown(generateSubscript(i))).collect(toList()),
        coefficients.keySet().stream()
            .collect(Collectors.toMap(identity(), index -> unknown(generateSubscriptIndex(index)))),
        name);
  }

  private UnknownCoefficient unknown(String suffix) {
    return new UnknownCoefficient(id + suffix);
  }

  public static boolean isConstantIndex(List<Integer> index) {
    if (index.size() < 2) {
      return true;
    }
    for (int i = 0; i < index.size() - 1; i++) {
      if (index.get(i) != 0) {
        return false;
      }
    }
    return true;
  }

  public static boolean isUnitIndex(List<Integer> index) {
    if (index.isEmpty()) {
      return false;
    }
    if (index.get(index.size() - 1) != 2) {
      return false;
    }
    for (int i = 0; i < index.size() - 1; i++) {
      if (index.get(i) != 0) {
        return false;
      }
    }
    return true;
  }

  public static boolean isWeirdIndex(List<Integer> index) {
    if (index.isEmpty()) {
      return true;
    }
    if (index.get(index.size() - 1) != 1) {
      return false;
    }
    for (int i = 0; i < index.size() - 1; i++) {
      if (index.get(i) != 0) {
        return false;
      }
    }
    return true;
  }

  public static int indexWeight(List<Integer> e) {
    return indexWeight(e, 2);
  }

  public static int indexWeightOld(List<Integer> e, int exponent) {
    int weight = e.get(e.size() - 1) + 1;
    for (int i = 0; i < e.size() - 1; i++) {
      weight += Math.pow(e.get(i) + 1, exponent);
    }
    return weight;
  }

  public static int indexWeight(List<Integer> e, int exponent) {
    int weight = 1 + ((e.get(e.size() - 1) + 1) * 2);
    for (int i = 0; i < e.size() - 1; i++) {
      weight += e.get(i);
    }
    return (int) Math.pow(weight, exponent);
  }

  public static List<Integer> unitIndex(int size) {
    return constantIndex(size, 2);
  }

  public static List<Integer> constantIndex(int size, int value) {
    final var unitIndex = Util.zero(size);
    unitIndex.add(value);
    return unitIndex;
  }

  private static <K> boolean coefficientsEqualUnderZeroClosure(
      Map<K, Coefficient> a, Map<K, Coefficient> b) {
    return a.entrySet().stream()
            .allMatch(entry -> entry.getValue().equals(b.getOrDefault(entry.getKey(), ZERO)))
        && b.entrySet().stream()
            .allMatch(entry -> entry.getValue().equals(a.getOrDefault(entry.getKey(), ZERO)));
  }

  private static Coefficient nullToZero(Coefficient coefficient) {
    return coefficient == null ? ZERO : coefficient;
  }

  private static boolean coefficientsEqualUnderZeroClosure(
      List<Coefficient> a, List<Coefficient> b) {
    return Streams.zip(
            a.stream().map(Annotation::nullToZero),
            b.stream().map(Annotation::nullToZero),
            Object::equals)
        .allMatch(x -> x);
  }

  public List<Integer> getUnitIndex() {
    return unitIndex(size());
  }

  public Annotation substitute(Map<Coefficient, KnownCoefficient> solution) {
    if (solution.keySet().stream().anyMatch(c -> c instanceof KnownCoefficient)) {
      throw bug("cannot substitute known coefficient");
    }
    return new Annotation(
        this.rankCoefficients.stream()
            .map(
                coefficient ->
                    coefficient == null || coefficient instanceof KnownCoefficient
                        ? coefficient
                        : solution.getOrDefault(coefficient, ZERO))
            .collect(toList()),
        this.coefficients.entrySet().stream()
            .collect(
                toMap(
                    Map.Entry::getKey,
                    entry ->
                        entry.getValue() instanceof KnownCoefficient
                            ? entry.getValue()
                            : solution.getOrDefault(entry.getValue(), ZERO))),
        getNameAndId());
  }

  public Coefficient getCoefficientOrDefine(List<Integer> index) {
    if (index.size() != size() + 1) {
      throw new IllegalArgumentException();
    }
    if (isAllZeroes(index)) {
      throw bug("attempting to create zero index");
    }
    return coefficients.computeIfAbsent(index, key -> unknown(generateSubscriptIndex(key)));
  }

  public Coefficient getCoefficientOrDefine(int... index) {
    return getCoefficientOrDefine(Ints.asList(index));
  }

  public Coefficient getCoefficientOrZero(int... index) {
    return getCoefficientOrZero(Ints.asList(index));
    // return requireNonNullElse(getCoefficient(index), ZERO);
  }

  public Coefficient getCoefficientOrZero(List<Integer> index) {
    if (index.size() != size() + 1) {
      throw new IllegalArgumentException();
    }
    if (isAllZeroes(index)) {
      throw bug("attempting to create zero index");
    }
    final var elem = coefficients.get(index);
    // log.info("{}{} = 0", id, generateSubscriptIndex(index));
    return Objects.requireNonNullElse(elem, ZERO);
  }

  public String toString() {
    /*
    if (size() == 0) {
      return "∅";
    }
     */
    StringBuilder sb = new StringBuilder("[");
    boolean nonzero = false;
    for (int i = 0; i < size(); i++) {
      final var q = rankCoefficients.get(i);
      if (q == null || q == ZERO) {
        continue;
      }
      nonzero = true;

      sb.append(i).append(" ↦ ").append(q);

      if (i < size() - 1) {
        sb.append(", ");
      }
    }
    final var schoenmakerPart =
        coefficients.entrySet().stream()
            .sorted(Map.Entry.comparingByKey(Annotation.INDEX_COMPARATOR))
            .filter(e -> e.getValue() != ZERO)
            .map(
                e ->
                    e.getKey().stream().map(Object::toString).collect(joining(" ", "(", ")"))
                        + " ↦ "
                        + e.getValue())
            .collect(Collectors.joining(", "));

    if (!schoenmakerPart.equals("") && size() > 0 && nonzero) {
      sb.append(", ");
    }

    sb.append(schoenmakerPart);

    return sb.append("]").toString();
  }

  public String toFunctionString() {
    /*
    if (size() == 0) {
      return "∅";
    }
     */
    StringBuilder sb = new StringBuilder("[");
    boolean nonzero = false;
    for (int i = 0; i < size(); i++) {
      final var q = rankCoefficients.get(i);
      if (q == null || q == ZERO) {
        continue;
      }
      nonzero = true;

      if (!q.equals(ONE)) {
        sb.append(q);
        sb.append("·");
      }

      sb.append("p").append(generateSubscript(i));

      if (i < size() - 1) {
        sb.append(" + ");
      }
    }
    final var schoenmakerPart =
        coefficients.entrySet().stream()
            .sorted(Map.Entry.comparingByKey(Annotation.INDEX_COMPARATOR))
            .filter(e -> e.getValue() != ZERO)
            .map(
                e -> {
                  final var index = e.getKey();
                  final var coefficient = e.getValue();

                  if (isUnitIndex(index)) {
                    return coefficient.toString();
                  }

                  String result = "";
                  if (!coefficient.equals(ONE)) {
                    result += coefficient;
                    result += "·";
                  }

                  return result
                      + "p"
                      + index.stream().map(Util::generateSubscript).collect(joining(" ", "₍", "₎"));
                })
            .collect(Collectors.joining(" + "));

    if (!schoenmakerPart.equals("") && size() > 0 && nonzero) {
      sb.append(" + ");
    }

    sb.append(schoenmakerPart);

    if (sb.length() == 1) {
      return "0";
    }

    return sb.append("]").toString();
  }

  public String toLongString() {
    return toLongString(
        IntStream.range(0, size()).mapToObj(i -> "_" + generateSubscript(i)).collect(toList()));
  }

  public static String toPotential(List<String> names, List<Integer> index) {
    if (isUnitIndex(index)) {
      throw new IllegalArgumentException("unit index must be handled at a higher level");
    }
    final var items = new ArrayList<String>(index.size());
    for (int i = 0; i < index.size() - 1; i++) {
      if (index.get(i) == 0) {
        continue;
      }
      var item = "";
      if (index.get(i) != 1) {
        item += index.get(i) + " · ";
      }
      item += "|" + names.get(i) + "|";
      items.add(item);
    }
    if (index.get(index.size() - 1) != 0) {
      items.add(String.valueOf(index.get(index.size() - 1)));
    }
    return "log(" + String.join(" + ", items) + ")";
  }

  public String toLongString(List<String> parameters) {
    StringBuilder sb = new StringBuilder();
    boolean nonzero = false;
    for (int i = 0; i < size(); i++) {
      var q = getRankCoefficientOrZero(i);
      if (ZERO.equals(q)) {
        continue;
      }
      nonzero = true;
      if (!ONE.equals(q)) {
        sb.append(q);
        sb.append(" · ");
      }
      sb.append("rk(");
      sb.append(parameters.get(i));
      sb.append(")");

      if (i < size() - 1) {
        sb.append(" + ");
      }
    }
    final var schoenmakerPart =
        coefficients.entrySet().stream()
            .filter(e -> e.getValue() != ZERO)
            .map(
                e -> {
                  final var index = e.getKey();
                  final var coefficient = e.getValue();

                  final boolean cancelIndex = isUnitIndex(index);
                  final boolean cancelCoefficient =
                      coefficient.equals(new KnownCoefficient(Fraction.ONE));

                  if (cancelCoefficient && cancelIndex) {
                    return "1";
                  }
                  if (cancelIndex) {
                    return coefficient.toString();
                  }
                  if (cancelCoefficient) {
                    return toPotential(parameters, index);
                  }
                  return coefficient + " · " + toPotential(parameters, index);
                })
            .collect(Collectors.joining(" + "));

    if (!schoenmakerPart.equals("") && size() > 0 && nonzero) {
      sb.append(" + ");
    }

    sb.append(schoenmakerPart);

    if (sb.length() == 0) {
      return "0";
    }

    return sb.toString();
  }

  public Coefficient getRankCoefficient(int index) {
    final var result = rankCoefficients.get(index);
    if (result == null) {
      throw bug("missed");
    }
    return result;
  }

  public Coefficient getRankCoefficient() {
    if (size() != 1) {
      throw new IllegalStateException("must be of size exactly 1");
    }
    return getRankCoefficient(0);
  }

  public Coefficient getRankCoefficientOrZero(int i) {
    return nullToZero(rankCoefficients.get(i));
  }

  public Coefficient getRankCoefficientOrZero() {
    if (size() != 1) {
      throw new IllegalStateException("must be of size exactly 1");
    }
    return getRankCoefficientOrZero(0);
  }

  public Coefficient getRankCoefficientOrDefine() {
    if (size() != 1) {
      throw new IllegalStateException("must be of size exactly 1");
    }
    return getRankCoefficientOrDefine(0);
  }

  public Coefficient getCoefficient(List<Integer> index) {
    if (index.size() != size() + 1) {
      throw new IllegalArgumentException();
    }
    if (isAllZeroes(index)) {
      throw bug("attempting to access zero index");
    }
    if (!coefficients.containsKey(index)) {
      throw bug("missed");
    }
    return coefficients.get(index);
  }

  public Coefficient getUnitCoefficientOrZero() {
    return getCoefficientOrZero(unitIndex(size()));
  }

  public Coefficient getUnitCoefficient() {
    return getCoefficient(unitIndex(size()));
  }

  private Coefficient getCoefficient(int... index) {
    return getCoefficient(Ints.asList(index));
  }

  public Iterable<Map.Entry<List<Integer>, Coefficient>> getCoefficients() {
    return coefficients.entrySet();
  }

  public Stream<Map.Entry<List<Integer>, Coefficient>> streamNonRankCoefficients() {
    return coefficients.entrySet().stream();
  }

  public int size() {
    return rankCoefficients.size();
  }

  public String getNameAndId() {
    return this.name + "#" + id;
  }

  public boolean coefficientsEqual(Annotation other) {
    return coefficientsEqualUnderZeroClosure(rankCoefficients, other.rankCoefficients)
        && coefficientsEqualUnderZeroClosure(coefficients, other.coefficients);
    // && Objects.equals(coefficients, other.coefficients);
  }

  public Annotation reorder(List<Integer> reorderedIndices) {
    return new Annotation(
        Util.reorder(rankCoefficients, reorderedIndices),
        coefficients.entrySet().stream()
            .collect(
                Collectors.toMap(
                    (Map.Entry<List<Integer>, Coefficient> entry) -> {
                      final List<Integer> mappedKey =
                          Util.reorder(entry.getKey(), reorderedIndices);
                      mappedKey.add(entry.getKey().get(entry.getKey().size() - 1));
                      return mappedKey;
                    },
                    Map.Entry::getValue)),
        name);
  }

  public Set<List<Integer>> coefficientIndices() {
    return coefficients.keySet();
  }

  public EqualsSumConstraint sumAllCoefficients(Coefficient c) {
    return new EqualsSumConstraint(
        c,
        Stream.concat(rankCoefficients.stream(), coefficients.values().stream()).toList(),
        "sum of annotation " + getNameAndId());
  }

  public EqualsSumConstraint sumRankCoefficients(Coefficient c) {
    return new EqualsSumConstraint(
        c,
        rankCoefficients.stream().toList(),
        "sum of rank coefficients of annotation " + getNameAndId());
  }

  public EqualsSumConstraint sumCoefficients(Coefficient c) {
    return new EqualsSumConstraint(
        c,
        coefficients.values().stream().toList(),
        "sum of rank coefficients of annotation " + getNameAndId());
  }

  public Annotation rename(String newName) {
    return new Annotation(rankCoefficients, coefficients, newName);
  }

  public List<Constraint> abs(UnknownCoefficient sum) {
    final var x = UnknownCoefficient.unknown("x");
    final var y = UnknownCoefficient.unknown("y");
    return List.of(
        sumRankCoefficients(x),
        sumCoefficients(y),
        new EqualsSumConstraint(sum, List.of(x, y), "(opt)"));
  }

  public boolean isUnknown() {
    return Streams.concat(rankCoefficients.stream(), coefficients.values().stream())
        .anyMatch(x -> x instanceof UnknownCoefficient);
  }

  public Stream<List<Integer>> potentialFunctions(Annotation other) {
    return Stream.concat(streamNonRankCoefficients(), other.streamNonRankCoefficients())
        .map(Map.Entry::getKey)
        .distinct()
        .sorted(INDEX_COMPARATOR);
  }

  public static Stream<List<Integer>> indexUnion(Annotation... annotations) {
    return Stream.of(annotations)
        .flatMap(Annotation::streamNonRankCoefficients)
        .map(Map.Entry::getKey)
        .distinct()
        .sorted(INDEX_COMPARATOR);
  }

  public Stream<Map.Entry<List<Integer>, Pair<Coefficient, Coefficient>>> union(Annotation other) {
    return indexUnion(this, other)
        .map(
            potentialFunction ->
                new AbstractMap.SimpleEntry<>(
                    potentialFunction,
                    Pair.of(
                        this.getCoefficientOrZero(potentialFunction),
                        other.getCoefficientOrZero(potentialFunction))));
  }

  public List<Constraint> increment(Annotation other, int cost, String reason) {
    return increment(other, new Fraction(cost), reason);
  }

  public List<Constraint> increment(Annotation other, Fraction cost, String reason) {
    return increment(other, new KnownCoefficient(cost), reason);
  }

  public static Pair<Annotation, List<Constraint>> add(Annotation a, Annotation b) {
    if (a.size() != b.size()) {
      throw new IllegalArgumentException("annotations must be of same size");
    }

    final int size = a.size();

    final var constraints = new ArrayList<Constraint>();

    final var rankCoefficients = new ArrayList<Coefficient>(size);

    for (int i = 0; i < size; i++) {
      final var ai = a.getRankCoefficientOrZero(i);
      if (ai.equals(ZERO)) {
        rankCoefficients.add(b.getRankCoefficient(i));
        continue;
      }
      final var bi = b.getRankCoefficientOrZero(i);
      if (bi.equals(ZERO)) {
        rankCoefficients.add(a.getRankCoefficient(i));
        continue;
      }

      final var ci = UnknownCoefficient.unknown("x");
      constraints.add(new EqualsSumConstraint(ci, List.of(ai, bi), "(add)"));
      rankCoefficients.add(ci);
    }

    final var coefficients = new HashMap<List<Integer>, Coefficient>();

    indexUnion(a, b)
        .forEach(
            i -> {
              final var ai = a.getCoefficientOrZero(i);
              final var bi = b.getCoefficientOrZero(i);

              if (ai.equals(ZERO)) {
                coefficients.put(i, bi);
                return;
              }
              if (bi.equals(ZERO)) {
                coefficients.put(i, ai);
                return;
              }

              UnknownCoefficient ci = UnknownCoefficient.unknown("x");
              constraints.add(new EqualsSumConstraint(ci, List.of(ai, bi), "(add)"));
              coefficients.put(i, ci);
            });

    return Pair.of(
        new Annotation(rankCoefficients, coefficients, a.getName() + "+" + b.getName()),
        constraints);
  }

  public static Annotation subtract(Annotation a, Annotation b) {
    if (a.size() != b.size()) {
      throw new IllegalArgumentException("annotations must be of same size");
    }

    final int size = a.size();

    final var rankCoefficients = new ArrayList<Coefficient>(size);

    for (int i = 0; i < size; i++) {
      final var ai = a.getRankCoefficientOrZero(i);
      if (ai.equals(ZERO)) {
        rankCoefficients.add(b.getRankCoefficient(i));
        continue;
      }
      final var bi = b.getRankCoefficientOrZero(i);
      if (bi.equals(ZERO)) {
        rankCoefficients.add(a.getRankCoefficient(i));
        continue;
      }

      if (!(ai instanceof KnownCoefficient) || !(bi instanceof KnownCoefficient)) {
        throw new UnsupportedOperationException("must all be known");
      }

      rankCoefficients.add(
          Coefficient.of(
              ((KnownCoefficient) ai).getValue().subtract(((KnownCoefficient) bi).getValue())));
    }

    final var coefficients = new HashMap<List<Integer>, Coefficient>();

    indexUnion(a, b)
        .forEach(
            i -> {
              final var ai = a.getCoefficientOrZero(i);
              final var bi = b.getCoefficientOrZero(i);

              if (ai.equals(ZERO)) {
                coefficients.put(i, bi);
                return;
              }
              if (bi.equals(ZERO)) {
                coefficients.put(i, ai);
                return;
              }

              if (!(ai instanceof KnownCoefficient) || !(bi instanceof KnownCoefficient)) {
                throw new UnsupportedOperationException("must all be known");
              }

              coefficients.put(
                  i,
                  Coefficient.of(
                      ((KnownCoefficient) ai)
                          .getValue()
                          .subtract(((KnownCoefficient) bi).getValue())));
            });

    return new Annotation(rankCoefficients, coefficients, a.getName() + "-" + b.getName());
  }

  /** Constraints for this = other + cost */
  public List<Constraint> increment(Annotation other, Coefficient cost, String reason) {
    if (this.size() != other.size()) {
      throw new IllegalArgumentException("annotations must be of same size");
    }

    if (cost instanceof KnownCoefficient
        && ((KnownCoefficient) cost).getValue().getNumerator() == 0) {
      return EqualityConstraint.eq(this, other, reason);
    }

    return concat(
            range(0, this.size())
                .mapToObj(
                    i ->
                        new EqualityConstraint(
                            this.getRankCoefficientOrZero(i),
                            other.getRankCoefficientOrZero(i),
                            reason)),
            union(other)
                .map(
                    entry -> {
                      var thisCoefficient = entry.getValue().getLeft();
                      var otherCoefficient = entry.getValue().getRight();
                      return isUnitIndex(entry.getKey())
                          ? new OffsetConstraint(thisCoefficient, otherCoefficient, cost, reason)
                          : new EqualityConstraint(thisCoefficient, otherCoefficient, reason);
                    }))
        .collect(toList());
  }

  public Coefficient getRankCoefficientOrDefine(int i) {
    if (rankCoefficients.get(i) == null) {
      rankCoefficients.set(i, unknown(generateSubscript(i)));
    }
    return rankCoefficients.get(i);
  }

  public static Stream<List<Integer>> nonRankIndices(Annotation a, Annotation b) {
    return Stream.concat(a.streamNonRankCoefficients(), b.streamNonRankCoefficients())
        .map(Map.Entry::getKey)
        .distinct()
        .sorted(INDEX_COMPARATOR);
  }

  private static String shortNameForGenerated(String name) {
    if (name.matches(".*#\\d+")) {
      return name.substring(name.lastIndexOf("#") + 1);
    }
    return name;
  }

  public boolean isNonInteger() {
    return Stream.concat(
            streamNonRankCoefficients().map(Map.Entry::getValue), rankCoefficients.stream())
        .filter(not(Objects::isNull))
        .filter(x -> x instanceof KnownCoefficient)
        .map(x -> ((KnownCoefficient) x).getValue())
        .anyMatch(not(Util::isInteger));
  }

  public boolean isZero() {
    return Streams.concat(rankCoefficients.stream(), coefficients.values().stream())
        .allMatch(x -> x == null || ZERO.equals(x));
  }

  public Pair<Annotation, List<Constraint>> multiply(KnownCoefficient factor) {
    if (factor.getValue().equals(Fraction.ONE)) {
      return Pair.of(this, emptyList());
    }

    final var constraints =
        new ArrayList<Constraint>(rankCoefficients.size() + coefficients.size());
    final var resultRankCoefficients = new ArrayList<Coefficient>(rankCoefficients.size());

    for (final var rankCoefficient : rankCoefficients) {
      if (rankCoefficient == null) {
        resultRankCoefficients.add(null);
      } else {
        final var x = UnknownCoefficient.unknown("x");
        constraints.add(new EqualsProductConstraint(x, List.of(rankCoefficient, factor), "(mul)"));
        resultRankCoefficients.add(x);
      }
    }

    final var resultCoefficients = new HashMap<List<Integer>, Coefficient>(coefficients.size());
    for (final var entry : coefficients.entrySet()) {
      final var x = UnknownCoefficient.unknown("x");
      constraints.add(new EqualsProductConstraint(x, List.of(entry.getValue(), factor), "(mul)"));
      resultCoefficients.put(entry.getKey(), x);
    }

    return Pair.of(
        new Annotation(resultRankCoefficients, resultCoefficients, name + " * " + factor),
        constraints);
  }
}
