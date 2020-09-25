package xyz.leutgeb.lorenz.lac.typing.resources;

import static com.google.common.collect.Comparators.lexicographical;
import static java.util.Collections.emptyMap;
import static java.util.Collections.emptySet;
import static java.util.Objects.requireNonNullElse;
import static java.util.stream.Collectors.joining;
import static java.util.stream.Collectors.toList;
import static java.util.stream.Collectors.toMap;
import static java.util.stream.Collectors.toUnmodifiableList;
import static java.util.stream.IntStream.range;
import static java.util.stream.Stream.concat;
import static xyz.leutgeb.lorenz.lac.typing.resources.coefficients.KnownCoefficient.ONE;
import static xyz.leutgeb.lorenz.lac.typing.resources.coefficients.KnownCoefficient.ZERO;
import static xyz.leutgeb.lorenz.lac.util.Util.bug;
import static xyz.leutgeb.lorenz.lac.util.Util.generateSubscript;
import static xyz.leutgeb.lorenz.lac.util.Util.repeat;

import com.google.common.collect.Streams;
import com.google.common.primitives.Ints;
import java.util.AbstractMap;
import java.util.ArrayList;
import java.util.Comparator;
import java.util.List;
import java.util.Map;
import java.util.Objects;
import java.util.Set;
import java.util.stream.Collectors;
import java.util.stream.IntStream;
import java.util.stream.Stream;
import lombok.extern.java.Log;
import xyz.leutgeb.lorenz.lac.typing.resources.coefficients.Coefficient;
import xyz.leutgeb.lorenz.lac.typing.resources.coefficients.KnownCoefficient;
import xyz.leutgeb.lorenz.lac.typing.resources.coefficients.UnknownCoefficient;
import xyz.leutgeb.lorenz.lac.typing.resources.constraints.Constraint;
import xyz.leutgeb.lorenz.lac.typing.resources.constraints.EqualityConstraint;
import xyz.leutgeb.lorenz.lac.typing.resources.constraints.EqualsSumConstraint;
import xyz.leutgeb.lorenz.lac.typing.resources.constraints.ExclusiveDisjunctiveConstraint;
import xyz.leutgeb.lorenz.lac.typing.resources.constraints.IfThenElseConstraint;
import xyz.leutgeb.lorenz.lac.typing.resources.constraints.OffsetConstraint;
import xyz.leutgeb.lorenz.lac.util.Fraction;
import xyz.leutgeb.lorenz.lac.util.Pair;
import xyz.leutgeb.lorenz.lac.util.Util;

@Log
public class Annotation {
  public static final Comparator<Iterable<Integer>> INDEX_COMPARATOR =
      lexicographical(Integer::compareTo);

  private final List<Coefficient> rankCoefficients;
  private final Map<List<Integer>, Coefficient> coefficients;
  String name;

  public Annotation(
      List<Coefficient> rankCoefficients,
      Map<List<Integer>, Coefficient> coefficients,
      String name) {
    this.name = name;
    this.rankCoefficients = rankCoefficients;

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

  public Annotation(int size, String name) {
    this(repeat(ZERO, size).collect(toList()), emptyMap(), name);
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
    return new Annotation(size, name);
  }

  public static Annotation zero(int size) {
    return zero(size, "zero-" + size);
  }

  /** Constructs an annotation of zero size that has zero potential. */
  public static Annotation empty() {
    return zero(0);
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

  public static List<Integer> unitIndex(int size) {
    final var unitIndex = Util.zero(size);
    unitIndex.add(2);
    return unitIndex;
  }

  private static <K> boolean coefficientsEqualUnderZeroClosure(
      Map<K, Coefficient> a, Map<K, Coefficient> b) {
    return a.entrySet().stream()
            .allMatch(entry -> entry.getValue().equals(b.getOrDefault(entry.getKey(), ZERO)))
        && b.entrySet().stream()
            .allMatch(entry -> entry.getValue().equals(a.getOrDefault(entry.getKey(), ZERO)));
  }

  public List<Integer> getUnitIndex() {
    return unitIndex(size());
  }

  public Annotation substitute(Map<Coefficient, KnownCoefficient> solution) {
    return new Annotation(
        this.rankCoefficients.stream()
            .map(
                coefficient ->
                    coefficient instanceof KnownCoefficient
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
        this.name);
  }

  public Coefficient getCoefficientOrZero(int... index) {
    return requireNonNullElse(getCoefficient(index), ZERO);
  }

  public Coefficient getCoefficientOrZero(List<Integer> index) {
    if (index.size() != size() + 1) {
      throw new IllegalArgumentException();
    }
    return coefficients.getOrDefault(index, ZERO);
  }

  public String toString() {
    if (size() == 0) {
      return "∅";
    }
    StringBuilder sb = new StringBuilder("[");
    boolean nonzero = false;
    for (int i = 0; i < size(); i++) {
      final var q = rankCoefficients.get(i);
      if (q == ZERO) {
        continue;
      }
      nonzero = true;
      sb.append(q).append("·p").append(generateSubscript(i));

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

                  if (isUnitIndex(index)) {
                    return coefficient.toString();
                  }

                  return coefficient
                      + "·p"
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
        IntStream.rangeClosed(1, size())
            .mapToObj(i -> "t" + generateSubscript(i))
            .collect(toList()),
        true);
  }

  public String toLongString(List<String> parameters) {
    return toLongString(parameters, true);
  }

  public String toLongString(List<String> parameters, boolean simplify) {
    StringBuilder sb = new StringBuilder();
    boolean nonzero = false;
    for (int i = 0; i < size(); i++) {
      final var q = rankCoefficients.get(i);
      if (q == ZERO && simplify) {
        continue;
      }
      nonzero = true;
      if (!(q instanceof KnownCoefficient)
          || !(((KnownCoefficient) q).getValue().equals(new Fraction(1)))) {
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
            .filter(e -> !Annotation.isUnitIndex(e.getKey()))
            .map(
                e -> {
                  final var index = e.getKey();
                  final var coefficient = e.getValue();
                  final var items = new ArrayList<String>(index.size());
                  for (int i = 0; i < index.size() - 1; i++) {
                    if (index.get(i) == 0 && simplify) {
                      continue;
                    }
                    var item = "";
                    if (index.get(i) != 1 || !simplify) {
                      item += index.get(i) + " · ";
                    }
                    item += "|" + parameters.get(i) + "|";
                    items.add(item);
                  }
                  if (index.get(index.size() - 1) != 0 || !simplify) {
                    items.add(String.valueOf(index.get(index.size() - 1)));
                  }
                  var result = "";
                  if (!coefficient.equals(new KnownCoefficient(Fraction.ONE)) || !simplify) {
                    result = coefficient + " · ";
                  }
                  return result + "lg(" + String.join(" + ", items) + ")";
                })
            .collect(Collectors.joining(" + "));

    if (!schoenmakerPart.equals("") && size() > 0 && nonzero) {
      sb.append(" + ");
    }

    sb.append(schoenmakerPart);

    final var units = getUnitCoefficientOrZero();
    if (units != null && !units.equals(new KnownCoefficient(Fraction.ZERO))) {
      if (sb.length() > 0) {
        sb.append(" + ");
      }
      sb.append(units);
    }

    if (sb.length() == 0) {
      return "0";
    }

    return sb.toString();
  }

  public Coefficient getRankCoefficient(int index) {
    return this.rankCoefficients.get(index);
  }

  public Coefficient getRankCoefficient() {
    if (size() != 1) {
      throw new IllegalStateException("must be of size exactly 1");
    }
    return getRankCoefficient(0);
  }

  public Coefficient getCoefficient(List<Integer> index) {
    if (index.size() != size() + 1) {
      throw new IllegalArgumentException();
    }
    if (!coefficients.containsKey(index)) {
      throw bug("missed");
    }
    return coefficients.get(index);
  }

  public Set<Constraint> setCoefficient(List<Integer> index, Coefficient value) {
    Coefficient existingValue = coefficients.get(index);
    if (existingValue != null) {
      return Set.of(new EqualityConstraint(existingValue, value, "setcoefficient"));
    }
    coefficients.put(index, value);
    return emptySet();
  }

  public Set<Constraint> setRankCoefficient(Integer index, Coefficient value) {
    final var existingValue = rankCoefficients.get(index);
    if (existingValue != null) {
      if (existingValue.equals(value)) {
        return emptySet();
      }
      return Set.of(new EqualityConstraint(existingValue, value, "setcoefficient"));
    }
    rankCoefficients.set(index, value);
    return emptySet();
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

  public Stream<Map.Entry<List<Integer>, Coefficient>> streamCoefficients() {
    return coefficients.entrySet().stream();
  }

  public int size() {
    return rankCoefficients.size();
  }

  public String getName() {
    return this.name;
  }

  public void setName(String name) {
    this.name = name;
  }

  public boolean coefficientsEqual(Annotation other) {
    return Objects.equals(rankCoefficients, other.rankCoefficients)
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

  public EqualsSumConstraint sumAllCoefficients(Coefficient c) {
    return new EqualsSumConstraint(
        c,
        Stream.concat(rankCoefficients.stream(), coefficients.values().stream())
            .collect(toUnmodifiableList()),
        "sum of annotation " + getName());
  }

  public EqualsSumConstraint sumRankCoefficients(Coefficient c) {
    return new EqualsSumConstraint(
        c,
        rankCoefficients.stream().collect(toUnmodifiableList()),
        "sum of rank coefficients of annotation " + getName());
  }

  public EqualsSumConstraint sumCoefficients(Coefficient c) {
    return new EqualsSumConstraint(
        c,
        coefficients.values().stream().collect(toUnmodifiableList()),
        "sum of rank coefficients of annotation " + getName());
  }

  public Annotation rename(String newName) {
    return new Annotation(rankCoefficients, coefficients, newName);
  }

  public List<Constraint> diff(Annotation other, UnknownCoefficient sum) {
    if (other.size() != this.size()) {
      throw new IllegalArgumentException("size of annotations must be equal");
    }

    var potentialFunctions =
        Stream.concat(streamCoefficients(), other.streamCoefficients())
            .map(Map.Entry::getKey)
            .distinct()
            .sorted(INDEX_COMPARATOR)
            .collect(toList());

    final List<Constraint> constraints = new ArrayList<>();
    final List<Coefficient> sumList = new ArrayList<>(potentialFunctions.size() + size());
    for (var potentialFunction : potentialFunctions) {
      final var x = UnknownCoefficient.unknown("x");
      constraints.add(
          new IfThenElseConstraint(
              new ExclusiveDisjunctiveConstraint(
                  new EqualityConstraint(
                      this.getCoefficientOrZero(potentialFunction), ZERO, "(opt)"),
                  new EqualityConstraint(
                      other.getCoefficientOrZero(potentialFunction), ZERO, "(opt)"),
                  "(opt)"),
              ONE,
              ZERO,
              x,
              "(opt)"));
      sumList.add(x);
    }

    for (int i = 0; i < size(); i++) {
      final var x = UnknownCoefficient.unknown("x");
      constraints.add(
          new IfThenElseConstraint(
              new ExclusiveDisjunctiveConstraint(
                  new EqualityConstraint(this.getRankCoefficient(i), ZERO, "(opt)"),
                  new EqualityConstraint(other.getRankCoefficient(i), ZERO, "(opt)"),
                  "(opt)"),
              ONE,
              ZERO,
              x,
              "(opt)"));
      sumList.add(x);
    }

    constraints.add(new EqualsSumConstraint(sum, sumList, "(opt)"));
    return constraints;
  }

  public List<Constraint> abs(UnknownCoefficient sum) {
    final var x = UnknownCoefficient.unknown("x");
    final var y = UnknownCoefficient.unknown("y");
    return List.of(
        sumRankCoefficients(x),
        sumCoefficients(y),
        new EqualsSumConstraint(sum, List.of(x, y), "(opt)"));
  }

  public List<Constraint> pairwiseDiff(Annotation other, UnknownCoefficient sum) {
    if (other.size() != this.size()) {
      throw new IllegalArgumentException("size of annotations must be equal");
    }

    var potentialFunctions =
        Stream.concat(streamCoefficients(), other.streamCoefficients())
            .map(Map.Entry::getKey)
            .distinct()
            .sorted(INDEX_COMPARATOR)
            .collect(toList());

    final List<Constraint> constraints = new ArrayList<>();
    final List<Coefficient> sumList = new ArrayList<>(potentialFunctions.size() + size());
    for (var potentialFunction : potentialFunctions) {
      final var x = UnknownCoefficient.unknown("x");
      constraints.add(
          new EqualsSumConstraint(
              x,
              List.of(
                  this.getCoefficientOrZero(potentialFunction),
                  other.getCoefficientOrZero(potentialFunction).negate()),
              "(opt)"));
      sumList.add(x);
    }

    for (int i = 0; i < size(); i++) {
      final var x = UnknownCoefficient.unknown("x");
      constraints.add(
          new EqualsSumConstraint(
              x, List.of(this.getRankCoefficient(i), other.getRankCoefficient(i)), "(opt)"));
      sumList.add(x);
    }

    constraints.add(new EqualsSumConstraint(sum, sumList, "(opt)"));
    return constraints;
  }

  public boolean isUnknown() {
    return Streams.concat(rankCoefficients.stream(), coefficients.values().stream())
        .anyMatch(x -> x instanceof UnknownCoefficient);
  }

  public Stream<List<Integer>> potentialFunctions(Annotation other) {
    return Stream.concat(streamCoefficients(), other.streamCoefficients())
        .map(Map.Entry::getKey)
        .distinct()
        .sorted(INDEX_COMPARATOR);
  }

  public Stream<Map.Entry<List<Integer>, Pair<Coefficient, Coefficient>>> union(Annotation other) {
    return Stream.concat(streamCoefficients(), other.streamCoefficients())
        .map(Map.Entry::getKey)
        .distinct()
        .sorted(INDEX_COMPARATOR)
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

  public List<Constraint> increment(Annotation other, Coefficient cost, String reason) {
    if (this.size() != other.size()) {
      throw new IllegalArgumentException("annotations must be of same size");
    }

    if (cost instanceof KnownCoefficient known && known.getValue().getNumerator() == 0) {
      return EqualityConstraint.eq(this, other, reason);
    }

    return concat(
            range(0, this.size())
                .mapToObj(
                    i ->
                        new EqualityConstraint(
                            this.getRankCoefficient(i), other.getRankCoefficient(i), reason)),
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
}
