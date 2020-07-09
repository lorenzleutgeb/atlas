package xyz.leutgeb.lorenz.lac.typing.resources;

import static java.util.Collections.unmodifiableList;
import static java.util.Collections.unmodifiableMap;
import static java.util.Objects.requireNonNullElse;
import static java.util.stream.Collectors.*;
import static xyz.leutgeb.lorenz.lac.Util.*;
import static xyz.leutgeb.lorenz.lac.typing.resources.coefficients.KnownCoefficient.ZERO;

import com.google.common.primitives.Ints;
import java.util.*;
import java.util.stream.Collectors;
import java.util.stream.IntStream;
import java.util.stream.Stream;
import lombok.extern.java.Log;
import org.hipparchus.fraction.Fraction;
import xyz.leutgeb.lorenz.lac.Util;
import xyz.leutgeb.lorenz.lac.typing.resources.coefficients.Coefficient;
import xyz.leutgeb.lorenz.lac.typing.resources.coefficients.KnownCoefficient;
import xyz.leutgeb.lorenz.lac.typing.resources.coefficients.UnknownCoefficient;

@Log
public class Annotation {
  private final List<Coefficient> rankCoefficients;
  private final Map<List<Integer>, Coefficient> coefficients;
  String name;

  public Annotation(
      List<Coefficient> rankCoefficients,
      Map<List<Integer>, Coefficient> coefficients,
      String name) {
    this.name = name;
    this.rankCoefficients = unmodifiableList(rankCoefficients);

    for (var l : coefficients.keySet()) {
      if (l.size() != this.size() + 1) {
        throw new IllegalArgumentException();
      }
    }

    this.coefficients = unmodifiableMap(coefficients);
  }

  public Annotation(int size, String name) {
    this.rankCoefficients = repeat(ZERO, size).collect(toUnmodifiableList());
    this.coefficients = new LinkedHashMap<>();
    this.name = name;
  }

  public static Annotation knownConstant(int size, String name, int potential) {
    return constant(size, name, new KnownCoefficient(new Fraction(potential)));
  }

  /**
   * Constructs an annotation with given size that has given constant potential. Note that the given
   * potential might be an unknown coefficient.
   */
  public static Annotation constant(int size, String name, Coefficient potential) {
    final var result = new Annotation(size, name);
    result.coefficients.put(unitIndex(size), potential);
    return result;
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
    StringBuilder sb = new StringBuilder("[");
    boolean nonzero = false;
    for (int i = 0; i < size(); i++) {
      final var q = rankCoefficients.get(i);
      if (q == ZERO) {
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
            .filter(e -> e.getValue() != ZERO)
            .map(
                e -> {
                  final var index = e.getKey();
                  final var coefficient = e.getValue();
                  return index.stream().map(Object::toString).collect(joining(", ", "(", ")"))
                      + " ↦ "
                      + coefficient;
                })
            .collect(Collectors.joining(", "));

    if (!schoenmakerPart.equals("") && size() > 0 && nonzero) {
      sb.append(", ");
    }

    sb.append(schoenmakerPart);

    if (sb.length() == 1) {
      return "∅";
    }

    return sb.append("]").toString();
  }

  public String toLongString() {
    return toLongString(
        IntStream.rangeClosed(1, size())
            .mapToObj(i -> "t" + Util.generateSubscript(i))
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
}
