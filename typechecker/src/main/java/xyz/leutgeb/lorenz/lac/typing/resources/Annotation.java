package xyz.leutgeb.lorenz.lac.typing.resources;

import static xyz.leutgeb.lorenz.lac.Util.bug;
import static xyz.leutgeb.lorenz.lac.Util.truncate;

import com.google.common.primitives.Ints;
import java.util.ArrayList;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;
import java.util.stream.Collectors;
import java.util.stream.Stream;
import lombok.extern.java.Log;
import org.hipparchus.fraction.Fraction;
import xyz.leutgeb.lorenz.lac.Util;
import xyz.leutgeb.lorenz.lac.typing.resources.coefficients.Coefficient;
import xyz.leutgeb.lorenz.lac.typing.resources.coefficients.KnownCoefficient;
import xyz.leutgeb.lorenz.lac.typing.resources.coefficients.UnknownCoefficient;
import xyz.leutgeb.lorenz.lac.typing.resources.solving.ConstraintSystemSolver;

@Log
public class Annotation {
  private static final Annotation EMPTY = empty();
  final String name;
  private final int size;
  private final List<Coefficient> rankCoefficients;
  private final Map<List<Integer>, Coefficient> coefficients;

  public Annotation(
      List<Coefficient> rankCoefficients,
      Map<List<Integer>, Coefficient> coefficients,
      String name) {
    this.rankCoefficients = rankCoefficients;
    this.coefficients = coefficients;
    this.size = rankCoefficients.size();
    this.name = name;
    for (var l : coefficients.keySet()) {
      if (l.size() != this.size + 1) {
        throw new IllegalArgumentException();
      }
    }
    // System.out.println("Annotation size: " + size);
  }

  public Annotation(int size, String name) {
    rankCoefficients = new ArrayList<>(size);
    coefficients = new LinkedHashMap<>();
    this.size = size;
    this.name = name;

    for (int i = 0; i < size; i++) {
      rankCoefficients.add(KnownCoefficient.ZERO);
    }
    // System.out.println("Annotation size: " + size);
  }

  public static Annotation empty() {
    return new Annotation(0, "0");
  }

  @Deprecated
  public static Annotation merge(Annotation... as) {
    if (as.length == 0) {
      return EMPTY;
    }

    if (as.length == 1) {
      return as[0];
    }

    int size = 0;
    for (var a : as) {
      size += a.size();
    }

    if (size == 0) {
      return EMPTY;
    }

    final var rankCoefficients = new ArrayList<Coefficient>(size);
    final var coefficients = new LinkedHashMap<List<Integer>, Coefficient>();
    final var name = new StringBuilder();
    for (int i = 0; i < as.length; i++) {
      final var a = as[i];
      name.append(a.name);
      rankCoefficients.add(a.getRankCoefficient());

      for (var e : a.getCoefficients()) {
        final var l = new ArrayList<Integer>(size + 1);
        for (int j = 0; j < i; j++) {
          l.add(0);
        }
        for (int j = 0; j < e.getKey().size() - 1; j++) {
          l.add(e.getKey().get(j));
        }
        for (int j = 0; j < (size - i - a.size()); j++) {
          l.add(0);
        }
        l.add(e.getKey().get(e.getKey().size() - 1));
        coefficients.put(l, e.getValue());
      }
    }
    return new Annotation(rankCoefficients, coefficients, name.toString());
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

  public Annotation substitute(Map<Coefficient, KnownCoefficient> solution) {
    final var rankCoefficients = new ArrayList<Coefficient>(this.rankCoefficients.size());
    final var coefficients = new LinkedHashMap<List<Integer>, Coefficient>();
    for (var entry : this.coefficients.entrySet()) {
      if (entry.getValue() instanceof KnownCoefficient) {
        coefficients.put(entry.getKey(), entry.getValue());
        continue;
      }
      if (!solution.containsKey(entry.getValue())) {
        // In this case we could not find a known coefficient to replace
        // an unknown coefficient. One possible workaround is to fall
        // back to zero:
        //
        //  coefficients.put(entry.getKey(), KnownCoefficient.ZERO);
        //
        // However, it is very likely that this actually is a symptom of
        // and implementation error.
        throw bug("cannot substitute value");
      }
      coefficients.put(entry.getKey(), solution.get(entry.getValue()));
    }
    for (var coefficient : this.rankCoefficients) {
      if (!solution.containsKey((coefficient))) {
        throw bug("cannot subs");
      }
      rankCoefficients.add(solution.get(coefficient));
    }
    return new Annotation(rankCoefficients, coefficients, this.name);
  }

  public void add(List<Integer> f, Coefficient coefficient) {
    if (f.size() != size + 1) {
      throw new IllegalArgumentException(
          "expected one more integer than there are trees being annotated");
    }
    if (this.coefficients.containsKey(f)) {
      throw new IllegalArgumentException("annotation already contains this key");
    }
    this.coefficients.put(f, coefficient);
  }

  @Deprecated
  public void add(Coefficient coefficient, ConstraintSystemSolver context) {
    var f = new ArrayList<Integer>(size + 1);
    for (int i = 0; i < size; i++) {
      f.add(0);
    }
    f.add(2);
    coefficients.compute(f, (k, v) -> v == null ? coefficient : v.add(coefficient, context));
  }

  public void add(Coefficient coefficient, int... index) {
    var f = Ints.asList(index);
    if (f.size() != size + 1) {
      throw new IllegalArgumentException(
          "expected one more integer than there are trees being annotated");
    }
    if (this.coefficients.containsKey(f)) {
      throw new IllegalArgumentException("annotation already contains this key");
    }
    this.coefficients.put(f, coefficient);
  }

  @Deprecated
  public Coefficient getOrFreshForReal(ConstraintSystemSolver context, int... index) {
    var f = Ints.asList(index);
    if (f.size() != size + 1) {
      throw new IllegalArgumentException(
          "expected one more integer than there are trees being annotated");
    }
    return this.coefficients.computeIfAbsent(f, k -> UnknownCoefficient.unknown());
  }

  public Coefficient getCoefficientOrZero(int... index) {
    final var result = getCoefficient(index);
    if (result == null) {
      // log.warning("Zeroing a coefficient because of absent counterpart.");
      return KnownCoefficient.ZERO;
    }
    return result;
  }

  public Coefficient getCoefficientOrZero(List<Integer> index) {
    if (index.size() != size + 1) {
      throw new IllegalArgumentException();
    }
    return coefficients.getOrDefault(index, KnownCoefficient.ZERO);
  }

  public String toShortString() {
    StringBuilder sb = new StringBuilder();
    sb.append(" ");
    for (int i = 0; i < size; i++) {
      final var q = rankCoefficients.get(i);
      sb.append(q);

      if (i < size - 1) {
        sb.append(" , ");
      }
    }
    if (size > 0) {
      sb.append(", ");
    }
    var entries = coefficients.entrySet();
    int i = 0;
    for (var entry : entries) {
      final var q = entry.getValue();
      sb.append(q);
      if (i < coefficients.size() - 1) {
        sb.append(" , ");
      }

      i++;
    }
    return truncate(sb.toString(), 1000);
  }

  public String toString() {
    StringBuilder sb = new StringBuilder();
    boolean nonzero = false;
    for (int i = 0; i < size; i++) {
      final var q = rankCoefficients.get(i);
      if (q == KnownCoefficient.ZERO) {
        continue;
      }
      nonzero = true;
      sb.append(q);
      sb.append(" · ");
      sb.append("rk(t");
      sb.append(Util.generateSubscript(i + 1));
      sb.append(")");

      if (i < size - 1) {
        sb.append(" + ");
      }
    }
    if (size > 0 && nonzero) {
      sb.append(" + ");
    }
    sb.append(
        coefficients.entrySet().stream()
            .filter(e -> !e.getValue().equals(KnownCoefficient.ZERO))
            .map(
                e -> {
                  final var sb2 = new StringBuilder();
                  sb2.append(e.getValue());
                  sb2.append(" · ");
                  sb2.append("lg(");

                  for (int j = 0; j < e.getKey().size() - 1; j++) {
                    sb2.append(e.getKey().get(j));
                    sb2.append(" · ");
                    sb2.append("|t");
                    sb2.append(Util.generateSubscript(j + 1));
                    sb2.append("| + ");

                    /*
                    if (j < e.getKey().size() - 1) {
                      sb2.append(" + ");
                    }
                     */
                  }
                  sb2.append(e.getKey().get(e.getKey().size() - 1));
                  sb2.append(")");
                  return sb2.toString();
                })
            .collect(Collectors.joining(" + ")));

    var result = sb.toString();
    if (result.equals("")) {
      return "0";
    }
    return result;
  }

  public String toStringForParameters(List<String> parameters, boolean simplify) {
    StringBuilder sb = new StringBuilder();
    boolean nonzero = false;
    for (int i = 0; i < size; i++) {
      final var q = rankCoefficients.get(i);
      if (q == KnownCoefficient.ZERO && simplify) {
        continue;
      }
      nonzero = true;
      sb.append(q);
      sb.append(" · ");
      sb.append("rank(");
      sb.append(parameters.get(i));
      sb.append(")");

      if (i < size - 1) {
        sb.append(" + ");
      }
    }
    final var schoenmakerPart =
        coefficients.entrySet().stream()
            .filter(e -> e.getValue() != KnownCoefficient.ZERO)
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
                  return result + "log(" + String.join(" + ", items) + ")";
                })
            .collect(Collectors.joining(" + "));

    if (!schoenmakerPart.equals("") && size > 0 && nonzero) {
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
    if (size != 1) {
      throw new IllegalStateException("must be of size exactly 1");
    }
    return getRankCoefficient(0);
  }

  public Coefficient getCoefficient(List<Integer> index) {
    if (index.size() != size + 1) {
      throw new IllegalArgumentException();
    }
    if (!coefficients.containsKey(index)) {
      throw bug("missed");
    }
    return coefficients.get(index);
  }

  private Coefficient getUnitCoefficientOrZero() {
    return getCoefficientOrZero(unitIndex(size));
  }

  private Coefficient getUnitCoefficient() {
    return getCoefficient(unitIndex(size));
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
    return this.size;
  }

  public String getName() {
    return this.name;
  }

  /*
  public Annotation extend(int newSize, int ) {
    if (newSize <= size) {
      throw new IllegalArgumentException("new size must be larger than size of this");
    }
    final var result = new Annotation(newSize);
    for (int i = 0; i < size; i++) {
      result.rankCoefficients.set(i, this.rankCoefficients.get(i));
    }
    for (var entry : coefficients.entrySet()) {

    }
  }
   */
}
