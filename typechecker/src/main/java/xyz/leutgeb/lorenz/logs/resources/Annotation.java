package xyz.leutgeb.lorenz.logs.resources;

import static xyz.leutgeb.lorenz.logs.Util.truncate;

import com.google.common.primitives.Ints;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;
import java.util.stream.Collectors;
import java.util.stream.Stream;
import lombok.extern.java.Log;
import org.hipparchus.fraction.Fraction;
import xyz.leutgeb.lorenz.logs.Util;
import xyz.leutgeb.lorenz.logs.ast.Expression;
import xyz.leutgeb.lorenz.logs.resources.coefficients.Coefficient;
import xyz.leutgeb.lorenz.logs.resources.coefficients.KnownCoefficient;

@Log
public class Annotation {
  private static final Annotation EMPTY = new Annotation(0);
  protected List<Coefficient> rankCoefficients;
  protected Map<List<Integer>, Coefficient> coefficients;
  protected int size;

  public Annotation(
      List<Coefficient> rankCoefficients, Map<List<Integer>, Coefficient> coefficients) {
    this.rankCoefficients = rankCoefficients;
    this.coefficients = coefficients;
    this.size = rankCoefficients.size();
    for (var l : coefficients.keySet()) {
      if (l.size() != this.size + 1) {
        throw new IllegalArgumentException();
      }
    }
  }

  public Annotation(int size) {
    rankCoefficients = new ArrayList<>(size);
    coefficients = new LinkedHashMap<>();
    this.size = size;

    for (int i = 0; i < size; i++) {
      rankCoefficients.add(KnownCoefficient.ZERO);
    }
  }

  public static Annotation empty() {
    return new Annotation(0);
  }

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
    final var coefficients = new HashMap<List<Integer>, Coefficient>();
    for (int i = 0; i < as.length; i++) {
      final var a = as[i];
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
    return new Annotation(rankCoefficients, coefficients);
  }

  public Annotation substitute(Constraints constraints) {
    final var rankCoefficients = new ArrayList<Coefficient>(this.rankCoefficients.size());
    final var coefficients = new HashMap<List<Integer>, Coefficient>();
    for (var entry : this.coefficients.entrySet()) {
      coefficients.put(entry.getKey(), constraints.substitute(entry.getValue()));
    }
    for (var coefficient : this.rankCoefficients) {
      rankCoefficients.add(constraints.substitute(coefficient));
    }
    return new Annotation(rankCoefficients, coefficients);
  }

  public Annotation greater(Expression source, Constraints constraints) {
    final var result = constraints.heuristic(size());
    // The returned annotation is asserted to have a greater potential than this.
    for (Map.Entry<List<Integer>, Coefficient> e : getCoefficients()) {
      constraints.le(source, e.getValue(), result.getCoefficient(e.getKey()));
    }
    for (int i = 0; i < size(); i++) {
      constraints.le(source, getRankCoefficient(i), result.getRankCoefficient(i));
    }
    return result;
  }

  public Annotation less(Expression source, Constraints constraints) {
    final var result = constraints.heuristic(size());
    // The returned annotation is asserted to have a smaller potential than this.
    for (Map.Entry<List<Integer>, Coefficient> e : getCoefficients()) {
      constraints.le(source, result.getCoefficient(e.getKey()), e.getValue());
    }
    for (int i = 0; i < size(); i++) {
      constraints.le(source, result.getRankCoefficient(i), getRankCoefficient(i));
    }
    return result;
  }

  public void add(List<Integer> f, Coefficient coefficient) {
    if (f.size() != size + 1) {
      throw new IllegalArgumentException(
          "expected one more integer than there are trees being annotated");
    }
    this.coefficients.put(f, coefficient);
  }

  @Deprecated
  public void add(Coefficient coefficient, Constraints context) {
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
    this.coefficients.put(f, coefficient);
  }

  @Deprecated
  public Coefficient getOrFreshForReal(Constraints context, int... index) {
    var f = Ints.asList(index);
    if (f.size() != size + 1) {
      throw new IllegalArgumentException(
          "expected one more integer than there are trees being annotated");
    }
    return this.coefficients.computeIfAbsent(f, k -> context.unknown());
  }

  public Coefficient getCoefficientOrZero(int... index) {
    final var result = getCoefficient(index);
    if (result == null) {
      log.warning("Zeroing a coefficient because of absent counterpart.");
      return KnownCoefficient.ZERO;
    }
    return result;
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
      sb.append("rank(t");
      sb.append(Util.generateSubscript(i + 1));
      sb.append(")");

      if (i < size - 1) {
        sb.append(" + ");
      }
    }
    if (size > 0 && nonzero) {
      sb.append(" + ");
    }
    nonzero = false;
    var entries = coefficients.entrySet();
    int i = 0;
    for (var entry : entries) {
      final var q = entry.getValue();
      if (q == KnownCoefficient.ZERO) {
        continue;
      }
      nonzero = true;
      sb.append(q);
      sb.append(" · ");
      sb.append("log(");
      for (int j = 0; j < size; j++) {
        sb.append(entry.getKey().get(j));
        sb.append(" · ");
        sb.append("|t");
        sb.append(Util.generateSubscript(j + 1));
        sb.append("| + ");

        if (j < size - 1) {
          sb.append(" + ");
        }
      }

      // if (size > 0)
      sb.append(entry.getKey().get(size));
      sb.append(")");

      if (i < coefficients.size() - 1) {
        sb.append(" + ");
      }

      i++;
    }
    if (!nonzero) {
      sb.append("0");
    }
    return sb.toString();
  }

  public String toStringForParameters(List<String> parameters) {
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
      sb.append("rank(");
      sb.append(parameters.get(i));
      sb.append(")");

      if (i < size - 1) {
        sb.append(" + ");
      }
    }
    if (size > 0 && nonzero) {
      sb.append(" + ");
    }
    return sb.toString()
        + coefficients
            .entrySet()
            .stream()
            .filter(e -> e.getValue() != KnownCoefficient.ZERO)
            .map(
                e -> {
                  final var index = e.getKey();
                  final var coefficient = e.getValue();
                  final var items = new ArrayList<String>(index.size());
                  for (int i = 0; i < index.size() - 1; i++) {
                    if (index.get(i) == 0) {
                      continue;
                    }
                    var item = "";
                    if (index.get(i) != 1) {
                      item += index.get(i) + " · ";
                    }
                    item += "|" + parameters.get(i) + "|";
                    items.add(item);
                  }
                  if (index.get(index.size() - 1) != 0) {
                    items.add(String.valueOf(index.get(index.size() - 1)));
                  }
                  var result = "";
                  if (!coefficient.equals(new KnownCoefficient(Fraction.ONE))) {
                    result = coefficient + " · ";
                  }
                  return result + "log(" + String.join(" + ", items) + ")";
                })
            .collect(Collectors.joining(" + "));
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
    return coefficients.get(index);
  }

  public Coefficient getCoefficient(int... index) {
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
}
