package xyz.leutgeb.lorenz.logs.resources;

import com.google.common.primitives.Ints;
import java.util.ArrayList;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;
import xyz.leutgeb.lorenz.logs.Util;
import xyz.leutgeb.lorenz.logs.resources.coefficients.Coefficient;
import xyz.leutgeb.lorenz.logs.resources.coefficients.KnownCoefficient;

public class Annotation {
  public static final Annotation EMPTY = new Annotation(0);
  protected List<Coefficient> rankCoefficients;
  protected Map<List<Integer>, Coefficient> coefficients;
  protected int size;

  public Annotation(int size) {
    rankCoefficients = new ArrayList<>(size);
    coefficients = new LinkedHashMap<>();
    this.size = size;

    for (int i = 0; i < size; i++) {
      getRankCoefficients().add(KnownCoefficient.ZERO);
    }
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

  public void add(Coefficient coefficient, int... fs) {
    var f = Ints.asList(fs);
    if (f.size() != size + 1) {
      throw new IllegalArgumentException(
          "expected one more integer than there are trees being annotated");
    }
    this.coefficients.put(f, coefficient);
  }

  public Coefficient getOrFresh(Constraints context, int... fs) {
    var f = Ints.asList(fs);
    if (f.size() != size + 1) {
      throw new IllegalArgumentException(
          "expected one more integer than there are trees being annotated");
    }
    return this.coefficients.computeIfAbsent(f, k -> context.unknown());
  }

  public String toString() {
    StringBuilder sb = new StringBuilder();
    for (int i = 0; i < size; i++) {
      Coefficient q = rankCoefficients.get(i);
      sb.append(q);
      sb.append(" · ");
      sb.append("rank(t");
      sb.append(Util.generateSubscript(i + 1));
      sb.append(")");

      if (i < size - 1) {
        sb.append(" + ");
      }
    }
    if (size > 0) {
      sb.append(" + ");
    }
    var entries = coefficients.entrySet();
    int i = 0;
    for (var entry : entries) {
      sb.append(entry.getValue());
      sb.append(" · ");
      sb.append("log(");
      for (int j = 0; j < size; j++) {
        sb.append(entry.getKey().get(j));
        sb.append(" · ");
        sb.append("|t");
        sb.append(Util.generateSubscript(j + 1));
        sb.append("| + ");
      }

      // if (size > 0)
      sb.append(entry.getKey().get(size));
      sb.append(")");

      if (i < coefficients.size() - 1) {
        sb.append(" + ");
      }

      i++;
    }
    return sb.toString();
  }

  public List<Coefficient> getRankCoefficients() {
    return this.rankCoefficients;
  }

  public Coefficient getRankCoefficient() {
    if (size != 1) {
      throw new IllegalStateException("must be of size exactly 1");
    }
    return getRankCoefficients().get(0);
  }

  public Map<List<Integer>, Coefficient> getCoefficients() {
    return this.coefficients;
  }

  public int size() {
    return this.size;
  }
}
