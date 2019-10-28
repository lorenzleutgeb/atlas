package xyz.leutgeb.lorenz.logs;

import static guru.nidi.graphviz.model.Factory.node;

import com.microsoft.z3.RatNum;
import guru.nidi.graphviz.model.Node;
import java.io.PrintStream;
import java.util.ArrayList;
import java.util.Comparator;
import java.util.Iterator;
import java.util.List;
import java.util.PriorityQueue;
import java.util.function.BiFunction;
import java.util.function.ToDoubleFunction;
import java.util.stream.Collectors;
import java.util.stream.IntStream;
import lombok.extern.log4j.Log4j2;
import org.apache.commons.text.similarity.JaroWinklerDistance;
import org.apache.commons.text.similarity.SimilarityScore;
import org.hipparchus.fraction.Fraction;

@Log4j2
public class Util {
  private static final SimilarityScore<Double> SIMILARITY = new JaroWinklerDistance();

  public static String generateSubscript(int i) {
    StringBuilder sb = new StringBuilder();
    for (char ch : String.valueOf(i).toCharArray()) {
      if (Character.isDigit(ch)) {
        sb.append((char) ('\u2080' + (ch - '0')));
      } else {
        sb.append(ch);
      }
    }
    return sb.toString();
  }

  public static void indent(PrintStream out, int indentation) {
    for (int i = 0; i < indentation; i++) {
      out.print("    ");
    }
  }

  public static List<String> similar(
      String key, Iterator<String> candidates, double threshold, int limit) {
    ToDoubleFunction<String> similarity = x -> SIMILARITY.apply(key, x);
    Comparator<String> comparator = Comparator.comparingDouble(similarity).reversed();
    var similar = new PriorityQueue<>(comparator);
    while (candidates.hasNext()) {
      var candidate = candidates.next();
      if (similarity.applyAsDouble(candidate) > threshold) {
        similar.add(candidate);
      }
    }
    if (similar.size() < limit) {
      return new ArrayList<>(similar);
    }
    var result = new ArrayList<String>(limit);
    for (int i = 0; i < limit; i++) {
      result.add(similar.poll());
    }
    return result;
  }

  public static void ensureLibrary(String name) {
    try {
      System.loadLibrary(name);
    } catch (UnsatisfiedLinkError e) {
      log.fatal(
          "The library '"
              + name
              + "' is required, but could not be loaded. Make sure that a file named 'lib"
              + name
              + ".so' (or similar, since this name is platform-dependent) exists in one of the following paths: '"
              + System.getProperty("java.library.path")
              + "'.",
          e);
      System.exit(1);
    }
  }

  public static Fraction toFraction(RatNum x) {
    return new Fraction(
        x.getBigIntNumerator().intValueExact(), x.getBigIntDenominator().intValueExact());
  }

  public static <T, U> BiFunction<T, U, T> first() {
    return (T a, U b) -> a;
  }

  public static <T, U> BiFunction<T, U, U> second() {
    return (T a, U b) -> b;
  }

  public static boolean isZero(List<Integer> xs) {
    return xs.stream().allMatch(x -> x == 0);
  }

  public static List<Integer> zero(int n) {
    return IntStream.generate(() -> 0).limit(n).boxed().collect(Collectors.toList());
  }

  public static RuntimeException bug(String message) {
    return new RuntimeException("bug: " + message);
  }

  public static String truncate(String s, int n) {
    return s.length() > n ? s.substring(0, n - 3) + "..." : s;
  }

  public static Node objectNode(Object o) {
    if (o == null) {
      return node("null");
    }
    return node(o.getClass().getSimpleName() + "@" + System.identityHashCode(o))
        .with("label", o.toString());
  }

  public static String capitalizeFirstLetter(String original) {
    if (original == null || original.length() == 0) {
      return original;
    }
    return original.substring(0, 1).toUpperCase() + original.substring(1);
  }

  public static RuntimeException notImplemented(String feature) {
    return new UnsupportedOperationException("not implemented: " + feature);
  }

  public static String repeat(String str, int times) {
    return new String(new char[times]).replace("\0", str);
  }
}
