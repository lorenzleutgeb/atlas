package xyz.leutgeb.lorenz.logs;

import com.microsoft.z3.RatNum;
import java.io.PrintStream;
import java.util.ArrayList;
import java.util.Comparator;
import java.util.Iterator;
import java.util.List;
import java.util.PriorityQueue;
import java.util.function.ToDoubleFunction;
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
      sb.append((char) ('\u2080' + (ch - '0')));
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
}
