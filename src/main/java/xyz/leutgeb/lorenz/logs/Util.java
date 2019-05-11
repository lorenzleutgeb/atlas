package xyz.leutgeb.lorenz.logs;

import java.io.PrintStream;
import java.util.ArrayList;
import java.util.Comparator;
import java.util.Iterator;
import java.util.List;
import java.util.PriorityQueue;
import java.util.function.ToDoubleFunction;
import org.apache.commons.text.similarity.JaroWinklerDistance;
import org.apache.commons.text.similarity.SimilarityScore;

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
}
