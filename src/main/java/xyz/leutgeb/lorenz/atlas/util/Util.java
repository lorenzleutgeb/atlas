package xyz.leutgeb.lorenz.atlas.util;

import static guru.nidi.graphviz.model.Factory.node;
import static java.util.stream.Collectors.joining;
import static java.util.stream.Collectors.toList;
import static java.util.stream.Collectors.toMap;
import static java.util.stream.Stream.generate;
import static xyz.leutgeb.lorenz.atlas.typing.resources.coefficients.KnownCoefficient.ONE;
import static xyz.leutgeb.lorenz.atlas.typing.resources.coefficients.KnownCoefficient.ZERO;

import com.microsoft.z3.RatNum;
import guru.nidi.graphviz.model.Node;
import java.io.IOException;
import java.io.PrintStream;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.ArrayList;
import java.util.Collection;
import java.util.Comparator;
import java.util.Iterator;
import java.util.List;
import java.util.Map;
import java.util.Objects;
import java.util.PriorityQueue;
import java.util.Random;
import java.util.Set;
import java.util.Stack;
import java.util.function.BiFunction;
import java.util.function.Function;
import java.util.function.Supplier;
import java.util.function.ToDoubleFunction;
import java.util.stream.Collectors;
import java.util.stream.Stream;
import lombok.extern.slf4j.Slf4j;
import org.hipparchus.fraction.Fraction;
import xyz.leutgeb.lorenz.atlas.ast.Identifier;
import xyz.leutgeb.lorenz.atlas.typing.resources.coefficients.Coefficient;

@Slf4j
public class Util {
  public static final String TOOL_NAME = "atlas";

  private static final Random RANDOM = new Random(0L);
  private static final String LIBRARY_PATH = "java.library.path";
  private static final String LIBRARY_PATH_PREFIX =
      "./:./lib/:../lib:/usr/lib/x86_64-linux-gnu:/usr/lib/x86_64-linux-gnu/jni";

  public static String generateSubscriptIndex(List<Integer> index) {
    return index.stream().map(Util::generateSubscript).collect(joining(" ", "₍", "₎"));
  }

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

  public static String generateSuperscript(int i) {
    StringBuilder sb = new StringBuilder();
    for (char ch : String.valueOf(i).toCharArray()) {
      if (Character.isDigit(ch)) {
        if (ch == '1') {
          sb.append('\u00b9');
        } else if (ch == '2') {
          sb.append('\u00b2');
        } else if (ch == '3') {
          sb.append('\u00b3');
        } else {
          sb.append((char) ('\u2080' + (ch - '0')));
        }
      } else {
        sb.append(ch);
      }
    }
    return sb.toString();
  }

  public static String escapeSubscript(String s) {
    return s.replace("\u2080", "0")
        .replace("\u2081", "1")
        .replace("\u2082", "2")
        .replace("\u2083", "3")
        .replace("\u2084", "4")
        .replace("\u2085", "5")
        .replace("\u2086", "6")
        .replace("\u2087", "7")
        .replace("\u2088", "8")
        .replace("\u2089", "9")
        .replace("₍", "_")
        .replace("₎", "_");
  }

  public static void indent(PrintStream out, int indentation) {
    if (indentation == 0) {
      return;
    }
    for (int i = 0; i < indentation; i++) {
      out.print("  ");
    }
  }

  public static List<String> similar(
      String key, Iterator<String> candidates, double threshold, int limit) {
    ToDoubleFunction<String> similarity = x -> JaroWinklerDistance.apply(key, x);
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

  public static String undefinedText(String undefined, Iterable<String> defined) {
    return undefinedText(undefined, defined.iterator());
  }

  public static String undefinedText(String undefined, Iterator<String> defined) {
    return "'"
        + undefined
        + "' is not defined. (Did you mean "
        + Util.similar(undefined, defined, 0.5, 4)
        + "?)";
  }

  private static synchronized void patchLibraryPath() {
    final String libraryPath = System.getProperty(LIBRARY_PATH);
    if (!libraryPath.startsWith(LIBRARY_PATH_PREFIX)) {
      System.setProperty(
          LIBRARY_PATH, LIBRARY_PATH_PREFIX + (libraryPath.isEmpty() ? "" : ":" + libraryPath));
    }
  }

  public static void loadLibrary(String name) {
    final String libraryName = System.mapLibraryName(name);
    patchLibraryPath();
    try {
      System.loadLibrary(name);
    } catch (UnsatisfiedLinkError e) {
      log.warn(
          "The library '"
              + name
              + "' is required, but could not be loaded. Make sure that a file named '"
              + libraryName
              + " exists in one of the following paths: '"
              + System.getProperty(LIBRARY_PATH)
              + "'. Execution will continue but may fail at a later time because of this.");
    }
  }

  public static Fraction toFraction(RatNum x) {
    return new Fraction(
        x.getBigIntNumerator().intValueExact(), x.getBigIntDenominator().intValueExact());
  }

  public static boolean isInteger(Fraction fraction) {
    return 1 == fraction.getDenominator();
  }

  public static <T, U> BiFunction<T, U, T> first() {
    return (T a, U b) -> a;
  }

  public static <T, U> BiFunction<T, U, U> second() {
    return (T a, U b) -> b;
  }

  public static boolean isAllZeroes(List<Integer> xs) {
    return xs.stream().allMatch(x -> x == 0);
  }

  public static List<Integer> zero(int n) {
    return repeat(0, n).collect(toList());
  }

  public static List<Coefficient> zeroCoefficients(int n) {
    return repeat(ZERO, n).collect(Collectors.toList());
  }

  public static String fqnToFlatFilename(String fqn) {
    return fqn.replace(".", "~");
  }

  public static RuntimeException bug() {
    return new RuntimeException("bug");
  }

  public static RuntimeException bug(String message) {
    return new RuntimeException("bug: " + message);
  }

  private static String randomHex(int length) {
    StringBuilder sb = new StringBuilder();
    while (sb.length() < length) {
      sb.append(String.format("%08x", RANDOM.nextInt()));
    }
    return sb.substring(0, length);
  }

  public static String randomHex() {
    return randomHex(64);
  }

  public static String truncate(String s, int n) {
    return s.length() > n ? s.substring(0, n - 3) + "..." : s;
  }

  public static String stamp(Object o) {
    if (o == null) {
      return "null";
    }
    return o.getClass().getSimpleName() + "_" + System.identityHashCode(o);
  }

  public static Node rawObjectNode(Object o) {
    return rawObjectNode(o, "");
  }

  private static Node rawObjectNode(Object o, String suffix) {
    return node(stamp(o) + suffix);
  }

  public static Node objectNode(Object o, String label, String suffix) {
    if (o == null) {
      return node("null");
    }
    return rawObjectNode(o, suffix).with("label", label);
  }

  public static Node objectNode(Object o) {
    return objectNode(o, o == null ? "null" : o.toString());
  }

  private static Node objectNode(Object o, String label) {
    return objectNode(o, label, "");
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

  public static <T> Stream<T> repeat(final T item, long times) {
    return generate(() -> item).limit(times);
  }

  public static String repeat(String str, int times) {
    return new String(new char[times]).replace("\0", str);
  }

  @SafeVarargs
  public static <T> Stack<T> stack(T... elements) {
    final var result = new Stack<T>();
    for (int i = elements.length - 1; i >= 0; i--) {
      result.push(elements[i]);
    }
    return result;
  }

  public static Set<String> setOfNames(Set<Identifier> ids) {
    return ids.stream().map(Identifier::getName).collect(Collectors.toSet());
  }

  public static <E> E pick(Collection<E> set) {
    if (set.isEmpty()) {
      throw new IllegalArgumentException("cannot get element from empty set");
    }
    return set.iterator().next();
  }

  public static <E> List<E> append(List<E> a, List<E> b) {
    if (a.isEmpty()) {
      return b;
    }
    if (b.isEmpty()) {
      return a;
    }

    final var result = new ArrayList<E>(a.size() + b.size());
    result.addAll(a);
    result.addAll(b);
    return result;
  }

  public static <T, U> Function<T, U> fallback(Function<T, U> primary, Function<T, U> fallback) {
    return t -> {
      final U result = primary.apply(t);
      if (result == null) {
        return fallback.apply(t);
      }
      return result;
    };
  }

  public static <E> List<E> flatten(List<List<E>> list) {
    return list.stream().flatMap(Collection::stream).collect(toList());
  }

  public static <E> List<E> reorder(List<E> list, List<Integer> indices) {
    return indices.stream().map(list::get).collect(toList());
  }

  @SafeVarargs
  public static <K> Map<List<K>, Coefficient> ones(List<K>... indices) {
    return Stream.of(indices).collect(toMap(Function.identity(), x -> ONE));
  }

  public static <T> Supplier<T> supply(T value) {
    return () -> value;
  }

  public static String toVectorString(List<Integer> vector) {
    return mapToString(vector.stream()).collect(Collectors.joining(",", "(", ")"));
  }

  public static <T> Stream<String> mapToString(Stream<T> stream) {
    return stream.map(Objects::toString);
  }

  public static boolean goodForReading(Path path) {
    return Files.exists(path) && Files.isReadable(path) && Files.isRegularFile(path);
  }

  public <E> Iterable<E> toIterable(Stream<E> stream) {
    return stream::iterator;
  }

  public static PrintStream output(Path path) throws IOException {
    Files.createDirectories(path.getParent());
    return new PrintStream(Files.newOutputStream(path));
  }

  public static boolean haveClass(String className) {
    try {
      Class.forName(className);
    } catch (ClassNotFoundException e) {
      return false;
    }
    return true;
  }

  /** See {@link org.graalvm.nativeimage.ImageInfo#inImageRuntimeCode} */
  public static boolean inImageRuntimeCode() {
    return "runtime".equals(System.getProperty("org.graalvm.nativeimage.imagecode"));
  }

  public static boolean flag(Class<?> requester, Map<String, String> arguments, String name) {
    return Boolean.parseBoolean(getProperty(requester, name, arguments, "false"));
  }

  public static String getProperty(Class<?> requester, String name, String fallback) {
    return System.getProperty(requester.getName() + "." + name, fallback);
  }

  public static String getProperty(Class<?> requester, String name) {
    return getProperty(requester, name, null);
  }

  public static String getProperty(
      Class<?> requester, String name, Map<String, String> arguments, String fallback) {
    if (arguments.containsKey(name)) {
      return arguments.get(name);
    }
    return getProperty(requester, name, fallback);
  }
}
