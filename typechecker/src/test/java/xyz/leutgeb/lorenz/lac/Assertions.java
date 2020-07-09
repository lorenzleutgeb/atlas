package xyz.leutgeb.lorenz.lac;

import com.google.common.collect.Comparators;
import java.util.*;
import java.util.stream.Stream;
import org.opentest4j.AssertionFailedError;
import tech.tablesaw.api.StringColumn;
import tech.tablesaw.api.Table;
import xyz.leutgeb.lorenz.lac.typing.resources.AnnotatingContext;
import xyz.leutgeb.lorenz.lac.typing.resources.Annotation;

public class Assertions {
  public static <V> V assertAnnotationEquals(Annotation expected, Annotation actual, String name) {
    if (expected.coefficientsEqual(actual)) {
      return null;
    }
    var indexColumn = new ArrayList<String>();
    var expectedColumn = new ArrayList<String>();
    var actualColumn = new ArrayList<String>();

    Stream.concat(
            expected.streamCoefficients().map(Map.Entry::getKey),
            actual.streamCoefficients().map(Map.Entry::getKey))
        .sorted(Comparators.lexicographical(Integer::compareTo))
        .distinct()
        .forEach(
            index -> {
              var expectedValue = expected.getCoefficientOrZero(index);
              var actualValue = actual.getCoefficientOrZero(index);
              if (!expectedValue.equals(actualValue)) {
                indexColumn.add(index.toString());
                expectedColumn.add(expectedValue.toString());
                actualColumn.add(actualValue.toString());
              }
            });

    final Table table =
        Table.create(
            name,
            StringColumn.create("Index", indexColumn),
            StringColumn.create("Expected", expectedColumn),
            StringColumn.create("Actual", actualColumn));

    throw new AssertionFailedError(table.toString(), expected, actual);
  }

  public static <V> V assertAnnotationEquals(Annotation expected, Annotation actual) {
    return assertAnnotationEquals(expected, actual, expected.getName());
  }

  public static <V> V assertContextEquals(AnnotatingContext expected, AnnotatingContext actual) {
    // assertEquals(Set.of(expected.getIds()), Set.of(actual.getIds()));
    assertAnnotationEquals(
        expected.getAnnotation(), actual.reorder(expected.getIds()).getAnnotation());
    return null;
  }

  public static <V> V assertContextEquals(
      List<String> expectedIds, Annotation expectedAnnotation, AnnotatingContext actual) {
    return assertContextEquals(new AnnotatingContext(expectedIds, expectedAnnotation), actual);
  }

  public static <V> V assertContextEqualsByPrefixes(
      List<String> expectedIds, Annotation expectedAnnotation, AnnotatingContext actual) {
    Set<String> actualIds = new HashSet<>(actual.getIds());
    List<String> reordered = new ArrayList<>(actual.size());
    outer:
    for (String expectedId : expectedIds) {
      for (Iterator<String> iterator = actualIds.iterator(); iterator.hasNext(); ) {
        final String actualId = iterator.next();
        if (actualId.startsWith(expectedId)) {
          reordered.add(actualId);
          iterator.remove();
          continue outer;
        }
      }
    }
    return assertContextEquals(new AnnotatingContext(reordered, expectedAnnotation), actual);
  }
}
