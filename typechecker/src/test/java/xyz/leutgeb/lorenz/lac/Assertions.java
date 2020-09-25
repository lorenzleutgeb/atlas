package xyz.leutgeb.lorenz.lac;

import java.util.ArrayList;
import java.util.HashSet;
import java.util.Iterator;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.stream.Stream;
import org.opentest4j.AssertionFailedError;
import tech.tablesaw.api.StringColumn;
import tech.tablesaw.api.Table;
import xyz.leutgeb.lorenz.lac.ast.Identifier;
import xyz.leutgeb.lorenz.lac.typing.resources.AnnotatingContext;
import xyz.leutgeb.lorenz.lac.typing.resources.Annotation;
import xyz.leutgeb.lorenz.lac.typing.resources.proving.Obligation;

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
        .sorted(Annotation.INDEX_COMPARATOR)
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
            StringColumn.create("Expected (" + expected.getName() + ")", expectedColumn),
            StringColumn.create("Actual (" + actual.getName() + ")", actualColumn));

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
      List<Identifier> expectedIds, Annotation expectedAnnotation, AnnotatingContext actual) {
    return assertContextEquals(new AnnotatingContext(expectedIds, expectedAnnotation), actual);
  }

  public static <V> V assertContextEquals(
      List<Identifier> expectedIds, Annotation expectedAnnotation, Obligation actual) {
    return assertContextEquals(expectedIds, expectedAnnotation, actual.getContext());
  }

  public static <V> V assertContextEqualsByPrefixes(
      List<String> expectedIds, Annotation expectedAnnotation, AnnotatingContext actual) {
    Set<Identifier> actualIds = new HashSet<>(actual.getIds());
    List<Identifier> matchedIds = new ArrayList<>(actual.size());
    outer:
    for (String expectedId : expectedIds) {
      for (Iterator<Identifier> iterator = actualIds.iterator(); iterator.hasNext(); ) {
        final Identifier actualId = iterator.next();
        if (actualId.getName().startsWith(expectedId)) {
          matchedIds.add(actualId);
          iterator.remove();
          continue outer;
        }
      }
    }
    return assertContextEquals(new AnnotatingContext(matchedIds, expectedAnnotation), actual);
  }
}
