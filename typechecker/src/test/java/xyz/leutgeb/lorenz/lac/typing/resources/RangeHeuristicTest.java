package xyz.leutgeb.lorenz.lac.typing.resources;

import static org.junit.jupiter.api.Assertions.assertTrue;

import com.google.common.collect.Lists;
import java.util.List;
import org.junit.jupiter.api.Test;
import xyz.leutgeb.lorenz.lac.typing.resources.heuristics.RangeHeuristic;
import xyz.leutgeb.lorenz.lac.typing.resources.heuristics.SmartRangeHeuristic;
import xyz.leutgeb.lorenz.lac.typing.resources.rules.W;

class RangeHeuristicTest {
  @Test
  void generate() {
    var it = new RangeHeuristic(0, 4);
    var result = it.generate("", 3);
    assertTrue(true);
  }

  @Test
  void bar() {
    var it = SmartRangeHeuristic.DEFAULT;
    var result = it.generate("", 3);
    System.out.println(result);
    assertTrue(true);
  }

  @Test
  void foo() {
    /*
    assertEquals(-1, comparator.compare(List.of(0, 0, 0), List.of(0, 0, 1)));
    assertEquals(-1, comparator.compare(List.of(0, 0, 0), List.of(0, 1, 0)));
    assertEquals(-1, comparator.compare(List.of(0, 0, 0), List.of(1, 0, 0)));

    assertEquals(-1, comparator.compare(List.of(0, 0, 0), List.of(0, 1, 1)));
    assertEquals(-1, comparator.compare(List.of(0, 0, 0), List.of(1, 1, 0)));
    assertEquals(-1, comparator.compare(List.of(0, 0, 0), List.of(1, 0, 1)));
    assertEquals(-1, comparator.compare(List.of(0, 0, 1), List.of(0, 1, 1)));

    assertEquals(0, comparator.compare(List.of(0, 0, 1), List.of(1, 1, 0)));

    assertEquals(-1, comparator.compare(List.of(0, 0, 1), List.of(1, 0, 1)));
    assertEquals(-1, comparator.compare(List.of(0, 1, 0), List.of(0, 1, 1)));
    assertEquals(-1, comparator.compare(List.of(0, 1, 0), List.of(1, 1, 0)));
    assertEquals(-1, comparator.compare(List.of(0, 1, 0), List.of(1, 0, 1)));
    assertEquals(-1, comparator.compare(List.of(1, 0, 0), List.of(0, 1, 1)));
    assertEquals(-1, comparator.compare(List.of(1, 0, 0), List.of(1, 1, 0)));
    assertEquals(-1, comparator.compare(List.of(1, 0, 0), List.of(1, 0, 1)));

    assertEquals(0, comparator.compare(List.of(1, 0, 1), List.of(1, 0, 1)));
    assertEquals(-1, comparator.compare(List.of(1, 0, 1), List.of(1, 1, 1)));
    assertEquals(1, comparator.compare(List.of(1, 0, 1), List.of(1, 0, 0)));
    assertEquals(1, comparator.compare(List.of(1, 0, 1), List.of(0, 0, 1)));
    assertEquals(1, comparator.compare(List.of(1, 0, 1), List.of(0, 0, 0)));
    assertEquals(-1, comparator.compare(List.of(0, 0, 0), List.of(0, 0, 1)));
    assertEquals(-1, comparator.compare(List.of(0, 0, 0), List.of(0, 0, 2)));
    assertEquals(-1, comparator.compare(List.of(0, 0, 1), List.of(0, 0, 2)));
    assertEquals(0, comparator.compare(List.of(0, 0, 1), List.of(0, 1, 0)));
    assertEquals(0, comparator.compare(List.of(1, 0, 1), List.of(0, 1, 1)));
    assertEquals(0, comparator.compare(List.of(1, 0, 1), List.of(0, 1, 0)));
    var lists =
        new ArrayList<>(
            List.of(
                List.of(1, 0, 1),
                List.of(0, 0, 0),
                List.of(1, 1, 1),
                List.of(1, 1, 0),
                List.of(0, 1, 0)));
    lists.sort(comparator);
    */

    final var digits = List.of(0, 1);
    final var numbers = Lists.cartesianProduct(List.of(digits, digits));
    final var pairs = Lists.cartesianProduct(numbers, numbers);

    for (var pair : pairs) {
      if (W.ordered(pair.get(0), pair.get(1))) {
        System.out.println(pair.get(0) + " < " + pair.get(1));
      }
    }
  }
}
