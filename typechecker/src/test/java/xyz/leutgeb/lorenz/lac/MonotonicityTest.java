package xyz.leutgeb.lorenz.lac;

import static java.util.Collections.emptySet;
import static java.util.Collections.singletonList;
import static org.junit.jupiter.api.Assertions.assertEquals;

import com.google.common.collect.Lists;
import com.google.common.collect.Sets;
import java.util.List;
import java.util.Set;
import java.util.stream.Collectors;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.Disabled;
import org.junit.jupiter.api.Test;
import xyz.leutgeb.lorenz.lac.typing.resources.rules.W;

class MonotonicityTest {
  @Test
  @Disabled
  void useZ3() {
    final var digits = List.of(0, 1, 2);
    final var numbers = Lists.cartesianProduct(List.of(digits, digits, singletonList(0)));
    final var pairs = Lists.cartesianProduct(numbers, numbers);
    final var knowledge = Set.of(List.of(0, 1));

    final var resultZ3With = W.lessThanOrEqual(numbers, knowledge);
    final var resultWith =
        pairs.stream()
            .filter(pair -> W.lessThanOrEqual(pair.get(0), pair.get(1), knowledge))
            .peek(pair -> System.out.println("w:    " + pair.get(0) + " <= " + pair.get(1)))
            .collect(Collectors.toSet());
    System.out.println(Sets.difference(Set.copyOf(resultZ3With), resultWith));
    System.out.println("Missing these: " + Sets.difference(resultWith, Set.copyOf(resultZ3With)));

    final var resultZ3Without = W.lessThanOrEqual(numbers, emptySet());
    final var resultWithout =
        pairs.stream()
            .filter(pair -> W.lessThanOrEqual(pair.get(0), pair.get(1), emptySet()))
            .peek(pair -> System.out.println("w/out: " + pair.get(0) + " <= " + pair.get(1)))
            .collect(Collectors.toSet());
    System.out.println(Sets.difference(Set.copyOf(resultZ3Without), resultWithout));
    System.out.println(Sets.difference(resultWithout, Set.copyOf(resultZ3Without)));

    assertEquals(Set.copyOf(resultWithout), Set.copyOf(resultZ3Without));
    assertEquals(Set.copyOf(resultWith), Set.copyOf(resultZ3With));

    for (var pair : resultWith) {
      final var swapped = List.of(pair.get(1), pair.get(0));
      if (resultWith.contains(swapped)) {
        if (!pair.equals(swapped)) {
          // assertEquals(swapped, pair);
          Assertions.fail(pair.toString());
        }
      }
    }

    for (var pair : resultWithout) {
      if (!resultWith.contains(pair)) {
        Assertions.fail("Missing result: " + pair.toString());
      }
      final var swapped = List.of(pair.get(1), pair.get(0));
      if (resultWithout.contains(swapped)) {
        if (!pair.equals(swapped)) {
          // assertEquals(swapped, pair);
          Assertions.fail(pair.toString());
        }
      }
    }

    System.out.println(Sets.difference(resultWith, resultWithout));

    Assertions.assertTrue(resultWith.size() > resultWithout.size());
  }
}
