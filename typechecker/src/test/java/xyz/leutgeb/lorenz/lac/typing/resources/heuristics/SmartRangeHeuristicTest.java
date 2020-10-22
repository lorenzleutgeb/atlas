package xyz.leutgeb.lorenz.lac.typing.resources.heuristics;

import org.junit.jupiter.api.Test;

import java.util.List;
import java.util.stream.Stream;

import static org.junit.jupiter.api.Assertions.assertAll;
import static org.junit.jupiter.api.Assertions.assertEquals;

class SmartRangeHeuristicTest {
  @Test
  void generateSize() {
    int treeSize = 2;
    Stream<List<Integer>> stream = SmartRangeHeuristic.DEFAULT.generate(treeSize);
    assertAll(stream.map(index -> () -> assertEquals(treeSize + 1, index.size())));
  }
}
