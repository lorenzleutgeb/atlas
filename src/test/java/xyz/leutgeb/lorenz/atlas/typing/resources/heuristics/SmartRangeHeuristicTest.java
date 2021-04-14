package xyz.leutgeb.lorenz.atlas.typing.resources.heuristics;

import static org.junit.jupiter.api.Assertions.assertAll;
import static org.junit.jupiter.api.Assertions.assertEquals;

import java.util.List;
import java.util.stream.Stream;
import org.junit.jupiter.api.Test;

class SmartRangeHeuristicTest {
  @Test
  void generateSize() {
    int treeSize = 2;
    Stream<List<Integer>> stream = SmartRangeHeuristic.DEFAULT.generate(treeSize);
    assertAll(stream.map(index -> () -> assertEquals(treeSize + 1, index.size())));
  }
}
