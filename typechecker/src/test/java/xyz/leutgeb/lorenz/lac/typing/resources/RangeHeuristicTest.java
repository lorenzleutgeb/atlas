package xyz.leutgeb.lorenz.lac.typing.resources;

import static org.junit.jupiter.api.Assertions.assertTrue;

import org.junit.jupiter.api.Test;
import xyz.leutgeb.lorenz.lac.typing.resources.heuristics.RangeHeuristic;

class RangeHeuristicTest {

  @Test
  void generate() {
    var it = new RangeHeuristic(0, 4);
    var result = it.generate("", 2);
    assertTrue(true);
  }
}
