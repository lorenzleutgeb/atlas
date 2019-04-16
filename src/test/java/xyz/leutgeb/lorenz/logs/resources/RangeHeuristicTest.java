package xyz.leutgeb.lorenz.logs.resources;

import static org.junit.jupiter.api.Assertions.*;

import org.junit.jupiter.api.Test;

class RangeHeuristicTest {

  @Test
  void generate() {
    var it = new RangeHeuristic(0, 4);
    var result = it.generate(2, new Constraints());
    assertTrue(true);
  }
}
