package xyz.leutgeb.lorenz.atlas;

import static xyz.leutgeb.lorenz.atlas.TestUtil.*;

import java.util.Collections;
import java.util.Map;
import org.junit.jupiter.api.Test;

public class SequentialSolvingTest {
  @Test
  public void sequential() {
    final var immutableAnnotations =
        Map.of(
            "SplayTree.insert",
            Config.of("SplayTree/insert"),
            "SplayTree.splay",
            Config.of("SplayTree/splay"),
            "SplayTree.splay_max",
            Config.of("SplayTree/splay_max"),
            "SplayTree.delete",
            Config.of(),
            "SplayHeap.partition",
            Config.of("SplayHeap/partition"),
            "SplayHeap.insert",
            Config.of("SplayHeap/insert"),
            "SplayHeap.del_min",
            Config.of("SplayHeap/del_min"),
            "PairingHeap.del_min_via_merge_pairs_isolated",
            Config.of("PairingHeap/del_min_via_merge_pairs_isolated"),
            "PairingHeap.merge_pairs_isolated",
            Config.of("PairingHeap/merge_pairs_isolated"),
            "PairingHeap.insert_isolated",
            Config.of("PairingHeap/insert_isolated"));

    final var program = loadAndNormalizeAndInferAndUnshare(immutableAnnotations.keySet());
    program.solve(
        extractAnnotations(immutableAnnotations),
        extractTactics(immutableAnnotations),
        true,
        true,
        true,
        Collections.emptySet());
  }
}
