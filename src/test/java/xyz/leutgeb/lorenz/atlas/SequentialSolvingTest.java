package xyz.leutgeb.lorenz.atlas;

import static xyz.leutgeb.lorenz.atlas.TestUtil.loadAndNormalizeAndInferAndUnshare;

import java.io.IOException;
import java.nio.file.Paths;
import java.util.Collections;
import java.util.Map;
import java.util.stream.Collectors;
import org.junit.jupiter.api.Test;
import xyz.leutgeb.lorenz.atlas.typing.simple.TypeError;
import xyz.leutgeb.lorenz.atlas.unification.UnificationError;

public class SequentialSolvingTest {
  @Test
  public void sequential() throws UnificationError, TypeError, IOException {
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

    final var annotations =
        immutableAnnotations.entrySet().stream()
            .filter(entry -> entry.getValue().annotation.isPresent())
            .collect(
                Collectors.toMap(Map.Entry::getKey, entry -> entry.getValue().annotation.get()));

    final var tactics =
        immutableAnnotations.entrySet().stream()
            .filter(entry -> entry.getValue().tactic.isPresent())
            .collect(
                Collectors.toMap(
                    Map.Entry::getKey,
                    entry ->
                        Paths.get(
                            ".",
                            "src",
                            "test",
                            "resources",
                            "tactics",
                            entry.getValue().tactic.get() + ".txt")));

    program.solve(annotations, tactics, true, Collections.emptySet());
  }
}
