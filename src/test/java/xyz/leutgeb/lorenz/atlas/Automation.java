package xyz.leutgeb.lorenz.atlas;

import static java.util.Collections.emptyMap;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static xyz.leutgeb.lorenz.atlas.Tactics.*;
import static xyz.leutgeb.lorenz.atlas.TestUtil.TACTICS;
import static xyz.leutgeb.lorenz.atlas.TestUtil.loadAndNormalizeAndInferAndUnshare;

import java.util.Collections;
import java.util.HashMap;
import java.util.Map;
import java.util.stream.Collectors;
import java.util.stream.Stream;
import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.Arguments;
import org.junit.jupiter.params.provider.MethodSource;
import xyz.leutgeb.lorenz.atlas.typing.resources.CombinedFunctionAnnotation;

public class Automation {
  private static Stream<Arguments> instances() {
    return Stream.of(
        Arguments.of(
            "RandMeldableHeap",
            Map.of(
                "meld", RAND_MELDABLEHEAP_MELD_EXPECTED,
                "insert", RAND_MELDABLEHEAP_INSERT_EXPECTED,
                "del_min", RAND_MELDABLEHEAP_DEL_MIN_EXPECTED)),
        Arguments.of("RandTreeSort", Map.of("descend", RAND_TREESORT_DESCEND_EXPECTED)),
        Arguments.of(
            "RandTreeSort",
            Map.of(
                "insert", RAND_TREESORT_INSERT_EXPECTED,
                "delete_max", RAND_TREESORT_DELETE_MAX_EXPECTED,
                "remove", RAND_TREESORT_REMOVE_EXPECTED)),
        Arguments.of(
            "SplayTree",
            Map.of(
                "splay",
                SPLAYTREE_SPLAY_EXPECTED,
                "insert",
                SPLAYTREE_INSERT_EXPECTED,
                "splay_max",
                SPLAYTREE_SPLAY_MAX_EXPECTED,
                "delete",
                SPLAYTREE_DELETE_EXPECTED)),
        Arguments.of(
            "SplayHeap",
            Map.of(
                "partition", SPLAYHEAP_PARTITION_EXPECTED,
                "insert", SPLAYHEAP_INSERT_EXPECTED,
                "del_min", SPLAYHEAP_DEL_MIN_EXPECTED)),
        Arguments.of(
            "PairingHeap",
            Map.of(
                "merge_pairs_isolated",
                PAIRINGHEAP_MERGE_PAIRS_ISOLATED_EXPECTED,
                "del_min_via_merge_pairs_isolated",
                PAIRINGHEAP_DEL_MIN_VIA_MERGE_PAIRS_ISOLATED_EXPECTED,
                "insert_isolated",
                PAIRINGHEAP_INSERT_ISOLATED_EXPECTED)),
        Arguments.of("RandSplayTree", Map.of("splay", RAND_SPLAYTREE_SPLAY_EXPECTED)),
        Arguments.of(
            "RandSplayTree",
            Map.of(
                "insert",
                RAND_SPLAYTREE_INSERT_EXPECTED,
                "splay_max",
                RAND_SPLAYTREE_SPLAY_EXPECTED,
                "delete",
                RAND_SPLAYTREE_SPLAY_EXPECTED)));
  }

  @ParameterizedTest
  @MethodSource("instances")
  public void test(String module, Map<String, CombinedFunctionAnnotation> instance) {
    instance =
        instance.entrySet().stream()
            .collect(Collectors.toMap(e -> module + "." + e.getKey(), Map.Entry::getValue));
    final boolean tactics = false;
    final var program = loadAndNormalizeAndInferAndUnshare(instance.keySet());
    final var result =
        program.solve(
            new HashMap<>(instance),
            tactics ? program.lookupTactics(emptyMap(), TACTICS) : Collections.emptyMap(),
            false,
            true,
            false,
            Collections.emptySet());
    assertTrue(result.isSatisfiable());
    program.printAllInferredSignaturesInOrder(System.out);
  }
}
