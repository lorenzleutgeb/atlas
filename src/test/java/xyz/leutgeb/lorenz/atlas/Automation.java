package xyz.leutgeb.lorenz.atlas;

import static java.util.Collections.emptyMap;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static xyz.leutgeb.lorenz.atlas.Tactics.PAIRINGHEAP_DELETE_MIN_VIA_MERGE_PAIRS_ISOLATED_EXPECTED;
import static xyz.leutgeb.lorenz.atlas.Tactics.PAIRINGHEAP_INSERT_ISOLATED_EXPECTED;
import static xyz.leutgeb.lorenz.atlas.Tactics.RAND_MELDABLEHEAP_DELETE_MIN_EXPECTED;
import static xyz.leutgeb.lorenz.atlas.Tactics.RAND_MELDABLEHEAP_INSERT_EXPECTED;
import static xyz.leutgeb.lorenz.atlas.Tactics.RAND_MELDABLEHEAP_MELD_EXPECTED;
import static xyz.leutgeb.lorenz.atlas.Tactics.RAND_SEARCHTREE_DELETE_EXPECTED;
import static xyz.leutgeb.lorenz.atlas.Tactics.RAND_SEARCHTREE_DELETE_MAX_EXPECTED;
import static xyz.leutgeb.lorenz.atlas.Tactics.RAND_SEARCHTREE_INSERT_EXPECTED;
import static xyz.leutgeb.lorenz.atlas.Tactics.RAND_SPLAYHEAP_DELETE_MIN_EXPECTED;
import static xyz.leutgeb.lorenz.atlas.Tactics.RAND_SPLAYHEAP_INSERT_EXPECTED;
import static xyz.leutgeb.lorenz.atlas.Tactics.RAND_SPLAYTREE_INSERT_EXPECTED;
import static xyz.leutgeb.lorenz.atlas.Tactics.RAND_SPLAYTREE_SPLAY_EXPECTED;
import static xyz.leutgeb.lorenz.atlas.Tactics.SPLAYHEAP_DELETE_MIN_EXPECTED;
import static xyz.leutgeb.lorenz.atlas.Tactics.SPLAYHEAP_INSERT_EXPECTED;
import static xyz.leutgeb.lorenz.atlas.Tactics.SPLAYTREE_DELETE_EXPECTED;
import static xyz.leutgeb.lorenz.atlas.Tactics.SPLAYTREE_INSERT_EXPECTED;
import static xyz.leutgeb.lorenz.atlas.Tactics.TREE_DESCEND_EXPECTED;
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
import org.slf4j.impl.SimpleLogger;
import xyz.leutgeb.lorenz.atlas.typing.resources.CombinedFunctionAnnotation;

public class Automation {
  static {
    System.setProperty(SimpleLogger.DEFAULT_LOG_LEVEL_KEY, "warn");
    System.setProperty(SimpleLogger.LOG_KEY_PREFIX + "xyz.leutgeb.lorenz", "debug");
    System.setProperty(SimpleLogger.SHOW_SHORT_LOG_NAME_KEY, Boolean.TRUE.toString());
    System.setProperty(SimpleLogger.SHOW_THREAD_NAME_KEY, Boolean.FALSE.toString());
  }

  private static Stream<Arguments> instances() {
    return Stream.of(
        Arguments.of(
            "SplayHeap",
            Map.of(
                "insert", SPLAYHEAP_INSERT_EXPECTED,
                "delete_min", SPLAYHEAP_DELETE_MIN_EXPECTED)),
        Arguments.of(
            "RandSplayHeap",
            Map.of(
                "insert", RAND_SPLAYHEAP_INSERT_EXPECTED,
                "delete_min", RAND_SPLAYHEAP_DELETE_MIN_EXPECTED)),
        Arguments.of(
            "SplayTree",
            Map.of("insert", SPLAYTREE_INSERT_EXPECTED, "delete", SPLAYTREE_DELETE_EXPECTED)),
        Arguments.of(
            "RandSplayTree",
            Map.of(
                "insert", RAND_SPLAYTREE_INSERT_EXPECTED, "delete", RAND_SPLAYTREE_SPLAY_EXPECTED)),
        Arguments.of(
            "RandMeldableHeap",
            Map.of(
                "meld", RAND_MELDABLEHEAP_MELD_EXPECTED,
                "insert", RAND_MELDABLEHEAP_INSERT_EXPECTED,
                "delete_min", RAND_MELDABLEHEAP_DELETE_MIN_EXPECTED)),
        Arguments.of("Tree", Map.of("descend", TREE_DESCEND_EXPECTED)),
        Arguments.of(
            "CoinSearchTree",
            Map.of(
                "insert",
                RAND_SEARCHTREE_INSERT_EXPECTED,
                "delete",
                RAND_SEARCHTREE_DELETE_EXPECTED,
                "delete_max",
                RAND_SEARCHTREE_DELETE_MAX_EXPECTED)),
        Arguments.of(
            "PairingHeap",
            Map.of(
                "delete_min_via_merge_pairs_isolated",
                PAIRINGHEAP_DELETE_MIN_VIA_MERGE_PAIRS_ISOLATED_EXPECTED,
                "insert_isolated",
                PAIRINGHEAP_INSERT_ISOLATED_EXPECTED)));
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
            false,
            false,
            Collections.emptySet());
    assertTrue(result.isSatisfiable());
    program.printAllInferredSignaturesInOrder(System.out);
    program.printAllBoundsInOrder(System.out);
  }
}
