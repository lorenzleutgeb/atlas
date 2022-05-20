#! /usr/bin/env bash

set -uo pipefail

export ATLAS_SEARCH=${ATLAS_SEARCH:-$PWD/src/test/resources/examples}

ARGS=$@

function now {
	date --iso-8601=seconds
}

function say {
	echo -e $@
}

function run {
	now
	(set -x ; atlas run $ARGS $@)
	now
}

cat <<EOM
Order of reproduction:
 1. CAV 2022
   a. Introductory Example (Section 2.1)
   b. Table 1 (in reverse order)
   c. Table 2
 2. CAV 2021
   a. Table 1
EOM

say '\n\n# CAV 2022'

say '\n## Introductory Example (Section 2.1)'
run RandTree.descend

say '\n## Main Results (Table 1)'
say '\n\nBenchmarks will now be executed by increasing size/analysis time, i.e.'
say '\n\ni.e. looking at Table 1, from bottom to top.'

say '\n### Coin Search Tree'
# NOTE: delete calls delete_max
run CoinSearchTree.insert CoinSearchTree.delete

say '\n### Randomised Meldable Heap'
# NOTE: insert/delete_min calls meld
run RandMeldableHeap.insert RandMeldableHeap.delete_min

say '\n### Randomised Splay Heap'
run RandSplayHeap.insert RandSplayHeap.delete_min

say '\n### Randomised Splay Tree'
run RandSplayTree.insert RandSplayTree.delete RandSplayTree.splay

say '\n## Varying Cost and Probability (Table 2)'

N="1/3 1/2 2/3"
for P in $N
do
	for C in $N
	do
		say "\n### p = ${P}, c = ${C}\n"
		run RandSplayTreeMatrix.P${P//\//}C${C//\//}.splay
	done
done

say '\n----\n\n# CAV 2021'

say '\n# Table 1'

say '\n## Splay Tree'
# NOTE: delete calls splay and splay_max
# NOTE: insert calls splay
run SplayTree.insert SplayTree.delete

say '\n## Splay Heap\n(improved for "insert" and "partition", see paragraph "Deterministic benchmarks" in Sec. 6)'
# NOTE: insert calls partition
run SplayHeap.insert SplayHeap.delete_min

say '\n## Pairing Heap'
run PairingHeap.insert_isolated PairingHeap.merge_pairs_isolated PairingHeap.delete_min_via_merge_pairs_isolated PairingHeap.merge_isolated
