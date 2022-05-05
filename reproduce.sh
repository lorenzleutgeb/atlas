#! /usr/bin/env bash

#set -euo pipefail

export ATLAS_SEARCH=${ATLAS_SEARCH:-$PWD/src/test/resources/examples}

cat <<EOM
This script will automatically reproduce all results from

  Automated Expected Amortised Cost Analysis of Probabilistic Data Structures
  Lorenz Leutgeb, Georg Moser, Florian Zuleger
  CAV 2022
  (to appear, submitted as regular paper #4)

and its predecessor

  ATLAS: Automated Amortised Complexity Analysis of Self-adjusting Data Structures
  Lorenz Leutgeb, Georg Moser, Florian Zuleger
  CAV 2021
  https://doi.org/10.1007/978-3-030-81688-9_5

The order of reproduction is as follows:
 - CAV 2022
   * Introductory Example (Section 2.1)
   * Table 1 (in reverse order)
   * Table 2
 - CAV 2021
   * Table 1

By default, logs will be written to 'atlas.log'.

ATLAS implements both
  1. Type Checking
     Given a function definition and an annotation,
     attempt to check whether the annotation
     well-types the function definition.
     (decision problem)
  2. Type Inference
     Given a function definition,
     attempt to infer an annotation which well-types
     the function definition, preferring small bounds on
     expected amortised cost.
     (optimisation problem)

EOM

ARGS=$@

while true; do
    read -p "Do you wish to enable type inference? " YN
    case $YN in
        [Yy]* ) ARGS="$ARGS --infer"; break 2;;
        [Nn]* ) break 2;;
        * ) echo "Please answer yes or no.";;
    esac
done

function say {
  echo -e $@
}

function run {
  (set -x ; atlas run $ARGS $@)
}

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
run PairingHeap.insert_isolated PairingHeap.merge_pairs_isolated PairingHeap.delete_min_via_merge_merge_pairs_isolated PairingHeap.merge_isolated
