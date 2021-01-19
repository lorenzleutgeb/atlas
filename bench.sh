#! /usr/bin/env bash

# NOTE: Before running this, set a reasonable time limit for Z3!

OUT=json-out-night
JAR=./build/libs/lac-v0.0.2-17-g2847b1e-dirty-shadow.jar
FUNCTIONS=(SplayTree.splay_eq SplayTree.splay_eq_min SplayTree.splay_max_eq SplayHeap.insert SplayHeap.del_min PairingHeap.merge_pairs_isolated PairingHeap.insert PairingHeap.merge PairingHeap.pass1 PairingHeap.pass2)
RESOURCES="./src/test/resources"

# TODO Prover:
#  - How many times was (w) applied.

for INFER_OR_CHECK in 'check' 'infer'
do
	for TACTICS in 'true' 'false'
	do
		for F in "${FUNCTIONS[@]}"
		do
			TACTICS_AS_STRING=$([ "$TACTICS" == "true" ] && echo "with" || echo "without")
			TACTICS_AS_FLAG=$([ "$TACTICS" == "true" ] && echo "--tactics $RESOURCES/tactics" || echo "")
			INFER_AS_FLAG=$([ "$INFER_OR_CHECK" == "infer" ] && echo "--infer" || echo "")

			JSON=$OUT/$F/$TACTICS_AS_STRING/$INFER_OR_CHECK/result.json

			if [ -f $JSON ]
			then
				echo "Skipping $JSON"
			fi

			set -x
			java -jar $JAR \
				--home $RESOURCES/examples \
				run $INFER_AS_FLAG \
				$TACTICS_AS_FLAG \
				--json $JSON \
				"${F//./\\.}"

			set +x
		done
	done
done
