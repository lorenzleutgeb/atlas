#! /usr/bin/env bash

# NOTE: Before running this, set a reasonable time limit for Z3!

OUT=json-out-night2
JAR=./build/libs/atlas-v0.0.2-37-g95a98dc-dirty-shadow.jar
FUNCTIONS=(SplayTree.splay SplayTree.splay_max SplayHeap.insert SplayHeap.del_min PairingHeap.merge_pairs_isolated PairingHeap.insert PairingHeap.merge)
RESOURCES="./src/test/resources"

# TODO Prover:
#  - How many times was (w) applied.

for INFER_OR_CHECK in 'infer'
do
	for TACTICS in 'false'
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
