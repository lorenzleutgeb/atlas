#! /usr/bin/env bash

FUNCTIONS=(SplayTree.splay_eq SplayTree.splay_eq_min SplayTree.splay_max_eq SplayHeap.insert SplayHeap.del_min PairingHeap.insert PairingHeap.merge_pairs_isolated PairingHeap.insert PairingHeap.merge PairingHeap.pass1 PairingHeap.pass2)

for INFER_OR_CHECK in 'check' 'infer'
do
	for TACTICS in 'true' 'false'
	do
		for F in "${FUNCTIONS[@]}"
		do
			TACTICS_AS_STRING=$([ "$TACTICS" == "true" ] && echo "with" || echo "without")
			JSON=json-out/$F/$TACTICS_AS_STRING/$INFER_OR_CHECK/result.json

			if [ ! -f $JSON ]
			then
				echo "$JSON"
			fi
		done
	done
done
