#! /usr/bin/env bash

set -euo pipefail

function atlas_run () {
	(set -x ;atlas --home src/test/resources/examples \
	  run --tactics src/test/resources/tactics \
	  $@ )
}


echo -e '\nSplayTree (inference)\n'
atlas_run --infer "SplayTree\\.(splay(_max)?|insert|delete)"

echo -e '\nSplayHeap (inference)\n'
atlas_run --infer "SplayHeap\\.(insert|del_min)"

echo -e '\nPairingHeap EXCEPT merge (inference)\n'
atlas_run --infer "PairingHeap\\.(insert|merge_pairs|del_min_via_merge_pairs)_isolated"

echo -e '\nPairingHeap.merge (checking)\n'
atlas_run "PairingHeap\\.merge_isolated"

echo -e '\nPairingHeap WITH merge (inference)\n'
atlas_run --infer "PairingHeap\\.(insert|merge_pairs|del_min_via_merge_pairs|merge)_isolated"
