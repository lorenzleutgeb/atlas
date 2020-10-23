# Artifact

## Contents of `artifact`

### `lac`

The presented system compiled as an x86-64 ELF binary.
It will dynamically link to `libc`. Usage of is explained below.

### `dependencies`

Contains the Ubuntu package dependencies. They must be installed before the
artifact can be evaluated. Do this by running

    sudo dpkg -i artifact/dependencies/*.deb

### `resources`

Contains source code of the function definitions to be analyzed
(`*.ml` files) as well as tactics for guided proof.

Contains grammars of the input languages for reference.

## Invoking the Artifact

In a Bash-compatible shell, run

    cd artifact

to change into the artifact directory. Paths below are relative to this folder.
Then run

    ./lac --help

To get a help text. To verify the results in the
accompanying paper, you will need the "run"
subcommand

    ./lac run --help

Create a directory where output can be placed

    mkdir out

For example, to check the type annotation of the function definition
`PairingHeap.link`, run

    ./lac run --home=resources/examples "PairingHeap.link"

Note that the `--home` parameter corresponds to the module search directory, so
the module `PairingHeap` corresponds to the file

    resources/examples/PairingHeap.ml

Note that the last positional parameter in the previous invocation is
interpreted as a regular expression.
This way, resource annotations for multiple functions can be inferred/checked
together, even if their definitions do not depend on each other. An extreme,
unrealistic invocation is

    ./lac run --home=resources/examples '.*'

Which will attempt to infer resource annotations for all function definitions.
However, the artifact also contains some non-terminating definitions
(see `Infinite.ml`) and checking all at once will require a lot of time/memory.

To infer resource annotations with coefficients defined over the rational
numbers, use `--rational`, like so

    ./lac run --home=resources/examples --infer --rational 'PairingHeap.link'

To check a resource annotation, you will have to edit the corresponding `*.ml`
file. The program will print the source of all definitions that are loaded via
the `run` subcommand.

There is another subcommand, which is helpful to understand how definitions are
translated before constraints are generated and tactics applied:

    ./lac lnf --home=resources/examples --out=mydir

will print function definitions in let-normal-form to the directory passed as
`out` argument. This directory must exist and be writable. This command works
without considering resource annotations, so they will not be printed to the
resulting file.

To speed up resource annotation inference/checking, tactics can be used. The
artifact contains some tactics in

    resources/tactics/<module-name>/<function-name>.txt

To enable them, use the `run` subcommand with the `--tactics` parameter.

## Results Reported in the TACAS Tool Paper

The artifact is consistent with the results reported in the TACAS tool paper
(see Fig. 8 on page 16), submitted on 2020-10-23. These results differ slightly
from the results in the paper submitted on 2020-10-16, since we fixed a bug and
added two additional benchmark (`SplayTree.splay_max_eq`, and
`PairingHeap.insert`).

To verify the results presented in the accompanying paper, run the following commands:

    ./lac run --home resources/examples --tactics resources/tactics "SplayTree\\.splay(_max)?_eq"
    ./lac run --home resources/examples --tactics resources/tactics "SplayTree\\.splay_eq_min"
    ./lac run --home resources/examples --tactics resources/tactics "SplayHeap\\.(insert,del_min)"
    ./lac run --home resources/examples --tactics resources/tactics "PairingHeap\\.(insert|merge_pairs_isolated)"
    ./lac run --home resources/examples --tactics resources/tactics "PairingHeap\\.pass(1|2)"

These correspond to the following lines/names in the paper (line by line):

    `SplayTree.splay` (fist line) and `SplayTree.splay_max` (third line)
    `SplayTree.splay` minimised (second line)
    `SplayHeap.insert` (fourth line) and `SplayHeap.del_min` (fifth line)
    `PairingHeap.merge_pairs`, `PairingHeap.insert` and `PairingHeap.merge` (sixth, seventh and eighth line)
    `PairingHeap.pass1` and `PairingHeap.pass2` (ninth and tenth line)

## Resource Limits

The artifact imposes following resource limits:

  Wall clock runtime: 15 minutes
  Limit for Z3 memory: 24GiB

All results were computed on a machine with 32GiB main memory, and computations took on the order of
less than a second up to fifteen minutes.

## Archiving

The authors will not take extra steps to archive the artifact. Its source code is freely available at

    https://github.com/lorenzleutgeb/lac

and is also archived on Zenodo, see

    https://doi.org/10.5281/zenodo.4122492

Note that Zenodo will archive the source of the artifact, not the artifact itself.
