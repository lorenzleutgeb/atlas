# ARTIFACT 

## Important Notice

This is the "README" accompanying the artifact, according to
"Packaging Guidelines", "Artifact Evaluation" website at

  http://i-cav.org/2022/artifact-evaluation/

This file has not been named `README.md` or similar, but `ARTIFACT.md`
instead, to avoid confusion with `README.md` in the source repository
of the tool.

Artifact submission 2 (this artifact) is associated with
the regular paper submission 4.

This artifact applies for all three badges (functional, available, reusable). For
each badge, there is one corresponding section in this file.

## Open Virtual Appliance (OVA) vs. Docker Image

The Docker image is much smaller in size compared to the
Open Virtual Appliance, since it does not contain a graphical desktop.

The Docker image does not include any kind of package manager, and it
is therefore more cumbersome to add install additional software if the
need arises during evaluation. In the OVA version, the [Nix][nix]
package manager is installed, so installation of additional software
is as simple as

    nix-env -iA nixos.hello

Refer to <https://search.nixos.org/> to search for packages.

## Functional

Please make sure to first read the section "Using" of `README.md` for general
remarks on operating the tool, then return to this section which addresses
reproduction of results.

To verify the results in the associated paper, you will need the "run"
subcommand

    atlas run --help

For example, to check the type annotation of the function definition
`PairingHeap.link`, run

    atlas run PairingHeap.link

Note that the last positional parameter in the previous invocation is
interpreted as a "fully qualified name" of a function definition.
It is possible to pass multiple such names.
This way, resource annotations for multiple functions can be inferred/checked
together, even if their definitions do not depend on each other.

To enable type inference, pass `--infer`:

    atlas run --infer PairingHeap.link

There is another subcommand, which is helpful to understand how definitions are
translated before constraints are generated and tactics applied:

    atlas lnf --out=mydir

will print function definitions in let-normal-form to the directory passed as
`out` argument. This directory must exist and be writable. This command works
without considering resource annotations, so they will not be printed to the
resulting file.

To speed up resource annotation inference/checking, tactics can be used.
The artifact contains some tactics in

    $HOME/atlas/src/test/resources/tactics/<module-name>/<function-name>.txt

To enable them, use the `run` subcommand with the `--tactics` parameter (see
below).

### Results Reported in the Associated Paper

> Document in detail how to reproduce the experimental results of the paper
> using the artifact; [...]

#### Table 1

#### Table 2


> [...]; keep this process simple through easy-to-use scripts and
> provide detailed documentation assuming minimum expertise.

The script at `$HOME/atlas/reproduce.sh` will run all above commands in sequence.

### Source Code

> When possible include source code within your virtual machine image and point
> to the most relevant and interesting parts of the source code tree.

Please refer to `$HOME/atlas/src/main/`, and "Highlighted Files" in `README.md`.

### Log Files

> We encourage the authors to include log files that were produced by their
> tools, and point to the relevant log files in the artifact description.

We do not provide log files, but would like to note that by default, the tool
will append logs to `$PWD/atlas.log` (`$PWD` stands for the current working directory).

Verbosity of logging can be adjusted by setting properties in
`atlas.properties`. The file contains some helpful comments and references to
online documentation for property names and possible values.

## Available

> To get the available badge, please upload your VM to a repository that
> provides a DOI, such as Zenodo, figshare, or Dryad and use this DOI link
> in your artifact submission.

This artifact was submitted via a DOI issued by Zenodo.

## Reusable

> Ensure that your tool is usable independently of your artifact VM or
> container, by making your source code and a set of representative experiments
> available on a public platform (personal website, code-hosting platform…),
> under a license that allows reuse.

The source code for this artifact is publicly available at

  https://github.com/lorenzleutgeb/atlas

and input files and representative experiments are publicly available at

  https://github.com/lorenzleutgeb/atlas-examples

These two repositories are split on purpose, so that future and/or competing
implementations may share and collaborate on the input files separated from
this concrete implementation.

The contents of both repositories are licensed to allow re-use. See

  https://github.com/lorenzleutgeb/atlas/blob/main/LICENSE
  https://github.com/lorenzleutgeb/atlas-examples/blob/main/LICENSE

> Your source code release should include detailed documentation
> (setup and usage instructions).

Please refer to

  https://github.com/lorenzleutgeb/atlas/blob/main/README.md

Note that this file is also included in the artifact.

> Ensure that the set-up process for your artifact is reproducible by including
> a script used to build the VM or container (Vagrantfile, Docker script,
> Bash script) that allows users to automatically reproduce your artifact’s
> set-up.  Try to keep this script reasonably simple.

Please refer to

  https://github.com/lorenzleutgeb/atlas/blob/main/README.md

Note that this process is highly reproducible, because it is implemented 
using the Nix package manager. To learn more, please refer to

  https://nixos.org/guides/how-nix-works.html
  https://doi.org/10.1017/S0956796810000195
  https://r13y.com/

> Include instructions for reviewers explaining how to exercise your artifact
> on new inputs; in particular, document what inputs your tool support.

Consider the example inputs at 

  https://github.com/lorenzleutgeb/atlas-examples

The grammar(s) used to generate the parser for the input language can be found
at

    src/main/antlr/**.g4

This programming language is necessarily simple, to allow for easier analysis,
but sufficiently complex to express self-balancing trees and heaps.

One could implement additional functions and define them in a new Module
(i.e. a new `*.ml` file).

It is also possible to provide resource annotations for specific function
definitions. This can also be observed in the examples.

> When applicable, make sure that your tool accepts inputs in standard formats
> (e.g. SMTLIB).

We think that this is not applicable for our *input* of our tool, but indeed our
tool provides SMTLIB as a standard *output* format. The tool prints the output
folder name to standard output upon invocation.

### Usage beyond the Paper

> Ensure that your tool is reusable on inputs beyond those included in the paper
> by describing one experiment beyond those in the paper that a reviewer might
> run (a new program to feed to your compiler, a new problem for your SMT
> solver, etc.)

Our examples contain tests for nonterminating programs. One case for which
nontermination can be detected, is `Infinite.infinite_18`. Run it with

    atlas run --infer Infinite.infinite_18

#### Results reported at CAV 2021

See <https://doi.org/10.1007/978-3-030-81688-9_5>

To verify the results presented in Table 1 on page 3 of the associated paper,
run the following commands (and remember, that `--home` is omitted).

For the first group of four lines (`SplayTree`):

    atlas run --tactics [...] --infer \
        SplayTree.splay SplayTree.splay_max SplayTree.insert SplayTree.delete

For the second group of three lines (`SplayHeap`):

    atlas run --tactics [...] --infer SplayHeap.insert Splayheap.del_min

For the third group of four lines (`PairingHeap`) **EXCEPT** `PairingHeap.merge`:

    atlas run --tactics [...] --infer \
        PairingHeap.insert_isolated Pairingheap.merge_pairs_isolated \
	PairingHeap.del_min_via_merge_pairs_isolated PairingHeap.merge_isolated

#### Checking other annotations

To check a custom resource annotation, you may define a new function (or copy
an example definition) and will have to edit the corresponding `*.ml`
file. The program will print the source of all definitions that are loaded via
the `run` subcommand.

#### The Search Path

The tool will look for modules (which in turn contain function definitions)
within a directory called the "search path". This concept is very similar
to the "search path" of the Glasgow Haskell Compiler. One notable difference
is that ATLAS accepts exacly one directory as search path, while GHC accepts
a set of directories to form the search path.

The search path can be set using the `--search` argument:

    atlas --search=$HOME/atlas/src/test/resources/examples run PairingHeap.link

Note that the `--search` parameter corresponds to the module search directory, so
the full path to the module `PairingHeap` would in this case correspond to

    $HOME/atlas/src/test/resources/examples/PairingHeap.ml

In the artifact, the search path is preset via the environment variable
`$ATLAS_SEARCH` and there is no need to modify its value to reproduce our
results. However, it is possible to create new modules in a different directory,
in which case changing it would be required.

### Translating Signatures to Bounds

We will describe the process in detail and take `PairingHeap.link` as an
example:

    PairingHeap.link ∷ Tree α → Tree α | [[0 ↦ 1, (0 2) ↦ 2, (1 0) ↦ 2] → [0 ↦ 1, (0 2) ↦ 2, (1 0) ↦ 1], {[(1 0) ↦ 2] → [(1 0) ↦ 2], [] → [], [(1 1) ↦ 1] → [(1 0) ↦ 1]}]

We drop the function name, and the "simple" part of the type:

    [[0 ↦ 1, (0 2) ↦ 2, (1 0) ↦ 2] → [0 ↦ 1, (0 2) ↦ 2, (1 0) ↦ 1], {[(1 0) ↦ 2] → [(1 0) ↦ 2], [] → [], [(1 1) ↦ 1] → [(1 0) ↦ 1]}]

What is left now is a sequence with two elements. The first element is the
annotation for `PairingHeap.link` with cost and the second element is the
annotation for `PairingHeap.link` without cost ("cost free").

Since we are interested in the amortised cost of `PairingHeap.link`, we focus
on the first element of the sequence:

    [0 ↦ 1, (0 2) ↦ 2, (1 0) ↦ 2] → [0 ↦ 1, (0 2) ↦ 2, (1 0) ↦ 1]

We translate this short notation into two resource functions, over terms
familiar from the table in our paper:

    Q(h) = rk(h) + 2 \log(2) + 2 \log(|h|)
    P(h) = rk(h) + 2 \log(2) + 1 \log(|h|)

Note that $Q(h)$ annotates the argument and $P(h)$ annotates the result of `PairingHeap.link`.
According to the design of our type system, $P(h)$ can be directly used as potential, and it relates to amortised cost and actual cost as follows:

    c_{amortised}(link) = \lambda h . c_{actual}(link)(h) + P(link(h)) - P(h)

And further, according to the soundness of our type system we have

    Q(h) \geq P(link(h)) + c_{actual}(link)(h)

To bound amortised cost, we take the difference of $Q$ and $P$:

    D(h) = Q(h) - P(h) \geq P(link(h)) + c_{actual}(link)(h) - P(h) = c_{amortised}(link)(h)

Thus, the upper bound for amortised cost of `PairingHeap.link` that is represented by the above result, is

    \log(|h|)

## Resource Usage and Limits

The artifact imposes following resource limits on Z3:

  Wall clock runtime: 2H
  (see property `com.microsoft.z3.timeout` in `atlas.properties`)

  Limit for memory: 24GiB
  (see property `com.microsoft.z3.memory_max_size` in `atlas.properties`)

These (and other) parameters as well as logging configuration can be changed
in `atlas.properties`.

All results were computed on a machine with an Intel Xeon W-1290P 64GiB
processor and 64GiB main memory, and computations took on the order of less
than a second up to fifteen minutes.

## Z3 Statistics

These can be checked by inspecting `out/2d…/sat/z3-statistics.txt`, where
the ellipsis means some other hexadecimal digits. At the moment there is no
mapping between arguments passed to the artifact on invocation and the name of
the output directory, so you will have to use an indirect method, for example,
check when the output directory was changed.

The contents of `z3-statistics.txt` is a dump of the statistics that Z3
provides, most notably `max_memory`.

[nix]: https://nixos.org/nix
