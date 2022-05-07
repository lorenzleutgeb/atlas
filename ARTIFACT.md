# ARTIFACT 

This is the "README" accompanying the artifact, according to
"Packaging Guidelines", "Artifact Evaluation" website at

  http://i-cav.org/2022/artifact-evaluation/

This file has not been named `README.md` or similar, but `ARTIFACT.md`
instead, to avoid confusion with `README.md` in the source repository
of the tool.

Artifact submission 2 (this artifact) is associated with
the regular paper submission 4.

This artifact applies for all three badges (functional, available, reusable).
For each badge, there is one corresponding section in this file.

## Installing Additional Software

The [Nix][nix] package manager comes pre-installed with the Open Virtual Appliance.
Installation of additional software is as simple as

    nix-env -iA nixos.hello

Refer to <https://search.nixos.org/> to search for packages and to
<https://nixos.org/learn.html> for more information on Nix and NixOS.

## Functional

Please make sure to first read the section "Using" of `README.md` for general
remarks on operating the tool, then return to this section which addresses
reproduction of results.

### Results Reported in the Associated Paper

The artifact comes with two scripts to aid evaluation.
  1. The script at `$HOME/atlas/evaluate-faster.sh` will invoke ATLAS
     to verify (by type checking), the results in the paper,
     using proof tactics from `$HOME/atlas/src/test/resources/tactics`
     that were improved manually.
  2. The script at `$HOME/atlas/evaluate-fast.sh` will invoke ATLAS
     to verify (by type checking), the results in the paper.
  3. The script at `$HOME/atlas/evaluate.sh` will run invoke ATLAS
     to recompute (by type inference) the results from the paper,
     using options to speed up type inference (see below).
  4. The script at `$HOME/atlas/evaluate-slow.sh` will run invoke ATLAS
     to recompute (by type inference) the results from the paper,
     without any caveats.

We recommend skimming the source code of all three scripts to gain an
intuitive understanding in how they differ.

They all invoke

  atlas run [--infer] [other options]

which is the most important command of the tool.

We suggest starting with `evaluate-fast.sh` (depending on the size of the instance,
each invocation should take between a few seconds up to 10 minutes).

All results were computed on a machine with an Intel Xeon W-1290P processor
(10 cores, 20 threads, up to 5.2GHz) and 64GiB DDR4-ECC main memory.
We expect that type inference within the virtual machine and on consumer
hardware will be severly degraded, e.g. might have inacceptable runtime
especially for larger instances like with `evaluate.sh` and `evaluate-slow.sh`.

Note that the options turned on for `evaluate.sh` are orthogonal to the use of
improved tactics.

### Type Checking

Verifying the results in the associated paper by type checking is done
by invoking the "run" subcommand.

    atlas run --help

For example, to check the type annotation of the function definition
`PairingHeap.link`, run

    atlas run PairingHeap.link

Note that the last positional parameter in the previous invocation is
interpreted as a "fully qualified name" of a function definition.
It is possible to pass multiple such names.
This way, resource annotations for multiple functions can be inferred/checked
together, even if their definitions do not depend on each other.

### Type Inference

To recompute the results in the associated paper, enable type inference,
by passing `--infer`:

    atlas run --infer PairingHeap.link

Note that type inference is considerably slower than type checking.

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

> Does the artifact have a license which allows reuse,
> repurposing, and is easy to use?

See `LICENSE`.

> Are all dependencies and used libraries well documented and up to date?

See `build.gradle.kts`, where dependencies of the Java codebase are
defined. These dependencies are
["locked" using Gradle Dependency Locking][gradle-dependency-locking],
which means that all dependencies with their concrete version number
are documented in `{buildscript-,}gradle.lockfile`.

The dependencies of the development environment, and of the artifact
(OVA) itself are defined in `flake.nix` and "locked" with Nix,
which means that all dependencies with the concrete repository
and revision in which they are defined are documented in `flake.lock`.

The Gradle builld is wrapped for Nix using `gradle2nix`, which
generates `gradle-env.json` and `gradle-env.json`
(derived from `build.gradle.kts` by executing Gradle), which means
that also all Gradle dependencies are documented with their concrete
URL and hash.

> Does the artifact provide documented interfaces for extensions
> or is the artifact open source?

The artifact is open source. All inputs/benchmarks are open source.

The source code for this artifact is publicly available at

  https://github.com/lorenzleutgeb/atlas

These two repositories are split on purpose, so that future and/or competing
implementations may share and collaborate on the input files separated from
this concrete implementation.

The grammar(s) used to generate the parser for the input language can be found
at

    src/main/antlr/**.g4

This programming language is necessarily simple, to allow for easier analysis,
but sufficiently complex to express self-balancing trees and heaps.

The contents of both repositories are licensed to allow re-use. See

  https://github.com/lorenzleutgeb/atlas/blob/main/LICENSE
  https://github.com/lorenzleutgeb/atlas-examples/blob/main/LICENSE

ATLAS uses Z3 to produce SMTLIB compatible files for the underlying
SMT instances. Check the output folder after execution.

> Can the artifact be used in a different environment (e.g., built on
> another system, used outside of the VM or Docker image, etc.)?

A Docker image build is declared in `flake.nix`, however it is not well
tested (lack of personal resources to test both a Docker image and an OVA).

The tool can be built with Gradle, which is a build tool common in the Java
ecosystem, or using Nix, which in turn wraps the Gradle build tool.

For building instructions, please refer to

  https://github.com/lorenzleutgeb/atlas/blob/main/README.md

Note that this file is also included in the artifact.

Note that the Nix build process is highly reproducible, because it is implemented.
To learn more, please refer to

  https://nixos.org/guides/how-nix-works.html
  https://doi.org/10.1017/S0956796810000195
  https://r13y.com/

One could implement additional functions and define them in a new Module
(i.e. a new `*.ml` file).

It is also possible to provide resource annotations for specific function
definitions. This can also be observed in the examples.

### Usage beyond the Paper

> Ensure that your tool is reusable on inputs beyond those included in the paper
> by describing one experiment beyond those in the paper that a reviewer might
> run (a new program to feed to your compiler, a new problem for your SMT
> solver, etc.)

#### Non-Terminating Examples

Our examples contain tests for non-terminating programs, see the `Infinite` module.

#### Speeding up Type Inference

There are two additional options that restrict the search space,
to consider especially in case the runtime of ATLAS is not acceptable
in the virtual machine.

##### Simple Annotations

The first option, `--simple-annotations=true`, will further restrict the
templates used for annotating function definitions. While the default
technique allows for coefficients like

  q_(1, 1, 0) or q_(0, 1, 2)

which correspond to

  log(|t1| + |t2|) and log(|t2| + 2)

the restriction to "simple annotations" disallows multiple tree-size terms
and the combination of a constant with a tree-size term per logarithm. 
Both above coefficients would be excluded.
Note that the results for "(Randomised) Meldable Heap" and "Coin Search Tree",
as well as for the introductory example, are of this form.

##### Equal Ranks

The second option, `--equal-ranks=true`, will impose the constraint

  q* = q'*    i.e. "the rank coefficients for input and output must be equal"

to all function annotations. Which forces inference of logarithmic
cost only. We do not enable this by default to avoid the implicit assumption
that any function definition for which annotations are to be inferred
has logarithmic cost.

##### Tactics

Tactics can be used in place of ATLAS automatic tactic generation.
This facility is especially helpful for prototyping/extending automatic
generation.

Since the automatic tactic generation has considerably improved since
CAV 2021, the existing tactics might not give signicant acceleration
compared to automatically generated tactics. They might even be outdated.

The artifact contains some tactics in

    $HOME/atlas/src/test/resources/tactics/<module-name>/<function-name>.txt

To enable them, use the `run` subcommand with the `--tactics` parameter (see
below), e.g.

    atlas run --tactics [...] --infer \
        SplayTree.splay SplayTree.splay_max SplayTree.insert SplayTree.delete

#### Further Commands

These subcommands are not as well-maintained, but might still be helpful.

##### Generation of Let-Normal-Form

This subcommand prints function definitions after translation to let-normal-form,
i.e. the form before constraints are generated and tactics applied.

    atlas lnf ...

This command works without considering resource annotations,
so they will not be printed to the resulting file.

##### Generation of Haskell Code

Experimental translation to Haskell.

    atlas hs
##### Generation of Java Code

Experimental translation to Java. Fails if there are pairs/tuples
in the input.

    atlas java

#### Results reported at CAV 2021

  ATLAS: Automated Amortised Complexity Analysis of Self-adjusting Data Structures
  Lorenz Leutgeb, Georg Moser, Florian Zuleger
  CAV 2021
  https://doi.org/10.1007/978-3-030-81688-9_5

The script `reproduce.sh` will try to reproduce the results presented in Table 1
on page 3 of the paper.

#### Checking other annotations

To check a custom resource annotation, you may define a new function (or copy
an example definition) and will have to edit the corresponding `*.ml`
file. The program will print the source of all definitions that are loaded via
the `run` subcommand.

#### Comparing (tick) and (tick:ast)

(tick:ast) can be enabled via `--use-tick-ast=true`.

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

Note that Q(h) annotates the argument and P(h) annotates the result of `PairingHeap.link`.
According to the design of our type system, P(h) can be directly used as potential, and it relates to amortised cost and actual cost as follows:

    c_{amortised}(link) = λ h . c_{actual}(link)(h) + P(link(h)) - P(h)

And further, according to the soundness of our type system we have

    Q(h) ≥ P(link(h)) + c_{actual}(link)(h)

To bound amortised cost, we take the difference of Q and P:

    D(h) = Q(h) - P(h) ≥ P(link(h)) + c_{actual}(link)(h) - P(h) = c_{amortised}(link)(h)

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

## Z3 Statistics

These can be checked by inspecting `out/2d…/sat/z3-statistics.txt`, where
the ellipsis means some other hexadecimal digits. At the moment there is no
mapping between arguments passed to the artifact on invocation and the name of
the output directory, so you will have to use an indirect method, for example,
check when the output directory was changed.

The contents of `z3-statistics.txt` is a dump of the statistics that Z3
provides, most notably `max_memory`.

[nix]: https://nixos.org/nix
[gradle-dependency-locking]: https://docs.gradle.org/current/userguide/dependency_locking.html	