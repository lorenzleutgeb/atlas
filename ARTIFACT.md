# ATLAS

## Important Notice

This is the "readme" accompanying the artifact, according to "Submitting your
artifact", item 1.2. of the "Artifact Evaluation" website at

  http://i-cav.org/2021/artifact-evaluation/

This file has been named not been named `README.md` or similar, but `ARTIFACT.md`
instead, to avoid confusion with the `README.md` file in the source repository
of the artifact.

This artifact submitted as #329 for "Artifact Evaluation", is associated with
the paper submission as #294 for "Main track".

This artifact aims for all three badges (functional, available, reusable). For
each badge, there is one corresponding section in this file.

## Open Virtual Appliance (OVA) vs. Docker Image

The Docker image does not include any kind of package manager. It is therefore
much more cumbersome to add additional software if the need arises during
evaluation.
Note that in the OVA, the [Nix][nix] package manager is installed, so
installation is as simple as

    nix-env -iA nixos.hello

The Docker image is much smaller in size, because it does not contain a
graphical desktop.

## Functional

The art

# Artifact

## Contents of `artifact`

## Invoking the Artifact

In a Bash-compatible shell, run

    cd artifact

to change into the artifact directory. Paths below are relative to this folder.
Then run

    ./atlas --help

To get a help text. To verify the results in the
accompanying paper, you will need the "run"
subcommand

    ./atlas run --help

For example, to check the type annotation of the function definition
`PairingHeap.link`, run

    ./atlas run --home=resources/examples "PairingHeap.link"

Note that the `--home` parameter corresponds to the module search directory, so
the module `PairingHeap` corresponds to the file

    resources/examples/PairingHeap.ml

Note that the last positional parameter in the previous invocation is
interpreted as a regular expression.
This way, resource annotations for multiple functions can be inferred/checked
together, even if their definitions do not depend on each other. An extreme,
unrealistic invocation is

    ./atlas run --home=resources/examples '.*'

Which will attempt to infer resource annotations for all function definitions.
However, the artifact also contains some non-terminating definitions
(see `Infinite.ml`) and checking all at once will require a lot of time/memory.

To infer resource annotations with coefficients defined over the rational
numbers, use `--rational`, like so

    ./atlas run --home=resources/examples --infer --rational 'PairingHeap.link'

To check a resource annotation, you will have to edit the corresponding `*.ml`
file. The program will print the source of all definitions that are loaded via
the `run` subcommand.

There is another subcommand, which is helpful to understand how definitions are
translated before constraints are generated and tactics applied:

    ./atlas lnf --home=resources/examples --out=mydir

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

    ./atlas run --home resources/examples --tactics resources/tactics "SplayTree\\.splay(_max)?_eq"
    ./atlas run --home resources/examples --tactics resources/tactics "SplayTree\\.splay_eq_min"
    ./atlas run --home resources/examples --tactics resources/tactics "SplayHeap\\.(insert,del_min)"
    ./atlas run --home resources/examples --tactics resources/tactics "PairingHeap\\.(insert|merge_pairs_isolated)"
    ./atlas run --home resources/examples --tactics resources/tactics "PairingHeap\\.pass(1|2)"

These correspond to the following lines/names in the paper (line by line):

    `SplayTree.splay` (fist line) and `SplayTree.splay_max` (third line)
    `SplayTree.splay` minimised (second line)
    `SplayHeap.insert` (fourth line) and `SplayHeap.del_min` (fifth line)
    `PairingHeap.merge_pairs`, `PairingHeap.insert` and `PairingHeap.merge` (sixth, seventh and eighth line)
    `PairingHeap.pass1` and `PairingHeap.pass2` (ninth and tenth line)

> Document in detail how to reproduce the experimental results of the paper
> using the artifact; keep this process simple through easy-to-use scripts and
> provide detailed documentation assuming minimum expertise.



> Ensure the artifact is in the state ready to run. It should work without a network connection. It should not require the user to install additional software before running, that is, all required packages should be installed on the provided virtual machine.

> The artifact should use reasonably modest resources (RAM, number of cores), so that the results can be reproduced on various hardware platforms including laptops. The evaluation should take a reasonable amount of time to complete (no more than 3 to 5 hours). If this is not the case for your benchmarks, make sure to also include simpler benchmarks that do not require a long running time. If this is not possible, please contact PC and AEC chairs as soon as possible.

> When possible include source code within your virtual machine image and point to the most relevant and interesting parts of the source code tree.

> We encourage the authors to include log files that were produced by their tools, and point to the relevant log files in the artifact description.

## Available

This artifact was submitted via Zenodo.

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
implementations may share and collaborate on the input files in separated from
this concrete implementation.

The contents of both repositories are licensed to allow re-use. See

  https://github.com/lorenzleutgeb/atlas/blob/main/LICENSE
TODO
  https://github.com/lorenzleutgeb/atlas-examples/blob/main/LICENSE

> Your source code release should include detailed documentation
> (setup and usage instructions).

Please refer to

  https://github.com/lorenzleutgeb/atlas/blob/main/README.md

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

TODO

> When applicable, make sure that your tool accepts inputs in standard formats
> (e.g. SMTLIB).

We think that this is not applicable for our *input* of our tool, but indeed our
tool provides SMTLIB as a standard *output* format.

TODO

> Ensure that your tool is reusable on inputs beyond those included in the paper
> by describing one experiment beyond those in the paper that a reviewer might
> run (a new program to feed to your compiler, a new problem for your SMT
> solver, etc.)

TODO


## Resource Limits

The artifact imposes following resource limits on Z3:

  Wall clock runtime: 15 minutes
  Limit for memory: 24GiB

These (and other) parameters as well as logging configuration can be changed in `atlas.properties`.

All results were computed on a machine with 32GiB main memory, and computations took on the order of
less than a second up to fifteen minutes.


# Response

We have no response for reviews 2 and 3 because there were no questions raised.
A response regarding review 1 (with inline quotes) follows.

We thank all reviewers for their efforts, which will certainly result in improving the quality of the artifact and our work.

---

Regarding your item (1):

Please consider the following list of invocations which correspond directly to
lines in the result table from our paper:

    ./lac run --home resources/examples --tactics resources/tactics "SplayTree\\.splay_eq"
    ./lac run --home resources/examples --tactics resources/tactics "SplayTree\\.splay_eq_min"
    ./lac run --home resources/examples --tactics resources/tactics "SplayTree\\.splay_max_eq"
    ./lac run --home resources/examples --tactics resources/tactics "SplayHeap\\.insert"
    ./lac run --home resources/examples --tactics resources/tactics "SplayHeap\\.del_min"
    ./lac run --home resources/examples --tactics resources/tactics "PairingHeap\\.merge_pairs_isolated"
    ./lac run --home resources/examples --tactics resources/tactics "PairingHeap\\.insert"
    ./lac run --home resources/examples --tactics resources/tactics "PairingHeap\\.merge"
    ./lac run --home resources/examples --tactics resources/tactics "PairingHeap\\.pass1"
    ./lac run --home resources/examples --tactics resources/tactics "PairingHeap\\.pass2"

Regarding your remark:

> The current explanation about which command maps to which line in the results table is confusing primarily because it is not clear how to get multiple lines of results from one command.

We would like to clarify: If the artifact is (a) invoked with a function
definition name of a function definition that depends on other function
definitions (by application), or (b) invoked with a regular expression that
matches multiple function definition names, then the output will contain
multiple annotated function types. This is the case also with the output that
you quote in your review as part of item (3), which we will address below.
We reproduce your output here:

~~~
PairingHeap.link ∷ Tree α → Tree α | [[0 ↦ 1, (0 2) ↦ 2, (1 0) ↦ 2] → [0 ↦ 1, (0 2) ↦ 2, (1 0) ↦ 1], {[(1 0) ↦ 2] → [(1 0) ↦ 2], [] → [], [(1 1) ↦ 1] → [(1 0) ↦ 1]}]
PairingHeap.pass2 ∷ Tree α → Tree α | [[0 ↦ 3, (0 2) ↦ 1, (1 0) ↦ 4] → [0 ↦ 1, (0 2) ↦ 1, (1 0) ↦ 1], {[(1 0) ↦ 2] → [(1 0) ↦ 2], [] → [], [(1 0) ↦ 2, (1 1) ↦ 2, (1 2) ↦ 2] → [(1 0) ↦ 2]}]
PairingHeap.pass1 ∷ Tree α → Tree α | [[0 ↦ 3, (0 2) ↦ 1, (1 0) ↦ 2] → [0 ↦ 1, (0 2) ↦ 1, (1 0) ↦ 1], {[(1 0) ↦ 2] → [(1 0) ↦ 2], [] → [], [(1 0) ↦ 2, (1 1) ↦ 2, (1 2) ↦ 2] → [(1 0) ↦ 2], [(1 0) ↦ 1] → [(1 0) ↦ 1]}]
~~~

Consider this output, and that it represents a result for three functions, which
you probably obtained by just one invocation of the artifact:

    ./lac run --home resources/examples --tactics resources/tactics "PairingHeap\\.pass(1|2)"

Regarding your remark:

> Some naming is confusing too, e.g., are `PairingHeap.merge_pairs` and `PairingHeap.merge` the same thing because the only command that includes the word merge is this one: `…`

We would like to clarify: No, PairingHeap.merge and PairingHeap.merge_pairs
are two different functions. Please see the file `resources/examples/PairingHeap.ml` in which both are
defined (starting on lines 169 and 212 respectively).
The command that you mentioned, however, will also produce the result for
`PairingHeap.merge`. It will produce the results for `PairingHeap.merge_pairs`
and `PairingHeap.insert` because these two names are matched by the regular
expression directly. Furthermore, since `PairingHeap.insert` calls
`PairingHeap.merge`, the result for this function will also be produced even
though its name is not explicitly referenced in the invocation of the artifact.
We agree, however, that we should have made this more explicit in the `Readme` to accompany
the artifact. Please consider the more explicit list of invocations which we
have given above, which mentions both `PairingHeap.merge` and
`PairingHeap.merge_pairs_isolated`.

Regarding your remark:

> Should I run the commands provided directly as written and the example below should print 3 results? Then I would read the 1st result as the 6th line of the table and the 2nd result as the 7th line and the 3rd result as the 8th result of the table?

Yes, this was our intention. And further, regarding

> Or should I split the command up, e.g., instead of the command above should I run:
> `…`
> `…`

We did not intend you to do this, when we wrote `Readme.txt`, but it is one
possible way of leveraging a regular expression that matches multiple function
definition names. It produces the same results.

> Also the table has 12 lines in the paper but no upper bound is computed for lines 4 and 5, so you refer to line 6 as line 4 and line 7 as line 5 etc.

Yes, we are mistaken here. Thank you for pointing this out.

> Is `(insert,del_min)` a typo, and should it be written `(insert|del_min)`?

Yes, this is a typo and it should indeed be written `(insert|del_min)`. It arose from a confusion of shell globbing patterns and regular expressions. Thank
you for pointing this out.


Regarding your item (2):

Actually, none of the instances requires 32GiB memory. We first present resource
usage statistics obtained locally using the artifact:

| Regular Expression        | Z3 max. Memory Usage [MiB] |
|---------------------------|----------------------------|
| `PairingHeap\.pass(1\|2)` |                        883 |
| `SplayTree\.splay_eq`     |                       1059 |
| `PairingHeap\.del_min`    |                        655 |

These can be checked by inspecting `out/2d…/sat/z3-statistics.txt`, where
the ellipsis means some other hexadecimal digits. At the moment there is no
mapping between arguments passed to the artifact on invocation and the name of
the output directory, so you will have to use an indirect method, for example,
check when the output directory was changed.

The contents of `z3-statistics.txt` is a dump of the statistics that Z3
provides, most notably the `max_memory`.


Regarding your item (3):

We will describe the process in detail. Before we address `PairingHeap.pass1`
and `PairingHeap.pass2` (these results are more complicated because they refer
to potential differences, and not amortised cost, see our remark in the paper),
we first interpret the output for `PairingHeap.link`.

We start from the output provided in your review:

~~~
PairingHeap.link ∷ Tree α → Tree α | [[0 ↦ 1, (0 2) ↦ 2, (1 0) ↦ 2] → [0 ↦ 1, (0 2) ↦ 2, (1 0) ↦ 1], {[(1 0) ↦ 2] → [(1 0) ↦ 2], [] → [], [(1 1) ↦ 1] → [(1 0) ↦ 1]}]
PairingHeap.pass2 ∷ Tree α → Tree α | [[0 ↦ 3, (0 2) ↦ 1, (1 0) ↦ 4] → [0 ↦ 1, (0 2) ↦ 1, (1 0) ↦ 1], {[(1 0) ↦ 2] → [(1 0) ↦ 2], [] → [], [(1 0) ↦ 2, (1 1) ↦ 2, (1 2) ↦ 2] → [(1 0) ↦ 2]}]
PairingHeap.pass1 ∷ Tree α → Tree α | [[0 ↦ 3, (0 2) ↦ 1, (1 0) ↦ 2] → [0 ↦ 1, (0 2) ↦ 1, (1 0) ↦ 1], {[(1 0) ↦ 2] → [(1 0) ↦ 2], [] → [], [(1 0) ↦ 2, (1 1) ↦ 2, (1 2) ↦ 2] → [(1 0) ↦ 2], [(1 0) ↦ 1] → [(1 0) ↦ 1]}]
~~~

Since we first focus on `PairingHeap.link` we ignore output lines 2 and 3:

~~~
PairingHeap.link ∷ Tree α → Tree α | [[0 ↦ 1, (0 2) ↦ 2, (1 0) ↦ 2] → [0 ↦ 1, (0 2) ↦ 2, (1 0) ↦ 1], {[(1 0) ↦ 2] → [(1 0) ↦ 2], [] → [], [(1 1) ↦ 1] → [(1 0) ↦ 1]}]
~~~

We drop the function name, and the "simple" part of the type:

~~~
[[0 ↦ 1, (0 2) ↦ 2, (1 0) ↦ 2] → [0 ↦ 1, (0 2) ↦ 2, (1 0) ↦ 1], {[(1 0) ↦ 2] → [(1 0) ↦ 2], [] → [], [(1 1) ↦ 1] → [(1 0) ↦ 1]}]
~~~

What is left now is a sequence with two elements. The first element is the
annotation for `PairingHeap.link` with cost and the second element is the
annotation for `PairingHeap.link` without cost ("cost free").

Since we are interested in the amortised cost of `PairingHeap.link`, we focus
on the first element of the sequence:

~~~
[0 ↦ 1, (0 2) ↦ 2, (1 0) ↦ 2] → [0 ↦ 1, (0 2) ↦ 2, (1 0) ↦ 1]
~~~

We translate this short notation into two resource functions, over terms familiar from the table in our paper:

$$
Q(h) = rk(h) + 2 \log(2) + 2 \log(|h|)
$$
$$
P(h) = rk(h) + 2 \log(2) + 1 \log(|h|)
$$

Note that $Q(h)$ annotates the argument and $P(h)$ annotates the result of `PairingHeap.link`.
According to the design of our type system, $P(h)$ can be directly used as potential, and it relates to amortised cost and actual cost as follows:

$$
c_{amortised}(link) = \lambda h . c_{actual}(link)(h) + P(link(h)) - P(h)
$$

And further, according to the soundness of our type system we have

$$
Q(h) \geq P(link(h)) + c_{actual}(link)(h)
$$

To bound amortised cost, we take the difference of $Q$ and $P$:

$$
D(h) = Q(h) - P(h) \geq P(link(h)) + c_{actual}(link)(h) - P(h) = c_{amortised}(link)(h)
$$

Thus, the upper bound for amortised cost of `PairingHeap.link` that is represented by the above result, is

$$ \log(|h|) $$

Now that we have explained how to interpret a result for amortised cost, we will
consider `PairingHeap.pass1` and `PairingHeap.pass2` which are results about
potential difference. Consider our remark from the paper:

> While we can automatically obtain an upper bound on `pass_1` and `pass_2`,
> the bound is too high.
> However, we can automatically confirm the logarithmic bounds from [Nipkow and Brinkop 2019] on the potential differences for these functions (indicated by $\dagger$).

The first two steps are the same. We focus on the result for `PairingHeap.pass1`. Therefore, we ignore output lines 1 and 2:

~~~
PairingHeap.pass1 ∷ Tree α → Tree α | [[0 ↦ 3, (0 2) ↦ 1, (1 0) ↦ 2] → [0 ↦ 1, (0 2) ↦ 1, (1 0) ↦ 1], {[(1 0) ↦ 2] → [(1 0) ↦ 2], [] → [], [(1 0) ↦ 2, (1 1) ↦ 2, (1 2) ↦ 2] → [(1 0) ↦ 2], [(1 0) ↦ 1] → [(1 0) ↦ 1]}]
~~~

We drop the function name, and the "simple" part of the type:

~~~
[[0 ↦ 3, (0 2) ↦ 1, (1 0) ↦ 2] → [0 ↦ 1, (0 2) ↦ 1, (1 0) ↦ 1], {[(1 0) ↦ 2] → [(1 0) ↦ 2], [] → [], [(1 0) ↦ 2, (1 1) ↦ 2, (1 2) ↦ 2] → [(1 0) ↦ 2], [(1 0) ↦ 1] → [(1 0) ↦ 1]}]
~~~

Again, what is left now is a sequence with two elements. The first element is the
annotation for `PairingHeap.pass1` with cost and the second element is the
annotation for `PairingHeap.pass1` without cost ("cost free").
Given the note in our paper quoted above, the result with cost is bad (very
high bound, might be addressed by future work). You can verify this yourself
by applying the method we described for `PairingHeap.link` above.
So, what we are interested in here is potential difference, which is encoded
in the second element of the sequence, i.e. the annotation without cost. We
focus:

~~~
{[(1 0) ↦ 2] → [(1 0) ↦ 2], [] → [], [(1 0) ↦ 2, (1 1) ↦ 2, (1 2) ↦ 2] → [(1 0) ↦ 2], [(1 0) ↦ 1] → [(1 0) ↦ 1]}
~~~

From this, we drop the zero annotation. Trivially if the "incoming" potential is
zero and we assign no cost to function calls, then the "outgoing" potential can
be zero.

~~~
{[(1 0) ↦ 2] → [(1 0) ↦ 2], [(1 0) ↦ 2, (1 1) ↦ 2, (1 2) ↦ 2] → [(1 0) ↦ 2], [(1 0) ↦ 1] → [(1 0) ↦ 1]}
~~~

We further drop the two annotations that assign the same potential to "input"
and "output" and omit curly braces.

~~~
[(1 0) ↦ 2, (1 1) ↦ 2, (1 2) ↦ 2] → [(1 0) ↦ 2]
~~~

We repeat the reasoning explained above for the case with cost, and take the difference.

~~~
[(1 1) ↦ 2, (1 2) ↦ 2]
~~~

Again, in terms of the table:

$$
2 \cdot \log(|h| + 1) + 2 \cdot \log(|h| + 2)
$$

Simplify to get the same form as in the table:

$$
2 \cdot (\log(|h| + 1) + \log(|h| + 2))
$$

# Review 1

## Final Review
```
(weak accept)

Thank you for the detailed response to the initial check. It would be great if some of that explanation could be included in the artifact. 

I'm still not sure whether I should have had the error message about the unsatisfiable constraint system when running this command:

./lac run --home resources/examples --tactics resources/tactics "SplayTree\\.splay_eq_min"

I think this is an error and I hope that the authors can fix this in the artifact. 

I've left my score as a weak accept, since I believe all the other results can be reproduced. I also have not carefully followed the steps to convert the terminal output into the same format as the results in the table for all outputs, so I leave my confidence score as a medium. 
```

## Initial Check
```
(weak accept)

I was able to install the dependencies, and run the example command
./lac run --home=resources/examples "PairingHeap.link"

I ran the commands in the README for reproducing the results reported in the TACAS tool paper, and I have the following questions and comments:

1) Please can you provide the individual command used to produce each line of the table, in the order the results appear in the table or clarify better how the commands map to the table lines?

The current explanation about which command maps to which line in the results table is confusing primarily because it is not clear how to get multiple lines of results from one command. Also the table has 12 lines in the paper but no upper bound is computed for lines 4 and 5, so you refer to line 6 as line 4 and line 7 as line 5 etc. Some naming is confusing too, e.g., are PairingHeap.merge_pairs and PairingHeap.merge the same thing because the only command that includes the word merge is this one:

./lac run --home resources/examples --tactics resources/tactics "PairingHeap\\.(insert|merge_pairs_isolated)"

I'm not sure whether I get multiple results from running one command or if I need to split the commands up. Should I run the commands provided directly as written and the example below should print 3 results? Then I would read the 1st result as the 6th line of the table and the 2nd result as the 7th line and the 3rd result as the 8th result of the table? Or should I split the command up, e.g., instead of the command above should I run:
./lac run --home resources/examples --tactics resources/tactics "PairingHeap\\.(insert)"
./lac run --home resources/examples --tactics resources/tactics "PairingHeap\\.(merge_pairs_isolated)"

Is (insert,del_min) a typo, and should it be written (insert|del_min)?

2) Some of the commands seem to need more resources than available in the TACAS VM. I note that the results were computed on a machine with 32GiB main memory, but it would be helpful to provide an estimate of the memory actually required since it appears to be large. If 32GiB is required, it is recommended that authors provide a way to produce a subset of results that can be produced on hardware platforms (including laptops) using more modest resources.

3) I would appreciate guidance on how to interpret the output, e.g., how would I produce the 9th and 10th lines of the table from the experimental output which looks like this:
~~~
PairingHeap.link ∷ Tree α → Tree α | [[0 ↦ 1, (0 2) ↦ 2, (1 0) ↦ 2] → [0 ↦ 1, (0 2) ↦ 2, (1 0) ↦ 1], {[(1 0) ↦ 2] → [(1 0) ↦ 2], [] → [], [(1 1) ↦ 1] → [(1 0) ↦ 1]}]
PairingHeap.pass2 ∷ Tree α → Tree α | [[0 ↦ 3, (0 2) ↦ 1, (1 0) ↦ 4] → [0 ↦ 1, (0 2) ↦ 1, (1 0) ↦ 1], {[(1 0) ↦ 2] → [(1 0) ↦ 2], [] → [], [(1 0) ↦ 2, (1 1) ↦ 2, (1 2) ↦ 2] → [(1 0) ↦ 2]}]
PairingHeap.pass1 ∷ Tree α → Tree α | [[0 ↦ 3, (0 2) ↦ 1, (1 0) ↦ 2] → [0 ↦ 1, (0 2) ↦ 1, (1 0) ↦ 1], {[(1 0) ↦ 2] → [(1 0) ↦ 2], [] → [], [(1 0) ↦ 2, (1 1) ↦ 2, (1 2) ↦ 2] → [(1 0) ↦ 2], [(1 0) ↦ 1] → [(1 0) ↦ 1]}]
~~~


For information, the outputs I obtained from running the 5 commands provided are as follows:

Running
./lac run --home resources/examples --tactics resources/tactics "SplayTree\\.splay(_max)?_eq"

Produced the following result:
INFO Done.
INFO Solving constraints...
Killed

Running
./lac run --home resources/examples --tactics resources/tactics "SplayTree\\.splay_eq_min"

Produced:
ERROR Constraint system is unsatisfiable!
Killed

Running
./lac run --home resources/examples --tactics resources/tactics "SplayHeap\\.(insert,del_min)"

Produced:
INFO Output will go to /home/tacas21/sub158/artifact/./out/2d3bd9d9fb81
INFO Generating constraints...
INFO Done.
INFO Solving constraints...
INFO Done. Result(s):


Running:
./lac run --home resources/examples --tactics resources/tactics "PairingHeap\\.(insert|merge_pairs_isolated)"

Produced:
INFO Done.
INFO Solving constraints...
Killed

Running:
./lac run --home resources/examples --tactics resources/tactics "PairingHeap\\.pass(1|2)"

Produced:
PairingHeap.link ∷ Tree α → Tree α | [[0 ↦ 1, (0 2) ↦ 3, (1 0) ↦ 1, (1 2) ↦ 1] → [0 ↦ 1, (0 2) ↦ 2, (1 0) ↦ 1], {[] → [], [(1 0) ↦ 2] → [(1 0) ↦ 2], [(1 0) ↦ 1] → [(1 0) ↦ 1]}]
PairingHeap.pass2 ∷ Tree α → Tree α | [[0 ↦ 3, (0 2) ↦ 1, (1 0) ↦ 4] → [0 ↦ 1, (0 2) ↦ 1, (1 0) ↦ 1], {[] → [], [(1 0) ↦ 2, (1 1) ↦ 2, (1 2) ↦ 2] → [(1 0) ↦ 2], [(1 0) ↦ 2] → [(1 0) ↦ 2]}]
PairingHeap.pass1 ∷ Tree α → Tree α | [[0 ↦ 3, (0 2) ↦ 1, (1 0) ↦ 2] → [0 ↦ 1, (0 2) ↦ 1, (1 0) ↦ 1], {[(1 0) ↦ 1] → [(1 0) ↦ 1], [(1 0) ↦ 2] → [(1 0) ↦ 2], [(1 0) ↦ 2, (1 1) ↦ 2, (1 2) ↦ 2] → [(1 0) ↦ 2], [] → []}]
```

# Review 2

Note that this review went from "accept" to "weak accept".

```
(weak accept)

I could successfully check, infer, and annotate-and-check resource
annotations. Moreover I could run the tactics that helped in these
tasks. The instructions were clear and easy to follow.

What was not really clear, as the author's response shows, was how to interpret
the results and thus be able to thoroughly check the reproducibility of
Figure 8. The lack of a good presentation for the output is the biggest drawback
of the artifact.

Another aspect to improve would be to point out the cases where tactics are
helpful. For example one can cleary see their benefit on

  "PairingHeap\\.(insert|merge_pairs_isolated)"

but not on, say,

   "PairingHeap.merge".

Finally, I'm confident that the other experiments could be reproduced given the necessary
time and exact instructions as how to do so.

```

# Review 3

## Final Review
```
(accept)

I was able to produce all the entries in figure 8 from the paper except the "SplayTree.splay minimised solution" one. It gives an error that the core is unsatisfiable. Here is the output that I got:

--------------------------------
..
..
[1, 0] <= [1, 1]
[1, 0] <= [1, 2]
[1, 1] <= [1, 2]
INFO Done.
INFO Solving constraints...
ERROR Constraint system is unsatisfiable!
INFO Unsatisfiable core (raw from Z3):
(assert (let ((a!1 (= (to_real |b1fd8a6449b8f60d7c1ff5341d383973438f1e428ec8eeeaf247b57cd942d568.sum[2]|) (+ (/ 1.0 2.0) (to_real (- |b1fd8a6449b8f60d7c1ff5341d383973438f1e428ec8eeeaf247b57cd942d568.f[1]|)) (to_real |b1fd8a6449b8f60d7c1ff5341d383973438f1e428ec8eeeaf247b57cd942d568.f[6]|) (to_real |b1fd8a6449b8f60d7c1ff5341d383973438f1e428ec8eeeaf247b57cd942d568.f[7]|) (to_real |b1fd8a6449b8f60d7c1ff5341d383973438f1e428ec8eeeaf247b57cd942d568.f[9]|))))) (=> |0353929749: (w 797 ≤ 60) b1fd8a6449b8f60d7c1ff5341d383973438f1e428ec8eeeaf247b57cd942d568.sum[2] = Σ... + q[2] (farkas)| a!1)))
INFO Done. Result(s): 
INFO UNSAT
---------------------------------

I believe this is due to some small error which can be fixed easily. 

Also, the authors should have written down the conversion of output to human readable form in the readme file. It was very helpful that it was in the rebuttal. The authors could've also translated it directly into something readable, it was very cryptic. 

Apart from these minor things, I believe the tool works as intended and suggest acceptance.

```
