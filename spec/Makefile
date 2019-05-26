## Default target: run ott, and typeset the spec.
default: ott ps pdf coq-def
old_default: ott ps pdf
## Run ott, and compile the spec in proof assistants (Coq, HOL and Isabelle).
## Build everything. Requires Coq, Isabelle, and HOL.
## See hol/README for HOL version requirement.
world: all hol-proof

#### Options for ott hackers ####
## Run `make separate_ott_runs=1' to run a separate ott process for
## each output languages (slower, but sometimes useful in case of
## language-specific bugs in ott).
separate_ott_runs =

#### General targets ####
ott: splay.tex splay.v
ps: splay.ps
pdf: splay.pdf
coq-def: splay.vo
coq: coq-def

#### Directories ####
############ CHANGE topdir TO REFER TO YOUR TOP LEVEL OTT INSTALLATION ########
topdir = /home/lorenz/src/gh/ott-lang/ott
#../..
#/home/so294/scratch/ott_distro_0.10.13
ott_coq_lib_dir = $(topdir)/coq
poly_lib = /Users/so294/polyml.5.1/lib

#### Programs and their arguments ####
COQC = coqc
#COQ_INCLUDE = -I $(ott_coq_lib_dir)
COQ_INCLUDE = -R $(ott_coq_lib_dir) Ott
COQ_FLAGS =
DVIPS = dvips
DVIPSFLAGS = -Ppdf -j0 -G0
LATEX = latex
OTT = $(topdir)/src/ott
#OTT = $(topdir)/bin/ott
OTT_COMMON_FLAGS = -pp_grammar -tex_show_categories true -coq_expand_list_types false -isa_generate_lemmas false
## Run `make OTT_VERBOSITY_FLAGS= target' to cause ott to spew out a
## lot of debugging output.
OTT_VERBOSITY_FLAGS = -show_defns false  -tex_show_categories true
#-show_post_sort false
OTT_FLAGS = -no_rbcatn true $(OTT_COMMON_FLAGS) $(OTT_VERBOSITY_FLAGS)
# -lem_debug true
PS2PDF = ps2pdf

#### Preprocessing ####
splay_sources = syntax typing
# not working yet: reduction
splay_combinations = \
	splay_plain splay_module splay_module_typedef splay_typedef

## Extract the desired language fragment
splay_plain_%.ott: %.ott
	cat $< >$@.tmp
	mv -f $@.tmp $@
splay_module_%.ott: %.ott
	sed -e 's/\(\(%[^%]\)*\)%m/\1/' $< >$@.tmp
	mv -f $@.tmp $@
splay_typedef_%.ott: %.ott
	sed -e 's/\(\(%[^%]\)*\)%d/\1/' $< >$@.tmp
	mv -f $@.tmp $@
splay_module_typedef_%.ott: %.ott
	sed -e 's/\(\(%[^%]\)*\)%d/\1/' -e 's/\(\(%[^%]\)*\)%m/\1/' $< >$@.tmp
	mv -f $@.tmp $@

## Multiple extraction targets
splay_%.ott: splay_%_syntax.ott # splay_%_typing.ott splay_%_reduction.ott ;

#### Ott ####
splay.rawtex splay.v: \
  splay.ott $(OTT) # splay%_typing.ott splay%_reduction.ott $(OTT)
	# Note: no protection against interruptions for hol and isabelle,
	# because ott requires a file name ending in Script.sml.
	$(OTT) $(OTT_FLAGS)  -o splay.tmp.tex -o splay.v splay.ott
	mv -f splay.tmp.tex splay.rawtex

splay%.grammar: splay%_syntax.ott $(OTT) # splay%_typing.ott splay%_reduction.ott $(OTT)
	$(OTT) $(OTT_FLAGS) -writesys $@.tmp $(@:.grammar=)_syntax.ott # $(@:.grammar=)_typing.ott $(@:.grammar=)_reduction.ott
	mv -f $@.tmp $@

$(OTT): %:
#	cd $(@D) && $(MAKE) $(@F)

## Generate all combinations
splay_combinations.dvi: $(splay_combinations:=.dvi) ;
splay_combinations.pdf: $(splay_combinations:=.pdf) ;
splay_combinations.ps: $(splay_combinations:=.ps) ;
splay_combinations.tex: $(splay_combinations:=.tex) ;
splay_combinations.v: $(splay_combinations:=.v) ;

#### TeX ####
%.tex: %.rawtex ott-preamble.sed
	sed -f ott-preamble.sed <$< >$@.tmp
	mv -f $@.tmp $@

.SUFFIXES: .tex .dvi .ps .pdf
.tex.dvi:
	$(LATEX) $<
	$(LATEX) $<
	$(LATEX) $<
.dvi.ps:
	$(DVIPS) $(DVIPS_FLAGS) -o $@ $<
.ps.pdf:
	$(PS2PDF) $< $@

#### Coq ####
coq_libs = $(ott_coq_lib_dir)/ott_list
coq-libs: $(coq_libs:=.vo)
$(coq_libs:=.vo): %:
	cd $(@D) && $(MAKE) $(@F)
.SUFFIXES: .v .vo
.v.vo:
	$(COQC) $(COQ_INCLUDE) $(COQ_FLAGS) $<

#### Cleanup ####
clean:
	rm -f *.grammar *.rawtex
	rm -f *.aux *.dvi *.lof *.log *.lot *.ps *.pdf *.toc
	rm -f *.vo *.ui *.uo *.isa
	rm -f splay.tex splay.v
	rm -f *.tmp tmp.* hol_splay
	rm -rf .HOLMK
	rm -rf *~

#### Dependencies ####
## Ott preprocessed sources
$(splay_combinations:=_syntax.ott):
$(splay_combinations:=_typing.ott):
$(splay_combinations:=_reduction.ott):
## Ott output
$(splay_combinations:=.rawtex): $(OTT)
$(splay_combinations:=.tex): $(OTT)
$(splay_combinations:=.dvi): ott-spec.ltx
$(splay_combinations:=.v): $(OTT)
## Coq
splay_base.vo: splay_base.v splay_typedef.vo
splay_examples_adhoc_1.vo: splay_examples_adhoc_1.v splay_typedef.vo splay_base.vo
