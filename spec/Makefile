## Default target: run ott, and typeset the spec.
default: ott ps pdf
all: default coq-def isa-def
## Run ott, and compile the spec in proof assistants (Coq, HOL and Isabelle).
## Build everything. Requires Coq, Isabelle, and HOL.
## See hol/README for HOL version requirement.
world: all

#### Options for ott hackers ####
## Run `make separate_ott_runs=1' to run a separate ott process for
## each output languages (slower, but sometimes useful in case of
## language-specific bugs in ott).
separate_ott_runs =

#### General targets ####
ott: splay.tex splay.v splay.thy
ps: splay.ps
pdf: splay.pdf
coq-def: splay.vo
isa-def: splay.isa
coq: coq-def
isa: isa-def

#### Directories ####
############ CHANGE topdir TO REFER TO YOUR TOP LEVEL OTT INSTALLATION ########

topdir = TODO
ott_coq_lib_dir = $(topdir)/coq
poly_lib = TODO

#### Programs and their arguments ####
COQC = coqc
#COQ_INCLUDE = -I $(ott_coq_lib_dir)
COQ_INCLUDE = -R $(ott_coq_lib_dir) Ott
COQ_FLAGS =
DVIPS = dvips
DVIPSFLAGS = -Ppdf -j0 -G0
LATEX = latex
OTT = ott
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
splay_%.ott: splay_%_syntax.ott # TODO splay_%_typing.ott splay_%_reduction.ott ;

#### Ott ####
splay%.rawtex: splay%_syntax.ott splay%_typing.ott splay%_reduction.ott $(OTT)
	$(OTT) $(OTT_FLAGS) -o $@ $(@:.tex=)_syntax.ott $(@:.tex=)_typing.ott $(@:.tex=)_reduction.ott
#	mv -f $@.tmp $@

full_splay%.thy: splay%_syntax.ott splay%_typing.ott splay%_reduction.ott $(OTT)
	# Note: no protection against interruptions, because ott requires
	# a file name ending in .thy.
	$(OTT) $(OTT_FLAGS) -o $@ $(@:.thy=)_syntax.ott $(@:.thy=)_typing.ott $(@:.thy=)_reduction.ott

splay.thy: splay.ott $(OTT)
	$(OTT) $(OTT_FLAGS) -o $@ $(@:.thy=).ott

splay%.v: splay%_syntax.ott splay%_typing.ott splay%_reduction.ott $(OTT)
	$(OTT) $(OTT_FLAGS) -coq_expand_list_types false -o $@ $(@:.v=)_syntax.ott $(@:.v=)_typing.ott $(@:.v=)_reduction.ott
#	mv -f $@.tmp $@

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
caml_combinations.thy: $(caml_combinations:=.thy) ;
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

#### Isabelle ####
.SUFFIXES: .thy .isa
.thy.isa:
	$(ISABELLE) $(ISABELLE_FLAGS) -e '$(ISABELLE_WRAPPER)'
	touch $@ # TODO: can Isabelle produce a proof trace of some kind?

#### Cleanup ####
clean:
	rm -f *.grammar *.rawtex
	rm -f *.aux *.dvi *.lof *.log *.lot *.ps *.pdf *.toc
	rm -f *.vo *.ui *.uo *.isa
	rm -f $(caml_combinations:=_syntax.ott) $(caml_combinations:=_typing.ott) $(caml_combinations:=_reduction.ott)
	rm -f $(caml_combinations:=.tex) $(caml_combinations:=Script.sml) $(caml_combinations:=.thy) $(caml_combinations:=.v)
	rm -f *Theory.sig *Theory.sml
	rm -f *.tmp tmp.* hol_caml
	rm -rf .HOLMK
	rm -rf *~

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
$(splay_combinations:=.thy): $(OTT)
## Coq
splay_base.vo: splay_base.v splay_typedef.vo
splay_examples_adhoc_1.vo: splay_examples_adhoc_1.v splay_typedef.vo splay_base.vo
