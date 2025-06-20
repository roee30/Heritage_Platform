############################################################################
#                                                                          #
#                     The Sanskrit Heritage Platform                       #
#                                                                          #
#                        Gérard Huet & Pawan Goyal                         #
#                                                                          #
############################################################################
# Makefile of Sanskrit Heritage Software  28-05-2020  Copyright INRIA 2020 #
############################################################################
# Prerequisites: Ocaml 4.07 and Camlp4 preprocessor

# For change of make in all recursive Makefiles
MAKE=make

# ZEN computational linguistics toolkit Ocaml library 
ZEN=../Zen/ML

# Literate programming tool ocamlweb (available from Ocaml hump) J.C. Filliatre
NOWEB=./ocamlweb 

ZENSOURCES=gen.ml list2.ml word.ml share.mli share.ml unshare.ml trie.ml \
mini.ml deco.ml lexmap.ml minimap.mli minimap.ml # zen_lexer.ml

VPATH=$(ZEN) 

# debug by $(MAKE) debug, which calls ocaml -I +camlp4 -I . 
# loading the Pidgin ML syntax by .ocamlinit

# $(MAKE) clean; $(MAKE) depend; $(MAKE) all

# CORE package - utilities, transducers, sanskrit and french lexers, dico spec
CORE=$(DISTR_CORE) paths.ml data.ml
DISTR_CORE = control.ml version.ml date.ml canon.ml min_lexer.ml \
             transduction.ml encode.ml order.ml chunker.ml skt_lexer.ml \
             sanskrit.mli sanskrit.ml test_stamp.ml dir.mli dir.ml

# GRAM package - phonetics, morphology, segmentation and tagging engines
GRAM = index.ml phonetics.ml int_sandhi.ml skt_morph.mli morphology.mli \
       naming.ml inflected.mli inflected.ml sandhi.ml sandhier.ml pada.ml \
       nouns.mli nouns.ml verbs.mli verbs.ml parts.ml conj_infos.mli \
       automaton.ml automata.ml make_automaton.ml make_preverb_automaton.ml \
       automata_vector.mli make_transducers.ml make_xml_data.ml \
       morpho_string.ml morpho.ml declension.ml conjugation.ml indexer.ml \
       indexerd.ml phases.ml lemmatizer.ml auto.mli load_transducers.ml \
       dispatcher.mli dispatcher.ml segmenter.ml load_morphs.ml lexer.mli \
       lexer.ml rank.ml scl_parser.ml reader.ml parser.ml constraints.mli \
       constraints.ml multilingual.ml paraphrase.mli paraphrase.ml \
       bank_lexer.ml checkpoints.ml graph_segmenter.ml \
       interface.mli interface.ml user_aid.ml reset_caches.ml \
       restore_caches.ml params.mli params.ml

# WEB package - HTML, HTTP, CGI utilities for Web interface
WEB = html.ml web.ml css.ml cgi.ml morpho_html.ml chapters.ml morpho_scl.ml \
    mk_index_page.ml mk_grammar_page.ml mk_reader_page.ml mk_sandhi_page.ml \
    mk_corpus_page.ml

# CORPUS package - corpus manager
CORPUS = corpus.mli corpus.ml web_corpus.mli web_corpus.ml		\
 	 corpus_manager.mli corpus_manager.ml corpus_manager_cgi.ml	\
	 save_corpus_params.mli save_corpus_params.ml save_corpus_cgi.ml \
	 mkdir_corpus_params.mli mkdir_corpus_params.ml			\
	 mkdir_corpus_cgi.ml mk_corpus.ml

# extra file SCLpaths.ml for SCL interfacing - not distributed.
# debug statseg.ml

# TREE package - tree bank manager and syntax analyser - legacy
TREE = parse_tree.ml parse_apte.ml stemmer.ml tag_tree.ml tag_apte.ml 

DEBUG=debug.ml 

ENGINE=$(CORE) $(GRAM) $(WEB) $(CORPUS) 
INACTIVE=$(TREE) nyaaya.ml mk_nyaaya_page.ml regression.ml regression_lexer.ml \
statseg.ml

SOURCES=$(ENGINE) $(DEBUG) $(INACTIVE) 

CORENATIVES=list2.cmx gen.cmx paths.cmx version.cmx word.cmx canon.cmx \
min_lexer.cmx phonetics.cmx transduction.cmx encode.cmx skt_lexer.cmx \
chunker.cmx sanskrit.cmx order.cmx data.cmx

ANNOTFLAGS = -annot -bin-annot
LIBS = -I C:/opam/.opam/4.14/lib/uutf -I +camlp4 -I $(ZEN)
COMPILE=ocamlopt -pp "camlp4rf" $(LIBS) -c	$(ANNOTFLAGS)

COMPILEI=ocamlc -pp "camlp4rf" $(LIBS) -c	\
		$(ANNOTFLAGS)

LINK=ocamlopt $(LIBS) dynlink.cmxa camlp4lib.cmxa uutf.cmxa

# standard installation of Sanskrit Heritage platform - assumes ZEN library

all: make_transducers make_xml_data engine sandhi_test

engine: test_version cgis reset_caches restore_caches static_pages mk_corpus 

# reader_plugin parse_apte regression 

make_transducers: $(CORENATIVES) make_transducers.cmx make_automaton.cmx \
make_preverb_automaton.cmx
	$(LINK) $(CORENATIVES) share.cmx trie.cmx mini.cmx deco.cmx \
automata.cmx make_automaton.cmx make_preverb_automaton.cmx \
make_transducers.cmx -o make_transducers

make_xml_data: $(CORENATIVES) date.cmx html.cmx deco.cmx \
make_xml_data.cmx 
	$(LINK) $(CORENATIVES) str.cmxa date.cmx html.cmx deco.cmx \
make_xml_data.cmx -o make_xml_data

# testing consistency of Heritage_resources and Heritage_platform
test_version: paths.cmx gen.cmx version.cmx control.cmx test_stamp.cmx
	$(LINK) paths.cmx gen.cmx version.cmx control.cmx  test_stamp.cmx \
-o test_stamp
        
static_pages: css mk_index_page mk_grammar_page mk_sandhi_page mk_reader_page \
		mk_corpus_page

# legacy in need of re-design
regression: rank.cmx regression.cmx
	$(LINK) $(CORENATIVES) unix.cmxa date.cmx control.cmx deco.cmx \
lexmap.cmx inflected.cmx html.cmx SCLpaths.cmx web.cmx naming.cmx \
morpho_string.cmx morpho.cmx load_transducers.cmx pada.cmx phases.cmx \
dispatcher.cmx chapters.cmx morpho_html.cmx bank_lexer.cmx cgi.cmx \
segmenter.cmx morpho_scl.cmx load_morphs.cmx lexer.cmx constraints.cmx \
rank.cmx regression.cmx -o regression

obsolete: tagger panini

COPYRIGHT: 
	echo "COPYRIGHT is hand-made"

install:
	echo "go back to development directory to do make install"

reset_caches: reset_caches.cmx
	$(LINK) $(CORENATIVES) str.cmxa unix.cmxa trie.cmx deco.cmx html.cmx \
date.cmx SCLpaths.cmx web.cmx reset_caches.cmx -o reset_caches

restore_caches: restore_caches.cmx
	$(LINK) $(CORENATIVES) str.cmxa trie.cmx deco.cmx share.cmx mini.cmx \
html.cmx SCLpaths.cmx date.cmx web.cmx control.cmx lexmap.cmx inflected.cmx \
bank_lexer.cmx int_sandhi.cmx nouns.cmx automaton.cmx restore_caches.cmx \
-o restore_caches

# clears the cached lexicon (irreversible operation)
clear_cache: reset_caches
	./reset_caches

parse_apte: parse_apte.cmx
	$(LINK) $(CORENATIVES) date.cmx deco.cmx segmenter.cmx \
load_transducers.cmx pada.cmx naming.cmx dispatcher.cmx stemmer.cmx \
bank_lexer.cmx parse_tree.cmx parse_apte.cmx -o parse_apte

tag_apte: tag_apte.cmx
	$(LINK) $(CORENATIVES) date.cmx deco.cmx segmenter.cmx naming.cmx \
load_transducers.cmx pada.cmx dispatcher.cmx stemmer.cmx \
bank_lexer.cmx tag_tree.cmx tag_apte.cmx -o tag_apte

test_apte: parse_apte
	./parse_apte <../CORPUS/APTE/apte.e 

test_apte_tag: tag_apte
	./tag_apte <../CORPUS/APTE/apte.e 

debug_apte: share.cmo date.cmo deco.cmo segmenter.cmo canon.cmo encode.cmo naming.cmo \
skt_lexer.cmo sanskrit.cmo load_transducers.cmo dispatcher.cmo stemmer.cmo \
bank_lexer.cmo tag_tree.cmo tag_apte.cmo
	(cat debug_apte.ml ; cat -) | ocaml $(LIBS) -I .

cgis: skt_morph.cmi auto.cmi indexer indexerd declension conjugation \
lemmatizer reader parser sandhier reader interface user_aid \
corpus_manager save_corpus mkdir_corpus

indexer: indexer.cmx
	$(LINK) str.cmxa list2.cmx gen.cmx paths.cmx version.cmx date.cmx \
control.cmx word.cmx canon.cmx trie.cmx deco.cmx min_lexer.cmx phonetics.cmx \
transduction.cmx encode.cmx html.cmx SCLpaths.cmx data.cmx web.cmx cgi.cmx \
index.cmx naming.cmx morpho_string.cmx morpho.cmx order.cmx chapters.cmx \
morpho_html.cmx indexer.cmx -o indexer

indexerd: indexerd.cmx 
	$(LINK) str.cmxa list2.cmx gen.cmx paths.cmx version.cmx date.cmx \
control.cmx word.cmx min_lexer.cmx canon.cmx phonetics.cmx transduction.cmx \
encode.cmx deco.cmx html.cmx SCLpaths.cmx data.cmx web.cmx naming.cmx cgi.cmx \
morpho_string.cmx morpho.cmx order.cmx chapters.cmx morpho_html.cmx \
indexerd.cmx -o indexerd

lemmatizer: lemmatizer.cmx 
	$(LINK) str.cmxa list2.cmx gen.cmx paths.cmx version.cmx date.cmx \
control.cmx word.cmx deco.cmx min_lexer.cmx canon.cmx phonetics.cmx \
transduction.cmx encode.cmx html.cmx SCLpaths.cmx data.cmx web.cmx cgi.cmx \
naming.cmx morpho_string.cmx morpho.cmx order.cmx chapters.cmx morpho_html.cmx \
lemmatizer.cmx -o lemmatizer

declension: declension.cmx share.cmx
	$(LINK) str.cmxa list2.cmx gen.cmx paths.cmx version.cmx date.cmx \
control.cmx word.cmx canon.cmx min_lexer.cmx phonetics.cmx transduction.cmx \
encode.cmx deco.cmx int_sandhi.cmx lexmap.cmx inflected.cmx bank_lexer.cmx \
html.cmx SCLpaths.cmx data.cmx web.cmx nouns.cmx naming.cmx morpho_string.cmx \
morpho.cmx order.cmx chapters.cmx morpho_html.cmx cgi.cmx share.cmx trie.cmx \
index.cmx multilingual.cmx declension.cmx -o declension

conjugation: conjugation.cmx
	$(LINK) str.cmxa list2.cmx gen.cmx paths.cmx version.cmx date.cmx \
control.cmx word.cmx canon.cmx min_lexer.cmx phonetics.cmx transduction.cmx \
encode.cmx deco.cmx int_sandhi.cmx lexmap.cmx inflected.cmx parts.cmx pada.cmx \
verbs.cmx html.cmx SCLpaths.cmx data.cmx web.cmx naming.cmx morpho_string.cmx \
morpho.cmx order.cmx chapters.cmx morpho_html.cmx cgi.cmx share.cmx trie.cmx \
index.cmx multilingual.cmx conjugation.cmx -o conjugation

sandhier: sandhier.cmx
	$(LINK) str.cmxa list2.cmx gen.cmx paths.cmx version.cmx control.cmx \
word.cmx min_lexer.cmx canon.cmx phonetics.cmx transduction.cmx encode.cmx \
sandhi.cmx int_sandhi.cmx date.cmx html.cmx SCLpaths.cmx data.cmx web.cmx \
cgi.cmx sandhier.cmx -o sandhier

reader: reader.cmx 
	$(LINK) str.cmxa unix.cmxa list2.cmx gen.cmx paths.cmx version.cmx \
date.cmx control.cmx word.cmx canon.cmx min_lexer.cmx phonetics.cmx \
transduction.cmx encode.cmx skt_lexer.cmx chunker.cmx sanskrit.cmx deco.cmx \
lexmap.cmx inflected.cmx html.cmx SCLpaths.cmx data.cmx web.cmx naming.cmx \
morpho_string.cmx morpho.cmx load_transducers.cmx pada.cmx phases.cmx \
dispatcher.cmx order.cmx chapters.cmx morpho_html.cmx cgi.cmx segmenter.cmx \
morpho_scl.cmx load_morphs.cmx lexer.cmx constraints.cmx rank.cmx \
bank_lexer.cmx scl_parser.cmx checkpoints.cmx reader.cmx -o reader 

LEGACY=
# LEGACY= nums.cmxa 

only:
	ocamlopt -pp "camlp4rf" -I C:/opam/.opam/4.14/lib/uutf -I +camlp4 -I ../Zen/ML -I . -c \
		-annot -bin-annot interface.ml
	ocamlopt -I C:/opam/.opam/4.14/lib/uutf -I +camlp4 -I ../Zen/ML dynlink.cmxa \
		camlp4lib.cmxa uutf.cmxa str.cmxa unix.cmxa \
	list2.cmx debug.cmx gen.cmx paths.cmx version.cmx \
	date.cmx control.cmx word.cmx canon.cmx min_lexer.cmx phonetics.cmx \
	transduction.cmx encode.cmx skt_lexer.cmx chunker.cmx sanskrit.cmx deco.cmx \
	lexmap.cmx inflected.cmx html.cmx SCLpaths.cmx data.cmx web.cmx naming.cmx \
	morpho_string.cmx morpho.cmx load_transducers.cmx pada.cmx phases.cmx \
	dispatcher.cmx order.cmx chapters.cmx morpho_html.cmx bank_lexer.cmx cgi.cmx \
	load_morphs.cmx int_sandhi.cmx nouns.cmx share.cmx minimap.cmx mini.cmx \
	graph_segmenter.cmx checkpoints.cmx automaton.cmx params.cmx \
	save_corpus_params.cmx dir.cmx corpus.cmx web_corpus.cmx interface.cmx \
	-o interface
	cp interface interface.exe

interface: interface.cmx minimap.cmx debug.cmx only

user_aid: user_aid.cmx
	$(LINK) str.cmxa list2.cmx gen.cmx paths.cmx version.cmx date.cmx \
control.cmx word.cmx canon.cmx deco.cmx min_lexer.cmx phonetics.cmx \
transduction.cmx encode.cmx skt_lexer.cmx chunker.cmx sanskrit.cmx html.cmx \
SCLpaths.cmx data.cmx web.cmx naming.cmx morpho_string.cmx morpho.cmx order.cmx \
chapters.cmx morpho_html.cmx bank_lexer.cmx cgi.cmx phases.cmx checkpoints.cmx \
user_aid.cmx -o user_aid

parser: parser.cmx 
	$(LINK) str.cmxa unix.cmxa list2.cmx gen.cmx paths.cmx version.cmx \
date.cmx control.cmx word.cmx canon.cmx min_lexer.cmx phonetics.cmx \
transduction.cmx encode.cmx skt_lexer.cmx chunker.cmx sanskrit.cmx deco.cmx \
lexmap.cmx inflected.cmx html.cmx SCLpaths.cmx data.cmx web.cmx naming.cmx \
morpho_string.cmx morpho.cmx load_transducers.cmx pada.cmx phases.cmx \
dispatcher.cmx order.cmx chapters.cmx morpho_html.cmx bank_lexer.cmx cgi.cmx \
segmenter.cmx morpho_scl.cmx load_morphs.cmx lexer.cmx constraints.cmx \
checkpoints.cmx paraphrase.cmx scl_parser.cmx parser.cmx -o parser

# corpus manager by Idir Lankri:
corpus_manager: corpus_manager_cgi.cmx
	$(LINK) str.cmxa unix.cmxa list2.cmx gen.cmx paths.cmx		\
		version.cmx date.cmx control.cmx word.cmx canon.cmx	\
		min_lexer.cmx phonetics.cmx transduction.cmx encode.cmx	\
		skt_lexer.cmx chunker.cmx sanskrit.cmx deco.cmx	\
		lexmap.cmx inflected.cmx html.cmx SCLpaths.cmx data.cmx \
		web.cmx	naming.cmx morpho_string.cmx morpho.cmx		\
		load_transducers.cmx pada.cmx phases.cmx dispatcher.cmx	\
		order.cmx chapters.cmx morpho_html.cmx bank_lexer.cmx	\
		cgi.cmx load_morphs.cmx int_sandhi.cmx nouns.cmx	\
		share.cmx minimap.cmx mini.cmx $(LEGACY) \
		graph_segmenter.cmx checkpoints.cmx automaton.cmx	\
		params.cmx save_corpus_params.cmx dir.cmx corpus.cmx	\
		web_corpus.cmx multilingual.cmx				\
		mkdir_corpus_params.cmx corpus_manager.cmx $< -o $@

save_corpus: save_corpus_cgi.cmx
	$(LINK) str.cmxa unix.cmxa list2.cmx gen.cmx paths.cmx		\
		version.cmx date.cmx control.cmx word.cmx canon.cmx	\
		min_lexer.cmx phonetics.cmx transduction.cmx encode.cmx	\
		skt_lexer.cmx chunker.cmx sanskrit.cmx deco.cmx	\
		lexmap.cmx inflected.cmx html.cmx SCLpaths.cmx data.cmx	\
		web.cmx naming.cmx morpho_string.cmx morpho.cmx		\
		load_transducers.cmx pada.cmx phases.cmx dispatcher.cmx	\
		order.cmx chapters.cmx morpho_html.cmx bank_lexer.cmx	\
		cgi.cmx load_morphs.cmx int_sandhi.cmx nouns.cmx	\
		share.cmx minimap.cmx mini.cmx $(LEGACY) \
		graph_segmenter.cmx checkpoints.cmx automaton.cmx	\
		params.cmx save_corpus_params.cmx dir.cmx corpus.cmx	\
		web_corpus.cmx multilingual.cmx				\
		mkdir_corpus_params.cmx corpus_manager.cmx $< -o $@

mkdir_corpus: mkdir_corpus_cgi.cmx
	$(LINK) str.cmxa unix.cmxa list2.cmx gen.cmx paths.cmx		\
		version.cmx date.cmx control.cmx word.cmx canon.cmx	\
		min_lexer.cmx phonetics.cmx transduction.cmx encode.cmx	\
		skt_lexer.cmx chunker.cmx sanskrit.cmx deco.cmx	\
		lexmap.cmx inflected.cmx html.cmx SCLpaths.cmx data.cmx	\
		web.cmx naming.cmx morpho_string.cmx morpho.cmx		\
		load_transducers.cmx pada.cmx phases.cmx dispatcher.cmx	\
		order.cmx chapters.cmx morpho_html.cmx bank_lexer.cmx	\
		cgi.cmx load_morphs.cmx int_sandhi.cmx nouns.cmx	\
		share.cmx minimap.cmx mini.cmx $(LEGACY) \
		graph_segmenter.cmx checkpoints.cmx automaton.cmx	\
		params.cmx save_corpus_params.cmx dir.cmx corpus.cmx	\
		web_corpus.cmx multilingual.cmx				\
		mkdir_corpus_params.cmx corpus_manager.cmx $< -o $@

mk_corpus: mk_corpus.cmx
	$(LINK) str.cmxa unix.cmxa list2.cmx gen.cmx paths.cmx		\
		version.cmx date.cmx control.cmx word.cmx canon.cmx	\
		min_lexer.cmx phonetics.cmx transduction.cmx encode.cmx	\
		skt_lexer.cmx chunker.cmx sanskrit.cmx deco.cmx	\
		lexmap.cmx inflected.cmx html.cmx SCLpaths.cmx data.cmx	\
		web.cmx naming.cmx morpho_string.cmx morpho.cmx		\
		load_transducers.cmx pada.cmx phases.cmx dispatcher.cmx	\
		order.cmx chapters.cmx morpho_html.cmx bank_lexer.cmx	\
		cgi.cmx load_morphs.cmx int_sandhi.cmx nouns.cmx	\
		share.cmx minimap.cmx mini.cmx $(LEGACY) \
		graph_segmenter.cmx checkpoints.cmx automaton.cmx	\
		params.cmx save_corpus_params.cmx dir.cmx corpus.cmx	\
		web_corpus.cmx  multilingual.cmx			\
		mkdir_corpus_params.cmx $< -o $@

css: css.cmx
	$(LINK) $(CORENATIVES) str.cmxa date.cmx html.cmx SCLpaths.cmx web.cmx \
css.cmx -o css

mk_index_page: mk_index_page.cmx 
	$(LINK) $(CORENATIVES) str.cmxa date.cmx html.cmx SCLpaths.cmx \
web.cmx mk_index_page.cmx -o mk_index_page

mk_grammar_page: mk_grammar_page.cmx 
	$(LINK) $(CORENATIVES) str.cmxa date.cmx html.cmx SCLpaths.cmx \
web.cmx mk_grammar_page.cmx -o mk_grammar_page

mk_reader_page: mk_reader_page.cmx
	$(LINK) $(CORENATIVES) str.cmxa unix.cmxa control.cmx cgi.cmx date.cmx \
SCLpaths.cmx html.cmx web.cmx params.cmx dir.cmx corpus.cmx web_corpus.cmx \
mk_reader_page.cmx -o mk_reader_page

mk_sandhi_page: mk_sandhi_page.cmx
	$(LINK) $(CORENATIVES) str.cmxa date.cmx html.cmx SCLpaths.cmx \
web.cmx mk_sandhi_page.cmx -o mk_sandhi_page

mk_corpus_page: mk_corpus_page.cmx
	$(LINK) $(CORENATIVES) str.cmxa unix.cmxa date.cmx html.cmx \
SCLpaths.cmx web.cmx params.cmx dir.cmx cgi.cmx corpus.cmx web_corpus.cmx \
$< -o $@

# site_pages must be recomputed after configuration and creation of DICO dir
# the style sheet must be recomputed, since it contains absolute URLs as well
site_pages: mk_index_page mk_grammar_page mk_reader_page mk_sandhi_page	\
		mk_corpus_page css
	./mk_index_page
	cd ../DICO; ln -f index.fr.html index.html # backward compatibility
	./mk_grammar_page
	cd ../DICO; ln -f grammar.fr.html grammar.html # id
	./mk_reader_page -en
	./mk_reader_page -fr
	cd ../DICO; ln -f reader.fr.html reader.html # id
	./mk_sandhi_page
	cd ../DICO; ln -f sandhi.fr.html sandhi.html # id
	./mk_corpus_page
	cd ../DICO; ln -f corpus.fr.html corpus.html # id
# XML info file copied in DICO by make web_services in main Makefile
	./css 
# but install sets index.html to .fr. ou .en. according to config lexicon choice 

# sandhi non-regression tests
sandhi_test: sandhi_test.cmx
	$(LINK) list2.cmx gen.cmx paths.cmx version.cmx \
word.cmx canon.cmx min_lexer.cmx phonetics.cmx transduction.cmx encode.cmx \
sandhi.cmx int_sandhi.cmx sandhi_test.cmx -o sandhi_test
	./sandhi_test

# tagger1 uni-phase for noun phrases - obsolete but good for teaching
tagger1: tagger1.cmx 
#	cd $(DATA) && $(MAKE) transn.rem
	$(LINK) list2.cmx gen.cmx paths.cmx version.cmx \
word.cmx canon.cmx min_lexer.cmx phonetics.cmx transduction.cmx \
skt_lexer.cmx deco.cmx sandhi.cmx chunker.cmx sanskrit.cmx naming.cmx \
morpho.cmx segmenter1.cmx tagger1.cmx -o tagger1

# enter in ocaml toplevel '#use "debug.ml";' in order to debug 
# adapt to your need in modules 
# NB best used by first hiding all .mli, make clean, make depend 
debug: gen.cmo version.cmo date.cmo word.cmo min_lexer.cmo order.cmo \
canon.cmo phonetics.cmo min_lexer.cmo transduction.cmo chunker.cmo \
sanskrit.cmo sandhi.cmo int_sandhi.cmo share.cmo trie.cmo deco.cmo \
skt_lexer.cmo encode.cmo order.cmo lexmap.cmo inflected.cmo parts.cmo pada.cmo \
verbs.cmo morpho_html.cmo cgi.cmo html.cmo web.cmo indexer.cmo declension.cmo \
zen_lexer.cmo data.cmo automaton.cmo
	(cat debug.ml ; cat -) | ocaml $(LIBS) -I .

morpho_debug: list2.cmo word.cmo deco.cmo morpho_string.cmo paths.cmo \
install.cmo canon.cmo min_lexer.cmo gen.cmo transduction.cmo phonetics.cmo \
encode.cmo naming.cmo version.cmo date.cmo html.cmo morpho.cmo 
	(cat morpho_debug.ml ; cat -) | ocaml $(LIBS) -I .

# Documentation of the ML library - all packages 
doc: 
	$(MAKE) Heritage_Platform.pdf

Heritage_Platform.pdf: $(ENGINE) 
	$(NOWEB) --noweb --tex ../DOC/docintro.tex $(ENGINE) -o ../DOC/Heritage_Platform.tex
	cd ../DOC && pdflatex Heritage_Platform


.depend depend:: # $(ENGINE) # remove dependency on sources to avoid depend comp
	> .depend.new
	for i in $(ZENSOURCES); do \
	ocamldep -pp camlp4rf $(LIBS) -I . $(ZEN)/$$i >> .depend.new ; done
	for i in $(ENGINE); do \
	ocamldep -pp camlp4rf $(LIBS) -I . $$i >> .depend.new ; done
	mv .depend.new .depend

clean:
	rm -f *.cmo *.cmi *.cmx *.ppi *.ppo *.o *.annot *.cmt *.cmti
	rm -f css indexer indexerd sandhier reader parser lemmatizer \
declension conjugation mk_index_page mk_grammar_page mk_reader_page regression \
mk_sandhi_page mk_corpus_page sandhi_test reset_caches interface user_aid \
parse_apte tag_apte corpus_manager save_corpus mkdir_corpus mk_corpus
#	make dico.cmi auto.cmi conj_infos.cmi # needed to get dependencies right 

.SUFFIXES: .ml .mli .cmx .cmo .cmi

.ml.cmo:
	$(COMPILEI) $<

.mli.cmi:
	$(COMPILEI) $<

.ml.cmx:
	$(COMPILE) $<

include .depend
