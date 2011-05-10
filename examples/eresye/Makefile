#
# Makefile
#

EBIN=.

include ../../Makefile.conf

.SUFFIXES: .erl .beam

OBJS=\
auto.beam \
cannibals.beam \
journal.beam \
journal_sample.beam \
phil.beam \
prodcons.beam \
sieve.beam




ERL_OBJS=$(foreach file, $(OBJS), $(file))

OTHER_OBJS=
FIND=find

all: $(ERL_OBJS) $(OTHER_OBJS)

journal.erl: journal.onto
	sh compile_ontology.sh journal

%.beam: %.erl
	$(BEAM) $<

clean:
	$(RM) -f *.beam journal.hrl journal.erl

clearbak:
	@$(FIND) . -type f -name \*~ -exec rm {} \;

