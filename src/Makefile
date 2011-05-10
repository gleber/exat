#
# Makefile
#

EBIN=../ebin

include ../Makefile.conf

.SUFFIXES: .erl .beam

OBJS=\
ontology.beam \
ontology_service.beam \
fipa_ontology_sl_codec.beam \
sl_parser.beam \
sl.beam \
mtp.beam \
exat.beam \
acl.beam \
agent.beam \
fipa_semantics_simple.beam \
match_lib.beam \
semantics.beam \
envelope.beam \
mtp.beam \
exat_app.beam \
exat_sup.beam \
exat_server.beam \
logger.beam \
object.beam \
eventmanager.beam \
sync.beam \
multisync.beam \
ams.beam \
pingagent.beam \
pingeragent.beam \
http_parser.beam \
http_server.beam \
media_type.beam \
eresye_agenda.beam \
eresye.beam \
eresye_ontology_resolver.beam \
eresye_tree_list.beam



ERL_OBJS=$(foreach file, $(OBJS), $(EBIN)/$(file))

OTHER_OBJS=
FIND=find

all: $(ERL_OBJS) $(OTHER_OBJS)
	cp exat.app $(EBIN)

fipa_ontology_sl_codec.erl: $(EBIN)/ontology.beam ../priv/fipa_ontology.onto
	(cd ../priv; \
	erl -noshell -pa ../ebin -eval \
	'ontology:compile ("fipa_ontology", [include, sl_codec]),halt ().'; \
	mv fipa_ontology.hrl ../include; \
	mv fipa_ontology_sl_codec.erl ../src; \
	cd ../src)
# 	(cd ../priv; \
# 	sh compile_fipa_ontology.sh fipa_ontology; \
# 	mv fipa_ontology.hrl ../include; \
# 	mv fipa_ontology_sl_codec.erl ../src; \
# 	cd ../src)

http_parser.erl: http_parser.yrl
	$(YRL) $<

media_type.erl: media_type.yrl
	$(YRL) $<

sl_parser.erl: sl_parser.yrl
	$(YRL) $<

$(EBIN)/%.beam: %.erl
	$(BEAM) $<

clean:
	@(cd $(EBIN); $(RM) -f *.beam)

clearbak:
	@$(FIND) . -type f -name \*~ -exec rm {} \;

