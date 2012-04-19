all: deps fast

fast:
	./rebar compile

deps:
	./rebar get-deps
	./rebar compile

clean:
	./rebar clean

clearbak:
	@$(FIND) . -type f -name \*~ -exec rm {} \;

dist:
	(cd ..; \
	cp -f ERESYE-1.2.4/doc/* eXAT-1.3/doc ; \
	rm -f eXAT-1.3.tar.gz; tar zcvf eXAT-1.3.tar.gz \
	eXAT-1.3/ChangeLog \
	eXAT-1.3/ebin/* \
	eXAT-1.3/exat.sh  \
	eXAT-1.3/LICENSE.txt  \
	eXAT-1.3/Makefile.conf  \
	eXAT-1.3/README.txt    \
	eXAT-1.3/src/* \
	eXAT-1.3/doc/eXAT_Manual.pdf \
	eXAT-1.3/doc/Domain_Of_Relatives_Example.pdf \
	eXAT-1.3/doc/ERESYE_Paper.pdf \
	eXAT-1.3/examples/* \
	eXAT-1.3/include/* \
	eXAT-1.3/Makefile \
	eXAT-1.3/priv/* \
	eXAT-1.3/RELEASES.txt)
