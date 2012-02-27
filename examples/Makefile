#
# Makefile
#

DIRS=$(shell echo */)

all:
	for i in $(DIRS) ; do \
		(cd $$i; make) ; \
	done

clean:
	@(cd simple;make clean)
	@(cd eresye;make clean)
