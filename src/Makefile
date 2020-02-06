PROGNAME=bezier
include lib/github.com/diku-dk/lys/common.mk

.PHONY: sync
sync:
	futhark pkg sync

.PHONY: realclean
realclean:
	$(MAKE) clean
	rm -rf *~
