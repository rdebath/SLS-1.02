SUBDIRS = dir msg sort


all:
	@echo "Please install the whole RPC package and then try the demos individually."

each:
	@for i in $(SUBDIRS); do (cd ./$$i && echo $$i: && $(MAKE) all) || exit 1; done

.DEFAULT:
	@for i in $(SUBDIRS); do (cd ./$$i && echo $$i: && $(MAKE) $@) || exit 1; done

