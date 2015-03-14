SUBDIRS = nslookup nsquery nstest

all:

.DEFAULT:
	@for i in $(SUBDIRS); do (cd ./$$i && echo $$i: && $(MAKE) $@); done

