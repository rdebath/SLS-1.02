SUBDIRS = inetd portmap rpcinfo rpcgen tsort rpcsvc rstat

all:

strip:
	-find . -type f -perm +0100 -print | xargs strip

.DEFAULT:
	@for i in $(SUBDIRS); do (cd ./$$i && echo $$i: && $(MAKE) $@) || exit 1; done

