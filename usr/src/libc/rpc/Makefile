#
# Makefile for SUN rpc functions
#
#

include ../Makeconfig

include ../Makerules

LIB=../librpc.a
LIB:=../$(REALLIBC)

JUMP_DIR:=../jump/$(SHLIB)
JUMP_LIB=libc

ifndef OPTFLAGS
OPTFLAGS= -O6
endif

CFLAGS = $(WFLAGS) $(OPTFLAGS) $(INC) $(XCFLAGS)

SRCS = auth_none.c auth_unix.c auth_prot.c bindresvport.c
SRCS = auth_none.c auth_unix.c auth_prot.c \
	clnt_generic.c clnt_perror.c clnt_raw.c clnt_simple.c \
	clnt_tcp.c clnt_udp.c rpc_dtblsz.c get_myaddr.c \
	getrpcent.c getrpcport.c pmap_clnt.c pmap_getmaps.c \
	pmap_getport.c pmap_prot.c pmap_prot2.c pmap_rmt.c \
	rpc_prot.c rpc_comdata.c rpc_callmsg.c svc.c svc_auth.c \
	svc_au_ux.c svc_raw.c svc_run.c svc_simple.c \
	svc_tcp.c svc_udp.c xdr.c xdr_array.c xdr_float.c xdr_mem.c \
	xdr_rec.c xdr_ref.c xdr_stdio.c

OBJS = auth_none.o auth_unix.o auth_prot.o bindresvport.o
OBJ1S = auth_none.o auth_unix.o auth_prot.o \
	clnt_generic.o clnt_perror.o clnt_raw.o clnt_simple.o \
	clnt_tcp.o clnt_udp.o rpc_dtblsz.o get_myaddr.o \
	getrpcent.o getrpcport.o pmap_clnt.o pmap_getmaps.o
OBJ2S=	pmap_getport.o pmap_prot.o pmap_prot2.o pmap_rmt.o \
	rpc_prot.o rpc_callmsg.o svc.o svc_auth.o \
	svc_au_ux.o svc_raw.o svc_run.o svc_simple.o \
	svc_tcp.o svc_udp.o xdr.o xdr_array.o xdr_float.o xdr_mem.o \
	xdr_rec.o xdr_ref.o xdr_stdio.o

# rpc_comdata.o built by jump table

HDRS = auth.h auth_unix.h clnt.h netdb.h pmap_clnt.h pmap_prot.h \
	pmap_rmt.h rpc.h rpc_msg.h svc.h svc_auth.h types.h xdr.h

library: $(OBJ1S) $(OBJ2S)
	$(AR) uvc $(LIB) $(OBJ1S)
	$(AR) uvc $(LIB) $(OBJ2S)


clean:
	$(RM) -f core *.o *.a tmp_make foo
	-for i in *.c;do \
	  if [ -f $$i ]; then \
	    $(RM) -f `basename $$i .c`.s; \
	  fi; \
	done

dep:
	sed '/\#\#\# Dependencies/q' < Makefile > tmp_make
	(for i in *.c;do echo -n `echo $$i | sed 's,\.c,\.s,'`" "; \
	$(CPP) -M $$i;done) >> tmp_make
	cp tmp_make Makefile

### Dependencies:
