CFLAGS=-g
RM=-rm -f
OBJ=xdr.o yplib.o

all:		ypbind ypmatch ypcat ypwhich

ypmatch:	$(OBJ) ypmatch.o
		$(CC) $(CFLAGS) $(OBJ) ypmatch.o -o ypmatch -lrpcsvc

ypcat:		$(OBJ) ypcat.o
		$(CC) $(CFLAGS) $(OBJ) ypcat.o -o ypcat -lrpcsvc

ypbind:		$(OBJ) ypbind.o
		$(CC) $(CFLAGS) $(OBJ) ypbind.o -o ypbind -lrpcsvc

ypwhich:	$(OBJ) ypwhich.o
		$(CC) $(CFLAGS) $(OBJ) ypwhich.o -o ypwhich -lrpcsvc

$(OBJ):		yp_prot.h theo_yp.h ypclnt.h

clean:
		$(RM) ypcat ypbind ypmatch ypwhich *.o *~
