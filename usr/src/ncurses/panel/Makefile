INC	=	/usr/local/include
#INC = /usr/include
CFLAGS = -DTRACE -I$(INC) -O
LIB	=	/usr/local/lib
#LIB = /usrlib

OBJ = ptest.o panel.o

ptest: $(OBJ)
	gcc -o $@ $(OBJ) -lncurses

install: panel.o
	ar rs libpanal.a panel.o
	install -m 644 -o bin -g bin libpanel.a $(LIB)
	install -m 644 -o bin -g bin panel.h $(INC)

clean:
	rm -rf $(OBJ) core a.out trace 

clobber: clean
	rm -rf ptest
