CFLAGS= -I/usr/local/include -O
OBJECTS= sok.o readscreen.o showscreen.o play.o save.o score.o

sokoban: $(OBJECTS)
	gcc -o sokoban $(OBJECTS) -lncurses 

clean:
	-rm $(OBJECTS) sokoban trace

clobber: clean
	-rm sok.score
