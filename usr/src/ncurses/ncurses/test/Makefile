# Makefile for ncurses ascii programs 

CFLAGS =  -O6 

TESTS = battle gdc hanoi knight rain worm ascii acs scroll getch slk

test: test.o
	gcc -o test test.o -lncurses

battle: battle.o
	gcc -o battle battle.o -lncurses

gdc: gdc.o
	gcc -o gdc  gdc.o -lncurses

hanoi: hanoi.o
	gcc -o hanoi hanoi.o -lncurses

knight: knight.o
	gcc -o knight knight.o -lncurses

rain: rain.o
	gcc -o rain rain.o -lncurses

sked: sked.o
	gcc -o sked sked.o -lncurses
worm: worm.o
	gcc -o worm worm.o -lncurses

slk: slk.o
	gcc -o slk slk.o -lncurses

ascii: ascii.o
	gcc -o ascii ascii.o -lncurses

getch: getch.o
	gcc -o getch getch.o -lncurses

acs: acs.o
	gcc -o acs acs.o -lncurses

caps: caps.o
	gcc -o caps caps.o -lncurses

scroll: scroll.o
	gcc -o scroll scroll.o -lncurses

all: $(TESTS) 

clean:
	rm -rf *.o trace $(TESTS)

