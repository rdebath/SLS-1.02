# Hi, this is just a small makefile I added.
#
BIN_DIR = /usr/X386/bin

all: $(BIN_DIR)/vgaset

$(BIN_DIR)/vgaset: svga.h vgaset.c.z
	gunzip vgaset.c.z
	gcc -s -O6 vgaset.c -o $@
	chmod ug=rxs $@
	chmod o= $@
	gzip vgaset.c

vgaset.c.z: 
	gzip vgaset.c 


