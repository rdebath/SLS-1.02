: This file is a complement to zip.prj for Turbo C 2.0 users.
: Use it to assemble im_lm.asm then enter TC, change the compilation
: model from small to compact if you wish (thus removing a limitation on
: the number of files but getting slower code), and press F9...
: Note: currently, im_lm.asm does not work in the compact model with Turbo C.
: If you wish to use the compact model, #define NO_ASM in im_lmat.c and
: remove im_lm.obj from zip.prj.
tasm -t -ml -DDYN_ALLOC im_lm;
: Let's do ship while we're here
tcc -w -a -d -G -O -Z -ms -Ic:\tc\include -Lc:\tc\lib ship
