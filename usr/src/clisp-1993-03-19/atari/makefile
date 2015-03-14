GCC=e:\gnu\exec\gcc.ttp

all : lisp.prg

alglobal.o : alglobal.s
	$(GCC) -c alglobal.s

ari68000.o : ari68000.s
	$(GCC) -c ari68000.s

array.o : array.s
	$(GCC) -c array.s

charstrg.o : charstrg.s
	$(GCC) -c charstrg.s

control.o : control.s
	$(GCC) -c control.s

debug.o : debug.s
	$(GCC) -c debug.s

eval.o : eval.s
	$(GCC) -c eval.s

hashtabl.o : hashtabl.s
	$(GCC) -c hashtabl.s

io.o : io.s
	$(GCC) -c io.s

lisparit.o : lisparit.s
	$(GCC) -c lisparit.s

list.o : list.s
	$(GCC) -c list.s

misc.o : misc.s
	$(GCC) -c misc.s

package.o : package.s
	$(GCC) -c package.s

pathname.o : pathname.s
	$(GCC) -c pathname.s

predtype.o : predtype.s
	$(GCC) -c predtype.s

record.o : record.s
	$(GCC) -c record.s

sequence.o : sequence.s
	$(GCC) -c sequence.s

spvw.o : spvw.s
	$(GCC) -c spvw.s

spvwtabf.o : spvwtabf.s
	$(GCC) -c spvwtabf.s

spvwtabo.o : spvwtabo.s
	$(GCC) -c spvwtabo.s

spvwtabs.o : spvwtabs.s
	$(GCC) -c spvwtabs.s

stream.o : stream.s
	$(GCC) -c stream.s

symbol.o : symbol.s
	$(GCC) -c symbol.s

lisp.prg : alglobal.o ari68000.o array.o charstrg.o control.o debug.o eval.o \
           hashtabl.o io.o lisparit.o list.o misc.o package.o pathname.o \
           predtype.o record.o sequence.o spvw.o spvwtabf.o spvwtabo.o \
           spvwtabs.o stream.o symbol.o
	$(GCC) -o lisp.prg @all_o

