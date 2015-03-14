# This makefile will print a hardcopy of the Csound Reference Manual.
# Edit the first 4 lines, then type 'make all' to print the whole manual,
# 'make one' for chapter one, etc.  Includes an index and quickref section.
# Final copy requires two PostScript music example inserts (pp. 73 and 76).
# The reference page numbers are consistent with our ditroff laser pagination.
# If you have problems, you can ftp the csound.manual PostScript file in one of
# its forms (.ps.tar, .ps.tar.Z), untar, & cat the parts to a PostScript printer.
# If you don't have a PostScript printer, you should request a hardcopy from MIT.

D = groff -ms -M/usr/lib/tmac -fN
# old macros, font N = NewCent  (else defaults to palatino)

E = geqn
# equation preprocessor

T = gtbl
# table preprocessor

P = rsh media-lab "qpr -q brahms" <
# print command

O = cat > $@
M = macros
R = manual.ps.tar

ORCH = syntax constants value pitch arith express \
assign orchead instr goto reinit ihold \
g1.line g2.phasor g3.table g4.oscil g5.buzz g6.adsyn g7.fof g8.pluck g9.rand \
m1.linen m1.linex m2.port m3.lpread m4.rms m5.downsamp m6.delayr m7.deltap \
m8.comb m9.specdata m91.sensdata m92.soundin m93.pan m94.display

NSCORE = nscore.1 nscore.2

SCOT = scot.1 scot.2 scot.3 scot.4 scot.5

ALLSECS = title copy cont pref one two three four five six seven app
ALLPSS  = title.ps copy.ps cont.ps pref.ps one.ps two.ps three.ps \
	  four.ps five.ps six.ps seven.ps app.ps
EXAMPSS	= ex1.ps ex2.ps

all:	print

clean:
	-for file in $(ALLPSS); do rm $$file; done
	-rm $R

$(ALLSECS):
	make $@.ps
	$P $@.ps

print:	$(ALLPSS) $(EXAMPSS)
	for file in $(ALLPSS) $(EXAMPSS); do $P $$file; done

ps:	$(ALLPSS) $(EXAMPSS)

pstar:	ps
	-rm $R
#	tar cvf $R
#	for file in $(ALLPSS) $(EXAMPSS); do tar rvf $R $$file; done
	tar cvf $R *.ps

title.ps: titlepage
	$D $M titlepage | $O

copy.ps: Copyright
	$D $M Copyright | $O

cont.ps: contents
	$T $M contents | $D | $O

pref.ps: preface
	$D $M preface | $O

one.ps:	tutorial
	$D $M tutorial | $O

two.ps:	$(ORCH)
	$D -n6 $M $(ORCH) | $O

three.ps: $(NSCORE)
	$D -n43 $M $(NSCORE) | $O

four.ps: genroutines
	$E $M genroutines | $D -n52 | $O

five.ps: cscore
	$D -n63 $M cscore | $O

six.ps:	$(SCOT)
	$D -n70 $M $(SCOT) | $O

seven.ps: commands
	$D -n81 $M commands | $O

app.ps:	quickref
	$D -n84 $M quickref | $O

