all: shownonascii heb6x13.snf mailto-hebrew

heb6x13.snf: heb6x13.bdf
	-bdftosnf heb6x13.bdf > heb6x13.snf
	-mkfontdir

shownonascii: shownonascii.source
	sed -e s@/u/nsb/lib/fonts@`pwd`@ < shownonascii.source > shownonascii
	chmod +x shownonascii

mailto-hebrew: mailto-hebrew.source
	sed -e s@/u/nsb/lib/fonts@`pwd`@ < mailto-hebrew.source > mailto-hebrew
	chmod +x mailto-hebrew

clean:
	rm -f heb6x13.snf shownonascii mailto-hebrew fonts.dir
