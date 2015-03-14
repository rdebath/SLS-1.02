

INSTALL=/usr/local/bin/install

R=
R=../../MM/DIST


install: install_dirs install_libs install_bins

install_root:
	$(INSTALL) -d -o bin -g bin $R

install_dirs:
	$(INSTALL) -d -o bin -g bin $R/usr
	$(INSTALL) -d -o bin -g bin $R/usr/local
	$(INSTALL) -d -o bin -g bin $R/usr/local/bin
	$(INSTALL) -d -o bin -g bin $R/usr/local/lib
	$(INSTALL) -d -o bin -g bin $R/usr/local/lib/mm

install_libs:
	$(INSTALL) -o bin -g bin fax.mailcap $R/usr/local/lib/mm
	$(INSTALL) -o bin -g bin mm.mailcap $R/usr/local/lib/mm
	$(INSTALL) -o bin -g bin fwd.mailcap $R/usr/local/lib/mm
	$(INSTALL) -o bin -g bin mmforward $R/usr/local/lib/mm
	$(INSTALL) -o bin -g bin mmgetchar $R/usr/local/lib/mm
	$(INSTALL) -o bin -g bin mmgetfilenm $R/usr/local/lib/mm
	$(INSTALL) -o bin -g bin mmsavefile $R/usr/local/lib/mm
	$(INSTALL) -o bin -g bin mmsavepart $R/usr/local/lib/mm
	$(INSTALL) -o bin -g bin mmshowextern $R/usr/local/lib/mm
	$(INSTALL) -o bin -g bin mmshowfax $R/usr/local/lib/mm
	$(INSTALL) -o bin -g bin mmshowpart $R/usr/local/lib/mm

install_bins:
	$(INSTALL) -o bin -g bin mmsend $R/usr/local/bin
	$(INSTALL) -o bin -g bin mmsendsh $R/usr/local/bin


