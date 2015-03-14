#
# Copyright 1990,91 by Thomas Roell, Dinkelscherben, Germany.
#
# Permission to use, copy, modify, distribute, and sell this software and its
# documentation for any purpose is hereby granted without fee, provided that
# the above copyright notice appear in all copies and that both that
# copyright notice and this permission notice appear in supporting
# documentation, and that the name of Thomas Roell not be used in
# advertising or publicity pertaining to distribution of the software without
# specific, written prior permission.  Thomas Roell makes no representations
# about the suitability of this software for any purpose.  It is provided
# "as is" without express or implied warranty.
#
# THOMAS ROELL DISCLAIMS ALL WARRANTIES WITH REGARD TO THIS SOFTWARE,
# INCLUDING ALL IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS, IN NO
# EVENT SHALL THOMAS ROELL BE LIABLE FOR ANY SPECIAL, INDIRECT OR
# CONSEQUENTIAL DAMAGES OR ANY DAMAGES WHATSOEVER RESULTING FROM LOSS OF USE,
# DATA OR PROFITS, WHETHER IN AN ACTION OF CONTRACT, NEGLIGENCE OR OTHER
# TORTIOUS ACTION, ARISING OUT OF OR IN CONNECTION WITH THE USE OR
# PERFORMANCE OF THIS SOFTWARE.
#
# Author:  Thomas Roell, roell@informatik.tu-muenchen.de
#

#
# dependencies
#
if [ ! -s /usr/options/kc.name ]
then
    echo "Please make first sure that the Kernel Configuration is installed"
    echo "correctly. Then try a reinstall of X386."
    exit 1
fi

if [ ! -s /usr/options/st.name ]
then
    echo "Please make first sure that the STREAMS Facilities are installed"
    echo "correctly. Then try a reinstall of X386."
    exit 1
fi

#
# Make a X11 label, so that following commercial products can be installed
# 
if [ ! -s /usr/options/xu.name ]
then
    echo "X11R5 X386 Version 1.2" >/usr/options/xu.name
fi

#
# Setup STREAMS for maximum performance
#
cd /usr/lib/X11/X386/install
cp pts.node /etc/conf/node.d/pts 2>/dev/null
cp sp.node /etc/conf/node.d/sp 2>/dev/null
cp ldterm.sdevice /etc/conf/sdevice.d/ldterm 2>/dev/null
cp ptem.sdevice /etc/conf/sdevice.d/ptem 2>/dev/null
cp ptm.sdevice /etc/conf/sdevice.d/ptm 2>/dev/null
cp pts.sdevice /etc/conf/sdevice.d/pts 2>/dev/null
cp sp.sdevice /etc/conf/sdevice.d/sp 2>/dev/null


/bin/sh /etc/conf/bin/idtune -m NSTREAM 128
/bin/sh /etc/conf/bin/idtune -m NQUEUE 512
/bin/sh /etc/conf/bin/idtune -m NBLK4096 4
/bin/sh /etc/conf/bin/idtune -m NBLK2048 32
/bin/sh /etc/conf/bin/idtune -m NBLK1024 32
/bin/sh /etc/conf/bin/idtune -m NBLK512 32
/bin/sh /etc/conf/bin/idtune -m NBLK256 64
/bin/sh /etc/conf/bin/idtune -m NBLK128 256
/bin/sh /etc/conf/bin/idtune -m NBLK64 256
/bin/sh /etc/conf/bin/idtune -m NBLK16 256
/bin/sh /etc/conf/bin/idtune -m NBLK4 128
/bin/sh /etc/conf/bin/idtune -m SHLBMAX 8

echo "*** New Installation ***"
echo "If this is a new installation of X386 1.0 use \"kconfig\" to build a new"
echo "kernel. X386 won't run without this new kernel."
echo ""
echo "<press return>"
read answer

#
# install addtional termcap & terminfo entries
#
grep xterm   /etc/termcap >/dev/null || cat xterm.termcap >>/etc/termcap
grep sun-cmd /etc/termcap >/dev/null || cat sun.termcap   >>/etc/termcap

tic sun.terminfo 2>/dev/null
tic xterm.terminfo 2>/dev/null

if [ -s /usr/lib/loadfont/vga437.bdf ]
then
    sed -e 's/FONT 8x16/FONT vga/' </usr/lib/loadfont/vga437.bdf \
    | bdftopcf -t >/usr/X386/lib/X11/fonts/misc/vga.pcf
    chmod 644 /usr/X386/lib/X11/fonts/misc/fonts.dir
    mkfontdir /usr/X386/lib/X11/fonts/misc
fi
