#!/bin/sh
# $XConsortium: xinitrc.cpp,v 1.4 91/08/22 11:41:34 rws Exp $

# Merge in defaults and keymaps

userresources=$HOME/.Xresources
usermodmap=$HOME/.Xmodmap
sysresources=/usr/lib/X11/xinit/.Xresources
sysmodmap=/usr/lib/X11/xinit/.Xmodmap
 
if [ -f $sysresources ]; then
    xrdb -merge $sysresources
fi
 
if [ -f $sysmodmap ]; then
    xmodmap $sysmodmap
fi
 
if [ -f $userresources ]; then
    xrdb -merge $userresources
fi
 
if [ -f $usermodmap ]; then
    xmodmap $usermodmap
fi

# Olwm & XView application like OPENWINHOME to be assigned

OPENWINHOME=/usr/openwin; export OPENWINHOME
PATH=$PATH:$OPENWINHOME/bin
HELPPATH=$OPENWINHOME/help; export HELPPATH
if [ -z "$MANPATH" ]; then
	MANPATH=/usr/local/man:/usr/man; export MANPATH
fi
MANPATH=$MANPATH:$OPENWINHOME/man

# Start some shelltools and the window manager

xterm -bg cyan -geom +100&
xterm -bg cyan -geom +1124&
xterm -bg cyan -geom +2148&
exec $OPENWINHOME/bin/olvwm -f
