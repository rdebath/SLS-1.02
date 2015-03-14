#!/bin/csh -f

cd /tmp
uudecode < $1
audiotool audio-file
rm -f audio-file $1
