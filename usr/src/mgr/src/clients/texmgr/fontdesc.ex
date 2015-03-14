#
# Sample C-TeX font configuration file.
#
# N.B.: choose only one of `box' or `blank' TFM fonts.
#	TYPE	SPEC	SLOP	PATH
#
# current directory always searched first
#	TYPE	SPEC	SLOP	PATH
font	pk	*	3	./%f.%mpk
font	gf	*	3	./%f.%mgf
font	pxl	*	3	./%f.%mpxl
# this is not pretty
font	blank	*	1	i/%f.tfm
font	box	*	1	./%f.tfm

#	TYPE	SPEC	SLOP	PATH
font	pk	*	3	/usr/local/lib/tex/fonts/%f.%mpk
font	gf	*	3	/usr/local/lib/tex/fonts/%f.%mgf
font	pxl	*	3	/usr/local/lib/tex/fonts/%f.%mpxl
# SLiTeX invisible fonts can be done with links:
# font	blank	*	1	/usr/local/lib/tex82/slitex82fonts/%f.tfm
# font	box	*	1	/usr/local/lib/tex82/fonts/%f.tfm
# font	box	*	1	/usr/local/lib/tex82/psfonts/%f.tfm
