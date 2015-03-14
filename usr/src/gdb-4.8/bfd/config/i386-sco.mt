# Target:  Intel 386 running SCO Unix.
TDEFAULTS = -DDEFAULT_VECTOR=i386coff_vec \
	-DSELECT_VECS='&i386coff_vec,&i386aout_vec,&sco_core_vec'
TDEFINES = -DSCO_CORE
TDEPFILES = sco-core.o
