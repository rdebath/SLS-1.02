
/* This work is copyrighted. See COPYRIGHT.OLD & COPYRIGHT.NEW for   *
*  details. If they are missing then this copy is in violation of    *
*  the copyright conditions.                                        */

/*
 * unctrl.h
 *
 */

extern char	*_unctrl[];

# define	unctrl(ch)	(_unctrl[(unsigned) ch])
