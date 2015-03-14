(* $Id: Checks.mi,v 1.2 1992/01/30 13:23:29 grosch rel $ *)

(* $Log: Checks.mi,v $
 * Revision 1.2  1992/01/30  13:23:29  grosch
 * redesign of interface to operating system
 *
 * Revision 1.1  1991/11/21  14:33:17  grosch
 * new version of RCS on SPARC
 *
 * Revision 1.0  88/10/04  11:46:50  grosch
 * Initial revision
 * 
 *)

(* Ich, Doktor Josef Grosch, Informatiker, Sept. 1987 *)

IMPLEMENTATION MODULE Checks;

FROM IO		IMPORT StdError, WriteS, WriteI, WriteNl;
FROM System	IMPORT ErrNum;

PROCEDURE ErrorCheck (s: ARRAY OF CHAR; n: INTEGER);
   BEGIN
      IF n < 0 THEN
	 WriteS (StdError, s);
	 WriteS (StdError, " : ");
	 WriteI (StdError, n, 2);
	 WriteS (StdError, ", errno	= ");
	 WriteI (StdError, ErrNum (), 2);
	 WriteNl (StdError);
      END;
   END ErrorCheck;

END Checks.
