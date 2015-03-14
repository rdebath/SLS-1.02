(* $Id: SysIODrv.mi,v 1.2 1992/01/30 13:23:29 grosch rel $ *)

(* $Log: SysIODrv.mi,v $
 * Revision 1.2  1992/01/30  13:23:29  grosch
 * redesign of interface to operating system
 *
 * Revision 1.1  1991/11/21  14:33:17  grosch
 * new version of RCS on SPARC
 *
 * Revision 1.0  88/10/04  11:47:28  grosch
 * Initial revision
 * 
 *)

(* Ich, Doktor Josef Grosch, Informatiker, Sept. 1987 *)

MODULE SysIODrv;

FROM SYSTEM	IMPORT
   ADDRESS	, ADR		;

FROM System	IMPORT 
   tFile	, OpenInput	, OpenOutput	, Read		,
   Write	, Close		;

FROM Checks	IMPORT
   ErrorCheck	;

VAR
   b	: ARRAY [0..1023] OF CHAR;
   f	: tFile;
   n	: INTEGER;

BEGIN
   f := OpenInput ("Makefile");
   ErrorCheck ("OpenInput", f);
   n := Read (f, ADR (b), 1024);
   ErrorCheck ("Read", n);
   Close (f);

   f := OpenOutput ("t");
   ErrorCheck ("OpenOutput", f);
   n := Write (f, ADR (b), 1024);
   ErrorCheck ("Write", n);
   Close (f);
END SysIODrv.
