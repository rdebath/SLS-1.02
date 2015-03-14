(* $Id: Source.mi,v 1.0 1992/08/07 14:42:03 grosch rel $ *)

(* $Log: Source.mi,v $
# Revision 1.0  1992/08/07  14:42:03  grosch
# Initial revision
#
 *)

(* Ich, Doktor Josef Grosch, Informatiker, Juli 1992 *)

IMPLEMENTATION MODULE Source;

FROM SYSTEM	IMPORT ADDRESS;
FROM System	IMPORT tFile, OpenInput, Read, Close;

PROCEDURE BeginSource (FileName: ARRAY OF CHAR): tFile;
   BEGIN
      RETURN OpenInput (FileName);
   END BeginSource;

PROCEDURE GetLine (File: tFile; Buffer: ADDRESS; Size: CARDINAL): INTEGER;
   CONST IgnoreChar = ' ';
   VAR n	: INTEGER;
   VAR BufferPtr: POINTER TO ARRAY [0..30000] OF CHAR;
   BEGIN
   (* # ifdef Dialog
      n := Read (File, Buffer, Size);
      (* Add dummy after newline character in order to supply a lookahead for rex. *)
      (* This way newline tokens are recognized without typing an extra line.      *)
      BufferPtr := Buffer;
      IF (n > 0) AND (BufferPtr^[n - 1] = 012C) THEN BufferPtr^[n] := IgnoreChar; INC (n); END;
      RETURN n;
      # else *)
      RETURN Read (File, Buffer, Size);
   (* # endif *)
   END GetLine;

PROCEDURE CloseSource (File: tFile);
   BEGIN
      Close (File);
   END CloseSource;

END Source.
