(* $Id: Texts.mi,v 1.2 1992/08/07 14:43:04 grosch rel $ *)

(* $Log: Texts.mi,v $
 * Revision 1.2  1992/08/07  14:43:04  grosch
 * added procedure IsEmpty
 *
 * Revision 1.1  1991/11/21  14:33:17  grosch
 * new version of RCS on SPARC
 *
 * Revision 1.0  88/10/04  11:47:37  grosch
 * Initial revision
 * 
 *)

(* Ich, Doktor Josef Grosch, Informatiker, 31.8.1988 *)

IMPLEMENTATION MODULE Texts;

FROM SYSTEM	IMPORT ADDRESS;
FROM IO		IMPORT tFile;
FROM Lists	IMPORT MakeList, Head, Tail;
FROM Strings	IMPORT tString, WriteL;
FROM StringMem	IMPORT tStringRef, PutString, GetString;

IMPORT Lists;

PROCEDURE MakeText (VAR Text: tText);
   BEGIN
      MakeList (Text);
   END MakeText;

PROCEDURE Append (VAR Text: tText; VAR String: tString);
   BEGIN
      Lists.Append (Text, ADDRESS (PutString (String)));
   END Append;

PROCEDURE Insert (VAR Text: tText; VAR String: tString);
   BEGIN
      Lists.Insert (Text, ADDRESS (PutString (String)));
   END Insert;

PROCEDURE IsEmpty (VAR Text: tText): BOOLEAN;
   BEGIN
      RETURN Lists.IsEmpty (Text);
   END IsEmpty;

PROCEDURE WriteText (f: tFile; Text: tText);
   VAR String	: tString;
   BEGIN
      WHILE NOT Lists.IsEmpty (Text) DO
	 GetString (tStringRef (Head (Text)), String);
	 WriteL (f, String);
	 Tail (Text);
      END;
   END WriteText;

END Texts.
