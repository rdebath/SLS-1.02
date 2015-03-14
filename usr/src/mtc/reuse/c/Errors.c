/* $Id: Errors.c,v 1.1 1992/08/13 12:29:12 grosch rel $ */

/* $Log: Errors.c,v $
 * Revision 1.1  1992/08/13  12:29:12  grosch
 * fix bugs with ANSI C
 *
 * Revision 1.0  1992/08/07  14:31:40  grosch
 * Initial revision
 *
 */

/* Ich, Doktor Josef Grosch, Informatiker, Juli 1992 */

static char rcsid [] = "$Id: Errors.c,v 1.1 1992/08/13 12:29:12 grosch rel $";

# include "Errors.h"

# ifdef __cplusplus
extern "C" {
#  include "System.h"
#  include "Memory.h"
#  include "Sets.h"
#  include "Idents.h"
}
# else
#  include "System.h"
#  include "Memory.h"
#  include "Sets.h"
#  include "Idents.h"
# endif

# define MaxError	100

static void yyExit () { Exit (1); }

void (* Errors_Exit) () = yyExit;

typedef struct {
   tPosition	Position;
   bool		IsErrorCode;
   short	ErrorNumber;
   short	ErrorCode;
   short	ErrorClass;
   short	InfoClass;
   union {
      int	vInteger;
      short	vShort;
      long	vLong;
      float	vReal;
      bool	vBoolean;
      char	vCharacter;
      tStringRef vString;
      tSet *	vSet;
      tIdent	vIdent;
   } Info;
} tError;

static void WriteHead	ARGS((tPosition Position, int ErrorClass));
static void WriteCode	ARGS((int ErrorCode));
static void WriteInfo	ARGS((int InfoClass, char * Info));
static void WriteMessage ARGS((bool IsErrorCode, int ErrorCode, int ErrorClass, tPosition Position, int InfoClass, char * Info));
static void StoreMessage ARGS((bool IsErrorCode, int ErrorCode, int ErrorClass, tPosition Position, int InfoClass, char * Info));
static int IsLess	ARGS((tError * i, tError * j));

static tError	ErrorTable [MaxError + 1];
static int	MessageCount;
static bool	IsStore		= false;
static void (*	HandleMessage) ARGS((bool IsErrorCode, int ErrorCode, int ErrorClass, tPosition Position, int InfoClass, char * Info)) = WriteMessage;
static FILE *	Out			= stderr;

void ErrorMessage
# if defined __STDC__ | defined __cplusplus
   (int ErrorCode, int ErrorClass, tPosition Position)
# else
   (ErrorCode, ErrorClass, Position)
   int ErrorCode, ErrorClass; tPosition Position;
# endif
{
   (* HandleMessage) (true, ErrorCode, ErrorClass, Position, xxNone, NULL);
}

void ErrorMessageI
# if defined __STDC__ | defined __cplusplus
   (int ErrorCode, int ErrorClass, tPosition Position, int InfoClass, char * Info)
# else
   (ErrorCode, ErrorClass, Position, InfoClass, Info)
   int ErrorCode, ErrorClass; tPosition Position; int InfoClass; char * Info;
# endif
{
   (* HandleMessage) (true, ErrorCode, ErrorClass, Position, InfoClass, Info);
}

void Message
# if defined __STDC__ | defined __cplusplus
   (char * ErrorText, int ErrorClass, tPosition Position)
# else
   (ErrorText, ErrorClass, Position)
   char * ErrorText; int ErrorClass; tPosition Position;
# endif
{
   (* HandleMessage) (false, MakeIdent (ErrorText, strlen (ErrorText)), ErrorClass, Position, xxNone, NULL);
}

void MessageI
# if defined __STDC__ | defined __cplusplus
   (char * ErrorText, int ErrorClass, tPosition Position, int InfoClass, char * Info)
# else
   (ErrorText, ErrorClass, Position, InfoClass, Info)
   char * ErrorText; int ErrorClass; tPosition Position; int InfoClass; char * Info;
# endif
{
   (* HandleMessage) (false, MakeIdent (ErrorText, strlen (ErrorText)), ErrorClass, Position, InfoClass, Info);
}

static void WriteHead
# if defined __STDC__ | defined __cplusplus
   (tPosition Position, int ErrorClass)
# else
   (Position, ErrorClass) tPosition Position; int ErrorClass;
# endif
{
   WritePosition (Out, Position);
   (void) fputs (": ", Out);
   switch (ErrorClass) {
   case xxFatal		: (void) fputs ("Fatal       ", Out); break;
   case xxRestriction	: (void) fputs ("Restriction ", Out); break;
   case xxError		: (void) fputs ("Error       ", Out); break;
   case xxWarning	: (void) fputs ("Warning     ", Out); break;
   case xxRepair	: (void) fputs ("Repair      ", Out); break;
   case xxNote		: (void) fputs ("Note        ", Out); break;
   case xxInformation	: (void) fputs ("Information ", Out); break;
   default		: (void) fprintf (Out, "Error class: %d ", ErrorClass);
   }
}

static void WriteCode
# if defined __STDC__ | defined __cplusplus
   (int ErrorCode)
# else
   (ErrorCode) int ErrorCode;
# endif
{
   switch (ErrorCode) {
   case xxNoText	: break;
   case xxSyntaxError	: (void) fputs ("syntax error"		, Out); break;
   case xxExpectedTokens: (void) fputs ("expected tokens"	, Out); break;
   case xxRestartPoint	: (void) fputs ("restart point"		, Out); break;
   case xxTokenInserted	: (void) fputs ("token inserted "	, Out); break;
   case xxTooManyErrors	: (void) fputs ("too many errors "	, Out); break;
   default		: (void) fprintf (Out, " error code: %d", ErrorCode);
   }
}

static void WriteInfo
# if defined __STDC__ | defined __cplusplus
   (int InfoClass, char * Info)
# else
   (InfoClass, Info) int InfoClass; char * Info;
# endif
{
   int i;
   if (InfoClass == xxNone) return;
   (void) fputs (": ", Out);
   switch (InfoClass) {
   case xxInteger	: (void) fprintf (Out, "%d", * (int *) Info); break;
   case xxShort		: i =  * (short *) Info; (void) fprintf (Out, "%d", i); break;
   case xxLong		: (void) fprintf (Out, "%ld", * (long *) Info); break;
   case xxReal		: (void) fprintf (Out, "%e", * (float *) Info); break;
   case xxBoolean	: (void) fprintf (Out, "%c", * (bool *) Info ? 'T' : 'F'); break;
   case xxCharacter	: (void) fprintf (Out, "%c", * Info); break;
   case xxString	: (void) fputs	 (Info, Out); break;
   case xxSet		: WriteSet	 (Out, (tSet *) Info); break;
   case xxIdent		: WriteIdent	 (Out, * (tIdent *) Info); break;
   default		: (void) fprintf (Out, "info class: %d", InfoClass);
   }
}

static void WriteMessage
# if defined __STDC__ | defined __cplusplus
   (bool IsErrorCode, int ErrorCode, int ErrorClass, tPosition Position, int InfoClass, char * Info)
# else
   (IsErrorCode, ErrorCode, ErrorClass, Position, InfoClass, Info)
   bool IsErrorCode; int ErrorCode, ErrorClass; tPosition Position; int InfoClass; char * Info;
# endif
{
  WriteHead (Position, ErrorClass);
  if (IsErrorCode) WriteCode (ErrorCode); else WriteIdent (Out, ErrorCode);
  WriteInfo (InfoClass, Info);
  (void) fputc ('\n', Out);
  if (ErrorClass == xxFatal && ! IsStore) (* Errors_Exit) ();
}

void WriteMessages
# if defined __STDC__ | defined __cplusplus
   (FILE * File)
# else
   (File) FILE * File;
# endif
{
   int i;
   char * Info;
   char s [256];

   qsort ((char *) & ErrorTable [1], MessageCount, sizeof (tError), IsLess);
   Out = File;
   for (i = 1; i <= MessageCount; i ++) {
      register tError * With = & ErrorTable [i];

      switch (With->InfoClass) {
      case xxInteger	: Info = (char *) & With->Info.vInteger	; break;
      case xxShort	: Info = (char *) & With->Info.vShort	; break;
      case xxLong	: Info = (char *) & With->Info.vLong	; break;
      case xxReal	: Info = (char *) & With->Info.vReal	; break;
      case xxBoolean	: Info = (char *) & With->Info.vBoolean	; break;
      case xxCharacter	: Info = (char *) & With->Info.vCharacter; break;
      case xxString	: StGetString (With->Info.vString, s); Info = s; break;
      case xxSet	: Info = (char *) With->Info.vSet; break;
      case xxIdent	: Info = (char *) & With->Info.vIdent	; break;
      }
      WriteMessage (With->IsErrorCode, With->ErrorCode, With->ErrorClass, With->Position, With->InfoClass, Info);
   }
   Out = stderr;
}

static void StoreMessage
# if defined __STDC__ | defined __cplusplus
(bool IsErrorCode, int ErrorCode, int ErrorClass, tPosition Position, int InfoClass, char * Info)
# else
(IsErrorCode, ErrorCode, ErrorClass, Position, InfoClass, Info)
bool IsErrorCode; int ErrorCode, ErrorClass; tPosition Position; int InfoClass; char * Info;
# endif
{
  if (MessageCount < MaxError) {
    MessageCount ++;
    {
      register tError * With = & ErrorTable [MessageCount];

      With->Position	= Position;
      With->IsErrorCode	= IsErrorCode;
      With->ErrorNumber	= MessageCount;
      With->ErrorCode	= ErrorCode;
      With->ErrorClass	= ErrorClass;
      With->InfoClass	= InfoClass;
      switch (With->InfoClass) {
      case xxInteger	: With->Info.vInteger	= * (int	*) Info; break;
      case xxShort	: With->Info.vShort	= * (short	*) Info; break;
      case xxLong	: With->Info.vLong	= * (long	*) Info; break;
      case xxReal	: With->Info.vReal	= * (float	*) Info; break;
      case xxBoolean	: With->Info.vBoolean	= * (bool	*) Info; break;
      case xxCharacter	: With->Info.vCharacter	= * (char	*) Info; break;
      case xxString	: With->Info.vString	= PutString (Info, strlen (Info)); break;
      case xxSet	: With->Info.vSet = (tSet *) Alloc ((unsigned long) sizeof (tSet));
        		  MakeSet (With->Info.vSet, Size ((tSet *) Info));
        		  Assign (With->Info.vSet, (tSet *) Info); break;
      case xxIdent	: With->Info.vIdent	= * (tIdent	*) Info; break;
      }
    }
  } else {
    {
      register tError * With = & ErrorTable [MessageCount];

      With->IsErrorCode	= true;
      With->ErrorCode	= xxTooManyErrors;
      With->ErrorClass	= xxRestriction;
      With->InfoClass	= xxNone;
    }
  }
  if (ErrorClass == xxFatal) { WriteMessages (stderr); (* Errors_Exit) (); }
}

static int IsLess
# if defined __STDC__ | defined __cplusplus
   (tError * i, tError * j)
# else
   (i, j) tError * i, * j;
# endif
{
  register int r = Compare (i->Position, j->Position);
  return r != 0 ? r : i->ErrorNumber - j->ErrorNumber;
}

void StoreMessages
# if defined __STDC__ | defined __cplusplus
   (bool Store)
# else
   (Store) bool Store;
# endif
{
  if (Store) {
    HandleMessage = StoreMessage;
    MessageCount  = 0;
  } else {
    HandleMessage = WriteMessage;
  }
  IsStore = Store;
}
