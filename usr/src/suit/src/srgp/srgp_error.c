#include "HEADERS.h"
#include "srgplocal.h"


#ifdef X11
static
char *(srgp_errmsgs[]) =
  {
   "UNUSED",
   "The canvas table is full: your attempt to create a canvas is ignored.\n",
   "You may not delete the screen canvas -- its life is too valuable!\n",
   "You may not delete the canvas which is currently active.\n",
   "SRGP is already enabled!  You can't reinitialize or change table sizes.\n",
   "SRGP is not enabled yet!  Have you heard about SRGP_begin() ?\n",
   "You sent a bad rectangle (%d,%d,%d,%d) (either your l>r, or your b>t).\n",
   "You sent a bad font index (%d) (either negative or too large).\n",
   "You sent a bad font index (%d) (that entry of font-table is undefined).\n",
   "You sent a bad count (%d) (it was negative).\n",
   "You sent a bad line-style spec (%d) (please use our named constants).\n",
   "You sent a bad marker-style spec (%d) (please use our named constants).\n",
   "You sent a bad write-mode spec (%d) (please use our named constants).\n",
   "SRGP_setColorTable: The %s of the range of pixel values you specified (%d)\n\tlies outside this machine's LUT.\n",
   "You sent a bad fill-style spec (%d) (please use our named constants).\n",
   "You sent a bad pattern index (%d).\n",
   "You sent a bad cursor index (%d).\n",
   "No known canvas possesses the index you sent.\n",
   "You sent a bad input device spec (%d).\n",
   "You sent a bad input mode spec (%d).\n",
   "You did a 'get' on a device that was NOT responsible\n\tfor the last wait_event exit.\n",
   "Whoops!  Event queue is full!  We tossed an event into oblivion...\n",
   "You sent a bad locator-echo type (%d) (please use our named constants).\n",
   "You sent a bad keyboard-processing mode (%d) (please use our named constants).\n",
   "You sent a list of %d points (either too many or too few).\n",
   "You sent a bad font filename (%s).\n",
   "I ran out of dynamic memory while allocating tables or creating a new canvas.\n",
   "NOT IMPLEMENTED YET!!!!\n",
   "X SERVER ERROR\n  (examining the core will be useful only if using X in synchronous mode):\n\t %s\n",
   "You sent a bad line-width spec (%d).\n",
   "You tried to get me to use an EMPTY entry in the pattern table.\n",
   "You sent a negative or zero marker size (%d).\n",
   "There is no more memory available for pix/bitmap pattern entries!\n",

   "UNUSED"
};

static char logmessage[] =
"%s FATAL ERROR:\n\
   %s\
\n\nI AM ABOUT TO INTENTIONALLY CRASH SO YOU CAN LOOK\n\
      AT THE ACTIVATION STACK USING A DEBUGGER.\n\n\
If your application is an SRGP application, please remember:\n\
   If the error message above says\n\
   that you sent a bad argument to a certain function,\n\
   you should run your program with tracing ON in order\n\
   to see exactly what you sent.\n\
If you already had tracing enabled, remember to look in\n\
   'SRGPlogfile' for the tracing messages.\n\n";
   
char **srgp__identifierOfMessages = srgp_errmsgs;
#endif


#ifdef THINK_C
static char logmessage[] = "%s FATAL ERROR:\n   %s\n";

int srgp__identifierOfMessages = 128;
#endif




#ifdef THINK_C
void SRGP__cleanupMacMemory (void);
#endif

static int zero = 0;

char *srgp__identifierOfPackage = "SRGP";

void
SRGP__error (errtype, arg1, arg2, arg3, arg4, arg5)
int errtype, arg1, arg2, arg3, arg4, arg5;
{
   int item;
   char processedmessage[400];
#ifdef THINK_C
   char rawmessage[256];
#endif
#ifdef X11
   char *rawmessage;
#endif

   /* SIMPLE HANDLING??? */   
   if (srgp__curErrHndlMode == NON_FATAL_ERRORS) {
      SRGP_errorOccurred = errtype;
      return;
   }

   /* DETERMINE THE MESSAGE CORRESPONDING TO THE ERROR TYPE */
#ifdef THINK_C
   GetIndString (rawmessage, srgp__identifierOfMessages, errtype);
   PtoCstr(rawmessage);
#endif
#ifdef X11
   rawmessage = srgp__identifierOfMessages[errtype];
#endif

   sprintf (processedmessage, rawmessage, arg1, arg2, arg3, arg4, arg5);

#ifdef X11
   fprintf (stderr, logmessage, srgp__identifierOfPackage, processedmessage);
#endif
#ifdef THINK_C
   SRGP__cleanupMacMemory();
   CtoPstr(processedmessage);
   CtoPstr(srgp__identifierOfPackage);
   ParamText (srgp__identifierOfPackage,processedmessage,NULL,NULL);
   if ((item=StopAlert (8811, NULL)) == 5)
      exit (0/zero);  /* will crash here intentionally */
   DisposeWindow (srgpmac__cwindow);
   ExitToShell();
#else
   abort ();
#endif
}
