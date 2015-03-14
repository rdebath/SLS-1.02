#include "HEADERS.h"
#include "srgplocal.h"


void
SRGP_loadCursor (int index, int shape)
{
   DEBUG_AIDS{
      SRGP_trace (SRGP_logStream, "SRGP_loadCursor %d %d\n", index, shape);
      srgp_check_system_state();
      srgp_check_cursor_index(index);
      LeaveIfNonFatalErr();
   }

#ifdef X11
   srgp__cursorTable[index] = XCreateFontCursor (srgpx__display, shape);
#endif

#ifdef THINK_C
   srgp__cursorTable[index] = GetCursor (shape);
#endif

   SRGP__updateLocatorCursorShape ();
}




#ifdef THINK_C
#define cursortype CursHandle
#endif
#ifdef X11
#define cursortype Cursor
#endif

void SRGP__initCursorTable (void)
{
   register i;
 
   for (i=0; i<=MAX_CURSOR_INDEX; i++)
      srgp__cursorTable[i] = (cursortype)NULL;

#ifdef THINK_C
   srgp__cursorTable[0] = (CursHandle)(-1);  
        /* special value representing the arrow cursor */
#endif
}
