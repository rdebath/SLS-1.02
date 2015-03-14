#include "HEADERS.h"
#include "srgplocal.h"
#include <assert.h>
#include "ChooseWhichQuickDraw.h"

/** THIS FILE IS FOR MAC IMPLEMENTATION ONLY
It contains conditionally compiled code based on the definition of
the COLOR_QUICKDRAW macro.
**/

#ifdef COLOR_QUICKDRAW
	static ReqListRec **srgpmac__reqlist;
	static CTabHandle srgpmac__savectab, srgpmac__applctab;

	static boolean applTableCurrentlyActivated = TRUE;
#endif



void SRGP__activateApplColorTable (void)
{
#ifdef COLOR_QUICKDRAW
   if (applTableCurrentlyActivated) return;
   
	SaveEntries (NULL, srgpmac__savectab, *srgpmac__reqlist);
	RestoreEntries (srgpmac__applctab, NULL, *srgpmac__reqlist);
	applTableCurrentlyActivated = TRUE;
#endif
}



void SRGP__deactivateApplColorTable (void)
{
#ifdef COLOR_QUICKDRAW
   if ( ! applTableCurrentlyActivated) return;
  
	RestoreEntries (srgpmac__savectab, NULL, *srgpmac__reqlist);
	applTableCurrentlyActivated = FALSE;
#endif
}



#ifdef COLOR_QUICKDRAW
static void InitReqList (int first, int last, ReqListRec ***reqlistptr)
{
	register i;
	
	(*reqlistptr) = (ReqListRec**)
		(NewHandle (sizeof(ReqListRec) + (last-first)*sizeof(short)));
	(**reqlistptr)->reqLSize = last-first;
	for (i=first; i<=last; i++)
		(**reqlistptr)->reqLData[i-first] = i;
}


static void InitMacColorTable (int first, int last, CTabHandle *ctabptr)
{
	register i;
 	CTabHandle ctab;
 	
	*ctabptr = ctab = (CTabHandle)
	   NewHandle (sizeof(ColorTable) + (last-first)*sizeof(ColorSpec));
	(*ctab)->ctSize = last-first;
	(*ctab)->ctSeed = GetCTSeed();
	(*ctab)->ctFlags = (short) (((unsigned short)(-1))<<15);
	for (i=first; i<=last; i++)
		(*ctab)->ctTable[i-first].value = i;
}
#endif


void SRGP_loadColorTable
   (int startentry, int count,
    unsigned short *redi, 
    unsigned short *greeni,
    unsigned short *bluei)
{
   register int i, j;
   RGBColor rgb;
   ReqListRec **rl;
   CTabHandle ctab;


   DEBUG_AIDS{
      SRGP_trace (SRGP_logStream, "SRGP_loadColorTable  %d  %d  %x %x %x\n",
		  startentry, count, redi, greeni, bluei);

      /* PERFORM CHECKING LEGALITY OF THE RANGE OF INDICES. */
      srgp_check_pixel_value (startentry, "start");
      srgp_check_pixel_value (startentry+count-1, "end");
      LeaveIfNonFatalErr();
   }

#ifdef COLOR_QUICKDRAW
   InitMacColorTable (startentry, startentry+count-1, &ctab);

   for (i=startentry,j=0; j<count; i++,j++){
      rgb.red = redi[j];
      rgb.green = greeni[j];
      rgb.blue = bluei[j];
      (**srgpmac__applctab).ctTable[i].rgb = rgb;
      (**ctab).ctTable[j].rgb = rgb;
   }
   
   InitReqList (startentry, startentry+count-1, &rl);
	RestoreEntries (ctab, NULL, *rl);
	DisposHandle (rl);
	DisposHandle (ctab);
#endif
}




void
SRGP_inquireColorTable 
   (int startentry, int count,
    unsigned short *redi, 
    unsigned short *greeni,
    unsigned short *bluei)
{
   RGBColor rgb;
   register i, j;

   DEBUG_AIDS{
      /* PERFORM CHECKING LEGALITY OF THE RANGE OF INDICES. */
      srgp_check_pixel_value (startentry, "start");
      srgp_check_pixel_value (startentry+count-1, "end");
      LeaveIfNonFatalErr();
   }


#ifdef COLOR_QUICKDRAW
   for (i=startentry, j=0; j<count; i++,j++) {
      rgb = (**srgpmac__applctab).ctTable[i].rgb;
      redi[j]=rgb.red;
      greeni[j]=rgb.green;
      bluei[j]=rgb.blue;
   }
#endif
}



#include <ctype.h>

void
SRGP_loadCommonColor (entry, name)
int entry;
char *name;   /* Null-terminated string of characters */
{
   unsigned short r, g, b;
   register i;
   char colordescrstr[256], nameinlowercase[50], *cptr, *lcptr;
   
   DEBUG_AIDS{
      SRGP_trace (SRGP_logStream, 
		  "SRGP_loadCommonColor  %d  %s\n", entry, name);
      srgp_check_pixel_value (entry, "start/end");
      LeaveIfNonFatalErr();
   }
   
#ifdef COLOR_QUICKDRAW
   for (cptr=name, lcptr=nameinlowercase; *cptr; cptr++)
      if (*cptr != ' ')
         *(lcptr++) = tolower(*cptr);
   *lcptr = '\0';
   
   i=1;
   while (1) {
      GetIndString (colordescrstr, 130, i);
      PtoCstr(colordescrstr);
      if (strlen(colordescrstr)==0) {
         /* OFF THE EDGE OF THE STRING LIST: bad color name */
         SRGP__error (ERR_BAD_COLOR_NAME, name);
         LeaveIfNonFatalErr();
      }
      else if (!strcmp(colordescrstr+12,nameinlowercase)) {
         /* FOUND! */
         assert (3==sscanf(colordescrstr, "%hu %hu %hu", &r, &g, &b));
         r = r<<8;
         g = g<<8;
         b = b<<8;
         PUSH_TRACE;
         SRGP_loadColorTable (entry, 1, &r, &g, &b);
         POP_TRACE;
         break;
      }
      else
         i++;
   }
#endif
}



static char message[] =
   "Insane parameter to SRGP_begin:  negative number of planes!";


void 
SRGP__initColor (int requested_planes)
{
   register i;
   SysEnvRec theWorld;
   
   srgp__base_colorindex = 0;   /* always true */
   SRGP_BLACK = 1;
   SRGP_WHITE = 0;
   
   SysEnvirons (1, &theWorld);
   
#ifdef COLOR_QUICKDRAW
   if ( ! theWorld.hasColorQD) {
      StopAlert (14369, NULL);
      exit (1);
   }
   
   srgp__available_depth = (**(((CGrafPtr)srgpmac__cwindow)->portPixMap)).pixelSize;
   
   {
      ColorSpec cspec;
      int actual__max_pixel_value;
      
      if (requested_planes < 0)
	 ReportSpecialError (message, FATAL);

      if ((requested_planes == 0) || 
	  (requested_planes > srgp__available_depth)) 
	 srgp__application_depth = srgp__available_depth;
      else
	 srgp__application_depth = requested_planes;
      srgp__max_pixel_value =
         actual__max_pixel_value = (1 << srgp__available_depth) - 1;
      /* The above line seems to ignore the depth the appl asked for;
         you'll see that later in this function I clean up. */
      
      
      InitReqList (0, actual__max_pixel_value, &srgpmac__reqlist);
      InitMacColorTable (0, actual__max_pixel_value, &srgpmac__savectab);
      InitMacColorTable (0, actual__max_pixel_value, &srgpmac__applctab);
      
      SaveEntries (NULL, srgpmac__savectab, *srgpmac__reqlist);
      
      /* Only a few entries of application LUT are init'd.
      /* Because of the Mac's non-support of XOR'ing pixel values, I must set
         both edges of the actual (entire) color table, so at least xor'ing
         1 ontop of 1 produces white, if the appl doesn't play with LUT entries 0,1 */
      PUSH_TRACE;
      
      SRGP_loadCommonColor (0, "white");
      SRGP_loadCommonColor (1, "black");
      SRGP_loadCommonColor (actual__max_pixel_value, "black");
      SRGP_loadCommonColor (actual__max_pixel_value-1, "white");
      
      srgp__max_pixel_value = (1 << srgp__application_depth) - 1;
      
      POP_TRACE;
   }
#else
   if ((theWorld.hasColorQD) && (requested_planes > 1)) {
      StopAlert (20065, NULL);
      exit (1);
   }
   srgp__available_depth = 1;
   srgp__application_depth = 1;
   srgp__max_pixel_value = 1;
#endif
}




void SRGP__cleanupColor()
{
	SRGP__deactivateApplColorTable ();
}

