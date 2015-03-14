/* (C) Copyright 1990, 1991, 1992 the University of Virginia */

#include "suit.h"

#ifdef RS6000
#define _POSIX_SOURCE
#endif

#include <sys/types.h>
#include <dirent.h>
#include <sys/stat.h>

#ifdef IBM_PC
#include <dir.h>
#define S_ISDIR(MODE) (MODE & S_IFDIR)
#endif

#ifdef IBM_PC
#define SEPARATOR_CHAR '\\'
#define SEPARATOR_STRING "\\"
#define UP_ONE_DIR_STRING "..\\ (Go up 1 directory level)"
#else
#define SEPARATOR_CHAR '/'
#define SEPARATOR_STRING "/"
#define UP_ONE_DIR_STRING "../ (Go up 1 directory level)"
#endif


/*--------------------------------------------------------------------------------*/
/*  This is a routine which does file globbing, might prove handy                 */
/*--------------------------------------------------------------------------------*/

/*
 *  input: "str" string will attempted to be matched
 *
 *         "pattern" string with wildcards that will match against "str".
 *
 *   wild:
 *	       *           = match 0 or more occurances of anything
 *	       [abc]       = match anyof "abc" (ranges supported)
 *	       {xx,yy,zz}  = match anyof "xx", "yy", or "zz"
 *             ?           = match any character
 *
 *  returns: whether str matches pattern
 */


boolean glob (char *str, char *pattern)
{
    char c,*cp;
    int	done=FALSE, ret_code, ok;
    
    if (pattern == NULL)
	return TRUE;
    if (strlen(pattern) == 0)
	return TRUE;
    if (strcmp(pattern, "*") == 0)
	return TRUE;
    
    while ((*pattern != '\0') && (!done) && 
	   (((*str=='\0') && ((*pattern=='{') || (*pattern=='*'))) || (*str!='\0'))) {
	switch (*pattern) {
	case '\\':
	    pattern++;
	    if (*pattern != '\0')
		pattern++;
	    break;
	case '*':
	    pattern++;
	    ret_code=FALSE;
	    while ((*str != '\0') && (!(ret_code=glob(str++,pattern)))) ;
	    if (ret_code) {
		while (*str != '\0') str++;
		while (*pattern != '\0') pattern++;
	    }
	    break;
	case '[':
	    pattern++;
	repeat:
	    if ((*pattern == '\0') || (*pattern == ']')) {
		done=TRUE;
		break;
	    } 
	    if (*pattern == '\\') {
		pattern++;
		if (*pattern == '\0') {
		    done=TRUE;
		    break;
		}
	    }
	    if (*(pattern+1) == '-') {
		c = *pattern;
		pattern+=2;
		if (*pattern == ']') {
		    done=TRUE;
		    break;
		}
		if (*pattern == '\\') {
		    pattern++;
		    if (*pattern == '\0') {
			done=TRUE;
			break;
		    }
		}
		if ((*str < c) || (*str > *pattern)) {
		    pattern++;
		    goto repeat;
		} 
	    } else if (*pattern != *str) {
		pattern++;
		goto repeat;
	    }
	    pattern++;
	    while ((*pattern != ']') && (*pattern != '\0')) {
		if ((*pattern == '\\') && (*(pattern+1) != '\0'))
		    pattern++;
		pattern++;
	    }
	    if (*pattern != '\0') {
		pattern++;
		str++;
	    }
	    break;
	case '?':
	    pattern++;
	    str++;
	    break;
	case '{':
	    pattern++;
	    while ((*pattern != '}') && (*pattern!='\0')) {
		cp = str;
		ok = TRUE;
		while (ok && (*cp != '\0') && (*pattern!='\0') &&
		       (*pattern!=',') && (*pattern!='}')) {
		    if (*pattern == '\\')
			pattern++;
		    ok=(*pattern == *cp);
		    cp++;
		    pattern++;
		}
		if (*pattern=='\0') {
		    ok=FALSE;
		    done=TRUE;
		    break;
		} else if (ok) {
		    str=cp;
		    while ((*pattern!='}') && (*pattern!='\0')) {
			pattern++;
			if (*pattern=='\\') {
			    pattern++;
			    if (*pattern=='}')
				pattern++;
			}
		    }
		} else {
		    while ((*pattern!='}') && (*pattern!=',') &&
			   (*pattern!='\0')) {
			pattern++;
			if (*pattern=='\\') {
			    pattern++;
			    if ((*pattern=='}') || (*pattern==','))
				pattern++;
			}
		    }
		}
		if (*pattern!='\0')
		    pattern++;
	    }
	    break;
	default:
	    if (*str == *pattern) {
		str++;
		pattern++;
	    } else {
		done=TRUE;
	    }
	}
    }
    while (*pattern == '*') pattern++;
    return ((*str == '\0') && (*pattern == '\0'));
}

/*--------------------------------------------------------------------------------*/


typedef struct {
    char *name;
    boolean directory;
} file_entry, *file_entry_ptr;


PRIVATE SUIT_textList FileNames = NULL;
PRIVATE unsigned int DirectoryCount;   /* the number of subdirectories in the current directory */
PRIVATE unsigned int FileCount;        /* the number of files in the current directory */
PRIVATE char *CurrentGlobPattern;      /* if NULL, don't do globbing */


int CompareTwoFiles (file_entry_ptr first, file_entry_ptr second)
{
    return (SUIT_caseInsensitiveCompare (first->name, second->name));
}


file_entry_ptr DummyEntry (char *name)
{
    static file_entry e;
    e.name = name;
    return &e;
}


/*--------------------------------------------------------------------------------*/
/*  This is the nasty archiecture-dependent file/directory stuff.                 */
/*--------------------------------------------------------------------------------*/


#define IS_A_DIRECTORY  1
#define IS_A_FILE       2

int ReadDirectory (char *currentDir)
{
    DIR *dirp = opendir (currentDir);
    struct dirent *dp;
    struct stat stbuf;
    int startSort = 0;

    if (dirp == NULL) {
	stat(currentDir, &stbuf);
	return (S_ISDIR(stbuf.st_mode))? -1 : IS_A_FILE;
    }

    GP_pushGraphicsState();
    GP_setCursor (WATCH_CURSOR);

    if (FileNames != NULL) {
	int i;
	for (i=0; i < DynSize(FileNames); i++) {
	    char *name = * (char **) DynGet(FileNames, i);
	    SUIT_free ((void *)name);
	}
	DynDestroy (FileNames);
    }
    FileNames = SUIT_defTextList(NULL, 0);

    /* add directory ".." by hand, then skip all other entries which start with "." */
	if (
#ifdef IBM_PC
		!(
		  ((strlen(currentDir) == 2)
		   || (strlen(currentDir) == 3))
		  && (currentDir[1] == ':'))
        &&
#endif
		!SUIT_stringsMatch (currentDir, SEPARATOR_STRING)
		) {   /* unless we're at root */
	SUIT_appendToTextList (FileNames, UP_ONE_DIR_STRING);
	DirectoryCount = 1;
	FileCount = 0;
	startSort = 1;
    }

    for (dp = readdir (dirp); dp != NULL; dp = readdir (dirp)) {
	if (dp->d_name[0] != '.') {
	    char fullPathName[500];
		strcpy (fullPathName, currentDir);
	    strcat (fullPathName, dp->d_name);
	    stat (fullPathName, &stbuf);
	    if (S_ISDIR(stbuf.st_mode)) {
		char buf[500];
		strcpy (buf, dp->d_name);
		strcat (buf, SEPARATOR_STRING);
		SUIT_appendToTextList (FileNames, buf);
		DirectoryCount++;
	    } else if (glob(dp->d_name, CurrentGlobPattern)) {
		SUIT_appendToTextList (FileNames, dp->d_name);
		FileCount++;
	    }
	}
    }
    if (DynSize(FileNames) > 1)
	DynQsort (FileNames, startSort, DynHigh(FileNames), CompareTwoFiles);

    if (dirp != NULL)
	closedir (dirp);

    GP_popGraphicsState();

    return IS_A_DIRECTORY;
}


/*--------------------------------------------------------------------------------*/


char *ChangeDirectory (char *currentDir)
{
    char *afterDots, *beforeDots;
    char upDirString[4];

    /* dennis hack-o-rama: brain-dead dec compiler! */
    upDirString[0] = SEPARATOR_CHAR;  /* who the heck spelled this anyway?!?! */
    upDirString[1] = '.';             /* separator */
    upDirString[2] = '.';
    upDirString[3] = '\0';

    afterDots = strstr(currentDir, upDirString);
    if (afterDots == NULL)
	return currentDir;	/* no .. in the string, so it's OK */
    if (afterDots == currentDir)
	return NULL;		/* we're at the root directory */
#ifdef IBM_PC
	if ((afterDots == (currentDir+2)) && (currentDir[1] == ':'))
		return NULL;		/* we're at the root directory */
#endif
	for (beforeDots = afterDots-1; *beforeDots != SEPARATOR_CHAR && beforeDots > currentDir ; beforeDots--) ;
	afterDots = strchr(afterDots+1, SEPARATOR_CHAR); /* find the next occurence of SEPARATOR_CHAR */
    if (afterDots == NULL)
	afterDots = "";
	strcpy (beforeDots+1, afterDots); /* compact the area in between out of the string */
    return ChangeDirectory(currentDir);
}


char *getDirectoryCount (void)
{
    static char countLabel[100];
    
    sprintf (countLabel, "%d Directories, %d Files", DirectoryCount, FileCount);
    return countLabel;
}



void writeFileInTypeinBox (SUIT_object scrollBoxList)
{
    SUIT_object textBox = SUIT_name (SUIT_relativeName(SUIT_getParent(scrollBoxList), "type in box"));
    char *temp = SUIT_copyString (SUIT_getText(scrollBoxList,CURRENT_VALUE));
	char *slash = strchr (temp, SEPARATOR_CHAR);
    if (slash != NULL)
	slash[0] = '\0';
    SUIT_setText (textBox, CURRENT_VALUE, temp);
    SUIT_setInteger (textBox, "cursor position", 0);
    SUIT_free(temp);
}



void remakeScrollBox (SUIT_object child)
{
    SUIT_object bboard = SUIT_getParent(child);
    SUIT_object scrollBoxList = SUIT_name (SUIT_relativeName(bboard,"scroll box"));

    SUIT_setTextList (scrollBoxList, LIST, FileNames);
    SUIT_setText (scrollBoxList, CURRENT_VALUE, "");
    SUIT_resetScrollableListToTop (scrollBoxList);
}



void ChooseJustification (SUIT_object dirLabel)
{
    SUIT_viewport vp;
    int w, a, d;

    vp = OBJECT_VIEWPORT(dirLabel);
    GP_setFont (SUIT_getFont (dirLabel, FONT));
    SRGP_inquireTextExtent (SUIT_getText(dirLabel,LABEL), &w, &a, &d);
    if (w > vp.top_right.x - vp.bottom_left.x)
	SUIT_setEnumString (dirLabel, JUSTIFICATION, "right");	
    else
	SUIT_setEnumString (dirLabel, JUSTIFICATION, "left");
}



void redrawLabels (SUIT_object textbox)
{
    SUIT_object bboard = SUIT_getParent(textbox);
    SUIT_object dirLabel = SUIT_name (SUIT_relativeName(bboard, "current directory label"));
    SUIT_object countLabel = SUIT_name (SUIT_relativeName(bboard, "count label"));
    SUIT_object typeBox = SUIT_name (SUIT_relativeName(bboard, "type in box"));
    
    SUIT_setText (dirLabel, LABEL, SUIT_getText(bboard, CURRENT_DIRECTORY));
    ChooseJustification (dirLabel);

    SUIT_setText (countLabel, LABEL, getDirectoryCount());
    SUIT_setText (typeBox, CURRENT_VALUE, "");
}



PRIVATE void ChangeFilter (SUIT_object fileBox, char *pname, char *ptype, Pointer new, Pointer old)
{
    if (SUIT_stringsMatch (pname, FILE_FILTER)) {
	CurrentGlobPattern = (char *) new;
	ReadDirectory (SUIT_getText (fileBox, CURRENT_DIRECTORY));
	remakeScrollBox (SUIT_getChild (fileBox, 0));
    }
}



PRIVATE boolean processInput (SUIT_object textBox)
{
    SUIT_object bboard = SUIT_getParent(textBox);
    SUIT_callbackFunctionPtr funct;
    char *userInput = SUIT_getText(textBox,CURRENT_VALUE);
    char *theCurrentDir = SUIT_getText(bboard,CURRENT_DIRECTORY);
    char requestedDir[500];
    int status;

    if (SUIT_stringsMatch (userInput, ""))
	return FALSE;

    if (strchr(userInput, '*') != NULL ||
	strchr(userInput, '[') != NULL ||
	strchr(userInput, ']') != NULL) { /* they entered a wildcard */
	SUIT_setText (bboard, FILE_FILTER, userInput);
	remakeScrollBox (textBox);
	SUIT_setText (textBox, CURRENT_VALUE, "");
	redrawLabels(textBox);
	return FALSE;
    }

    if (SUIT_stringContains (userInput, "..") >= 0) { /* wants to go up a directory */
	strcpy (requestedDir, theCurrentDir);
	strcat (requestedDir, userInput);
	if (ChangeDirectory(requestedDir) != NULL) { /* if not the root directory */
	    CurrentGlobPattern = SUIT_getText (bboard, FILE_FILTER);
	    ReadDirectory(requestedDir);
	    SUIT_setText (bboard, CURRENT_DIRECTORY, requestedDir);
	    remakeScrollBox (textBox);
	    SUIT_setText (textBox, CURRENT_VALUE, "");
	    redrawLabels(textBox);
	}
	return FALSE;
    }

	if (
#ifdef IBM_PC
		((strlen(userInput) > 1) && (userInput[1] == ':'))
		||
#endif
		(userInput[0] == SEPARATOR_CHAR)	/* typed in an absolute pathname */
	   ) {
	strcpy (requestedDir, userInput);
	if (userInput[strlen(userInput)-1] != SEPARATOR_CHAR)
	  strcat (requestedDir, SEPARATOR_STRING);
    } else {  			/* typed in a relative pathname */
	strcpy (requestedDir, theCurrentDir);
	strcat (requestedDir, userInput);
	if (userInput[strlen(userInput)-1] != SEPARATOR_CHAR)
	  strcat (requestedDir, SEPARATOR_STRING);
    }
    
    CurrentGlobPattern = SUIT_getText (bboard, FILE_FILTER);
    if ((status = ReadDirectory(requestedDir)) == IS_A_DIRECTORY) {
	SUIT_setText (bboard, CURRENT_DIRECTORY, requestedDir);
	remakeScrollBox (textBox);
	redrawLabels(textBox);
	return FALSE;
    } else if (status == -1) {
	char buf[500];
	sprintf (buf, "Error reading directory %s.",requestedDir);
	SUIT_inform (buf);
	return FALSE;
    }

    funct = (SUIT_callbackFunctionPtr) SUIT_getFunctionPointer(bboard, CALLBACK_FUNCTION);
    if (funct != NULL)
	funct(bboard);
    return TRUE;
}



PRIVATE void processInputCallback (SUIT_object typebox)
{
    (void) processInput(typebox);
}



void addTheScrollBox (SUIT_object bboard)
{
    SUIT_object scroller;
    
    scroller = SUIT_createScrollableList (SUIT_relativeName(bboard, "scroll box"), writeFileInTypeinBox);
    SUIT_setBoolean (scroller, "inside filebox", TRUE);	/* hack */
    SUIT_addChildToObject (bboard,scroller);
    remakeScrollBox (scroller);
}



void addTheLabels (SUIT_object bboard, char *label, char *typeBoxLabel)
{
    SUIT_object titleLabel, dirLabel, countLabel, Label;
    
    if (label != NULL && label[0] != '\0') {
	Label = SUIT_createLabel (SUIT_relativeName (bboard, "main label"));
	SUIT_setText (Label, LABEL, label);
	SUIT_setBoolean (Label, SHRINK_TO_FIT, FALSE);
	SUIT_setEnumString (Label, JUSTIFICATION, "left");
	SUIT_addChildToObject (bboard, Label);
    }

    titleLabel = SUIT_createLabel (SUIT_relativeName (bboard, "type in box label"));
    SUIT_setText (titleLabel, LABEL, typeBoxLabel);
    SUIT_setBoolean (titleLabel, SHRINK_TO_FIT, FALSE);
    SUIT_setEnumString (titleLabel, JUSTIFICATION, "left");
    SUIT_addChildToObject (bboard, titleLabel);

    dirLabel = SUIT_createLabel (SUIT_relativeName (bboard, "current directory label"));
    SUIT_setText (dirLabel, LABEL, SUIT_getText(bboard,CURRENT_DIRECTORY));
    SUIT_setBoolean (dirLabel, SHRINK_TO_FIT, FALSE);
    ChooseJustification (dirLabel);
    SUIT_addChildToObject (bboard, dirLabel);

    countLabel = SUIT_createLabel (SUIT_relativeName (bboard, "count label"));
    SUIT_setText (countLabel, LABEL, getDirectoryCount());
    SUIT_setBoolean (countLabel, SHRINK_TO_FIT, FALSE);
    SUIT_setEnumString (countLabel, JUSTIFICATION, "left");
    SUIT_addChildToObject (bboard, countLabel);
}



PRIVATE void SetFileName (SUIT_object obj, char *propName, char *propType, Pointer new, Pointer old)
{
    if (SUIT_stringsMatch(propName, CURRENT_VALUE)) {
	SUIT_object parent = SUIT_getParent(obj);
	char *currentDir = SUIT_getText (parent, CURRENT_DIRECTORY);
	char *temp = (char *) SUIT_malloc (strlen((char *)new) + strlen(currentDir) + 2);
	strcpy (temp, currentDir);
	if (currentDir[strlen(currentDir)-1] != SEPARATOR_CHAR)
	  strcat (temp, SEPARATOR_STRING);
	strcat (temp, (char *)new);
	SUIT_suspendMarkingRedisplay(parent);
	SUIT_setText (parent, CURRENT_VALUE, temp);
	SUIT_setText (parent, CURRENT_FILE, (char *)new);
	SUIT_resumeMarkingRedisplay(parent);
	SUIT_free(temp);
    } else if (SUIT_stringsMatch(propName, CURRENT_DIRECTORY)) {
	SUIT_object parent = SUIT_getParent(obj);
	SUIT_suspendMarkingRedisplay(parent);
	SUIT_setText (parent, CURRENT_DIRECTORY, (char *)new);
	SUIT_resumeMarkingRedisplay(parent);
    }
}



PRIVATE void moveInfoUp (SUIT_object obj, char *propName, char *propType, Pointer new, Pointer old)
{
    if (SUIT_stringsMatch (propName, CURRENT_VALUE)) {
	SUIT_suspendMarkingRedisplay (SUIT_getParent(obj));
	SUIT_setText (SUIT_getParent(obj), CURRENT_VALUE, (char *)new);
	SUIT_resumeMarkingRedisplay (SUIT_getParent(obj));
    }
    else if (SUIT_stringsMatch (propName, CURRENT_FILE)) {
	SUIT_suspendMarkingRedisplay (SUIT_getParent(obj));
	SUIT_setText (SUIT_getParent(obj), CURRENT_FILE, (char *)new);
	SUIT_resumeMarkingRedisplay (SUIT_getParent(obj));
    }
    else if (SUIT_stringsMatch (propName, CURRENT_DIRECTORY)) {
	SUIT_suspendMarkingRedisplay (SUIT_getParent(obj));
	SUIT_setText (SUIT_getParent(obj), CURRENT_DIRECTORY, (char *)new);
	SUIT_resumeMarkingRedisplay (SUIT_getParent(obj));
    }
}



void addTheTypeInBox (SUIT_object bboard)
{
    SUIT_object typer;
    
    typer = SUIT_createTypeInBox (SUIT_relativeName(bboard, "type in box"), processInputCallback);
    SUIT_addChildToObject (bboard, typer);
    SUIT_registerInterest (typer, SetFileName);
}



PRIVATE void HitFilebox (SUIT_object fbox, SUIT_event evt)
{
    static GP_time start;
    static boolean gotFirstClick = FALSE;
    static point prevPt;
    point currentPt;
    SUIT_object list;

    currentPt = evt.locator.position;
    list = SUIT_name(SUIT_relativeName(fbox, "scroll box"));
    if (list == NULL)
	return;
    
    if (evt.type == CLICK && SUIT_pointInObject (SUIT_getChild(list, 1), currentPt.x, currentPt.y)) {
	if (gotFirstClick) {
	    gotFirstClick = FALSE;
	    if (GP_timeDifference(GP_getCurrentTime(), start) < 1000 &&
		ABS(evt.locator.position.x - prevPt.x) < 5 &&
		ABS(evt.locator.position.y - prevPt.y) < 5) {
		(void) processInput (SUIT_name(SUIT_relativeName(fbox, "type in box")));
		return;
	    } else
		gotFirstClick = FALSE;
	} else {
	    start = GP_getCurrentTime();
	    prevPt = currentPt;
	    gotFirstClick = TRUE;
	}
    } else
	gotFirstClick = FALSE;

    SUIT_passEventDown (fbox, evt);
}




PRIVATE SUIT_object CreateFilebox (char *name, boolean hasBorder)
{
    SUIT_object retval = SUIT_createObject (name, hasBorder? "file box" : "borderless file box");
    SUIT_addDisplayToObject (retval, "standard", HitFilebox, SUIT_paintChildren);
    if (!hasBorder) 
	SUIT_deluxeSetBoolean (retval, HAS_BORDER, FALSE, CLASS);
    SUIT_deluxeSetBoolean (retval, CAN_BE_OPENED, TRUE, CLASS);
    SUIT_makePropertyTemporary (retval, CAN_BE_OPENED, CLASS);
    SUIT_registerInterest (retval, ChangeFilter);
    return retval;
}



PRIVATE boolean CheckForDirectory (SUIT_object fbox)
{
    return processInput(SUIT_name(SUIT_relativeName(fbox, "type in box")));
}



PRIVATE void FileboxCallback (SUIT_object filebox)
{
    /* this is really sort of hacky, but it does the job */
    SUIT_object parent = SUIT_getParent(filebox);
    SUIT_suspendMarkingRedisplay(parent);
    SUIT_setInteger (parent, BUTTON_PRESSED, (int)REPLY_OK);
    SUIT_setBoolean (parent, FINISHED, TRUE);
    SUIT_resumeMarkingRedisplay(parent);
}



PRIVATE SUIT_object CreateFileBox (char *name, char *startDirectory, char *label,
				   char *typeInBoxLabel, boolean hasBorder,
				   SUIT_callbackFunctionPtr callback)
{
    SUIT_object bboard, lab2, lab3, lab4, type, scroll;
    double x1, x2;
    int w, a, d;
    char *newStartDir;
    char *currentDir;

    bboard = CreateFilebox(name, hasBorder);
    SUIT_setText (bboard, CURRENT_VALUE, "");
    SUIT_setFunctionPointer (bboard, CALLBACK_FUNCTION, (SUIT_functionPointer)callback);
    
#ifdef X_WINDOWS
    if (startDirectory == NULL || startDirectory[0] == '\0') {
	currentDir = getenv("PWD");
	if ((currentDir == NULL) 
	    || (currentDir[strlen(currentDir)-1] != SEPARATOR_CHAR)) {
	    newStartDir = SUIT_malloc(strlen(currentDir)+2);
            strcpy(newStartDir, currentDir);
	    strcat(newStartDir, SEPARATOR_STRING);
	} else newStartDir = SUIT_createSafeString(currentDir);
    }	
#elif defined (IBM_PC)
	if (startDirectory == NULL || startDirectory[0] == '\0') {
	    newStartDir = SUIT_malloc(MAXDIR);
	    getcwd(newStartDir, MAXDIR);
	    if (newStartDir[strlen(newStartDir) - 1] != SEPARATOR_CHAR) 
	      strcat(newStartDir, SEPARATOR_STRING);
	}
#endif
    else if (startDirectory[strlen(startDirectory)-1] != SEPARATOR_CHAR) {
	newStartDir = SUIT_malloc(strlen(startDirectory)+2);
	strcpy(newStartDir, startDirectory);
	strcat(newStartDir, SEPARATOR_STRING);
    } else {
	newStartDir = SUIT_createSafeString(startDirectory);
    }


    SUIT_setText(bboard, CURRENT_DIRECTORY, newStartDir);
    
    CurrentGlobPattern = NULL;
    ReadDirectory (newStartDir);

    addTheScrollBox (bboard);
    scroll = SUIT_name (SUIT_relativeName (bboard, "scroll box"));
    
    addTheTypeInBox (bboard);
    type = SUIT_name (SUIT_relativeName (bboard, "type in box"));
    SUIT_setInteger(type, MARGIN, 5);

    addTheLabels (bboard, label, typeInBoxLabel);
    lab2 = SUIT_name (SUIT_relativeName (bboard, "type in box label"));
    lab3 = SUIT_name (SUIT_relativeName (bboard, "current directory label"));
    lab4 = SUIT_name (SUIT_relativeName (bboard, "count label"));

    GP_setFont (SUIT_getFont (bboard, FONT));
    SRGP_inquireTextExtent ("/usr/local/include/sys/", &w, &a, &d);
    x1 = hasBorder ? 0.05 : 0.0;
    x2 = hasBorder ? 0.95 : 1.0;

    if (label == NULL || label[0] == '\0') {
	SUIT_changeObjectSize (bboard, 2 * w, 18 * (a + d) + 10); 
	                                                  /* 10 for type margin*/
	SUIT_setViewport (lab2, VIEWPORT, SUIT_mapToParent(lab2, x1, 0.93, x2, 0.99));
	SUIT_setViewport (type, VIEWPORT, SUIT_mapToParent(type, x1, 0.79, x2, 0.89));
	SUIT_setViewport (scroll, VIEWPORT, SUIT_mapToParent(scroll, x1, 0.22, x2, 0.74));
	SUIT_setViewport (lab3, VIEWPORT, SUIT_mapToParent(lab3, x1, 0.1, x2, 0.17));
	SUIT_setViewport (lab4, VIEWPORT, SUIT_mapToParent(lab4, x1, 0.01, x2, 0.07));
    } else {
	SUIT_object lab1 = SUIT_name (SUIT_relativeName (bboard, "main label"));
	SUIT_changeObjectSize (bboard, 2 * w, 22 * (a + d) + 10);
	                                                  /* 10 for type margin*/
	SUIT_setViewport (lab1, VIEWPORT, SUIT_mapToParent(lab1, x1, 0.93, x2, 0.99));
	SUIT_setViewport (lab2, VIEWPORT, SUIT_mapToParent(lab2, x1, 0.82, x2, 0.89));
	SUIT_setViewport (type, VIEWPORT, SUIT_mapToParent(type, x1, 0.69, x2, 0.78));
	SUIT_setViewport (scroll, VIEWPORT, SUIT_mapToParent(scroll, x1, 0.17, x2, 0.66));
	SUIT_setViewport (lab3, VIEWPORT, SUIT_mapToParent(lab3, x1, 0.08, x2, 0.16));
	SUIT_setViewport (lab4, VIEWPORT, SUIT_mapToParent(lab4, x1, 0.01, x2, 0.07));
    }
    return bboard;
}



SUIT_object SUIT_createFileBrowser (char *name, char *startDirectory, char *label,
				    char *typeInBoxLabel, SUIT_callbackFunctionPtr callback)
{
    return CreateFileBox (name, startDirectory, label, typeInBoxLabel, TRUE, callback);
}



SUIT_object SUIT_createFileBrowserDialogBox (char *name, char *startDir, char *label,
					     char *typeInBoxLabel)
{
	char buf[100];
    SUIT_object fbox, dbox;

    sprintf (buf, "%s: file box", name);
    fbox = CreateFileBox (buf, startDir, label, typeInBoxLabel, FALSE, FileboxCallback);
    dbox = SUIT_createOKCancelDialogBox (name, fbox, CheckForDirectory);
    SUIT_setText (dbox, CURRENT_VALUE, "");
    SUIT_setBoolean (dbox, GRAB_RETURN_KEY, FALSE);
    SUIT_setText (dbox, CURRENT_DIRECTORY, SUIT_getText(fbox, CURRENT_DIRECTORY));
    SUIT_registerInterest (fbox, moveInfoUp);
    return dbox;
}


char *SUIT_askForFileName (char *startDir, char *label, char *typeInBoxLabel)
{
    static int number = 0;
    char buf[100];
    SUIT_object dbox;
    char *retval;

    sprintf (buf, "file browser %d", number++);
    dbox = SUIT_createFileBrowserDialogBox (buf, startDir, label, typeInBoxLabel);
    if (SUIT_activateDialogBox (dbox) == REPLY_OK) {
	retval = SUIT_getText (dbox, CURRENT_VALUE);
    } else
	retval = NULL;
    SUIT_destroyObject (dbox);
    return retval;
}
