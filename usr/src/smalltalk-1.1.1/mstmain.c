/***********************************************************************
 *
 *	Main Module
 *
 ***********************************************************************/

/***********************************************************************
 *
 * Copyright (C) 1990, 1991 Free Software Foundation, Inc.
 * Written by Steve Byrne.
 *
 * This file is part of GNU Smalltalk.
 *
 * GNU Smalltalk is free software; you can redistribute it and/or modify it
 * under the terms of the GNU General Public License as published by the Free
 * Software Foundation; either version 1, or (at your option) any later 
 * version.
 * 
 * GNU Smalltalk is distributed in the hope that it will be useful, but WITHOUT
 * ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or 
 * FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License for
 * more details.
 * 
 * You should have received a copy of the GNU General Public License along with
 * GNU Smalltalk; see the file COPYING.  If not, write to the Free Software
 * Foundation, 675 Mass Ave, Cambridge, MA 02139, USA.  
 *
 ***********************************************************************/


/*
 *    Change Log
 * ============================================================================
 * Author      Date       Change 
 * sbb	     12 Sep 91	  Fxied -I argument parsing code to properly gobble up
 *			  the file name.
 *
 * sbyrne    22 May 90	  Improved on Doug's mapping with macro to improve
 *			  readability.
 *
 * sbyrne    22 May 90	  Short name stuff added, thanks to Doug McCallum.
 *
 * sbyrne    25 Mar 90	  ProcessorScheduler is too long of a name for the
 *			  Atari; there are uniqueness problems.  Shortened to
 *			  ProcScheduler.   Also, fixed quietExecution; wasn't
 *			  set when reading from the terminal; should have been
 *			  set to false (since the loading of the quiet things
 *			  is over).
 *
 * sbyrne    15 Oct 89	  Added support for creating an "installed" version of
 *			  Smalltalk.  There is now an include file that the
 *			  installer can customize for his site that provides
 *			  default locations to be checked for the kernel .st
 *			  files and the binary image file, but these can be
 *			  overidden in two ways: a) by a file of the same
 *			  name in the user's current directory, or b)
 *			  environment variables SMALLTALK_KERNEL and
 *			  SMALLTALK_IMAGE.
 *
 * sbyrne     4 Jul 89	  Added support for user init files (in ~/.stinit),
 *			  which are invoked on every startup.  Also, added
 *			  support for initBlocks, which are blocks that are
 *			  stored in the system and invoked on each startup
 *			  (these could be used, for example, as an interim
 *			  measure for declaring C callouts until the callout
 *			  descriptor is converted to a Smalltalk object).
 *
 * sbyrne    10 Mar 89	  Added support for automatically loading image file if
 *			  it's newer than and of the system source files.
 *
 * sbyrne    27 Dec 88	  Created.
 *
 */


#include "mst.h"
#include "mst.tab.h"
#include "mstinterp.h"
#include "mstcomp.h"
#include "mstsave.h"
#include "mstsym.h"		/* for symbol table profiling */
#include "mstoop.h"		/* indirectly defines oopAt for sym tab prof */
#include "mstpaths.h"
#include "mstmain.h"
#include <stdio.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <sys/file.h>
#if defined(USG)
#include <unistd.h>
#endif

#ifndef MAXPATHLEN
#define MAXPATHLEN		1024 /* max length of a file and path */
#endif

#ifdef SHORTNAMES
#define MAP_FILE(long, short)	short
#else
#define MAP_FILE(long, short)	long
#endif


#ifndef atarist
#define INIT_FILE_NAME		".stinit"
#else
#define INIT_FILE_NAME		".tinit"
#endif


extern int		yydebug, lexDebug, numFreeOOPs;
extern YYSTYPE		yylval;
extern char		*getenv();

#ifdef symbol_table_profiling
extern int		adds, reused, reprobes, hitsOn[];
#endif /* symbol_table_profiling */

/* When true, this flag suppresses the printing of execution-related
 * messages, such as the number of byte codes executed by the
 * last expression, etc.
 */
Boolean			quietExecution;

char			*kernelFileDefaultPath, *imageFileDefaultPath;

/* This string contains the printed representation of the Smalltalk version
 * number
 */
char			versionString[50];

static Boolean 		processFile(), okToLoadBinary();
static void		loadStandardFiles(), loadUserInitFile(), initPaths(),
			findKernelFile(), parseArgs(), makeVersionString();
static unsigned long	getFileModifyTime();


/* Set by cmd line flag.  If true, Smalltalk is more verbose about what it's
 * doing.
 */
static Boolean		verbose = false;

/* If true, even both kernel and user method definitions are shown as
 * they are compiled.
 */
static Boolean		traceKernelDeclarations;

/* If true, execution tracing is performed when loading kernel method
 * definitions
 */
static Boolean		traceKernelExecution;

/* If true, skip date checking of kernel files vs. binary image; pretend
 * that binary image does not exist
 */
static Boolean		ignoreImage;


/* If non-nil, this is the name of the binary image to load, and overrides
 * the checking of the dates of the kernel source files against the image
 * file date.
 */
static char		*binaryImageName = nil;


/* Set by command line flag.  When this is true, the interpreter 
 * does not print out things like "execution begins" or information
 * about the number of byte codes executed.
 */
static Boolean		runQuietly = false;


/* The complete list of "kernel" class and method definitions.  Each of
 * these files is loaded, in the order given below.  Their last modification
 * dates are compared against that of the image file; if any are newer,
 * the image file is ignored, these files are loaded, and a new image file
 * is created.
 */
static char		*standardFiles[] = {
  "builtins.st", 
  "Object.st",
  "Message.st",
  "Magnitude.st",
  "Character.st",
  "Date.st",
  "Time.st",
  "Number.st",
  "Float.st",
  "Integer.st",
  "LookupKey.st",
  "Association.st",
  "Link.st",
  "Process.st",
  "Collection.st",
  MAP_FILE("SequenceableCollection.st", "SeqColl.st"),
  "LinkedList.st",
  "Semaphore.st",
  MAP_FILE("ArrayedCollection.st", "ArrColl.st"),
  "Array.st",
  "String.st",
  "Symbol.st",
  "ByteArray.st",
  MAP_FILE("CompiledMethod.st", "CompileMth.st"),
  "Interval.st",
  MAP_FILE("OrderedCollection.st", "OrdColl.st"),
  MAP_FILE("SortedCollection.st", "SortedColl.st"),
  "Bag.st",
  MAP_FILE("MappedCollection.st", "MappedColl.st"),
  "Set.st",
  "Dictionary.st",
  MAP_FILE("IdentityDictionary.st", "IdentDict.st"),
  MAP_FILE("SystemDictionary.st", "SysDict.st"),
  "Stream.st",
  MAP_FILE("PositionableStream.st", "PosStream.st"),
  "ReadStream.st",
  "WriteStream.st",
  MAP_FILE("ReadWriteStream.st", "RWStream.st"),
  "FileStream.st",
  "TokenStream.st",
  "Random.st",
  MAP_FILE("UndefinedObject.st", "UndefObj.st"),
  "Boolean.st",
  "False.st",
  "True.st",
#if !defined(USG) && !defined(atarist)
  "ProcessorScheduler.st",
#else
  "ProcSched.st",
#endif
  "Delay.st",
  "SharedQueue.st",
  "Behavior.st",
  MAP_FILE("ClassDescription.st", "ClassDescr.st"),
  "Class.st",
  "Metaclass.st",
  MAP_FILE("MethodContext.st", "MthContext.st"),
  MAP_FILE("BlockContext.st", "BlkContext.st"),
  "Memory.st",
  "WordMemory.st",
  "ByteMemory.st",
  "MethodInfo.st",
  "FileSegment.st",
  "SymLink.st",
  "initialize.st",
  "CFuncs.st",
  "Autoload.st",
  nil
};

#ifdef atarist
long _stksize = -1L;		/* what does this do? */
#endif
main(argc, argv)
int	argc;
char 	**argv;
{
  Boolean	loadBinary, traceUserDeclarations, traceUserExecution;
  int		filesProcessed;
  char		*imageName;

#ifdef USE_MONCONTROL
  moncontrol(0);		/* don't monitor the initial stuff */
#endif /* USE_MONCONTROL */

  yydebug = 0;
  traceKernelDeclarations = declareTracing = false;
  traceKernelExecution = executionTracing = false;
  regressionTesting = false;
  ignoreImage = false;
  verbose = false;

  initSignals();
  initMem();
  initCFuncs();
  initPaths();
  makeVersionString();
#if defined(USG)
  tzset();
#endif

  parseArgs(argc, argv);

  imageName = defaultImageName;

  if (binaryImageName) {
    loadBinary = true;
    imageName = binaryImageName;
  } else if (ignoreImage) {
    loadBinary = false;
  } else {
    loadBinary = okToLoadBinary(defaultImageName);
  }

  if (loadBinary && loadFromFile(imageName)) {
    initDefaultCompilationEnvironment();
    initSTDIOObjects();
    initInterpreter();
    gcOn();
  } else {
    initOOPTable();
    initDictionary();
    initSymbols();
    initInterpreter();

    installInitialMethods();

    traceUserDeclarations = declareTracing;
    traceUserExecution = executionTracing;
    if (!traceKernelDeclarations) {
      declareTracing = false;
    }
    if (!traceKernelExecution) {
      executionTracing = false;
    }

    gcOn();
    loadStandardFiles();

    declareTracing = traceUserDeclarations;
    executionTracing = traceUserExecution;

/* ***    gcOn(); *** */
    saveToFile(defaultImageName);
  }

#ifdef preserved /* Sun Mar 26 23:36:15 1989 */
/**/#ifdef profiling
/**/  monitor(0);
/**/
/**/  printf("results %d/%d\n", hits, misses);
/**/  { int i;
/**/    for (i = 0; i < 10240; i++) {
/**/      if (hitsOn[i]) {
/**/	printf("hitsOn[%d] = %4d\t%d\t%4x\t%8o", i, hitsOn[i],
/**/	       i, i, i);
/**/	printObject(oopAt(i));
/**/	printf("\n");
/**/      }
/**/    }
/**/  }
/**/#endif /* profiling */
#endif /* preserved Sun Mar 26 23:36:15 1989 */

#ifdef USE_MONCONTROL
  moncontrol(1);
#endif /* USE_MONCONTROL */

  invokeInitBlocks();
  loadUserInitFile();

#ifdef symbol_table_profiling
  printf("%d adds, %d reused %d reprobes\n", adds, reused, reprobes);

  { int i;
    for (i = 0; i < OOP_TABLE_SIZE; i++) {
      if (hitsOn[i]) {
	printf("hitsOn[%d] = %4d\t%d\t%4x\t%8o", i, hitsOn[i],
	       i, i, i);
	printObject(oopAt(i));
	printf("\n");
      }
    }
  }
#endif /* symbol_table_profiling */

  if (regressionTesting) {
    printf("Smalltalk Ready\n\n");
  } else {
    printf("Smalltalk %s Ready\n\n", versionString);
  }

  quietExecution = runQuietly || emacsProcess;
#ifndef LEXDEBUG
  for (filesProcessed = 0; *++argv; ) {
    if (argv[0][0] != '-') {
      processFile(argv[0], quietExecution);
      filesProcessed++;
    } else if (argv[0][1] == '-' || argv[0][1] == '\0') {
      /* either - by itself or -- indicates standard input */
      initLexer(false);
#ifdef USE_READLINE
      pushReadlineString();
#else
      pushUNIXFile(stdin, "stdin");
#endif /* USE_READLINE */
      yyparse();
      popStream(true);
    }
  }

  if (filesProcessed == 0) {	/* didn't do any from cmd line, so read stdin*/
    initLexer(false);
#ifdef USE_READLINE
    pushReadlineString();
#else
    pushUNIXFile(stdin, "stdin");
#endif /* USE_READLINE */
    yyparse();
  }

#ifdef USE_MONCONTROL
   moncontrol(0);
#endif /* USE_MONCONTROL */

#else /* debugging the lexer */

#ifdef USE_READLINE
    pushReadlineString();
#else
    pushUNIXFile(stdin, "stdin");
#endif /* USE_READLINE */

/********* THIS IS NOT UP TO DATE ... BEWARE ************/

  lexDebug = 1;
  while(!feof(infile)) {
    switch (yylex()) {
    case DOT:
      printf("DOT\n");
      break;
    case BANG:
      printf("BANG\n");
      break;
    case COLON:
      printf("COLON\n");
      break;
    case VERTICAL_BAR:
      printf("VERTICAL_BAR\n");
      break;
    case UPARROW:
      printf("UPARROW\n");
      break;
    case ASSIGN:
      printf("ASSIGN\n");
      break;
    case SHARP:
      printf("SHARP\n");
      break;
    case SEMICOLON:
      printf("SEMICOLON\n");
      break;
    case OPEN_PAREN:
      printf("OPEN_PAREN\n");
      break;
    case CLOSE_PAREN:
      printf("CLOSE_PAREN\n");
      break;
    case OPEN_BRACKET:
      printf("OPEN_BRACKET\n");
      break;
    case CLOSE_BRACKET:
      printf("CLOSE_BRACKET\n");
      break;
    case IDENTIFIER:
      printf("IDENTIFIER: %s\n", yylval.sval);
      break;
    case INTEGER_LITERAL:
      printf("INTEGER_LITERAL: %d\n", yylval.ival);
      break;
    case FLOATING_LITERAL:
      printf("FLOATING_LITERAL: %g\n", yylval.fval);
      break;
    case CHAR_LITERAL:
      printf("CHAR_LITERAL: %c\n", yylval.cval);
      break;
    case STRING_LITERAL:
      printf("STRING_LITERAL: %s\n", yylval.sval);
      break;
    case BINOP:
      printf("BINOP: %d\n", yylval.op);
      break;
    }
  }
#endif /* LEXDEBUG */
}

/*
 *	static void initPaths()
 *
 * Description
 *
 *	Sets up the paths for the kernel source directory and for where the
 *	saved Smalltalk binary image lives.  Uses environment variables
 *	SMALLTALK_KERNEL and SMALLTALK_IMAGE if they are set, otherwise uses
 *	the paths assigned in mstpaths.h.
 *
 */
static void initPaths()
{
  if ((kernelFileDefaultPath = (char *)getenv("SMALLTALK_KERNEL")) == nil) {
    kernelFileDefaultPath = KERNEL_PATH;
  }

  if ((imageFileDefaultPath = (char *)getenv("SMALLTALK_IMAGE")) == nil) {
    imageFileDefaultPath = IMAGE_PATH;
  }
}

static Boolean okToLoadBinary(imageFileName)
char	*imageFileName;
{
  unsigned long	imageFileTime;
  char		**fileNames, fullFileName[MAXPATHLEN],
  		fullImageName[MAXPATHLEN];

  findImageFile(imageFileName, fullImageName);
  imageFileTime = getFileModifyTime(fullImageName);

  if ((long)imageFileTime == -1) { /* not found */
    return (false);
  }

  for (fileNames = standardFiles; *fileNames; fileNames++) {
    findKernelFile(*fileNames, fullFileName);
    if (imageFileTime < getFileModifyTime(fullFileName)) {
      return (false);
    }
  }

  return (true);
}

static unsigned long getFileModifyTime(fileName)
char	*fileName;
{
  struct stat	st;

  if (stat(fileName, &st) < 0) {
    return ((unsigned long) -1);
  } else {
    return (st.st_mtime);
  }
}

/*
 *	static void loadStandardFiles()
 *
 * Description
 *
 *	Loads the kernel Smalltalk files.  It uses a vector of file names, and
 *	loads each file individually.  To provide for greater flexibility, if a
 *	one of the files exists in the current directory, that is used in
 *	preference to one in the default location.  The default location can be
 *	overridden at runtime by setting the SMALLTALK_KERNEL environment
 *	variable. 
 *
 */
static void loadStandardFiles()
{
  char		**fileNames, fullFileName[MAXPATHLEN];
  
  for (fileNames = standardFiles; *fileNames; fileNames++) {
    findKernelFile(*fileNames, fullFileName);
    if (!processFile(fullFileName, true)) {
      fprintf(stderr,
	      "Can't find system file '%s', proceeding without it.\n",
	      fullFileName);
    }
  }
}


/*
 *	static void findKernelFile(fileName, fullFileName)
 *
 * Description
 *
 *	Attempts to find a viable kernel Smalltalk file (.st file).  First
 *	tries the current directory to allow for overriding installed kernel
 *	files.  If that isn't found, the full path name of the installed kernel
 *	file is stored in fullFileName.  Note that the directory part of the
 *	kernel file name in this second case can be overridden by defining the
 *	SMALLTALK_KERNEL environment variable to be the directory that should
 *	serve as the kernel directory instead of the installed one.
 *
 * Inputs
 *
 *	fileName: 
 *		A simple file name, sans directory.
 *	fullFileName: 
 *		The file name to use for the particular kernel file is returned
 *		in this variable (which must be a string large enough for any
 *		file name).  If there is a file in the current directory with
 *		name "fileName", that is returned; otherwise the kernel path is
 *		prepended to fileName (separated by a slash, of course) and
 *		that is stored in the string pointed to by "fullFileName".
 *
 */
static void findKernelFile(fileName, fullFileName)
char *fileName, *fullFileName;
{
  if (access(fileName, R_OK) == 0) {
    strcpy(fullFileName, fileName);
  } else {
    sprintf(fullFileName, "%s/%s", kernelFileDefaultPath, fileName);
  }
}


static void loadUserInitFile()
{
  char		fileName[MAXPATHLEN], *home;

  if ((home = (char *)getenv("HOME")) != nil) {
    sprintf(fileName, "%s/%s", home, INIT_FILE_NAME);
    processFile(fileName, quietExecution);
  }
}

static Boolean processFile(fileName, quiet)
char	*fileName;
Boolean	quiet;
{
  FILE		*file;

  file = fopen(fileName, "r");
  if (file == NULL) {
    return (false);
  }

  if (verbose) {
    printf("Processing %s\n", fileName);
  }

  quietExecution = quiet;
  initLexer(false);
  pushUNIXFile(file, fileName);
  yyparse();
  popStream(true);

  return (true);
}

/*
 *	static void findImageFile(fileName, fullFileName)
 *
 * Description
 *
 *	Attempts to find a viable Smalltalk image file.  First
 *	tries the current directory to allow for overriding installed image
 *	files.  If that isn't found, the full path name of the installed image
 *	file is stored in fullFileName.  Note that the directory part of the
 *	image file name in this second case can be overridden by defining the
 *	SMALLTALK_IMAGE environment variable to be the directory that should
 *	serve as the image directory instead of the installed one.
 *
 * Inputs
 *
 *	fileName: 
 *		A simple file name, sans directory.
 *	fullFileName: 
 *		The file name to use for the particular image file is returned
 *		in this variable (which must be a string large enough for any
 *		file name).  If there is a file in the current directory with
 *		name "fileName", that is returned; otherwise the kernel path is
 *		prepended to fileName (separated by a slash, of course) and
 *		that is stored in the string pointed to by "fullFileName".
 *
 */
void findImageFile(fileName, fullFileName)
char *fileName, *fullFileName;
{
  if (access(fileName, R_OK) == 0) {
    strcpy(fullFileName, fileName);
  } else {
    sprintf(fullFileName, "%s/%s", imageFileDefaultPath, fileName);
  }
}


/*
 *	static void parseArgs(argc, argv)
 *
 * Description
 *
 *	This routine scans the command line arguments, accumulating information
 *	and setting flags.  This will probably be replaced by getopt in a
 *	future version of Smalltalk.
 *
 * Inputs
 *
 *	argc  : The number of arguments present
 *	argv  : Vector of strings that are the arguments.
 *
 */
static void parseArgs(argc, argv)
int	argc;
char	**argv;
{
  char		**hp, **av;
  char		*flags;

  static char	*helpText[] = {
"GNU Smalltalk usage:",
"",
"    mst [ flag ... ] [ file ...]",
"",
"Flags can appear either as -xyz or as -x -y -z.  The currently",
"defined set of flags is:",
"  -c\tDump core on fatal signal",
"  -d\tTrace compilation of user specified files",
"  -D\tTrace compilation of kernel and user files",
"  -e\tTrace execution of files specified on command line",
"  -E\tTrace execution of kernel and user files",
"  -H -h -?  Print this message and exit",
"  -i\tIgnore the image file; rebuild it from scratch",
"  -I file\tUse 'file' as the image file, instead of 'mst.im'",
"  -p\tRun Smalltalk as a 'process', i.e. from within GNU Emacs",
"  -q\tRun Smalltalk without printing execution information",
"  -r\tRun in regression test mode (printed messages are made constant)",
"  -v\tPrint the Smalltalk version number",
"  -V\tEnable verbose mode",
"  -y\tTurn on debugging in the parser",
"  - --\tRead input from standard input explicitly",
"",
"Files are loaded one after the other.  After the last one is loaded,",
"Smalltalk will exit.  If no files are specified, Smalltalk reads from",
"the terminal, with prompts.",
NULL
};


  for ( ; *++argv; ) {
    if (argv[0][0] == '-') {
      for (flags = &argv[0][1]; *flags; flags++) {
	switch (*flags) {
	case 'c':
	  makeCoreFile = true;
	  break;
	case 'D':
	  traceKernelDeclarations = true; /* fall thru */
	case 'd':
	  declareTracing = true;
	  break;
	case 'E':
	  traceKernelExecution = true; /* fall thru */
	case 'e':
	  executionTracing = true;
	  break;
	case 'h': case 'H': case '?':
	default:
	  for (hp = helpText; *hp != NULL; hp++) {
	    printf("%s\n", *hp);
	  }
          exit(0);
	case 'I':
	  binaryImageName = argv[1];
	  for (av = argv+1; *av; av++) {	/* remove this argument */
	    *av = *(av+1);
	  }
	  break;
	case 'i':
	  ignoreImage = true;
	  break;
	case 'p':
	  emacsProcess = true;
	  break;
	case 'q':
	  runQuietly = true;
	  break;
	case 'r':
	  regressionTesting = true;
	  break;
	case 'V':
	  verbose = true;
	  break;
	case 'v':
	  printf("GNU Smalltalk version %s\n", versionString);
	  printf("Copyright (C) 1990, 1991 Free Software Foundation, Inc.\n");
	  printf("Written by Steve Byrne.\n");
	  printf("Using kernel path: %s\n", kernelFileDefaultPath);
	  printf("Using image path : %s\n", imageFileDefaultPath);
	  break;
	case 'y':
	  yydebug = 1;
	  break;
	case '-':		/* this means standard input, so it's ok */
	  break;
	}
      }
    }
  }

  if (regressionTesting) {
    traceKernelDeclarations = declareTracing = false;
    traceKernelExecution = executionTracing = false;
    verbose = false;
  }
}

static void makeVersionString()
{
  if (sysVersEdit != 0) {
    sprintf(versionString, "%d.%d.%d", sysVersMajor, sysVersMinor,
	    sysVersEdit);
  } else {
    sprintf(versionString, "%d.%d", sysVersMajor, sysVersMinor);
  }
}
