#ifndef line
#ifdef sccs
static char     sccsid[] = "@(#) olwmslave.c 1.7 91/09/14 Sun Micro";
#endif
#endif
/*
 *      (c) Copyright 1989, 1990 Sun Microsystems, Inc. Sun design patents
 *      pending in the U.S. and foreign countries. See LEGAL_NOTICE
 *      file for terms of the license.
 */
/* ----------------------------------------------------------------------
 *	olwmslave.c
 * ----------------------------------------------------------------------*/

#include <stdio.h>
#ifdef SYSV
#include <string.h>
#else
#include <strings.h>
#endif
#include <xview/xview.h>

#ifndef MAXPATHLEN
#define MAXPATHLEN 256
#endif

#include "olwmslave.h"
#include "mem.h"

/* ----------------------------------------------------------------------
 * 	External globals
 * ----------------------------------------------------------------------*/
ScreenInfo	screenInfo;
Display		*display;
Xv_Server	dpyServer;

/* ----------------------------------------------------------------------
 *	Local Data
 * ----------------------------------------------------------------------*/
Bool		Done = False;
#define CMDBUFLEN	1024
char		cmdBuf[CMDBUFLEN];

/* ----------------------------------------------------------------------
 *	Forward Definitions
 * ----------------------------------------------------------------------*/
void		InitScreenInfo();
void		ParseScreenArgs();
Notify_value	InputReader();

/* ----------------------------------------------------------------------
 * 	main
 * ----------------------------------------------------------------------*/
/*ARGSUSED*/
main(argc,argv)
	int	argc;
	char	**argv;
{
	dpyServer = (Xv_Server)xv_init(
#ifdef OW_I18N
		XV_USE_LOCALE,		TRUE,
#endif
		XV_INIT_ARGC_PTR_ARGV,	&argc,argv,
		0);
	display = (Display *)xv_get(dpyServer,XV_DISPLAY);

#ifdef OW_I18N
	{
	char	*domain	= "olwmslave";
	char	*openwinDir;
	char	localePath[MAXPATHLEN];

	if ((openwinDir = getenv("OPENWINHOME")) != 0)
		(void)strcpy(localePath,openwinDir);
	else
		(void)strcpy(localePath,"/usr/share");
	(void)strcat(localePath,"/lib/locale");
	(void)bindtextdomain(domain,localePath);
	textdomain(domain);
	}
#endif

	InitScreenInfo(dpyServer);
	ParseScreenArgs(&argc,argv);

	RegisterHelpCmd();
	RegisterPropsCmd();
	SetCmdStream(stdin,stdout);
	notify_set_input_func((Notify_client)stdin,InputReader,fileno(stdin));

	while (!Done)
		notify_start();
	exit(0);
}

/* ----------------------------------------------------------------------
 *	makeDpyEnvString
 * ----------------------------------------------------------------------*/
static char *
makeDpyEnvString(dpy,nscreen)
	Display		*dpy;
	int		nscreen;
{
	char	dpyEnv[256];
	char	scrNum[5];
extern	char	*strtok();

	(void)strcpy(dpyEnv,"DISPLAY=");

	(void)strcat(dpyEnv,DisplayString(dpy));
	(void)strtok(dpyEnv,".");

	(void)sprintf(scrNum,".%d",nscreen);
	(void)strcat(dpyEnv,scrNum);

	return MemNewString(dpyEnv);
}

/* ----------------------------------------------------------------------
 *	InitScreenRoot
 * ----------------------------------------------------------------------*/
static void
InitScreenInfo(server)
	Xv_Server	server;
{
	int		i;
	ScreenRoot	*sr;

	screenInfo.numScreens = ScreenCount(display);

	screenInfo.screenList = (ScreenRoot *)MemAlloc(
		sizeof(ScreenRoot)*NumScreens());

	sr = ScreenList(0);
	for (i=0; i<NumScreens(); i++,sr++) {
		sr->screen = (Xv_Screen)xv_get(server,SERVER_NTH_SCREEN,i);
		sr->root = (Xv_Window)xv_get(sr->screen,XV_ROOT);
		sr->rootwin = (Window)xv_get(sr->root,XV_XID);
		sr->dpyEnvStr = makeDpyEnvString(display,i);
	}
}

/* ----------------------------------------------------------------------
 *	ParseScreenArgs
 * ----------------------------------------------------------------------*/
/*ARGSUSED*/
static void
ParseScreenArgs(argc,argv)
	int		*argc;
	char		**argv;
{
	int		i;
	ScreenRoot	*sr;

	sr = ScreenList(0);
	for (i=0; i<NumScreens(); i++,sr++) {
		sr->used = True;
	}
}

/* ----------------------------------------------------------------------
 *	InputReader
 * ----------------------------------------------------------------------*/
static Notify_value	
InputReader(client,fd)
	Notify_client	client;
	int		fd;
{
	int		count;

	count = read(fd,cmdBuf,CMDBUFLEN);

	switch (count) {
	case -1:		/* error */
		perror("olwmslave: read");
		exit(-1);
		break;
	case 0:			/* eof */
		notify_set_input_func(client,0,fd);
		Done = True;
		notify_stop();
		break;
	default:		/* normal */
		cmdBuf[count] = 0;
		ParseCmd(cmdBuf);
	}
	return NOTIFY_DONE;
}

