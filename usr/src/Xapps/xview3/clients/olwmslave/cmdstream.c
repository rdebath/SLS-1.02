#ifndef line
#ifdef sccs
static char	sccsid[] = "@(#) cmdstream.c 26.1 90/10/01 Sun Micro";
#endif
#endif
/*
 *      (c) Copyright 1989 Sun Microsystems, Inc. Sun design patents
 *      pending in the U.S. and foreign countries. See LEGAL_NOTICE
 *      file for terms of the license.
 */
/* ----------------------------------------------------------------------
 *      cmdstream.c
 * ---------------------------------------------------------------------*/

#include <stdio.h>
#ifdef SYSV
#include <string.h>
#else
#include <strings.h>
#endif
#include "cmdstream.h"
#include "mem.h"

extern	char	*strtok();

/* ----------------------------------------------------------------------
 *      Local Data Structures
 * ---------------------------------------------------------------------*/

typedef struct _CList {
	Command		*this;
	struct _CList	*next;
} CList;

typedef struct _CmdInfo {
	FILE		*input;
	FILE		*output;
	CList		*cmdListHead;
} CmdInfo;

/* ----------------------------------------------------------------------
 *      Data Definitions
 * ---------------------------------------------------------------------*/

static	CmdInfo		cmdInfo = {
	(FILE *)0, (FILE *)0, (CList *)0
};

/* ----------------------------------------------------------------------
 *      Local Forward Declarations
 * ---------------------------------------------------------------------*/

Command		*MatchCommand();
CmdAttr		*MatchAttr();
int		EncodeAttrValue();
int		DecodeAttrValue();

/* ----------------------------------------------------------------------
 *      SetCmdStream
 * ---------------------------------------------------------------------*/
void
SetCmdStream(instream,outstream)
	FILE		*instream;
	FILE		*outstream;
{
	cmdInfo.input = instream;
	cmdInfo.output = outstream;
}

/* ----------------------------------------------------------------------
 *      SendCmd
 * ---------------------------------------------------------------------*/
int
SendCmd(cmd)
	Command		*cmd;
{
	CmdAttr		*attr;
	int		i;

	if (cmdInfo.output == (FILE *)NULL)
		return FALSE;

	fprintf(cmdInfo.output,"%s\n",cmd->keyword);
	for (i=0; i<cmd->attrLen; i++) {
		attr = &(cmd->attrList[i]);
		fprintf(cmdInfo.output,"\t%s=",attr->name);
		EncodeAttrValue(attr,cmdInfo.output);
		fputs("\n",cmdInfo.output);
	}
	fputs(";\n",cmdInfo.output);
	fflush(cmdInfo.output);
	return TRUE;
}

/* ----------------------------------------------------------------------
 *      RegisterCmd
 * ---------------------------------------------------------------------*/
void
RegisterCmd(cmd)
	Command		*cmd;
{
	CList		*item;

	item = MemNew(CList);
	item->this = cmd;
	item->next = cmdInfo.cmdListHead;
	cmdInfo.cmdListHead = item;
}

/* ----------------------------------------------------------------------
 *      GetCmd
 * ---------------------------------------------------------------------*/
int
GetCmd()
{
#define	CMDBUFLEN	1024
	char		cmdBuf[CMDBUFLEN];

	while (fgets(cmdBuf,CMDBUFLEN,cmdInfo.input) != (char *)NULL) {
		if (ParseCmd(cmdBuf) == FALSE)
			return FALSE;
	}
	return TRUE;
}

/* ----------------------------------------------------------------------
 *      ParseCmd
 * ---------------------------------------------------------------------*/
int
ParseCmd(line)
	char		*line;
{
	char		*token;
	char		*sep = " \t\n";
	static Command	*cmd = (Command *)NULL;
	static CmdAttr	*attr = (CmdAttr *)NULL;

	for (token=strtok(line,sep); token; token=strtok(0,sep)) { 
		if (cmd == (Command *)NULL) {
			if ((cmd = MatchCommand(token)) == (Command *)NULL)
				return FALSE;
		} else if (token[0] == ';') {
			if (cmd->callback)
				(*cmd->callback)(cmd);
			cmd = (Command *)NULL;
			attr = (CmdAttr *)NULL;
		} else {
			if ((attr = MatchAttr(cmd,token)) == (CmdAttr *)NULL) {
				cmd = (Command *)NULL;
				return FALSE;
			}
		}
	}
	return FALSE;
}

/* ----------------------------------------------------------------------
 *      MatchKeyword
 * ---------------------------------------------------------------------*/
static Command *
MatchCommand(keyword)
	char		*keyword;
{
	CList		*item;

	for (item=cmdInfo.cmdListHead; item; item=item->next) {
		if (!strcmp(keyword,item->this->keyword)) {
			return item->this;
		}
	}
	return (Command *)NULL;
}

/* ----------------------------------------------------------------------
 *      MatchAttr
 * ---------------------------------------------------------------------*/
static CmdAttr *
MatchAttr(cmd,name)
	Command		*cmd;
	char		*name;
{
	int		i,equalindex;
	char		*equalpos;
	char		*strchr();

	equalpos = strchr(name,'=');

	if (!equalpos)
		return (CmdAttr *)NULL;

	equalindex = equalpos - name;

	for (i=0; i<cmd->attrLen; i++) {
		if (!strncmp(name,cmd->attrList[i].name,equalindex)) {
			if (!DecodeAttrValue(&cmd->attrList[i],++equalpos))
				return (CmdAttr *)NULL;
			return &(cmd->attrList[i]);
		}
	}
	return (CmdAttr *)NULL;
}

/* ----------------------------------------------------------------------
 *      EncodeAttrValue
 * ---------------------------------------------------------------------*/
static int
EncodeAttrValue(attr,stream)
	CmdAttr		*attr;
	FILE		*stream;
{
	switch (attr->type) {
	case INT:	fprintf(stream,"%d",attr->value.ival);
			break;
	case FLOAT:	fprintf(stream,"%f",attr->value.fval);
			break;
	case STRING:	fprintf(stream,"%s",attr->value.sval);
			break;
	}
	return TRUE;
}

/* ----------------------------------------------------------------------
 *      DecodeAttrValue
 * ---------------------------------------------------------------------*/
static int
DecodeAttrValue(attr,valuestr)
	CmdAttr		*attr;
	char		*valuestr;
{
	char		*ptr;
	int		status = TRUE;

	switch (attr->type) {
	case INT:	attr->value.ival = (int)strtol(valuestr,&ptr,10);
			if (ptr == valuestr)
				status = FALSE;
			break;
	case FLOAT:	 attr->value.fval = (float)strtod(valuestr,&ptr);
			if (ptr == valuestr)
				status = FALSE;
			break;
	case STRING:	attr->value.sval = MemNewString(valuestr);
			if (attr->value.sval == (char *)NULL)
				status = FALSE;
			break;
	default:
			status = FALSE;
			break;
	}
	return status;
}
