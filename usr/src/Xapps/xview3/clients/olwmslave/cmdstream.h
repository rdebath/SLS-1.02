/* @(#) cmdstream.h 26.1 90/09/30 Sun Microsystems */
/*
 *      (c) Copyright 1989 Sun Microsystems, Inc. Sun design patents
 *      pending in the U.S. and foreign countries. See LEGAL_NOTICE
 *      file for terms of the license.
 */
/* ----------------------------------------------------------------------
 *      cmdstream.h
 * ---------------------------------------------------------------------*/

#ifndef CMDSTREAM_H
#define CMDSTREAM_H

#ifndef TRUE
#define TRUE	1
#endif

#ifndef FALSE
#define FALSE	0
#endif

/* ----------------------------------------------------------------------
 *      Command Data Structures
 * ---------------------------------------------------------------------*/

typedef enum _AttrType {
	INT, FLOAT, STRING
} AttrType;

typedef union _AttrValue {
	int		ival;
	float		fval;
	char		*sval;
} AttrValue;

typedef struct _CmdAttr {
	char		*name;
	AttrType	type;
	AttrValue	value;
} CmdAttr;

typedef struct _Command {
	char		*keyword;
	void		(*callback)();
	int		attrLen;
	CmdAttr		*attrList;
} Command;

/* ----------------------------------------------------------------------
 *      Function Definitions
 * ---------------------------------------------------------------------*/

extern	void	SetCmdStream(/* FILE *instream, FILE * outstream */);
extern	int	SendCmd(/* Command* cmd */);
extern	void	RegisterCmd(/* Command *cmd */);
extern	int	GetCmd();
extern	int	ParseCmd(/* char *line */);

#endif /* CMDSTREAM_H */
