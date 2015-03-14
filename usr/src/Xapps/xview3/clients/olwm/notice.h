/*
 *      (c) Copyright 1990 Sun Microsystems, Inc. Sun design patents
 *      pending in the U.S. and foreign countries. See LEGAL_NOTICE
 *      file for terms of the license.
 */

#ident	"@(#)notice.h	26.8	91/09/14 SMI"

#define NOTICE_CANCEL	(-1)

#define NOTICE_BUTTON_COUNT(b)	sizeof((b))/sizeof(char *)

typedef struct _noticeBox {
	int	numButtons;	/* number of buttons */
	int	defaultButton;	/* index into buttonText array */
	char	**buttonText;	/* array of strings for button text */
	char	*msgText;
	int	boxX;		/* box origin (-1 =use default/centered) */
	int	boxY;		/* box origin (-1 =use default/centered) */
} NoticeBox;

/* function declarations */
extern int UseNoticeBox();
