/*######################################################################*
 *
 *	XMIME: MIME format handling control command
 *
 *		92.05.21	Yutaka Sato <ysato@etl.go.jp>
 */

#include "common.h"

#ifdef XMIME

#define OK_MIME		209
int COCK_MIME_HDR;

xmime(ac,av)
	char *av[];
{
	if( ac < 2 )
		goto CMDSYN;

	if( strcmp(av[1],"enable" ) == 0 ) COCK_MIME_HDR = 1; else
	if( strcmp(av[1],"disable") == 0 ) COCK_MIME_HDR = 0; else
		goto CMDSYN;

	fclose(art_fp);
	art_fp = NULL;

	printf("%d MIME encoding/decoding %s\r\n",OK_MIME,
		COCK_MIME_HDR ? "enabled":"disabled");
	(void)fflush(stdout);
	return;

CMDSYN:
	printf("%d Usage: MIME [enable/disable]\r\n",ERR_CMDSYN);
	(void)fflush(stdout);
	return;
}

#endif XMIME
