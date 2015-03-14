#include <stdio.h>
#include <sys/types.h>
#include <sys/vfs.h>

long myfree()
{
long space;
struct statfs *buf;

	buf=malloc(sizeof(struct statfs *));
	statfs("/",buf);
	space=buf->f_bavail;
	free(buf);
	return (space);
}
