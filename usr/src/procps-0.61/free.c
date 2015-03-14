/* free.c - a /proc implementation of free */
/* Dec14/92 by Brian Edmonds */

#include <stdio.h>
#include <fcntl.h>
#include <strings.h>
#include <getopt.h>
#include <unistd.h>

/* set this to 0 if you want bytes by default */
int kilob = 1;

int
main( int argc, char **argv )
{
	char buf1[80], buf2[80];
	char *titles[5], name[32];
	int i, n, col[5];

	/* check startup flags */
	while( ( i = getopt( argc, argv, "bk" ) ) != -1 ) switch( i )
	{
		case 'b': kilob = 0; break;
		case 'k': kilob = 1; break;
		default:
			fprintf( stderr, "usage: %s [-k|-b]\n", argv[0] );
			return -1;
	}

	/* redirect stdin to /proc/meminfo */
	close( 0 );
	if( open( "/proc/meminfo", O_RDONLY ) < 0 )
	{
		perror( "open" );
		return -1;
	}

	/* get the column titles */
	fgets( buf1, 80, stdin );
	for( i=0 ; i<5 ; i++ )
	{
		titles[i] = strtok( ( i ? NULL : buf1 ), " \t:" );
		if( ! titles[i] )
		{
			fprintf( stderr, "free: error reading /proc/meminfo\n" );
			return -1;
		}
	}
	fprintf( stdout, "%-7s %10s %10s %10s %10s %10s\n",
		"", titles[0], titles[1], titles[2], titles[3], titles[4] );

	/* read and translate data lines */
	while( fgets( buf2, 80, stdin ) )
	{
		n = sscanf( buf2, "%s %d %d %d %d %d", name,
			&col[0], &col[1], &col[2], &col[3], &col[4] );
		if( n < 1 ) continue;
		fprintf( stdout, "%-7s", name );
		for( i=1 ; i<n ; i++ )
			fprintf( stdout, " %10d", col[i-1]/(1+kilob*1023) );
		fprintf( stdout, "\n" );
	}

	return 0;
}

