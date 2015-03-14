
/* frag.c - simple fragmentation checker
            V1.0 by Werner Almesberger
	    V1.1 by Steffen Zahn, adding directory recursion
            V1.2 by Rob Hooft, adding hole counts
 */

#include <stdio.h>
#include <unistd.h>
#include <fcntl.h>
#include <limits.h>
#include <stdlib.h>
#include <string.h>
#include <dirent.h>
#include <sys/stat.h>


#define FIBMAP 1


typedef struct StackElem {
    struct StackElem *backref, *next;
    char name[NAME_MAX];
    char dir_seen;
} StackElem;

StackElem *top = NULL;


void discard( void )
{
    StackElem *se = top;
    if( se == NULL )
	return ;
    top = se->next;
    free(se);
}

void push( StackElem * se )
{
    se -> next = top;
    top = se;
}

char *p2s( StackElem *se, char *path )
{
    char *s;
    if( se->backref!=NULL ) {
	path = p2s( se->backref, path );
	if( path[-1]!='/' )
	    *path++ = '/';
    }
    s = se->name;
    while( *s )
	*path++ = *s++;
    return path;
}

char *path2str( StackElem *se, char *path )
{
    *(p2s( se, path ))=0;
    return path;
}
	      
void *xmalloc( size_t size )
{
    void *p;
    if( (p=malloc(size))==NULL ) {
	fprintf(stderr,"\nvirtual memory exhausted.\n");
	exit(1);
    }
    return p;
}

int main(int argc,char **argv)
{
    int fd,last,frags,blocks,block, this_frag, largest_frag;
    long sum_blocks=0, sum_frag_blocks=0, sum_files=0, sum_frag_files=0;
    long num_hole=0, sum_hole=0, hole;
    struct stat st;
    StackElem *se, *se1;
    char path[PATH_MAX], *p;
    DIR *dir;
    struct dirent *de;
    char silent_flag=0;
    
    if (argc < 2) {
        fprintf(stderr,"usage: %s [-s [-s]] filename ...\n",argv[0]);
        exit(1);
    }
    argc--; argv++;
    while (argc>0) {
	p = *argv;
	if( *p=='-' )
	    while( *++p )
	    switch( *p ){
	      case 's':
		silent_flag++; /* may be 1 or 2 */
		break;
	      default:
		fprintf(stderr,"\nunknown flag %c\n", *p );
		exit(1);
	    }
	else {
	    se = xmalloc( sizeof(StackElem) );
	    se->backref=NULL; se->dir_seen=0;
	    strcpy( se->name, p );
	    push(se);
	}
	argc--; argv++;
    }
    while ( top != NULL) {
	se = top;
	if( se->dir_seen )
	    discard();
	else {
	    path2str( se, path );
	    if (stat( path,&st) < 0) {
		perror( path );
		discard();
	    } else {
		if( S_ISREG(st.st_mode)) {
		    if ( (fd = open( path ,O_RDONLY)) < 0 ) {
			perror( path );
			discard();
		    } else {
			last = -1;
			frags = 0;
			hole = 0;
			this_frag=0; largest_frag=-1;
			for (blocks = 0; blocks < ((st.st_size+1023) >> 10); blocks++) {
			    block = blocks;
			    if (ioctl(fd,FIBMAP,&block) < 0) {
				perror(path);
				break;
			    }
			    this_frag++;
			    if (block) {
				if (last != block-1 && last != block+1) {
				    if( largest_frag<this_frag )
					largest_frag=this_frag;
				    this_frag=0;
				    frags++;
				}
				last = block;
			    } else {
				hole++;
			    }
			}
			if( largest_frag<this_frag )
			    largest_frag=this_frag;
			if( !silent_flag ) {
			    printf(" %3d%%  %s  (%d block(s), %d fragment(s), largest %d",
				   frags < 2 ? 0 : frags*100/blocks,
				   path,blocks,frags,largest_frag);
			    if (hole) {
				printf(", %d hole)\n",hole);
			    } else {
				printf(")\n");
			    }
			}
			sum_blocks+=blocks;
			if (hole) num_hole++;
			sum_hole+=hole;
			sum_files++;
			if( frags>1 ) {
			    sum_frag_blocks+=blocks-largest_frag;
			    sum_frag_files++;
			}
			discard();
			close(fd);
		    }
		} else if( S_ISDIR( st.st_mode ) ) { /* push dir contents */
		    if( (dir=opendir( path ))==NULL ) {
			perror(path);
			discard();
		    } else {
			if( silent_flag<2 )
			    printf("reading %s\n", path);
			while( (de=readdir(dir))!=NULL ) {
			    if( (strcmp(de->d_name,".")!=0)
			       && (strcmp(de->d_name,"..")!=0) ) {
				se1 = xmalloc( sizeof(StackElem) );
				se1->backref=se; se1->dir_seen=0;
				strcpy( se1->name, de->d_name );
				push(se1);
			    }
			}
			closedir( dir );
			se->dir_seen=1;
		    }
		} else /* if( S_ISREG(st.st_mode)) */
		    discard();
	    }
	} /* if( se->dir_seen ) */
    } /* while ( top != NULL) */
    if (sum_files>1) {
	printf("\nsummary:\n");
	printf(" %3ld%% file fragmentation (%ld of %ld files contain fragments)\n",sum_files<1 ? 0L : sum_frag_files*100/sum_files, sum_frag_files, sum_files);
	printf(" %3ld%% block fragmentation (%ld of %ld blocks are in fragments)\n",sum_blocks<1 ? 0L : sum_frag_blocks*100/sum_blocks, sum_frag_blocks, sum_blocks);
	if (num_hole>1) printf(" %ld files contain %ld blocks in holes\n",num_hole,sum_hole);
    }
    exit(0);
}

