/*
 * A client for term. Hacked about by Jeff Grills, bashed a little here and
 * there by me. Mostly formatting changes + a few error condition
 * changes. 
 * [-u] (unlink) by Darren Drake (drd@gnu.ai.mit.edu)
 */


#define I_IOCTL
#define I_SYS
#define I_STRING
#define DEBUG

#include "includes.h"
#include "client.h"
#include <sys/stat.h>
#include <time.h>

int debug = 0;
int force = 0;
int verbose = 1; 
int unlinkmode = 0;
int literal_filename = 0;

struct stat st;

/*---------------------------------------------------------------------------*/

int local_options ( char opt, char *optarg )
{
  switch(opt)
  {
  case 'f' :
    force = 1;
    break;
  case 'q' :
    verbose = 0;
    break;
  case 'u':
    unlinkmode = 1;
    break;
  case 'v' :
    verbose++;
    break;
  case 'l':
    literal_filename = 1;
    break;
  default:
    return -1;
  }
  return 0;
}
/*---------------------------------------------------------------------------*/

void main(int argc, char *argv[])
{
				/* From client.c */
  extern int lcompression, rcompression;
				/* locals for upload. */
  struct Buffer buffer = {{0},0,0,0};
  char *cutpath, *file, *remote, path[255], filename[255];
  int term, fd, first, remote_size, stdin_used, type, perms, i, perc;
  int ret, bytesent, total_bytesent, filesent;
  double cps;
  time_t total_starttime, total_stoptime, starttime, stoptime, etime;

  term = -1;
  fd = -1;
  first = -1;
  stdin_used = 0;
  priority = -2;
  bytesent = 0;
  starttime = 0;
  total_bytesent = 0;
  total_starttime = time(NULL);
  filesent = 0;
  path[0] = '\0';
  lcompression = rcompression = 0;
  
  /* Handle the options. We use the standard client argument handling */
  /* to do this. Options peculiar to upload are 'f' for force, q for */
  /* quiet, and v for verbose, u for unlink */
  if ( ((first = client_options(argc,argv,"fquvl",local_options)) == -1)
      || (first >= argc) ) {
    fprintf(stderr, 
	    "Usage: upload [-f] [-q] [-u] [-v] [-r] [-c] [-p <num>] <file>"
	    " [... <file>] [remote dir]\n"); 
    exit(1);
  }

  term = connect_server(term_server);

  /* check the last arg to see if it's a dir, and if so,  */
  /* that's the path we send to. */
  if ((first < argc) && (send_command(term, C_STAT, 0, "%s",
				      argv[argc-1]) >= 0)) {
    sscanf(command_result, "%d %d %d", &remote_size, &type, &perms );

    /* check if it's a dir, and writable. */
    if (type == 1) {
      strcpy(path, argv[--argc]);
      if ( ! (i = strlen(path))) {
	fprintf(stderr, "\tFatal. Zero length path passed.\n");
	exit(1);
      }
      if ( path[i - 1] != '/' ) {
	path[i] = '/';
	path[i+1] = '\0';
      }
    }
  }

  /* should check here for a file to send.  enforce command line. */
  /*   or, maybe no args should mean take from stdin. */
  /*   but then, what's the output file name? -q */

  while ( first < argc ) {

    /* close the input file if it was left open */
    if (fd > 0) {
      close(fd);
      fd = -1;
    }

    /* get the a filename to send.*/
    file = argv[first++];

    if ( (first+1 < argc) && (strcmp(argv[first],"-as") == 0) ) {
      remote = argv[first+1];
      first += 2;
    }
    else {
      /* leave the filename alone */
      remote = file;
      
      /* remove the pathname for the outgoing file */
      if (!literal_filename) {
          for ( cutpath = remote; *cutpath; cutpath++ )
              if ( ( *cutpath == '/' ) && ( *(cutpath+1) ) )
                  remote = cutpath + 1;
      }
    }
    
    /* prepend the specified path, if there is one. */
    if ( *path ) {
      strcpy(filename, path);
      strcat(filename, remote);
      remote = filename;
    }

    if ( verbose > 0 )
      if ( file == remote )
	fprintf(stderr, "sending %s\n",file);
      else
	fprintf(stderr, "sending %s as %s\n", file, remote );
    
    /* open input file, or use stdin. The  stdin is a bit of a kludge */
    /* but it can be useful. */
    fd = -1;
    if ( strcmp(file, "-") ) 
      fd = open(file, O_RDONLY);
    else 
      /* try to use stdin's file descriptor */
      if ( stdin_used++ ) {
	fprintf(stderr, "\tskipped : can only take input from stdin once\n");
	continue;
      }
      else
	fd = 0;
    
    if (fd < 0) {
      fprintf(stderr, "\tskipped : can't open local file\n");
      continue;
    }

    /* get some info on the local file. We need to know the file */
    /* size at the very least. */
    if (fd)
      stat(file, &st);

    /* see if the file exists to be resumed. Done by asking term to */
    /* stat the file on the remote end. */
    remote_size = 0;
    if (send_command(term, C_STAT, 0, "%s", remote) >= 0) {
      if (force)
	fprintf(stderr,"\twarning, overwriting file on remote host\n");
      else
	if (!fd) {
	  fprintf(stderr,"\tcannot resume from stdin\n");
	  continue;
	  }
	else {
	  
	  /* Ok. The remote file exists. lets check a few things here.. */
	  remote_size = atoi(command_result);
	  
	  /* remote file is same size. Skip it. */
	  if ( remote_size == st.st_size ) {
	    fprintf(stderr,"\tskipping, remote file is same size as local\n");
	    continue;
	  }
	  
	  /* remote file is larger than current file. Skip this as well.*/ 
	  if ( remote_size >  st.st_size ) {
	    fprintf(stderr,"\tskipping, remote file is larger than local\n");
	    continue;
	  }

	  fprintf(stderr, "\tattempting to restarting upload from %d\n",
		  remote_size);
	  
	  /* i'd really like a checksum of the file done here to make sure */
	  /* it's the same file -q */
	}
    }
    
    if (remote_size) {
      /* open file on the remote end. We use C_OPEN instead of */
      /* C_UPLOAD as we don't want to truncate the file. */
      if (send_command(term, C_OPEN, 0, "%s", remote) < 0) {
	fprintf(stderr,"\tskipped : Couldn't open remote file, %s\n",
		command_result);
	continue;
      }    
      
      /* do the remote file seek. */
      if (send_command(term, C_SEEK, 0, "%d", remote_size) < 0) {
	fprintf(stderr, "\tskipped, remote seek failed, %s\n",
		command_result); 
	continue;
      }
      
      /* do the local file seek */
      if (lseek(fd, remote_size, 0)<0) {
	fprintf(stderr, "\tskipped, local seek failed, ");
	perror("Reason given");
	continue;
      }
    }
    else {
      /* it's a new file. Use C_UPLOAD to open, and possiblely creat, */
      /* or truncate it. */
      if (send_command(term, C_UPLOAD, 0, "%s", remote) < 0) {
	fprintf(stderr,"\tskipped : Couldn't open remote file, %s\n",
		command_result); 
	continue;
      }
    }
    
    /* dump the file over the socket. We handle this by using C_DUMP */
    /* commands to escape the data. */
    if (verbose > 1)
      starttime = time(NULL);
    bytesent = 0;
    filesent++;
    do {
      if (!buffer.size) {
	ret = read_into_buff(fd, &buffer, 0);
	if (ret <=0 && term_errno) break;
	if (!buffer.size) continue;
	if (send_command(term, C_DUMP, 1, "%d", buffer.size) < 0) {
	  fprintf(stderr, "\taborted, couldn't turn off command"
		  " processing, %s\n", command_result );
	  continue;
	}
      } 
      else {
	ret = write_from_buff(term, &buffer, 0);
	if (ret <=0 && term_errno) {
	  fprintf(stderr, "\terror writing to term server. Exiting..\n");
	  exit(1);
	}
	bytesent += ret;
	total_bytesent += ret;
	remote_size += ret;

	if ( (verbose > 2)) {
	  stoptime = time(NULL);
	  if (stoptime - starttime)
	    cps = (float)(bytesent - BUFFER_SIZE) / (float)(stoptime - starttime);
	  else
	    cps = (float)0;

	  if ( fd && (st.st_size) ) {
	    perc = (remote_size*100) / st.st_size;
	    fprintf(stderr, "\r\t%d of %d (%d%%) at %.2f CPS. ", remote_size,
		    st.st_size, perc, cps );
	  }
	  else
	    if (!fd)
	      fprintf(stderr, "\r\t%d of at %.2f CPS. ", remote_size, cps );

	  fflush(stderr);
	}
      }
    } while (1); 

    /* Close the file */
    send_command(term , C_CLCLOSE, 0, 0);

    /* check the remote file after send */
    send_command(term, C_STAT, 0, "%s", remote);
    if ((send_command(term, C_STAT, 0, "%s", remote)) >= 0) {
      if (( fd && (atoi(command_result) != st.st_size) ) ||
	  ( (!fd) && (atoi(command_result) != bytesent) ))
 	fprintf(stderr, "\tremote file is a different size from local after"
		" upload!\n"); 
      else 
	/* unlink files if we wanna remove them after *sucessful* send */
	if(fd && unlinkmode) {
	  if((unlink(file))&&(verbose>1))
	    fprintf(stderr,"\tunable to remove sent file\n");
	  else
	    fprintf(stderr,"\tsent file removed.\n");
	}
    }
    else
      fprintf(stderr,"\tcouldn't stat remote file after upload. Please"
	      " check it.\n"); 
    
    /* give them cps ratings */
    if (verbose > 1) {
      stoptime = time(NULL);
      if ( (etime = (stoptime - starttime)) )
	cps = (double)bytesent / (double)etime;
      else
	cps = (double)bytesent;
      if ( verbose > 2)
	fprintf(stderr, "\r");
      fprintf(stderr, "\t%d bytes sent in %ld seconds, cps = %.2f\n",
	      bytesent, etime, cps ); 
    }
  }

  /* be very nice, and close the file if it was left open */
  if (fd > 0) {
    close(fd);
    fd = -1;
  }

  close(term);

  /* give them global cps rating */
  if ( (verbose > 1) && (filesent > 1) ) {
    total_stoptime = time(NULL);
    if ( (etime = ( total_stoptime - total_starttime )) )
      cps = (double)total_bytesent / (double)etime;
    else
      cps = (double)total_bytesent;
    fprintf(stderr, "%d total bytes sent in %ld seconds, cps ="
	    " %.2f\n", total_bytesent, etime, cps ); 
  }
}


