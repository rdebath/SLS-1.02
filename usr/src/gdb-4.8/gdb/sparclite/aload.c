/* Program to load an image into the SPARClite monitor board
   Copyright 1993 Free Software Foundation, Inc.

This program is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 2 of the License, or
(at your option) any later version.

This program is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with this program; if not, write to the Free Software
Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.  */

/* Call with:

   aload PROG TTY

ie: aload hello /dev/ttya

*/

#include <stdio.h>
#include <fcntl.h>
#include <termios.h>
#include <unistd.h>
#define min(A, B) (((A) < (B)) ? (A) : (B))

#include <sys/types.h>
#include <bfd.h>

extern void *malloc();

static void
sys_error(msg)
     char *msg;
{
  perror(msg);
  exit(1);
}

static void
error(msg)
     char *msg;
{
  fputs(msg, stdout);
  fputc('\n', stdout);
  exit(1);
}

static FILE *ttyf;

static void
sendex(outtxt, outlen, intxt, inlen, id)
     unsigned char *outtxt;
     int outlen;
     unsigned char *intxt;
     int inlen;
     char *id;
{
  char buf[100];
  int cc;

  if (outlen > 0)
    {
      cc = write(fileno(ttyf), outtxt, outlen);
      if (cc != outlen)
	sys_error("Write %s failed", id);
    }

  if (inlen > 0)
    {
      cc = read(fileno(ttyf), buf, inlen);	/* Get reply */
      if (cc != inlen)
	sys_error("Read %s reply failed", id);
      if (bcmp(buf, intxt, inlen) != 0)
	error("Bad reply to %s", id);
    }
}

main(argc, argv)
     int argc;
     char **argv;
{
  struct termios termios;
  unsigned char *loadaddr = (unsigned char *)0x40000000; /* Where the code goes */
  int cc, progsize, i;
  unsigned char buf[10];
  asection *section;
  bfd *pbfd;
  unsigned long entry;

  pbfd = bfd_openr(argv[1], 0);

  if (!pbfd)
    sys_error("Open of PROG failed");

/* setup the tty.  Must be raw, no flow control, 9600 baud */

  ttyf = fopen(argv[2], "r+");
  if (!ttyf)
    sys_error("Open of TTY failed");
  setbuf(ttyf, NULL);		/* Strictly unbuffered */

  if (tcgetattr(fileno(ttyf), &termios))
    sys_error("tcgetattr failed");

  termios.c_iflag = 0;
  termios.c_oflag = 0;
  termios.c_cflag = CS8 | CREAD | CLOCAL;
  termios.c_lflag = 0;
  termios.c_cc[VMIN] = 1;
  termios.c_cc[VTIME] = 0;

  if (cfsetospeed(&termios, B9600)
      || cfsetispeed(&termios, B9600))
    sys_error("cfset{i|o}speed failed");

  if (tcsetattr(fileno(ttyf), TCSANOW, &termios))
    sys_error("tcsetattr failed");

  sendex("", 1, "\xaa", 1, "alive?");
  sendex("\x55", 1, "\x55", 1, "alive");
  printf("[SPARClite appears to be alive]\n");

  if (!bfd_check_format (pbfd, bfd_object)) 
    error ("It doesn't seem to be an object file");

  for (section = pbfd->sections; section; section = section->next) 
    {
      if (bfd_get_section_flags (pbfd, section) & SEC_ALLOC)
	{
	  unsigned char *section_address;
	  unsigned long section_size;
	  const char *section_name;

	  section_name = bfd_get_section_name (pbfd, section);

	  section_address = bfd_get_section_vma (pbfd, section)
	    + loadaddr;
	  section_size = bfd_section_size (pbfd, section);

	  printf("[Loading section %s at %x (%d bytes)]\n",
		 section_name,
		 section_address,
		 section_size);

	  if (bfd_get_section_flags (pbfd, section) & SEC_LOAD) /* Text, data or lit */
	    {
	      file_ptr fptr;

	      fptr = 0;

	      while (section_size > 0)
		{
		  char buffer[1024];
		  int count, i;
		  unsigned char checksum;
		  static char inds[] = "|/-\\";
		  static int k = 0;

		  count = min (section_size, 1024);

		  bfd_get_section_contents (pbfd, section, buffer, fptr,
					    count);

		  checksum = 0;
		  for (i=0; i < count; i++)
		    checksum += buffer[i];

		  printf("\r%c", inds[k++ % 4]);
		  fflush(stdout);

		  sendex("\001", 1, "\x5a", 1, "load command");
		  sendex(&section_address, 4, NULL, 0, "load address");
		  sendex(&count, 4, NULL, 0, "program size");
		  sendex(buffer, count, &checksum, 1, "program");

		  section_address += count;
		  fptr += count;
		  section_size -= count;
		}
	    }
	  else			/* BSS */
	    printf ("Not loading BSS \n");
	}
    }

  entry = bfd_get_start_address (pbfd);
  
  printf("[Starting %s at 0x%x]\n", argv[1], entry);

  sendex("\003", 1, NULL, 0, "exec command");
  sendex(&entry, 4, "\x55", 1, "program start");
}
