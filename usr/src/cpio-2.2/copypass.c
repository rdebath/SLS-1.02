/* copypass.c - cpio copy pass sub-function.
   Copyright (C) 1990, 1991, 1992 Free Software Foundation, Inc.

   This program is free software; you can redistribute it and/or modify
   it under the terms of the GNU General Public License as published by
   the Free Software Foundation; either version 2, or (at your option)
   any later version.

   This program is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
   GNU General Public License for more details.

   You should have received a copy of the GNU General Public License
   along with this program; if not, write to the Free Software
   Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.  */

#include <stdio.h>
#include <sys/types.h>
#include <sys/stat.h>
#include "filetypes.h"
#include "system.h"
#include "cpiohdr.h"
#include "dstring.h"
#include "extern.h"

#ifndef HAVE_LCHOWN
#define lchown chown
#endif

/* Copy files listed on the standard input into directory `directory_name'.
   If `link_flag', link instead of copying.  */

void
process_copy_pass ()
{
  dynamic_string input_name;	/* Name of file from stdin.  */
  dynamic_string output_name;	/* Name of new file.  */
  int dirname_len;		/* Length of `directory_name'.  */
  int res;			/* Result of functions.  */
  char *slash;			/* For moving past slashes in input name.  */
  struct utimbuf times;		/* For resetting file times after copy.  */
  struct stat in_file_stat;	/* Stat record for input file.  */
  struct stat out_file_stat;	/* Stat record for output file.  */
  int in_file_des;		/* Input file descriptor.  */
  int out_file_des;		/* Output file descriptor.  */
  int existing_dir;		/* True if file is a dir & already exists.  */

  /* Initialize the copy pass.  */
  dirname_len = strlen (directory_name);
  ds_init (&input_name, 128);
  ds_init (&output_name, dirname_len + 2);
  strcpy (output_name.ds_string, directory_name);
  output_name.ds_string[dirname_len] = '/';
  output_is_seekable = TRUE;
  /* Initialize this in case it has members we don't know to set.  */
  bzero (&times, sizeof (struct utimbuf));

  /* Copy files with names read from stdin.  */
  while (ds_fgetstr (stdin, &input_name, name_end) != NULL)
    {
      char *link_name = NULL;

      /* Check for blank line and ignore it if found.  */
      if (input_name.ds_string[0] == '\0')
	{
	  error (0, 0, "blank line ignored");
	  continue;
	}

      /* Check for current directory and ignore it if found.  */
      if (input_name.ds_string[0] == '.'
	  && (input_name.ds_string[1] == '\0'
	      || (input_name.ds_string[1] == '/'
		  && input_name.ds_string[2] == '\0')))
	continue;

      if ((*xstat) (input_name.ds_string, &in_file_stat) < 0)
	{
	  error (0, errno, "%s", input_name.ds_string);
	  continue;
	}

      /* Make the name of the new file.  */
      for (slash = input_name.ds_string; *slash == '/'; ++slash)
	;
      ds_resize (&output_name, dirname_len + strlen (slash) + 2);
      strcpy (output_name.ds_string + dirname_len + 1, slash);

      existing_dir = FALSE;
      if (lstat (output_name.ds_string, &out_file_stat) == 0)
	{
	  if (S_ISDIR (out_file_stat.st_mode)
	      && S_ISDIR (in_file_stat.st_mode))
	    {
	      /* If there is already a directory there that
		 we are trying to create, don't complain about it.  */
	      existing_dir = TRUE;
	    }
	  else if (!unconditional_flag
		   && in_file_stat.st_mtime < out_file_stat.st_mtime)
	    {
	      error (0, 0, "%s not created: newer or same age version exists",
		     output_name.ds_string);
	      continue;		/* Go to the next file.  */
	    }
	  else if (unlink (output_name.ds_string))
	    {
	      error (0, errno, "cannot remove current %s",
		     output_name.ds_string);
	      continue;		/* Go to the next file.  */
	    }
	}

      /* Do the real copy or link.  */
      if (S_ISREG (in_file_stat.st_mode))
	{
	  /* Can the current file be linked to a another file?
	     Set link_name to the original file name.  */
#ifndef __MSDOS__
	  if (link_flag)
	    {
	      /* User said to link it if possible.  */
	      link_name = input_name.ds_string;
	    }
	  else if (in_file_stat.st_nlink > 1)
	    {
	      /* Is the file a link to a previously copied file?  */
	      link_name = find_inode_file (in_file_stat.st_ino,
					   major (in_file_stat.st_dev),
					   minor (in_file_stat.st_dev));
	      if (link_name == NULL)
		add_inode (in_file_stat.st_ino, output_name.ds_string,
			   major (in_file_stat.st_dev),
			   minor (in_file_stat.st_dev));
	    }
#endif

	  /* If there is a link name, do the link.  */
	  if (link_name != NULL)
	    {
#ifndef __MSDOS__
	      res = link (link_name, output_name.ds_string);
	      if (res < 0 && create_dir_flag)
		{
		  create_all_directories (output_name.ds_string);
		  res = link (link_name, output_name.ds_string);
		}
	      if (res == 0)
		{
		  if (verbose_flag)
		    error (0, 0, "%s linked to %s",
			   link_name, output_name.ds_string);
		}
	      else if (link_flag)
		{
		  error (0, errno, "cannot link %s to %s",
			 link_name, output_name.ds_string);
		}
	      else
#endif
		{
		  /* For some reason we can not link a previously linked file.
		     Do a complete copy.  */
		  link_name = NULL;
		}
	    }

	  /* If the file was not linked, copy contents of file.  */
	  if (link_name == NULL)
	    {
	      in_file_des = open (input_name.ds_string,
				  O_RDONLY | O_BINARY, 0);
	      if (in_file_des < 0)
		{
		  error (0, errno, "%s", input_name.ds_string);
		  continue;
		}
	      out_file_des = open (output_name.ds_string,
				   O_CREAT | O_WRONLY | O_BINARY, 0600);
	      if (out_file_des < 0 && create_dir_flag)
		{
		  create_all_directories (output_name.ds_string);
		  out_file_des = open (output_name.ds_string,
				       O_CREAT | O_WRONLY | O_BINARY, 0600);
		}
	      if (out_file_des < 0)
		{
		  error (0, errno, "%s", output_name.ds_string);
		  close (in_file_des);
		  continue;
		}

	      copy_files (in_file_des, out_file_des, in_file_stat.st_size);
	      empty_output_buffer (out_file_des);
	      if (close (in_file_des) < 0)
		error (0, errno, "%s", input_name.ds_string);
	      if (close (out_file_des) < 0)
		error (0, errno, "%s", output_name.ds_string);

	      /* Set the attributes of the new file.  */
	      if (!no_chown_flag)
		if ((chown (output_name.ds_string,
			    set_owner_flag ? set_owner : in_file_stat.st_uid,
		      set_group_flag ? set_group : in_file_stat.st_gid) < 0)
		    && errno != EPERM)
		  error (0, errno, "%s", output_name.ds_string);
	      /* chown may have turned off some permissions we wanted. */
	      if (chmod (output_name.ds_string, in_file_stat.st_mode) < 0)
		error (0, errno, "%s", output_name.ds_string);
	      if (reset_time_flag)
		{
		  times.actime = in_file_stat.st_atime;
		  times.modtime = in_file_stat.st_mtime;
		  if (utime (input_name.ds_string, &times) < 0)
		    error (0, errno, "%s", input_name.ds_string);
		  if (utime (output_name.ds_string, &times) < 0)
		    error (0, errno, "%s", output_name.ds_string);
		}
	      if (retain_time_flag)
		{
		  times.actime = times.modtime = in_file_stat.st_mtime;
		  if (utime (output_name.ds_string, &times) < 0)
		    error (0, errno, "%s", output_name.ds_string);
		}
	    }
	}
      else if (S_ISDIR (in_file_stat.st_mode))
	{
	  if (!existing_dir)
	    res = mkdir (output_name.ds_string, in_file_stat.st_mode);
	  else
	    res = 0;
	  if (res < 0 && create_dir_flag)
	    {
	      create_all_directories (output_name.ds_string);
	      res = mkdir (output_name.ds_string, in_file_stat.st_mode);
	    }
	  if (res < 0)
	    {
	      error (0, errno, "%s", output_name.ds_string);
	      continue;
	    }
	  if (!no_chown_flag)
	    if ((chown (output_name.ds_string,
			set_owner_flag ? set_owner : in_file_stat.st_uid,
		      set_group_flag ? set_group : in_file_stat.st_gid) < 0)
		&& errno != EPERM)
	      error (0, errno, "%s", output_name.ds_string);
	  /* chown may have turned off some permissions we wanted. */
	  if (chmod (output_name.ds_string, in_file_stat.st_mode) < 0)
	    error (0, errno, "%s", output_name.ds_string);
	  if (retain_time_flag)
	    {
	      times.actime = times.modtime = in_file_stat.st_mtime;
	      if (utime (output_name.ds_string, &times) < 0)
		error (0, errno, "%s", output_name.ds_string);
	    }
	}
#ifndef __MSDOS__
      else if (S_ISCHR (in_file_stat.st_mode) ||
	       S_ISBLK (in_file_stat.st_mode) ||
#ifdef S_ISFIFO
	       S_ISFIFO (in_file_stat.st_mode) ||
#endif
#ifdef S_ISSOCK
	       S_ISSOCK (in_file_stat.st_mode) ||
#endif
	       0)
	{
	  res = mknod (output_name.ds_string, in_file_stat.st_mode,
		       in_file_stat.st_rdev);
	  if (res < 0 && create_dir_flag)
	    {
	      create_all_directories (output_name.ds_string);
	      res = mknod (output_name.ds_string, in_file_stat.st_mode,
			   in_file_stat.st_rdev);
	    }
	  if (res < 0)
	    {
	      error (0, errno, "%s", output_name.ds_string);
	      continue;
	    }
	  if (!no_chown_flag)
	    if ((chown (output_name.ds_string,
			set_owner_flag ? set_owner : in_file_stat.st_uid,
		      set_group_flag ? set_group : in_file_stat.st_gid) < 0)
		&& errno != EPERM)
	      error (0, errno, "%s", output_name.ds_string);
	  /* chown may have turned off some permissions we wanted. */
	  if (chmod (output_name.ds_string, in_file_stat.st_mode) < 0)
	    error (0, errno, "%s", output_name.ds_string);
	  if (retain_time_flag)
	    {
	      times.actime = times.modtime = in_file_stat.st_mtime;
	      if (utime (output_name.ds_string, &times) < 0)
		error (0, errno, "%s", output_name.ds_string);
	    }
	}
#endif

#ifdef S_ISLNK
      else if (S_ISLNK (in_file_stat.st_mode))
	{
	  link_name = (char *) xmalloc ((unsigned int) in_file_stat.st_size + 1);

	  if (readlink (input_name.ds_string, link_name,
			in_file_stat.st_size) < 0)
	    {
	      error (0, errno, "%s", input_name.ds_string);
	      free (link_name);
	      continue;
	    }
	  link_name[in_file_stat.st_size] = '\0';

	  res = symlink (link_name, output_name.ds_string);
	  if (res < 0 && create_dir_flag)
	    {
	      create_all_directories (output_name.ds_string);
	      res = symlink (link_name, output_name.ds_string);
	    }
	  if (res < 0)
	    {
	      error (0, errno, "%s", output_name.ds_string);
	      free (link_name);
	      continue;
	    }

	  /* Set the attributes of the new link.  */
	  if (!no_chown_flag)
	    if ((lchown (output_name.ds_string,
			 set_owner_flag ? set_owner : in_file_stat.st_uid,
		      set_group_flag ? set_group : in_file_stat.st_gid) < 0)
		&& errno != EPERM)
	      error (0, errno, "%s", output_name.ds_string);
	  free (link_name);
	}
#endif
      else
	{
	  error (0, 0, "%s: unknown file type", input_name.ds_string);
	}

      if (verbose_flag)
	fprintf (stderr, "%s\n", output_name.ds_string);
      if (dot_flag)
	fputc ('.', stderr);
    }

  if (dot_flag)
    fputc ('\n', stderr);
  res = (output_bytes + io_block_size - 1) / io_block_size;
  if (res == 1)
    fprintf (stderr, "1 block\n");
  else
    fprintf (stderr, "%d blocks\n", res);
}
