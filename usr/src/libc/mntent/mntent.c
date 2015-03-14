#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <mntent.h>

struct mntent *getmntent(FILE *filep)
{
  char *cp, *sep = " \t\n";
  static char buff[MNTMAXSTR];
  static struct mntent mnt;

  /* Loop on the file, skipping comment lines. - FvK 03/07/93 */
  while(fgets(buff, sizeof buff, filep) != NULL) {
	if (buff[0] == '#' || buff[0] == '\n') continue;
	break;
  }

  mnt.mnt_fsname = strtok(buff, sep);
  if (mnt.mnt_fsname == NULL)
    return NULL;

  mnt.mnt_dir = strtok(NULL, sep);
  if (mnt.mnt_dir == NULL)
    return NULL;

  mnt.mnt_type = strtok(NULL, sep);
  if (mnt.mnt_type == NULL)
#if 0
    mnt.mnt_type = MNTTYPE_MINIX;
#else
    return NULL;
#endif

  mnt.mnt_opts = strtok(NULL, sep);
  if (mnt.mnt_opts == NULL)
    mnt.mnt_opts = "";

  cp = strtok(NULL, sep);
  mnt.mnt_freq = (cp != NULL) ? atoi(cp) : 0;

  cp = strtok(NULL, sep);
  mnt.mnt_passno = (cp != NULL) ? atoi(cp) : 0;

  return &mnt;
}

int addmntent(FILE *filep, const struct mntent *mnt)
{
  if (fseek(filep, 0, SEEK_END) < 0)
    return 1;

  if (fprintf(filep, "%s %s %s %s %d %d\n", mnt->mnt_fsname, mnt->mnt_dir,
	      mnt->mnt_type, mnt->mnt_opts, mnt->mnt_freq, mnt->mnt_passno)
      < 1)
    return 1;

  return 0;
}

char *hasmntopt(const struct mntent *mnt, const char *opt)
{
  return strstr(mnt->mnt_opts, opt);
}

int endmntent(FILE *filep)
{
  fclose(filep);
  return 1;
}
