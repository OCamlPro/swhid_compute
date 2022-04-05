/**
 * Filesystem helpers

 * Copyright (C) 2005, 2007, 2008, 2009  Sylvain Beucler

 * This file is part of GNU FreeDink

 * GNU FreeDink is free software; you can redistribute it and/or
 * modify it under the terms of the GNU General Public License as
 * published by the Free Software Foundation; either version 3 of the
 * License, or (at your option) any later version.

 * GNU FreeDink is distributed in the hope that it will be useful, but
 * WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 * General Public License for more details.

 * You should have received a copy of the GNU General Public License
 * along with this program.  If not, see
 * <http://www.gnu.org/licenses/>.
 */

#ifdef HAVE_CONFIG_H
#include <config.h>
#endif

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <strings.h>  /* strcasecmp */
#include <dirent.h>
#include <errno.h>

/* stat(2) */
#include <sys/types.h>
#include <sys/stat.h>
#include <unistd.h>

#include "binreloc.h"
#include "progname.h"
#include "relocatable.h"
#include "SDL.h"
#ifdef HAVE_LIBZIP
#  include "SDL_rwops_libzip.h"
#else
#  ifdef HAVE_ZZIPLIB
#include "SDL_rwops_zzip.h"
#  endif
#endif

#include "paths.h"

/* Returns a pointer to the end of the current path element (file or
   directory) */
static char*
end_of_elt(char *str)
{
  char *p = str;
  while ((*p != '/') && (*p != '\\') && (*p != '\0'))
    p++;
  return p;
}

/**
 * Look for filename case-insensitively, to mimic MS Woe's
 * case-insensitive file system. It modifies the 'filename' buffer
 * in-place. Also converts '\' to '/'. I REPEAT: the filename buffer
 * is modified, so it cannot be "a constant string", or you'll get a
 * segfault. */
void
ciconvert (char *filename)
{
#if defined _WIN32 || defined __WIN32__ || defined __CYGWIN__ || defined __EMX__ || defined __DJGPP__
  return;
#else
  /* Parse all the directories that composes filename */
  char *cur_dir = NULL;
  char *pcur_elt, *pend_of_elt, *pend_of_cur_dir;
  int error = 0;
  
  /* First simply try to open the file, constant complexity. Permits
     decent speed for careful authors who write exact filenames. */
  FILE *f;
  if ((f = fopen(filename, "r")) != NULL)
    {
      fclose(f);
      return;
    }

  /* Else, check each path element of the filename */
  /* No need to support volumes ("C:\"...) because this function
     already returned in case-insensitive environments (woe&dos) */
  if (filename[0] == '/')
    {
      cur_dir = malloc(strlen("/") + strlen(filename) + 1);
      strcpy(cur_dir, "/");
      pend_of_cur_dir = cur_dir + 1;
    }
  else
    {
      cur_dir = malloc(strlen("./") + strlen(filename) + 1);
      strcpy(cur_dir, "./");
      pend_of_cur_dir = cur_dir + 2;
    }
  pcur_elt = filename;
  do
    {
      char end_of_elt_backup;

      /* skip leading or multiple slashes */
      while (*pcur_elt == '/' || *pcur_elt == '\\')
	{
	  *pcur_elt = '/';
	  pcur_elt++;
	}

      pend_of_elt = end_of_elt(pcur_elt);
      end_of_elt_backup = *pend_of_elt;
      *pend_of_elt = '\0';
      /* pcur_elt is now a \0-terminated string containing the current
	 path element. */
      
      /* Now check if there's a matching entry in the directory */
      DIR *list;
      struct dirent *entry;
      int found = 0;
      
      list = opendir (cur_dir);
      if (list != NULL)
	{
	  /* note: we may want to sort list first */
	  while ((entry = readdir (list)) != NULL && !found)
	    {
	      if (strcasecmp(pcur_elt, entry->d_name) == 0)
		{
		  /* Good case-insensitive match: replace the
		     user-provided filename with it */
		  strcpy(pcur_elt, entry->d_name);
		  found = 1;
		}
	    }
	  closedir (list);
	}
      if (!found)
	error = 1;

      /* Prepare parsing next path element, unless the current element
	 was the last one */
      if (end_of_elt_backup != '\0')
	{
	  *pend_of_elt = '/'; /* restore */

	  /* Prepare next directory */
	  {
	    int cur_elt_len = pend_of_elt - pcur_elt;
	    strncpy(pend_of_cur_dir, pcur_elt, cur_elt_len + 1);
	    pend_of_cur_dir += cur_elt_len + 1;
	    *pend_of_cur_dir = '\0';
	  }
	  
	  /* go to the next path element */
	  pcur_elt = pend_of_elt + 1;
	}
    }
  while(*pend_of_elt != '\0' && !error);
  free(cur_dir);

  /* If there was an error, we return a half-converted path (maybe the
     file didn't exist yet, but leading directories still needed to be
     converted); otherwise, filename contains the fully-converted
     path, ready to be opened on a case-sensitive filesystem. */
#endif /* !_WIN32 */
}

/**
 * Does this file exist and can be opened?
 * Note: this is case-sensitive.
 */
int exist(char *name)
{
  FILE *fp = fopen(name, "rb");
  if (!fp)
    return 0;
  fclose(fp);
  return 1;
}

/**
 * Is it a directory that exists?
 */
int is_directory(char *name)
{
  char *tmp_filename = strdup(name);
  int accessible = 0;
  int retval = 0;

  struct stat buf;
  /* ciconvert(tmp_filename); */
  accessible = stat(tmp_filename, &buf);
  free(tmp_filename);

  if (accessible < 0)
    retval = 0;
  else
    retval = S_ISDIR(buf.st_mode);

  return retval;
}

/**
 * So-called "portable" dirname - that is, it supports backslash and
 * forward-slash. That way, it can process filenames from dink.ini, in
 * particular. Return a newly allocated string.
 */
char*
pdirname (const char* filename)
{
  char *retval = strdup(filename);
  char *pc = retval + strlen(retval);
  while (pc >= retval && *pc != '/' && *pc != '\\')
    pc--;
  if (pc >= retval)
    *pc = '\0';
  return retval;
}

SDL_RWops* find_resource_as_rwops(char *name)
{
  SDL_RWops* rwops = NULL;

  /** pkgdatadir, pkgdefaultdatadir, exedir **/
  FILE *in = NULL;
  if (in == NULL)
    in = paths_pkgdatafile_fopen(name, "rb");
  if (in == NULL)
    /* When the relocatable datadir fails, it may be worth trying the
       compile-time datadir nonetheless; in the gNewSense LiveCD, the
       path is mistakenly detected as /cow/usr/bin/freedink, and hence
       the datadir becomes /cow/usr/share/freedink, which doesn't
       exist. */
    in = paths_defaultpkgdatafile_fopen(name, "rb");
  if (in == NULL)
    in = paths_exedirfile_fopen(name, "rb");
  if (in != NULL)
    rwops = SDL_RWFromFP(in, /*autoclose=*/1);

  if (rwops != NULL)  
    return rwops;


  /** Bundled resources **/
  /* Look in appended ZIP archive */
  /* Looked at last, to avoid confusing the user with invisible
     files **/
  /* paths_getexefile() checks /proc (Linux), then argv[0] +
     PATH. Under Woe it uses GetModuleFileName(). The only way to make
     it fail is to execl("./freedink", "idontexist", 0); */
#ifdef HAVE_LIBZIP
  char *myself = strdup(paths_getexefile());
  rwops = SDL_RWFromZIP(myself, name);
  free(myself);
#else
#  ifdef HAVE_ZZIPLIB
  const char *myself = paths_getexefile();
  char *zippath = malloc(strlen(myself) + 1 + strlen(name) + 1);
  sprintf(zippath, "%s/%s", myself, name);
  /* sample zippath: "/usr/bin/freedink/LiberationSans-Regular.ttf" */
  rwops = SDL_RWFromZZIP(zippath, "rb");
  /* Retrieve error (if any) with: printf("%s\n", strerror(errno)); */
  free(zippath);
#  endif
#endif

  if (rwops != NULL)
     return rwops;

  /** Not found! **/
  return NULL;
}

/**
 * Append a line of text at the end of a file
 */
void add_text(char *text, char *filename)
{
  if (strlen(text) < 1)
    return;
  
  FILE *fp = paths_dmodfile_fopen(filename, "ab");
  if (fp != NULL)
    {
      fwrite(text, strlen(text), 1, fp);
      fclose(fp);
    }
  else
    {
      perror("add_text");
    }
}


/**
 * Read integer portably (same result with MSB and LSB
 * endianness). Source data is a file with little-endian data.
 */
int read_lsb_int(FILE *f)
{
  unsigned char buf[4];
  fread(buf, 4, 1, f);
  return (buf[3] << 24) | (buf[2] << 16) | (buf[1] << 8) | (buf[0]);
}


/**
 * Write integer portably (same result with MSB and LSB
 * endianness). Will write little-endian data to file.
 */
void write_lsb_int(int n, FILE *f)
{
  unsigned char buf[4];
  buf[0] = n & 0xFF;
  buf[1] = (n >> (1*8)) & 0xFF;
  buf[2] = (n >> (2*8)) & 0xFF;
  buf[3] = (n >> (3*8)) & 0xFF;
  fwrite(buf, 4, 1, f);
}

/**
 * Read short portably (same result with MSB and LSB
 * endianness). Source data is a file with little-endian data.
 */
short read_lsb_short(FILE *f)
{
  unsigned char buf[2];
  fread(buf, 2, 1, f);
  return (buf[1] << 8) | (buf[0]);
}


/**
 * Write short portably (same result with MSB and LSB
 * endianness). Will write little-endian data to file.
 */
void write_lsb_short(short n, FILE *f)
{
  unsigned char buf[2];
  buf[0] = n & 0xFF;
  buf[1] = (n >> (1*8)) & 0xFF;
  fwrite(buf, 2, 1, f);
}
