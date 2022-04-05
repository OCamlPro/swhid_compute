/**
 * DinkC script engine

 * Copyright (C) 1997, 1998, 1999, 2002, 2003  Seth A. Robinson
 * Copyright (C) 2005, 2006  Dan Walma
 * Copyright (C) 2005, 2007, 2008, 2009, 2011  Sylvain Beucler

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
#include <strings.h> /* compare */
#include <xalloc.h>

#include "gettext.h"
#define _(String) gettext (String)

#include "dinkc.h"
#include "dinkc_bindings.h"
#include "game_engine.h"
#include "input.h"
#include "paths.h"
#include "str_util.h"
#include "log.h"

int returnint = 0;
int bKeepReturnInt = 0;
char returnstring[200];
/* Used to tell decipher_string about the currently selected savegame
   in a choice menu; also abuse to tell which key is selected in
   joystick remapping */
unsigned short decipher_savegame = 0;

#define MAX_CALLBACKS 100
struct call_back
{
  int owner;
  /*bool*/int active;
  int type;
  char name[20];
  int offset;
  long min, max;
  int lifespan;
  unsigned long timer;
};
static struct call_back callback[MAX_CALLBACKS];

/* DinkC script buffer */
static char *rbuf[MAX_SCRIPTS]; //pointers to buffers we may need

/* Number of reserved ASCII indexes in .d BPE compression format */
#define NB_PAIRS_MAX 128


struct refinfo *rinfo[MAX_SCRIPTS];


/**
 * Decompress a .d DinkC script; also clean newlines. Check
 * contrib/d2c.c for more explanation about the decompression process.
 */
static void decompress(FILE *in, int script)
{
  int step = 512;
  int nb_read = 0;
  rbuf[script] = xmalloc(step);
  rbuf[script][0] = '\0';

  unsigned char stack[NB_PAIRS_MAX+1], pairs[NB_PAIRS_MAX][2];
  short c, top = -1;
  int nb_pairs = 0;
  
  /* Check for optional pair count and pair table */
  if ((c = fgetc(in)) > 127)
    {
      /* Read pairs table */
      nb_pairs = c - 128;
      int i, j;
      for (i = 0; i < nb_pairs; i++)
	{
	  for (j = 0; j < 2; j++)
	    {
	      int c = fgetc(in);
	      if (c == EOF)
		{
		  log_error("decompress: invalid header: truncated pair table");
		  free(rbuf[script]);
		  rbuf[script] = NULL;
		  return;
		}
	      if (c > i+128)
		{
		  log_error("decompress: invalid header: reference to a pair that is not registered yet");
		  free(rbuf[script]);
		  rbuf[script] = NULL;
		  return;
		}
	      pairs[i][j] = c;
	    }
	}
    }
  else
    {
      /* Non-compressed file, put back the character we read */
      ungetc(c, in);
    }
  
  for (;;)
    {
      /* Pop byte from stack or read byte from file */
      if (top >= 0)
	c = stack[top--];
      else if ((c = fgetc(in)) == EOF)
	break;
    
      /* Push pair on stack or output byte to file */
      if (c > 127)
	{
	  if ((c-128) >= nb_pairs)
	    {
	      log_error("decompress: invalid body: references non-existent pair");
	      break;
	    }
	  stack[++top] = pairs[c-128][1];
	  stack[++top] = pairs[c-128][0];
	}
      else
	{
	  rbuf[script][nb_read] = c;
	  nb_read++;
	  if ((nb_read % step) == 0)
	    rbuf[script] = xrealloc(rbuf[script], nb_read + step);
	}
    }
  rinfo[script]->end = nb_read;
  rbuf[script][nb_read] = '\0'; /* safety */
  rbuf[script] = xrealloc(rbuf[script], nb_read+1);
}

static void decompress_nocomp(FILE *in, int script)
{
  int step = 512;
  int nb_read = 0;
  rbuf[script] = xmalloc(step);
  rbuf[script][0] = '\0';

  int c;
  while ((c = getc(in)) != EOF)
    {
      rbuf[script][nb_read] = c;
      nb_read++;
      if ((nb_read % step) == 0)
	rbuf[script] = xrealloc(rbuf[script], nb_read + step);
    }
  rinfo[script]->end = nb_read;
  rbuf[script][nb_read] = '\0'; /* safety */
  rbuf[script] = xrealloc(rbuf[script], nb_read+1);
}


/**
 * Only load game metadata (timetime). Used when displaying the list
 * of saved games (see decipher_string).
 */
static /*bool*/int load_game_small(int num, char line[196], int *mytime)
{
  FILE *f = paths_savegame_fopen(num, "rb");
  if (f == NULL)
    {
      log_info("Couldn't quickload save game %d", num);
      return /*false*/0;
    }
  else
    {
      //int version = read_lsb_int(f);
      fseek(f, 4, SEEK_CUR);

      fread(line, 196, 1, f);
      line[195] = '\0';
      *mytime = read_lsb_int(f);
      fclose(f);

      return /*true*/1;
    }
}


/**
 * Load script from 'filename', save it in the first available script
 * slot, attach to game sprite #'sprite' if 'set_sprite' is 1.
 **/
int load_script(char filename[15], int sprite, /*bool*/int set_sprite)
{
  char temp[100];
  int script = 0;
  FILE *in = NULL;
  /*bool*/int comp = /*false*/0;
  
  log_info("LOADING %s", filename);
  
  sprintf(temp, "story/%s.d", filename);
  in = paths_dmodfile_fopen(temp, "rb");
  if (in == NULL)
    {
      sprintf(temp, "story/%s.c", filename);
      in = paths_dmodfile_fopen(temp, "rb");
      if (in == NULL)
	{
	  sprintf(temp, "story/%s.d", filename);
	  in = paths_fallbackfile_fopen(temp, "rb");
	  if (in == NULL)
	    {
	      sprintf(temp, "story/%s.c", filename);
	      in = paths_fallbackfile_fopen(temp, "rb");
	      if (in == NULL)
		{
		  log_warn("Script %s not found. (checked for .C and .D) (requested by %d?)", temp, sprite);
		  return 0;
		}
	    }
	}
    }
  
  strtoupper(temp);
  log_debug("Temp thingie is %c", temp[strlen(temp)-1]);
  if (temp[strlen(temp)-1] == 'D')
    comp = 1;
  else
    comp = 0;
  
  {
    int k = 1;
    for (; k < MAX_SCRIPTS; k++)
      if (rbuf[k] == NULL)
	break;
    script = k;
  }
  
  if (script == MAX_SCRIPTS)
    {
      log_error("Couldn't find unused buffer for script.");
      fclose(in);
      return 0;
    }

  log_info("Loading script %s.. (slot %d)", temp, script);
  rinfo[script] = XZALLOC(struct refinfo);
  if (rinfo[script] == NULL)
    {
      log_error("Couldn't allocate rscript %d.", script);
      return 0;
    }
  memset(rinfo[script], 0, sizeof(struct refinfo));
  /* For clarity: */
  rinfo[script]->current  = 0;
  rinfo[script]->cur_line = 1;
  rinfo[script]->cur_col  = 0;
  rinfo[script]->debug_line = 1;

  
  if (comp)
    {
      log_debug("Decompressing...");
      decompress(in, script);
    }
  else
    {
      log_debug("Reading from disk...");
      decompress_nocomp(in, script);
    }
  fclose(in);
  
  if (rbuf[script] == NULL)
    {
      log_error("Couldn't allocate rbuff %d.", script);
      free(rinfo[script]);
      rinfo[script] = NULL;
      return 0;
    }

  rinfo[script]->name = strdup(filename);
  rinfo[script]->sprite = sprite;
  
  if (set_sprite && sprite != 0 && sprite != 1000)
    spr[sprite].script = script;

  return script;
}


int dinkc_execute_one_liner(char* line)
{
  /* Find available script slot */
  int k = 1;
  for (k = 1; k < MAX_SCRIPTS; k++)
    if (rbuf[k] == NULL)
      break;

  if (k < MAX_SCRIPTS)
    {
      rinfo[k] = XZALLOC(struct refinfo);
      rinfo[k]->name = xmalloc(1);
      rinfo[k]->name[0] = '\0';
      rinfo[k]->sprite = 1000; /* survice screen change */
      rinfo[k]->level = 1; /* skip 'void main(void) {' parsing */
      rbuf[k] = (char*) malloc(255);
      strcpy(rbuf[k], line);
      process_line(k, rbuf[k], 0);
      return returnint;
    }
  else
    return -1;
}


/**
 * Remove leading spaces by shifting 'str' to the left, as much as
 * there is leading spaces.
 */
void strip_beginning_spaces(char *str)
{
  char *pc = str;
  int diff = 0;
/*   int i; */

  /* Find first non-space character (pos) */
  while (*pc == ' ')
    pc++;
  diff = pc - str;

  /* Shift string to the left from pos */
  /* Don't use str(str, pc) to avoid memory overlap */
  while (*pc != '\0')
    {
      *(pc - diff) = *pc;
      pc++;
    }
  *(pc - diff) = '\0';
}



/**
 * Locate a procedure (such as "void hit()")
 */
/*bool*/int locate(int script, char* lookup_proc)
{
  if (rinfo[script] == NULL)
    return 0;

  int save_current  = rinfo[script]->current;
  int save_cur_line = rinfo[script]->cur_line;
  int save_cur_col  = rinfo[script]->cur_col;
  int save_debug_line = rinfo[script]->debug_line;
  rinfo[script]->current  = 0;
  rinfo[script]->cur_line = 1;
  rinfo[script]->cur_col  = 0;
  rinfo[script]->debug_line = 1;

  char* line = NULL;
  char* word = NULL;

  while((line = read_next_line(script)) != NULL)
    {
      strip_beginning_spaces(line);
      
      int is_proc = 0;
      word = get_word(line, 1);
      if (compare(word, "VOID"))
	is_proc = 1;
      free(word);
      if (is_proc)
	{
	  char* cur_proc = NULL;
	  word = get_word(line, 2);
	  cur_proc = separate_string(word, 1, '(');
	  free(word);

	  int is_right_proc = 0;
	  if (compare(cur_proc, lookup_proc))
	    is_right_proc = 1;
	  free(cur_proc);

	  if (is_right_proc)
	    {
	      //clean up vars so it is ready to run
	      if (rinfo[script]->sprite != 1000)
		{
		  spr[rinfo[script]->sprite].move_active = 0;
		  if (dversion >= 108)
		    spr[rinfo[script]->sprite].move_nohard = 0;
		}
	      rinfo[script]->skipnext = /*false*/0;
	      rinfo[script]->onlevel = 0;
	      rinfo[script]->level = 0;
	      
	      free(line);
	      return 1;
	      //this is desired proc
	    }
	}
      free(line);
    }
  
  // Not found, restoring position
  rinfo[script]->current  = save_current;
  rinfo[script]->cur_line = save_cur_line;
  rinfo[script]->cur_col  = save_cur_col;
  rinfo[script]->debug_line = save_debug_line;
  return 0;
}

/**
 * Look for the 'label' label (e.g. 'loop:'), that is used by a "goto"
 * instruction. This sets the script->current field appropriately.
 **/
/*bool*/int locate_goto(char* expr, int script)
{
  replace_norealloc(";", "", expr);
  char* label = xmalloc(strlen(expr) + 1 + 1);
  sprintf(label, "%s:", expr);
  
  char* line = NULL;
  rinfo[script]->current = 0;
  rinfo[script]->cur_line = 1;
  while ((line = read_next_line(script)) != NULL)
    {
      strip_beginning_spaces(line);
      
      int is_right_label = 0;
      char* word = get_word(line, 1);
      replace_norealloc("\n", "", word);
      if (compare(word, label))
	is_right_label = 1;
      free(word);
      
      if (is_right_label)
	{
	  log_debug("Found goto : Line is %s, word is %s.", line, label);
	  
	  rinfo[script]->skipnext = /*false*/0;
	  rinfo[script]->onlevel = 0;
	  rinfo[script]->level = 0;
	  
	  free(label);
	  free(line);
	  return 1;
	  //this is desired label
	}
      free(line);
    }

  log_warn("%s: cannot goto %s", rinfo[script]->name, label);
  free(label);
  return 0;
}

/**
 * v1.07-style scope. This function is buggy: the first memory slot
 * has precedence (independently of local/global scope).
 * 
 * Return -1 if not found, slot index >1 if found. Slot 0 isn't
 * currently used by the engine.
 */
int search_var_with_this_scope_107(char* variable, int var_scope)
{
  int i;
  for (i = 1; i < MAX_VARS; i ++)
    if (play.var[i].active == 1
	&& ((play.var[i].scope == DINKC_GLOBAL_SCOPE) || (play.var[i].scope == var_scope))
	&& (compare(play.var[i].name, variable)))
      return i;
  return -1; /* not found */
}

/**
 * v1.08-style scope: local variables are searched before global
 * variables.
 *
 * Return -1 if not found, slot index >1 if found. Slot 0 isn't
 * currently used by the engine.
 */
int search_var_with_this_scope_108(char* variable, int var_scope)
{
  int search_scope[2];
  search_scope[0] = var_scope; /* first local scope */
  search_scope[1] = DINKC_GLOBAL_SCOPE; /* then global scope */

  int i;
  for (i = 0; i < 2; i++)
    {
      //We'll start going through every var, starting at one
      int v;
      for (v = 1; v < MAX_VARS; v++)
	{
	  //Okay... make sure the var is active,
	  //The scope should match the script,
	  //Then make sure the name is the same.
	  if (play.var[v].active
	      && play.var[v].scope == search_scope[i]
	      && compare (play.var[v].name, variable))
	    return v;
	}
    }
  return -1;
}

/**
 * 
 */
int search_var_with_this_scope(char* variable, int scope)
{
  if (dversion >= 108)
    return search_var_with_this_scope_108(variable, scope);
  return search_var_with_this_scope_107(variable, scope);      
}

/**
 * Expand 'variable' in the scope of 'script' and return the integer
 * value. Only used in function 'get_parms'.
 */
long decipher(char* variable, int script)
{
  // Special vars: &current_sprite and &current_script
  if (compare(variable, "&current_sprite"))
    return rinfo[script]->sprite;
  if (compare(variable, "&current_script"))
    return script;

  //v1.08 special variables.
  if (dversion >= 108)
    {
      if (compare(variable, "&return"))
	return returnint;
      if (compare(variable, "&arg1"))
	return rinfo[script]->arg1;
      if (compare(variable, "&arg2"))
	return rinfo[script]->arg2;
      if (compare(variable, "&arg3"))
	return rinfo[script]->arg3;
      if (compare(variable, "&arg4"))
	return rinfo[script]->arg4;
      if (compare(variable, "&arg5"))
	return rinfo[script]->arg5;
      if (compare(variable, "&arg6"))
	return rinfo[script]->arg6;
      if (compare(variable, "&arg7"))
	return rinfo[script]->arg7;
      if (compare(variable, "&arg8"))
	return rinfo[script]->arg8;
      if (compare(variable, "&arg9"))
	return rinfo[script]->arg9;
    }

  // Check in local and global variables
  int i = search_var_with_this_scope(variable, script);
  if (i != -1)
    return play.var[i].var;
  else
    return 0; // compatibility
}


/**
 * Replace all variables in a string; try longest variables
 * first. Known bug: may replace shorter variables (e.g. &gold instead
 * of &golden).
 */
void var_replace_107(char** line_p, int scope)
{
  char crap[20];
  int i;
  for (i = 1; i < MAX_VARS; i ++)
    if ((play.var[i].active == 1)
	&& ((play.var[i].scope == DINKC_GLOBAL_SCOPE) || (play.var[i].scope == scope)))
      {
	sprintf(crap, "%d", play.var[i].var);
	replace(play.var[i].name, crap, line_p);
      }
}

/**
 * Replace all variables in a string; try longest variables first.
 *
 * Possible improvements:
 * 
 * - Copy play.var[] and sort it by variable length (and avoid the
 *   recursion)
 *
 * - find vars in the string and replace them as-needed (requires
 *   understanding what exactly is an end-of-variable delimiter, if
 *   such a thing exists)
 */
void var_replace_108(int i, int script, char** line_p, char *prevar)
{
  while (i < MAX_VARS)
    {
      //First, make sure the variable is active.
      //Then, make sure it is in scope,
      //Then, see if the variable name is in the line
      //Then, prevar is null, or if prevar isn't null, see if current variable starts with prevar
      if (play.var[i].active
	  && i == search_var_with_this_scope_108(play.var[i].name, script)
	  && strstr (*line_p, play.var[i].name)
	  && (prevar == NULL || (prevar != NULL && strstr (play.var[i].name, prevar))))
	{
	  //Look for shorter variables
	  var_replace_108(i + 1, script, line_p, play.var[i].name);
	  //we didn't find any, so we replace!
	  char crap[20];
	  sprintf(crap, "%d", play.var[i].var);
	  replace(play.var[i].name, crap, line_p);
	}
      i++;
    }
}

/**
 * Replace all variables (&something) in 'line', with scope 'scope'
 */
void var_replace(char** line_p, int scope)
{
  if (dversion >= 108)
    var_replace_108(1, scope, line_p, NULL);
  else
    var_replace_107(line_p, scope);
}


/**
 * Similar to decipher, plus:
 * - expand special choice variables &savegameinfo and &buttoninfo
 * - it can replace several variables in the same string
 * - with v1.07 it has a prefix bug (see var_replace_107)
 */
void decipher_string(char** line_p, int script)
{
  char buffer[20 + 1];
  
  /* Replace all valid variables in 'line' */
  var_replace(line_p, script);
  
  if ((strchr(*line_p, '&') != NULL) && (script != 0))
    {
      sprintf(buffer, "%d", rinfo[script]->sprite); replace("&current_sprite", buffer, line_p);
      sprintf(buffer, "%d", script);                replace("&current_script", buffer, line_p);

      if (dversion >= 108)
	{
	  //v1.08 special variables.
	  sprintf(buffer, "%d", returnint);           replace("&return", buffer, line_p);
	  sprintf(buffer, "%d", rinfo[script]->arg1); replace("&arg1", buffer, line_p);
	  sprintf(buffer, "%d", rinfo[script]->arg2); replace("&arg2", buffer, line_p);
	  sprintf(buffer, "%d", rinfo[script]->arg3); replace("&arg3", buffer, line_p);
	  sprintf(buffer, "%d", rinfo[script]->arg4); replace("&arg4", buffer, line_p);
	  sprintf(buffer, "%d", rinfo[script]->arg5); replace("&arg5", buffer, line_p);
	  sprintf(buffer, "%d", rinfo[script]->arg6); replace("&arg6", buffer, line_p);
	  sprintf(buffer, "%d", rinfo[script]->arg7); replace("&arg7", buffer, line_p);
	  sprintf(buffer, "%d", rinfo[script]->arg8); replace("&arg8", buffer, line_p);
	  sprintf(buffer, "%d", rinfo[script]->arg9); replace("&arg9", buffer, line_p);
	}

      if (decipher_savegame != 0)
	{
	  int button_action = input_get_button_action(decipher_savegame-1);
	  if      (button_action == ACTION_ATTACK)    replace("&buttoninfo", _("Attack"), line_p);
	  else if (button_action == ACTION_TALK)      replace("&buttoninfo", _("Talk/Examine"), line_p);
	  else if (button_action == ACTION_MAGIC)     replace("&buttoninfo", _("Magic"), line_p);
	  else if (button_action == ACTION_INVENTORY) replace("&buttoninfo", _("Item Screen"), line_p);
	  else if (button_action == ACTION_MENU)      replace("&buttoninfo", _("Main Menu"), line_p);
	  else if (button_action == ACTION_MAP)       replace("&buttoninfo", _("Map"), line_p);
	  else if (button_action == ACTION_BUTTON7)   replace("&buttoninfo", _("Unused"), line_p);
	  else if (button_action == ACTION_BUTTON8)   replace("&buttoninfo", _("Unused"), line_p);
	  else if (button_action == ACTION_BUTTON9)   replace("&buttoninfo", _("Unused"), line_p);
	  else if (button_action == ACTION_BUTTON10)  replace("&buttoninfo", _("Unused"), line_p);
	  else if (button_action == ACTION_DOWN)      replace("&buttoninfo", _("Down"), line_p);
	  else if (button_action == ACTION_LEFT)      replace("&buttoninfo", _("Left"), line_p);
	  else if (button_action == ACTION_RIGHT)     replace("&buttoninfo", _("Right"), line_p);
	  else if (button_action == ACTION_UP)        replace("&buttoninfo", _("Up"), line_p);
	  else replace("&buttoninfo", _("Error: not mapped"), line_p);
	}
    }

  if ((decipher_savegame != 0) && compare(*line_p, "&savegameinfo"))
    {
      char gameinfo[196] = "";
      int mytime = 0;

      free(*line_p);
      if (load_game_small(decipher_savegame, gameinfo, &mytime) == 1)
	asprintf(line_p, _("Slot %d - %d:%02d - %s"), decipher_savegame,
		 mytime/60, mytime%60, gameinfo);
      else
	asprintf(line_p, _("Slot %d - Empty"), decipher_savegame);
    }
}

/**
 * 
 * name: name of the procedure() to call
 * n1: wait at least n1 milliseconds before callback
 * n2: wait at most n1+n2 milliseconds before callback
 * script: number of the script currently running
 **/
int add_callback(char name[20], int n1, int n2, int script)
{
  int k;
  for (k = 1; k < MAX_CALLBACKS; k++)
    {
      if (callback[k].active == /*false*/0)
	{
	  memset(&callback[k], 0, sizeof(callback[k]));
	  
	  callback[k].active = /*true*/1;
	  callback[k].min = n1;
	  callback[k].max = n2;
	  callback[k].owner = script;
	  strcpy(callback[k].name, name);
	  
	  log_debug("Callback added to %d.", k);
	  return(k);
	}
    }
  
  log_error("Couldn't add callback, all out of space");
  return 0;
}

void kill_callback(int cb)
{
  if (cb >= 0 && cb < MAX_CALLBACKS)
    callback[cb].active = /*false*/0;
}

void kill_callbacks_owned_by_script(int script)
{
  int i = 1;
  for (; i < MAX_CALLBACKS; i++)
    {
      if (callback[i].owner == script)
	{
	  log_debug("Kill_all_callbacks just killed %d for script %d", i, script);
	  //killed callback
	  callback[i].active = /*false*/0;
	}
    }
}


void kill_script(int k)
{
  if (rinfo[k] != NULL)
    {
      int i;

      kill_callbacks_owned_by_script(k);
      
      // Now let's kill all local vars associated with this script
      for (i = 1; i < MAX_VARS; i++)
	{
	  if (play.var[i].active && play.var[i].scope == k)
	    play.var[i].active = /*false*/0;
	}
      log_debug("Killed script %s. (num %d)", rinfo[k]->name, k);
      
      if (rinfo[k]->name != NULL)
	free(rinfo[k]->name);
      if (rinfo[k] != NULL)
	free(rinfo[k]);
      rinfo[k] = NULL;
      if (rbuf[k] != NULL)
	free(rbuf[k]);
      rbuf[k] = NULL;
    }
}


/**
 * Kill all scripts except those attached to pseudo-sprite 1000, which
 * is meant to survive across screen changes
 * (kill_all_scripts_for_real(...) is more brutal)
 *
 * Used by gfx_tiles only
 */
void kill_all_scripts(void)
{
  /* Kill scripts (except if attached to pseudo-sprite 1000) */
  int k = 1;
  for (; k < MAX_SCRIPTS; k++)
    {
      if (rinfo[k] != NULL)
	if (rinfo[k]->sprite != 1000)
	  kill_script(k);
    }
  
  /* Kill pending callbacks (except if attached to pseudo-sprite 1000) */
  for (k = 1; k < MAX_CALLBACKS; k++)
    {
      if (callback[k].active
	  && (!(rinfo[callback[k].owner] != NULL)
	      && (rinfo[callback[k].owner]->sprite == 1000)))
	{
	  log_debug("Killed callback %d.  (was attached to script %d)",
		    k, callback[k].owner);
	  callback[k].active = 0;
	}
    }
}

/**
 * Kill all scripts including those attached to pseudo-sprite 1000
 */
void kill_all_scripts_for_real(void)
{
  int k = 1;
  for (k = 1; k < MAX_SCRIPTS; k++)
    {
      if (rinfo[k] != NULL)
	kill_script(k);
    }
  
  for (k = 1; k < MAX_CALLBACKS; k++)
    {
      callback[k].active = 0;
    }
}


/**
 * Return the next single line from rbuf[script], starting at
 * rinfo[script]->current. Update line/column counters.
 */
char* read_next_line(int script)
{
  if (rinfo[script] == NULL || rbuf == NULL)
    {
      log_error("Tried to read script %d, it doesn't exist.", script);
      return NULL;
    }

  if (rinfo[script]->current >= rinfo[script]->end)
    {
      //at end of buffer
      return NULL;
    }

  /* remember the beginning of the line to be parsed, we'll use it in
     the debugging messages */
  rinfo[script]->debug_line = rinfo[script]->cur_line;

  int k = rinfo[script]->current;
  int start = k;
  for (; k < rinfo[script]->end; k++)
    {
      rinfo[script]->current++;
      rinfo[script]->cur_col++;
      
      if (rbuf[script][k] == '\n')
	{
	  rinfo[script]->cur_line++;
	  rinfo[script]->cur_col = 0;
	}
      if (rbuf[script][k] == '\n' || rbuf[script][k] == '\r')
	break;
    }

  if (k < rinfo[script]->end)
    {
      int len = rinfo[script]->current - start;
      char* buf = xmalloc(len + 1);

      char* pc = buf;
      int k = start;
	for (; k < rinfo[script]->current; k++, pc++)
	{
	  *pc = rbuf[script][k];

	  /* Compatibility substitutions, important when parsing
	     title_start/title_end, namely */
	  if (*pc == '\t') *pc = ' ';
	  if (*pc == '\r') *pc = '\n';
	}
      *pc = '\0'; /* for safety */
      return buf;
    }
  else
    {
      //at end of buffer
      return NULL;
    }
}

/**
 * Run callbacks, order by index. Sets the activation delay if
 * necessary. Kill obsolete callbacks along the way.
 *
 * Callbacks are set by wait() and set_callback_random().
 * 
 * spawn()/external()/etc. use other mechanisms. say_stop*() also use
 * callbacks, but implemented differently (spr[x].callback, processed
 * in updateFrame()).
 **/
void process_callbacks(void)
{
  int now = game_GetTicks();
  int i, k;

  for (i = 1; i < MAX_SCRIPTS; i++)
    {
      if (rinfo[i] != NULL)
	{
	  if (rinfo[i]->sprite > 0 && rinfo[i]->sprite != 1000 && spr[rinfo[i]->sprite].active == /*false*/0)
	    {
	      //kill this script, owner is dead
	      log_debug("Killing script %s, owner sprite %d is dead.", rinfo[i]->name, rinfo[i]->sprite);
	      kill_script(i);
	    }
	}
    }
  
  for (k = 1; k < MAX_CALLBACKS; k++)
    {
      if (callback[k].active)
	{
	  if (callback[k].owner > 0 && rinfo[callback[k].owner] == NULL)
	    {
	      //kill this process, it's owner sprite is 'effin dead.
	      log_debug("Killed callback %s because script %d is dead.",
			k, callback[k].owner);
	      callback[k].active = /*false*/0;
	    }
	  else
	    {
	      if (callback[k].timer == 0)
		{
		  //set timer
		  
		  if (callback[k].max > 0)
		    callback[k].timer = now + (rand() % callback[k].max) + callback[k].min;
		  else
		    callback[k].timer = now + callback[k].min;
		}
	      else
		{
		  if (callback[k].timer < now)
		    {
		      callback[k].timer = 0;
		      
		      if (compare(callback[k].name, ""))
			{
			  //callback defined no proc name, so lets assume they want to start the script where it
			  //left off
			  //kill this callback
			  callback[k].active = /*false*/0;
			  run_script(callback[k].owner);
			  log_debug("Called script %d from callback %d.",
				    callback[k].owner, k);
			}
		      else
			{
			  log_debug("Called proc %s from callback %d.", callback[k].name, k);
			  
			  //callback defined a proc name
			  if (locate(callback[k].owner,callback[k].name))
			    {
			      //found proc, lets run it
			      run_script(callback[k].owner);
			    }
			}
		    }
		}
	    }
	}
    }
}


/**
 * Run main() for all active sprites on screen
 */
void init_scripts()
{
  int k = 1;
  for (; k < MAX_SCRIPTS; k++)
    {
      if (rinfo[k] != NULL && rinfo[k]->sprite != 0
	  /* don't go out of bounds in spr[300], e.g. when sprite == 1000: */
	  && rinfo[k]->sprite < MAX_SPRITES_AT_ONCE
	  && spr[rinfo[k]->sprite].active)
	{
	  if (locate(k, "main"))
	    {
	      log_debug("Screendraw: running main of script %s..", rinfo[k]->name);
	      run_script(k);
	    }
	}
    }
}



void kill_scripts_owned_by(int sprite)
{
  int i;
        for (i = 1; i < MAX_SCRIPTS; i++)
        {
                if (rinfo[i] != NULL)
                {
                        if (rinfo[i]->sprite == sprite)
                        {
                                kill_script(i);

                        }

                }
        }

}

void kill_returning_stuff(int script)
{
  //Msg("Checking callbacks..");
  //check callbacks

  int i;
  // callbacks from wait() and run_script_by_number()
  for (i = 1; i < MAX_CALLBACKS; i++)
    {
      if (callback[i].active && callback[i].owner == script)
	//      if (compare(callback[i].name, ""))
	{
	  log_debug("killed a returning callback, ha!");
	  callback[i].active = /*false*/0;
	}

    }

  // callbacks from say_*()
  for (i = 1; i <= last_sprite_created; i++)
    {
      if (spr[i].active && spr[i].brain == 8 && spr[i].callback == script)
	{
	  log_debug("Killed sprites callback command");
	  spr[i].callback = 0;
	}
    }
}


void run_script(int script)
{
  int result;
  char* line = NULL;

  /* keep 'return' value? */
  if (dversion >= 108)
    {
      if (bKeepReturnInt == 1)
	{
	  bKeepReturnInt = 0;
	}
      else
	{
	  returnint = 0;
	}
    }
  else
    {
      returnint = 0;
    }
  returnstring[0] = 0;


  if (rinfo[script] != NULL)
    {
      log_debug("Script %s is entered at %d:%d (offset %d).",
		rinfo[script]->name,
		rinfo[script]->cur_line, rinfo[script]->cur_col,
		rinfo[script]->current);
    }
  else
    {
      log_error("Tried to run a script that doesn't exist in memory.  Nice work.");
    }

  int doelse_once = 0;
  while ((line = read_next_line(script)) != NULL)
    {
      while (1)
	{
	  strip_beginning_spaces(line);
	  if (strcmp(line, "\n") == 0)
	    break;

	  
	  int doelse = 0;
	  if (doelse_once == 1)
	    {
	      doelse = 1;
	      doelse_once = 0;
	    }
	  result = process_line(script, line, doelse);
	  

	  if (result == DCPS_DOELSE_ONCE)
	    {
	      doelse_once = 1;
	      /* now process the rest of the line */
	    }

	  if (result == DCPS_YIELD)
	    {
	      /* Quit script: */
	      log_debug("giving script the boot");
	      free(line);
	      return;
	    }
	  
	  if (result == DCPS_GOTO_NEXTLINE)
	    break;

	  /* else result == DCPS_CONTINUE */
	}
      free(line);
    }

  if (rinfo[script] != NULL && rinfo[script]->proc_return != 0)
    {
      run_script(rinfo[script]->proc_return);
      kill_script(script);
    }
}

int var_exists(char name[20], int scope)
{
  int i;
        for (i = 1; i < MAX_VARS; i++)
        {
                if (play.var[i].active)
                {
                        if (compare(play.var[i].name, name))
                        {

                                if (scope == play.var[i].scope)
                                {
                                        //Msg("Found match for %s.", name);
                                        return(i);
                                }
                        }



                }
        }

        return(0);
}

/**
 * Make new global functions (v1.08)
 */
void make_function (char file[10], char func[20])
{
  //See if it already exists

  int exists = 0;
  int i;
  for (i = 0; strlen (play.func[i].func) > 0 && i < 100; i++)
    {
      if (compare (func, play.func[i].func))
	{
	  exists = 1;
	  break;
	}
    }
  if (exists == 1)
    {
      strncpy (play.func[i].file, file, 10);
    }
  else
    {
      strncpy (play.func[0].file, file, 10);
      strncpy (play.func[0].func, func, 20);
    }
}


void make_int(char name[80], int value, int scope, int script)
{
        int dupe;
       int i;
        if (strlen(name) > 19)
        {

                log_error("[DinkC] %s:%d: varname %s is too long",
			  rinfo[script]->name, rinfo[script]->debug_line, name);
                return;
        }
        dupe = var_exists(name, scope);

        if (dupe > 0)
	  {
	    if (scope != DINKC_GLOBAL_SCOPE)
	      {
		log_warn("[DinkC] %s:%d: Local var %s already used in this procedure",
			 rinfo[script]->name, rinfo[script]->debug_line,
			 name, rinfo[script]->name);
		
		play.var[dupe].var = value;
	      }
	    else
	      {
		log_warn("[DinkC] %s:%d: var %s is already a global, not changing value",
			  rinfo[script]->name, rinfo[script]->debug_line, name);
	      }
	    return;
        }


        //make new var

        for (i = 1; i < MAX_VARS; i++)
        {
                if (play.var[i].active == /*false*/0)
                {

                        play.var[i].active = /*true*/1;
                        play.var[i].scope = scope;
                        strcpy(play.var[i].name, name);
                        //g("var %s created, used slot %d ", name,i);
                        play.var[i].var = value;
                        return;
                }
        }

        log_error("[DinkC] %s:%d: out of var space, all %d used",
		  rinfo[script]->name, rinfo[script]->debug_line, MAX_VARS);
}

/**
 * (re)Define variable
 *
 * name: variable name
 * newname: new value (unless that's a function call, cf. 'rest')
 * math: operator (one of '=', '+', '-', '*', '/')
 * script: in-memory script identifier
 * rest: text of the script after the operator (left-trimmed)
 */
void var_equals(char* name, char* newname, char math, int script, char rest[200])
{
  int newval = 0;
  struct varman *lhs_var = NULL;
  
  /** Ensure left-hand side is an existing variable **/
  if (name[0] != '&')
    {
      log_error("[DinkC] %s:%d:[var_equals]: unknown var %s",
		rinfo[script]->name, rinfo[script]->debug_line, name);
      return;
    }
  /* Find the variable slot */
  {
    int k = search_var_with_this_scope(name, script);
    if (k != -1)
      lhs_var = &(play.var[k]);
    
    if (lhs_var == NULL) /* not found */
      {
	log_error("[DinkC] %s:%d:[var_equals]: unknown var %s",
		  rinfo[script]->name, rinfo[script]->debug_line, name);
	return;
      }
  }

  /** Analyse right-hand side **/
  /* check if right-hand side is a function */
  if (strchr(rest, '(') != NULL)
    {
      process_line(script, rest, /*false*/0);
      newval = returnint;
      goto next2;
    }

  /* check if right-hand side is a variable to copy */
  /* remove trailing ';' */
  if (strchr(newname, ';') != NULL)
    replace_norealloc(";", "", newname);
  /* look for existing variable */
  {
    int k2 = search_var_with_this_scope(newname, script);
    if (k2 != -1)
      {
	newval = play.var[k2].var;
	//found var
	goto next2;
      }
  }
  /* also check special variables */
  if (compare(newname, "&current_sprite"))
    {
      newval = rinfo[script]->sprite;
      goto next2;
    }
  if (compare(newname, "&current_script"))
    {
      newval = script;
      goto next2;
    }
  if (dversion >= 108)
    {
      //v1.08 special variables.
      if (compare (newname, "&return"))
	{
	  newval = returnint;
	  goto next2;
	}
      if (compare (newname, "&arg1"))
	{
	  newval = rinfo[script]->arg1;
	  goto next2;
	}
      if (compare (newname, "&arg2"))
	{
	  newval = rinfo[script]->arg2;
	  goto next2;
	}
      if (compare (newname, "&arg3"))
	{
	  newval = rinfo[script]->arg3;
	  goto next2;
	}
      if (compare (newname, "&arg4"))
	{
	  newval = rinfo[script]->arg4;
	  goto next2;
	}
      if (compare (newname, "&arg5"))
	{
	  newval = rinfo[script]->arg5;
	  goto next2;
	}
      if (compare (newname, "&arg6"))
	{
	  newval = rinfo[script]->arg6;
	  goto next2;
	}
      if (compare (newname, "&arg7"))
	{
	  newval = rinfo[script]->arg7;
	  goto next2;
	}
      if (compare (newname, "&arg8"))
	{
	  newval = rinfo[script]->arg8;
	  goto next2;
	}
      if (compare (newname, "&arg9"))
	{
	  newval = rinfo[script]->arg9;
	  goto next2;
	}
    }
  /* otherwise, assume right-hand side is an integer */
  newval = atol(newname);


next2:
  /* Apply the right operation */
  if (math == '=')
    lhs_var->var = newval;
  if (math == '+')
    lhs_var->var += newval;
  if (math == '-')
    lhs_var->var -= newval;
  if (math == '/')
    lhs_var->var = lhs_var->var / newval;
  if (math == '*')
    lhs_var->var = lhs_var->var * newval;
}

/**
 * Evaluate a value (variable, int, or maths), in the context of
 * 'script'.
 */
int var_figure(char* h, int script)
{
  char* word = NULL;
  int ret = 0;
  int n1 = 0, n2 = 0;

  int is_one_word_equation = 0;
  word = get_word(h, 2);
  if (compare(word, ""))
    is_one_word_equation = 1;
  free(word);
  if (is_one_word_equation)
    {
      // variable -> integer
      if (h[0] == '&')
	decipher_string(&h, script);

      // integer
      ret = atol(h);
      return ret;
    }

  word = get_word(h, 1);
  decipher_string(&word, script);
  n1 = atol(word);
  free(word);

  word = get_word(h, 3);
  replace_norealloc(")", "", word);
  decipher_string(&word, script);
  n2 = atol(word);
  free(word);

  word = get_word(h, 2);
  log_debug("Compared %d to %d", n1, n2);

  if (compare(word, "=="))
    {
      if (n1 == n2) ret = 1; else ret = 0;
      free(word);
      return ret;
    }

  if (compare(word, ">"))
    {
      if (n1 > n2) ret = 1; else ret = 0;
      free(word);
      return ret;
    }

  if (compare(word, ">="))
    {
      if (n1 >= n2) ret = 1; else ret = 0;
      free(word);
      return ret;
    }
  
  
  if (compare(word, "<"))
    {
      if (n1 < n2) ret = 1; else ret = 0;
      free(word);
      return ret;
    }

  if (compare(word, "<="))
    {
      if (n1 <= n2) ret = 1; else ret = 0;
      free(word);
      return ret;
    }
  
  if (compare(word, "!="))
    {
      if (n1 != n2) ret = 1; else ret = 0;
      free(word);
      return ret;
    }
  
  free(word);
  return ret;
}

/**
 * Check if 'line' is a valid variable declaration, and define the
 * variable it to 0 (via make_int(...)). 'line' is modified.
 */
void int_prepare(char* line, int script)
{
  char* hold = strdup(line);

  char* name = NULL;
  char *temp = NULL;
  replace_norealloc("=", " ", line);
  temp = separate_string(line, 1, ';');
  strcpy(line, temp); // safe as strlen(line) <= strlen(temp)
  free(temp);
  name = get_word(line, 2);
  
  if (name[0] != '&')
    {
      log_error("[DinkC] %s:%d: can't create var %s, should be &%s.",
		rinfo[script]->name, rinfo[script]->debug_line,
		name, name);
    }
  else
    {
      make_int(name, 0, script, script);

      strcpy(line, hold);
    }
  free(name);
  free(hold);
}


void dinkc_init()
{
  dinkc_bindings_init();
}

void dinkc_quit()
{
  dinkc_bindings_quit();
}
