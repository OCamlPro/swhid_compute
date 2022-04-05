/**
 * Link game engine and DinkC script engine

 * Copyright (C) 1997, 1998, 1999, 2002, 2003  Seth A. Robinson
 * Copyright (C) 2005, 2006  Dan Walma
 * Copyright (C) 2005, 2007, 2008, 2009, 2010, 2011, 2012  Sylvain Beucler

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

#include "dinkc_bindings.h"

#include <stdio.h>
#include <stdlib.h> /* atol */
#include <time.h>
#include <math.h>
#include <alloca.h>
#include <string.h>
#include <ctype.h> /* tolower */
#include <xalloc.h>

/* Gnulib */
#include "hash.h"

#include "game_engine.h"
#include "screen.h"
#include "dinkvar.h"
#include "dinkc.h"
#include "freedink.h"
#include "gfx.h"
#include "gfx_fonts.h"
#include "gfx_palette.h"
#include "gfx_sprites.h"
#include "gfx_tiles.h"
#include "bgm.h"
#include "sfx.h"
#include "input.h"
#include "str_util.h"
#include "paths.h"
#include "log.h"
#include "dinkc_console.h"
#include "i18n.h"

/* store current procedure arguments expanded values of type 'int' (see get_parms) */
static long nlist[10];
/* store current procedure arguments of type 'string' (idem) */
static char* slist[10];
static char* cur_funcname;


/***************/
/*  DinkC API  */
/*             */
/***************/

/**
 * Short-hand to check for invalid sprites and avoid segfaults.
 * Also warn the D-Mod author about it.
 */
#define STOP_IF_BAD_SPRITE(sprite)                                             \
  if (sprite <= 0 || sprite >= MAX_SPRITES_AT_ONCE)                            \
    {                                                                          \
      log_error("[DinkC] %s:%d:%s: invalid sprite %d (offset %d)",             \
                rinfo[script]->name, rinfo[script]->debug_line,                \
                cur_funcname, sprite, rinfo[script]->current);                 \
      return;                                                                  \
    }

/**
 * sp_* functions used to call 'change_sprite' on spr[sprite] without
 * checking if 'sprite' was in [1; MAX_SPRITES_AT_ONCE-1]. Since
 * 'change_sprite' returns -1 when 'sprite' is inactive, that's also
 * what we return when the sprite is out of range.
 */
#define RETURN_NEG_IF_BAD_SPRITE(sprite)                                       \
  if (sprite <= 0 || sprite >= MAX_SPRITES_AT_ONCE)                            \
    {                                                                          \
      log_error("[DinkC] %s:%d:%s: invalid sprite %d (offset %d)",             \
                rinfo[script]->name, rinfo[script]->debug_line,                \
                cur_funcname, sprite, rinfo[script]->current);                 \
      *preturnint = -1;                                                        \
      return;                                                                  \
    }


void dc_sp_active(int script, int* yield, int* preturnint, int sprite, int sparg)
{
  RETURN_NEG_IF_BAD_SPRITE(sprite);
  *preturnint = change_sprite(sprite, sparg, &spr[sprite].active);
}

void dc_sp_attack_hit_sound(int script, int* yield, int* preturnint, int sprite, int sparg)
{
  RETURN_NEG_IF_BAD_SPRITE(sprite);
  *preturnint = change_sprite(sprite, sparg, &spr[sprite].attack_hit_sound);
}

void dc_sp_attack_hit_sound_speed(int script, int* yield, int* preturnint, int sprite, int sparg)
{
  RETURN_NEG_IF_BAD_SPRITE(sprite);
  *preturnint = change_sprite(sprite, sparg, &spr[sprite].attack_hit_sound_speed);
}

void dc_sp_attack_wait(int script, int* yield, int* preturnint, int sprite, int sparg)
{
  RETURN_NEG_IF_BAD_SPRITE(sprite);
  *preturnint = change_sprite(sprite, sparg+thisTickCount, &spr[sprite].attack_wait);
}

void dc_sp_base_attack(int script, int* yield, int* preturnint, int sprite, int sparg)
{
  RETURN_NEG_IF_BAD_SPRITE(sprite);
  *preturnint = change_sprite_noreturn(sprite, sparg, &spr[sprite].base_attack);
}

void dc_sp_base_die(int script, int* yield, int* preturnint, int sprite, int base_sequence)
{
  RETURN_NEG_IF_BAD_SPRITE(sprite);
  *preturnint = change_sprite_noreturn(sprite, base_sequence, &spr[sprite].base_die);
}

void dc_sp_base_hit(int script, int* yield, int* preturnint, int sprite, int sparg)
{
  RETURN_NEG_IF_BAD_SPRITE(sprite);
  *preturnint = change_sprite_noreturn(sprite, sparg, &spr[sprite].base_hit);
}

void dc_sp_base_idle(int script, int* yield, int* preturnint, int sprite, int sparg)
{
  RETURN_NEG_IF_BAD_SPRITE(sprite);
  *preturnint = change_sprite_noreturn(sprite, sparg, &spr[sprite].base_idle);
}

void dc_sp_base_walk(int script, int* yield, int* preturnint, int sprite, int sparg)
{
  RETURN_NEG_IF_BAD_SPRITE(sprite);
  *preturnint = change_sprite_noreturn(sprite, sparg, &spr[sprite].base_walk);
}

void dc_sp_brain(int script, int* yield, int* preturnint, int sprite, int sparg)
{
  RETURN_NEG_IF_BAD_SPRITE(sprite);
  *preturnint = change_sprite(sprite, sparg, &spr[sprite].brain);
}

void dc_sp_brain_parm(int script, int* yield, int* preturnint, int sprite, int sparg)
{
  RETURN_NEG_IF_BAD_SPRITE(sprite);
  *preturnint = change_sprite(sprite, sparg, &spr[sprite].brain_parm);
}

void dc_sp_brain_parm2(int script, int* yield, int* preturnint, int sprite, int sparg)
{
  RETURN_NEG_IF_BAD_SPRITE(sprite);
  *preturnint = change_sprite(sprite, sparg, &spr[sprite].brain_parm2);
}

void dc_sp_defense(int script, int* yield, int* preturnint, int sprite, int sparg)
{
  RETURN_NEG_IF_BAD_SPRITE(sprite);
  *preturnint = change_sprite(sprite, sparg, &spr[sprite].defense);
}

void dc_sp_dir(int script, int* yield, int* preturnint, int sprite, int sparg)
{
  RETURN_NEG_IF_BAD_SPRITE(sprite);
  *preturnint = change_sprite(sprite, sparg, &spr[sprite].dir);
  if (sparg != -1)
    changedir(spr[sprite].dir, sprite, spr[sprite].base_walk);
}

void dc_sp_disabled(int script, int* yield, int* preturnint, int sprite, int sparg)
{
  RETURN_NEG_IF_BAD_SPRITE(sprite);
  *preturnint = change_sprite(sprite, sparg, &spr[sprite].disabled);
}

void dc_sp_distance(int script, int* yield, int* preturnint, int sprite, int sparg)
{
  RETURN_NEG_IF_BAD_SPRITE(sprite);
  *preturnint = change_sprite(sprite, sparg, &spr[sprite].distance);
}

void dc_sp_exp(int script, int* yield, int* preturnint, int sprite, int sparg)
{
  RETURN_NEG_IF_BAD_SPRITE(sprite);
  *preturnint = change_sprite(sprite, sparg, &spr[sprite].exp);
}

void dc_sp_flying(int script, int* yield, int* preturnint, int sprite, int sparg)
{
  RETURN_NEG_IF_BAD_SPRITE(sprite);
  *preturnint = change_sprite(sprite, sparg, &spr[sprite].flying);
}

void dc_sp_follow(int script, int* yield, int* preturnint, int sprite, int sparg)
{
  RETURN_NEG_IF_BAD_SPRITE(sprite);
  *preturnint = change_sprite(sprite, sparg, &spr[sprite].follow);
}

void dc_sp_frame(int script, int* yield, int* preturnint, int sprite, int sparg)
{
  RETURN_NEG_IF_BAD_SPRITE(sprite);
  *preturnint = change_sprite(sprite, sparg, &spr[sprite].frame);
}

void dc_sp_frame_delay(int script, int* yield, int* preturnint, int sprite, int sparg)
{
  RETURN_NEG_IF_BAD_SPRITE(sprite);
  *preturnint = change_sprite(sprite, sparg, &spr[sprite].frame_delay);
}

void dc_sp_gold(int script, int* yield, int* preturnint, int sprite, int sparg)
{
  RETURN_NEG_IF_BAD_SPRITE(sprite);
  *preturnint = change_sprite(sprite, sparg, &spr[sprite].gold);
}

void dc_sp_hard(int script, int* yield, int* preturnint, int sprite, int sparg)
{
  RETURN_NEG_IF_BAD_SPRITE(sprite);
  *preturnint = change_sprite(sprite, sparg, &spr[sprite].hard);
  if (spr[sprite].sp_index != 0 && sparg != -1)
    pam.sprite[spr[sprite].sp_index].hard = *preturnint;
}

void dc_sp_hitpoints(int script, int* yield, int* preturnint, int sprite, int sparg)
{
  RETURN_NEG_IF_BAD_SPRITE(sprite);
  *preturnint = change_sprite(sprite, sparg, &spr[sprite].hitpoints);
}

void dc_sp_move_nohard(int script, int* yield, int* preturnint, int sprite, int sparg)
{
  RETURN_NEG_IF_BAD_SPRITE(sprite);
  *preturnint = change_sprite(sprite, sparg, &spr[sprite].move_nohard);
}

void dc_sp_mx(int script, int* yield, int* preturnint, int sprite, int sparg)
{
  RETURN_NEG_IF_BAD_SPRITE(sprite);
  *preturnint = change_sprite(sprite, sparg, &spr[sprite].mx);
}

void dc_sp_my(int script, int* yield, int* preturnint, int sprite, int sparg)
{
  RETURN_NEG_IF_BAD_SPRITE(sprite);
  *preturnint = change_sprite(sprite, sparg, &spr[sprite].my);
}

void dc_sp_noclip(int script, int* yield, int* preturnint, int sprite, int sparg)
{
  RETURN_NEG_IF_BAD_SPRITE(sprite);
  *preturnint = change_sprite(sprite, sparg, &spr[sprite].noclip);
}

void dc_sp_nocontrol(int script, int* yield, int* preturnint, int sprite, int sparg)
{
  RETURN_NEG_IF_BAD_SPRITE(sprite);
  *preturnint = change_sprite(sprite, sparg, &spr[sprite].nocontrol);
}

void dc_sp_nodraw(int script, int* yield, int* preturnint, int sprite, int sparg)
{
  RETURN_NEG_IF_BAD_SPRITE(sprite);
  *preturnint = change_sprite(sprite, sparg, &spr[sprite].nodraw);
}

void dc_sp_nohit(int script, int* yield, int* preturnint, int sprite, int sparg)
{
  RETURN_NEG_IF_BAD_SPRITE(sprite);
  *preturnint = change_sprite(sprite, sparg, &spr[sprite].nohit);
}

void dc_sp_notouch(int script, int* yield, int* preturnint, int sprite, int sparg)
{
  RETURN_NEG_IF_BAD_SPRITE(sprite);
  *preturnint = change_sprite(sprite, sparg, &spr[sprite].notouch);
}

void dc_sp_pframe(int script, int* yield, int* preturnint, int sprite, int sparg)
{
  RETURN_NEG_IF_BAD_SPRITE(sprite);
  *preturnint = change_sprite(sprite, sparg, &spr[sprite].pframe);
}

void dc_sp_picfreeze(int script, int* yield, int* preturnint, int sprite, int sparg)
{
  RETURN_NEG_IF_BAD_SPRITE(sprite);
  *preturnint = change_sprite(sprite, sparg, &spr[sprite].picfreeze);
}

void dc_sp_pseq(int script, int* yield, int* preturnint, int sprite, int sparg)
{
  RETURN_NEG_IF_BAD_SPRITE(sprite);
  *preturnint = change_sprite(sprite, sparg, &spr[sprite].pseq);
}

void dc_sp_que(int script, int* yield, int* preturnint, int sprite, int sparg)
{
  RETURN_NEG_IF_BAD_SPRITE(sprite);
  *preturnint = change_sprite(sprite, sparg, &spr[sprite].que);
}

void dc_sp_range(int script, int* yield, int* preturnint, int sprite, int sparg)
{
  RETURN_NEG_IF_BAD_SPRITE(sprite);
  *preturnint = change_sprite(sprite, sparg, &spr[sprite].range);
}

void dc_sp_reverse(int script, int* yield, int* preturnint, int sprite, int sparg)
{
  RETURN_NEG_IF_BAD_SPRITE(sprite);
  *preturnint = change_sprite(sprite, sparg, &spr[sprite].reverse);
}

void dc_sp_seq(int script, int* yield, int* preturnint, int sprite, int sparg)
{
  RETURN_NEG_IF_BAD_SPRITE(sprite);
  if ((sparg < 0 || sparg >= MAX_SEQUENCES) && sparg != -1)
    {
      log_error("[DinkC] %s:%d:%s: invalid sequence %d, ignoring (offset %d)",
                rinfo[script]->name, rinfo[script]->debug_line,
                cur_funcname, sparg, rinfo[script]->current);
      *preturnint = -1;
      return;
    }
  *preturnint = change_sprite(sprite, sparg, &spr[sprite].seq);
}

void dc_sp_size(int script, int* yield, int* preturnint, int sprite, int sparg)
{
  RETURN_NEG_IF_BAD_SPRITE(sprite);
  *preturnint = change_sprite(sprite, sparg, &spr[sprite].size);
}

void dc_sp_sound(int script, int* yield, int* preturnint, int sprite, int sparg)
{
  RETURN_NEG_IF_BAD_SPRITE(sprite);
  *preturnint = change_sprite(sprite, sparg, &spr[sprite].sound);
  if (sparg > 0)
    SoundPlayEffect(spr[sprite].sound,22050, 0, sprite, 1);
}

void dc_sp_speed(int script, int* yield, int* preturnint, int sprite, int sparg)
{
  RETURN_NEG_IF_BAD_SPRITE(sprite);
  *preturnint = change_sprite(sprite, sparg, &spr[sprite].speed);
  if (sparg != -1)
    changedir(spr[sprite].dir, sprite, spr[sprite].base_walk);
}

void dc_sp_strength(int script, int* yield, int* preturnint, int sprite, int sparg)
{
  RETURN_NEG_IF_BAD_SPRITE(sprite);
  *preturnint = change_sprite(sprite, sparg, &spr[sprite].strength);
}

void dc_sp_target(int script, int* yield, int* preturnint, int sprite, int sparg)
{
  RETURN_NEG_IF_BAD_SPRITE(sprite);
  *preturnint = change_sprite(sprite, sparg, &spr[sprite].target);
}

void dc_sp_timing(int script, int* yield, int* preturnint, int sprite, int sparg)
{
  RETURN_NEG_IF_BAD_SPRITE(sprite);
  *preturnint = change_sprite(sprite, sparg, &spr[sprite].timer);
}

void dc_sp_touch_damage(int script, int* yield, int* preturnint, int sprite, int sparg)
{
  RETURN_NEG_IF_BAD_SPRITE(sprite);
  *preturnint = change_sprite_noreturn(sprite, sparg, &spr[sprite].touch_damage);
}

void dc_sp_x(int script, int* yield, int* preturnint, int sprite, int sparg)
{
  RETURN_NEG_IF_BAD_SPRITE(sprite);
  *preturnint = change_sprite(sprite, sparg, &spr[sprite].x);
}

void dc_sp_y(int script, int* yield, int* preturnint, int sprite, int sparg)
{
  RETURN_NEG_IF_BAD_SPRITE(sprite);
  *preturnint = change_sprite(sprite, sparg, &spr[sprite].y);
}



void dc_sp_kill(int script, int* yield, int* preturnint, int sprite, int sparg)
{
  STOP_IF_BAD_SPRITE(sprite);
  spr[sprite].kill = sparg;
}

void dc_sp_editor_num(int script, int* yield, int* preturnint, int sprite)
{
  *preturnint = 0;
  if (sprite > 0 && sprite < MAX_SPRITES_AT_ONCE)
    *preturnint = spr[sprite].sp_index;
  else
    log_error("[DinkC] sp_editor_num: invalid sprite %d", sprite);
}


void dc_sp_kill_wait(int script, int* yield, int* preturnint, int sprite)
{
  if (sprite > 0 && sprite < MAX_SPRITES_AT_ONCE)
    spr[sprite].wait = 0;
  else
    log_error("[DinkC] sp_kill_wait: invalid sprite %d", sprite);
}

void dc_sp_script(int script, int* yield, int* preturnint, int sprite, char* dcscript)
{
  // (sprite, direction, until, nohard);
  if (sprite <= 0 || (sprite >= MAX_SPRITES_AT_ONCE && sprite != 1000))
    {
      log_error("[DinkC] %s:%d:%s: cannot process sprite %d??",
                rinfo[script]->name, rinfo[script]->debug_line, cur_funcname,
		sprite);
      return;
    }
  kill_scripts_owned_by(sprite);
  if (load_script(dcscript, sprite, /*true*/1) == 0)
    {
      *preturnint = 0;
      return;
    }

  int tempreturn = 0;
  if (sprite != 1000)
    {
      if (no_running_main == /*true*/1)
	log_info("Not running %s until later..", rinfo[spr[sprite].script]->name);
      if (no_running_main == /*false*/0 && sprite != 1000)
	locate(spr[sprite].script, "MAIN");
    
      tempreturn = spr[sprite].script;
    
      if (no_running_main == /*false*/0)
	run_script(spr[sprite].script);
    }
    
  *preturnint = tempreturn;
}


void dc_unfreeze(int script, int* yield, int* preturnint, int sprite)
{
  STOP_IF_BAD_SPRITE(sprite);

  if (spr[sprite].active)
    spr[sprite].freeze = 0;
  else
    log_error("[DinkC] Couldn't unfreeze sprite %d in script %d, it doesn't exist.", sprite, script);
}

void dc_freeze(int script, int* yield, int* preturnint, int sprite)
{
  STOP_IF_BAD_SPRITE(sprite);

  if (spr[sprite].active)
    spr[sprite].freeze = script;
  else
    log_error("[DinkC] Couldn't freeze sprite %d in script %d, it doesn't exist.", sprite, script);
}

void dc_set_callback_random(int script, int* yield, int* preturnint, char* procedure, int base, int range)
{
  int retval = add_callback(procedure, base, range, script);
  if (dversion >= 108)
    *preturnint = retval;
}

void dc_set_dink_speed(int script, int* yield, int* preturnint, int speed)
{
  if (dversion >= 108 && speed == 0)
    ; // do nothing
  else
    dinkspeed = speed;
}

void dc_reset_timer(int script, int* yield, int* preturnint)
{
  time(&time_start);
  play.minutes = 0;
}

void dc_set_keep_mouse(int script, int* yield, int* preturnint, int keep_mouse_p)
{
  keep_mouse = keep_mouse_p;
}

void dc_add_item(int script, int* yield, int* preturnint, char* dcscript, int sequence, int frame)
{
  add_item(dcscript, sequence, frame, ITEM_REGULAR);
}

void dc_add_magic(int script, int* yield, int* preturnint, char* dcscript, int sequence, int frame)
{
  add_item(dcscript, sequence, frame, ITEM_MAGIC);
}

void dc_add_exp(int script, int* yield, int* preturnint, int amount, int active_sprite)
{
  STOP_IF_BAD_SPRITE(active_sprite);

  if (dversion >= 108)
    // fix - made work with all sprites when
    // using add_exp DinkC command
    add_exp_force(amount, active_sprite);
  else
    add_exp(amount, active_sprite);
}

void dc_kill_this_item(int script, int* yield, int* preturnint, char* dcscript)
{
  kill_item_script(dcscript);
}

void dc_kill_this_magic(int script, int* yield, int* preturnint, char* dcscript)
{
  kill_mitem_script(dcscript);
}

void dc_show_bmp(int script, int* yield, int* preturnint, char* bmp_file, int show_map_dot, int unused)
{
  log_info("showing BMP");
  wait4b.active = /*false*/0;
  show_bmp(bmp_file, show_map_dot, script);
  *yield = 1;
}

void dc_copy_bmp_to_screen(int script, int* yield, int* preturnint, char* bmp_file)
{
  log_info("copying BMP");
  copy_bmp(bmp_file);
}

void dc_wait_for_button(int script, int* yield, int* preturnint)
{
  log_info("waiting for button with script %d", script);
  wait4b.script = script;
  wait4b.active = /*true*/1;
  wait4b.button = 0;
  *yield = 1;
}

void dc_stop_wait_for_button(int script, int* yield, int* preturnint)
{
  wait4b.active = /*false*/0;
}

void dc_load_screen(int script, int* yield, int* preturnint)
{
  /* STOP_IF_BAD_SPRITE(active_sprite); */

  //Msg("Loading map %d..",*pmap);
  update_screen_time();
  load_map(map.loc[*pmap]);

  // update indicator on mini-map
  if (map.indoor[*pmap] == 0)
    play.last_map = *pmap;
    
  return;
}

/**
 * Decipher a copy of 'text' (to avoid potentially realloc'ing it) and
 * call 'say_text(...)'
 */
static int say_text_from_dc(char* text, int active_sprite, int script)
{
  log_debug("[DinkC] %s:%d:%s(\"%s\", %d)", rinfo[script]->name,
	    rinfo[script]->debug_line, cur_funcname,
	    text, active_sprite);

  /* Translate text (before variable substitution) */
  char* translation = NULL;
  if (strlen(text) >= 2 && text[0] == '`')
    {
      char* temp = i18n_translate(rinfo[script]->name, rinfo[script]->debug_line, text+2);
      translation = xmalloc(strlen(temp) + 2 + 1);
      sprintf(translation, "%c%c%s", text[0], text[1], temp);
      free(temp);
    }
  else
    {
      translation = i18n_translate(rinfo[script]->name, rinfo[script]->debug_line, text);
    }

  /* Substitute variables */
  char* expanded = strdup(translation);
  free(translation);
  decipher_string(&expanded, script);

  int text_sprite = say_text(expanded, active_sprite, script);
  free(expanded);
  return text_sprite;
}

/**
 * Decipher a copy of 'text' (to avoid potentially realloc'ing it) and
 * call 'say_text_xy(...)'
 */
static int say_text_xy_from_dc(char* text, int x, int y, int script)
{
  log_debug("[DinkC] %s:%d:%s(\"%s\", %d, %d)", rinfo[script]->name,
	    rinfo[script]->debug_line, cur_funcname,
	    text, x, y);

  /* Translate text (before variable substitution) */
  char* translation = NULL;
  if (strlen(text) >= 2 && text[0] == '`')
    {
      char* temp = i18n_translate(rinfo[script]->name, rinfo[script]->debug_line, text+2);
      translation = xmalloc(strlen(temp) + 2 + 1);
      sprintf(translation, "%c%c%s", text[0], text[1], temp);
      free(temp);
    }
  else
    {
      translation = i18n_translate(rinfo[script]->name, rinfo[script]->debug_line, text);
    }

  /* Substitute variables */
  char* expanded = strdup(translation);
  free(translation);
  decipher_string(&expanded, script);

  int text_sprite = say_text_xy(expanded, x, y, script);
  free(expanded);
  return text_sprite;
}

void dc_say(int script, int* yield, int* preturnint, char* text, int active_sprite)
{
  /* 1000 is a valid value, and bad values don't trigger segfaults
     in this particular function; so don't validate active_sprite */
  /* STOP_IF_BAD_SPRITE(active_sprite); */

  if (active_sprite == 0)
    {
      log_error("[DinkC] say_stop: Sprite 0 can talk? Yeah, didn't think so.");
      return;
    }

  if (active_sprite != 1000)
    kill_text_owned_by(active_sprite);

  *preturnint = say_text_from_dc(text, active_sprite, script);
}

void dc_say_stop(int script, int* yield, int* preturnint, char* text, int active_sprite)
{
  /* STOP_IF_BAD_SPRITE(active_sprite); */

  if (active_sprite == 0)
    {
      log_error("[DinkC] say_stop: Sprite 0 can talk? Yeah, didn't think so.");
      return;
    }
    
  kill_text_owned_by(active_sprite);
  kill_text_owned_by(1);
  kill_returning_stuff(script);

  int sprite = say_text_from_dc(text, active_sprite, script);
  *preturnint = sprite;

  spr[sprite].callback = script;
  play.last_talk = script;
  //Msg("Sprite %d marked callback true.", sprite);
    
  *yield = 1;
}

void dc_say_stop_npc(int script, int* yield, int* preturnint, char* text, int active_sprite)
{
  /* STOP_IF_BAD_SPRITE(active_sprite); */

  /* no-op if already talking */
  if (text_owned_by(active_sprite))
    {
      *preturnint = 0;
      return;
    }
    
  kill_returning_stuff(script);

  int sprite = say_text_from_dc(text, active_sprite, script);
  spr[sprite].callback = script;
    
  *yield = 1;
}

void dc_say_stop_xy(int script, int* yield, int* preturnint, char* text, int x, int y)
{
  kill_returning_stuff(script);

  int sprite = say_text_xy_from_dc(text, x, y, script);
  spr[sprite].callback = script;
  spr[sprite].live = /*true*/1;
  play.last_talk = script;
  *yield = 1;
}

void dc_say_xy(int script, int* yield, int* preturnint, char* text, int x, int y)
{
  kill_returning_stuff(script);
  *preturnint = say_text_xy_from_dc(text, x, y, script);
}

void dc_draw_screen(int script, int* yield, int* preturnint)
{
  /* only refresh screen if not in a cut-scene */
  /* do it before draw_map_game() because that one calls
     kill_all_scripts(), which NULLifies rinfo[script] */
  if (rinfo[script]->sprite != 1000)
    *yield = 1;
  draw_map_game();
}

void dc_free_items(int script, int* yield, int* preturnint)
{
  *preturnint = 0;
  int i = 0;
  for (; i < NB_ITEMS; i++)
    {
      if (play.item[i].active == 0)
	*preturnint += 1;
    }
}

void dc_free_magic(int script, int* yield, int* preturnint)
{
  *preturnint = 0;

  int i = 0;
  for (; i < NB_MITEMS; i ++)
    {
      if (play.mitem[i-1].active == 0)
	*preturnint += 1;
    }
}

void dc_kill_cur_item(int script, int* yield, int* preturnint)
{
  *preturnint = 0;
  kill_cur_item();
  *yield = 1;
}

void dc_kill_cur_magic(int script, int* yield, int* preturnint)
{
  *preturnint = 0;
  kill_cur_magic();
  *yield = 1;
}

void dc_draw_status(int script, int* yield, int* preturnint)
{
  draw_status_all();
}

void dc_arm_weapon(int script, int* yield, int* preturnint)
{
  if (weapon_script != 0 && locate(weapon_script, "DISARM"))
    run_script(weapon_script);

  weapon_script = load_script(play.item[*pcur_weapon - 1].name, 1000, /*false*/0);
  if (locate(weapon_script, "ARM"))
    run_script(weapon_script);
}

void dc_arm_magic(int script, int* yield, int* preturnint)
{
  if (magic_script != 0 && locate(magic_script, "DISARM"))
    run_script(magic_script);
    
  magic_script = load_script(play.mitem[*pcur_magic - 1].name, 1000, /*false*/0);
  if (locate(magic_script, "ARM"))
    run_script(magic_script);
}

void dc_restart_game(int script, int* yield, int* preturnint)
{
  int mainscript;
  while (kill_last_sprite());
  kill_repeat_sounds_all();
  kill_all_scripts_for_real();
  mode = 0;
  screenlock = 0;
  kill_all_vars();
  memset(&hm, 0, sizeof(hm));
  input_set_default_buttons();

  mainscript = load_script("main", 0, /*true*/1);
    
  locate(mainscript, "main");
  run_script(mainscript);
  //lets attach our vars to the scripts
  attach();
  *yield = 1;
}

void dc_wait(int script, int* yield, int* preturnint, int delayms)
{
  kill_returning_stuff(script);
  add_callback("", delayms, 0, script);
  *yield = 1;
}

void dc_preload_seq(int script, int* yield, int* preturnint, int sequence)
{
  check_seq_status(sequence);
}

void dc_script_attach(int script, int* yield, int* preturnint, int sprite)
{
  /* STOP_IF_BAD_SPRITE(sprite); */
  rinfo[script]->sprite = sprite;
}

void dc_draw_hard_sprite(int script, int* yield, int* preturnint, int sprite)
{
  STOP_IF_BAD_SPRITE(sprite);

  update_play_changes();
  int l = sprite;
  rect mhard;
  rect_copy(&mhard, &k[seq[spr[l].pseq].frame[spr[l].pframe]].hardbox);
  rect_offset(&mhard, (spr[l].x- 20), spr[l].y);

  fill_hardxy(mhard);
  fill_back_sprites();
  fill_hard_sprites();
}


void dc_activate_bow(int script, int* yield, int* preturnint)
{
  spr[1].seq = 0;
  spr[1].pseq = 100+spr[1].dir;
  spr[1].pframe = 1;
  bow.active = /*true*/1;
  bow.script = script;
  bow.hitme = /*false*/0;
  bow.time = 0;
    
  /*      bowsound->Release();
    
  //lpDS->DuplicateSoundBuffer(ssound[42].sound,&bowsound);
  //bowsound->Play(0, 0, DSBPLAY_LOOPING);
  */
    
  *yield = 1;
}

void dc_disable_all_sprites(int script, int* yield, int* preturnint)
{
  int jj;
  for (jj = 1; jj < last_sprite_created; jj++)
    if (spr[jj].active) spr[jj].disabled = /*true*/1;
}

void dc_draw_background(int script, int* yield, int* preturnint)
{
  // (sprite, direction, until, nohard);
  draw_map_game_background();
}

void dc_draw_hard_map(int script, int* yield, int* preturnint)
{
  // (sprite, direction, until, nohard);
  log_info("Drawing hard map..");
  update_play_changes();
  fill_whole_hard();
  fill_hard_sprites();
  fill_back_sprites();
}

void dc_enable_all_sprites(int script, int* yield, int* preturnint)
{
  int jj;
  for (jj = 1; jj < last_sprite_created; jj++)
    if (spr[jj].active) spr[jj].disabled = /*false*/0;
}

void dc_fade_down(int script, int* yield, int* preturnint)
{
  // (sprite, direction, until, nohard);
  if (process_upcycle)
    {
      log_error("[DinkC] %s:%d: fade_down() called during fade_up(), ignoring fade_down()",
                rinfo[script]->name, rinfo[script]->debug_line);
    }
  else
    {
      process_downcycle = /*true*/1;
      cycle_clock = thisTickCount+1000;
      cycle_script = script;
    }
  *yield = 1;
}

void dc_fade_up(int script, int* yield, int* preturnint)
{
  // (sprite, direction, until, nohard);
  if (process_downcycle)
    {
      log_error("[DinkC] %s:%d: fade_up() called during fade_down(), forcing fade_up()",
                rinfo[script]->name, rinfo[script]->debug_line);
    }
  process_downcycle = 0; // priority over concurrent fade_down()
  process_upcycle = /*true*/1;
  cycle_script = script;
  *yield = 1;
}

void dc_get_burn(int script, int* yield, int* preturnint)
{
  *preturnint = 1;
}

void dc_get_last_bow_power(int script, int* yield, int* preturnint)
{
  *preturnint = bow.last_power;
}

void dc_get_version(int script, int* yield, int* preturnint)
{
  *preturnint = dversion;
}

void dc_kill_all_sounds(int script, int* yield, int* preturnint)
{
  kill_repeat_sounds_all();
}

void dc_kill_game(int script, int* yield, int* preturnint)
{
  log_info("Was told to kill game, so doing it like a good boy.");
  /* Send QUIT event to the main game loop,
     which will cleanly exit */
  SDL_Event ev;
  ev.type = SDL_QUIT;
  SDL_PushEvent(&ev);
  *yield = 1;
}

void dc_kill_this_task(int script, int* yield, int* preturnint)
{
  // (sprite, direction, until, nohard);
  if (rinfo[script]->proc_return != 0)
    {
      run_script(rinfo[script]->proc_return);
    }
  kill_script(script);
  *yield = 1;
}

void dc_scripts_used(int script, int* yield, int* preturnint)
{
  int m = 0;
  int i;
  for (i = 1; i < MAX_SCRIPTS; i++)
    if (rinfo[i] != NULL) m++;
  *preturnint = m;
}

void dc_stopcd(int script, int* yield, int* preturnint)
{
  log_debug("Stopped cd");
  killcd();
}

void dc_stopmidi(int script, int* yield, int* preturnint)
{
  // (sprite, direction, until, nohard);
  StopMidi();
}

void dc_turn_midi_off(int script, int* yield, int* preturnint)
{
  midi_active = /*false*/0;
}

void dc_turn_midi_on(int script, int* yield, int* preturnint)
{
  midi_active = /*true*/1;
}

void dc_count_item(int script, int* yield, int* preturnint, char* dcscript)
{
  int i;
  *preturnint = 0;
  for (i = 0; i < NB_ITEMS; i++)
    {
      if (play.item[i].active
	  && compare(play.item[i].name, dcscript))
	returnint++;
    }
}

void dc_count_magic(int script, int* yield, int* preturnint, char* dcscript)
{
  int i;
  *preturnint = 0;
  for (i = 0; i < NB_MITEMS; i++)
    {
      if (play.mitem[i].active
	  && compare(play.mitem[i].name, dcscript))
	returnint++;
    }
}

void dc_compare_sprite_script(int script, int* yield, int* preturnint, int sprite, char* dcscript)
{
  *preturnint = 0;
  STOP_IF_BAD_SPRITE(sprite);
 
  if (spr[sprite].active)
    {
      if (spr[sprite].script == 0)
	{
	  log_error("[DinkC] compare_sprite_script: Sprite %d has no script.", sprite);
	  return;
	}
      if (rinfo[spr[sprite].script] == NULL)
	{
	  log_error("[DinkC] compare_sprite_script: script %d for sprite %d was already killed!.",
		    sprite, spr[sprite].script);
	  return;
	}
      if (compare(dcscript, rinfo[spr[sprite].script]->name))
	{
	  *preturnint = 1;
	  return;
	}
    }
  else
    {
      log_error("[DinkC] compare_sprite_script: Can't compare sprite script, sprite not active.");
    }
}



void dc_compare_weapon(int script, int* yield, int* preturnint, char* dcscript)
{
  *preturnint = 0;
  if (*pcur_weapon >= 1 && *pcur_weapon <= NB_ITEMS)
    {
      if (compare(play.item[*pcur_weapon - 1].name, dcscript))
	*preturnint = 1;
    }
}

void dc_compare_magic(int script, int* yield, int* preturnint, char* dcscript)
{
  *preturnint = 0;

  if (*pcur_magic >= 1 && *pcur_magic <= NB_MITEMS)
    {
      if (dversion >= 108)
	{
	  if (compare(play.mitem[*pcur_magic - 1].name, dcscript))
	    *preturnint = 1;
	}
      else
	{
	  /* reproduce v1.07 bug: compare with regular item rather than
	     magic item */
	  if (compare(play.item[*pcur_magic - 1].name, dcscript))
	    *preturnint = 1;
	}
    }
}

void dc_init(int script, int* yield, int* preturnint, char* dink_ini_line)
{
  figure_out(dink_ini_line);
}

void dc_dink_can_walk_off_screen(int script, int* yield, int* preturnint, int can_walk_off_screen_p)
{
  walk_off_screen = can_walk_off_screen_p;
}

void dc_push_active(int script, int* yield, int* preturnint, int dink_can_push_p)
{
  push_active = dink_can_push_p;
}

void dc_stop_entire_game(int script, int* yield, int* preturnint, int stop_p)
{
  stop_entire_game = stop_p;
  SDL_BlitSurface(GFX_lpDDSBack, NULL, GFX_lpDDSTwo, NULL);
}


void dc_editor_type(int script, int* yield, int* preturnint, int editor_sprite, int type)
{
  if (editor_sprite < 0 || editor_sprite >= 100)
    return;
  *preturnint = change_edit_char(editor_sprite, type,
				 &play.spmap[*pmap].type[editor_sprite]);
}
void dc_editor_seq(int script, int* yield, int* preturnint, int editor_sprite, int seq)
{
  if (editor_sprite < 0 || editor_sprite >= 100)
    return;
  *preturnint = change_edit(editor_sprite, seq,
			    &play.spmap[*pmap].seq[editor_sprite]);
}

void dc_editor_frame(int script, int* yield, int* preturnint, int editor_sprite, int frame)
{
  if (editor_sprite < 0 || editor_sprite >= 100)
    return;
  *preturnint = change_edit_char(editor_sprite, frame,
				 &play.spmap[*pmap].frame[editor_sprite]);
}



void dc_move(int script, int* yield, int* preturnint,
	     int sprite, int direction, int destination_limit, int ignore_hardness_p)
{
  STOP_IF_BAD_SPRITE(sprite);
  spr[sprite].move_active = /*true*/1;
  spr[sprite].move_dir = direction;
  spr[sprite].move_num = destination_limit;
  spr[sprite].move_nohard = ignore_hardness_p;
  spr[sprite].move_script = 0;
  log_debug("Moving: Sprite %d, dir %d, num %d", sprite, direction, destination_limit);
}

void dc_spawn(int script, int* yield, int* preturnint,
	     char* dcscript)
{
  int mysc = load_script(dcscript, 1000, /*true*/1);
  if (mysc == 0)
    {
      *preturnint = 0;
      return;
    }
  locate(mysc, "MAIN");
  int tempreturn = mysc;
  run_script(mysc);
  *preturnint = tempreturn;
}

void dc_run_script_by_number(int script, int* yield, int* preturnint,
			     int script_index, char* funcname)
{
  if (locate(script_index, funcname))
    run_script(script_index);
}

void dc_playmidi(int script, int* yield, int* preturnint,
		 char* midi_file)
{
  //StopMidi();
  int regm = atol(midi_file);
  log_debug("Processing playmidi command.");
  if (regm > 1000)
    //cd directive
    {
      int cd_track = regm - 1000;
      log_info("playmidi - cd play command detected.");
      
      if (cd_inserted)
	{
	  if (cd_track == last_cd_track
	      && cdplaying())
	    {
	      *yield = 1;
	      return;
	    }
	  
	  log_info("Playing CD track %d.", cd_track);
	  if (PlayCD(cd_track) >= 0)
	    return;
	}
      else
	{
	  //cd isn't instered, can't play CD song!!!
	  char buf[10+4+1];
	  sprintf(buf, "%d.mid", cd_track);
	  log_info("Playing midi %s.", buf);
	  PlayMidi(buf);
	  // then try to play 'midi_file' as well:
	  // (necessary for START.c:playmidi("1003.mid"))
	}
    }
  log_info("Playing midi %s.", midi_file);
  PlayMidi(midi_file);
}

void dc_playsound(int script, int* yield, int* preturnint,
		  int sound_number, int min_speed, int rand_speed_to_add, int sprite, int repeat_p)
{
  if (sprite < 0 || sprite >= MAX_SPRITES_AT_ONCE)
    sprite = 0; // no "3d" volume effect... and no segfault :p

  if (sound_on)
    *preturnint = playsound(sound_number, min_speed, rand_speed_to_add, sprite, repeat_p);
  else
    *preturnint = 0;
}

void dc_sound_set_survive(int script, int* yield, int* preturnint,
			  int sound_bank, int survive_p)
{
  //let's set one sound to survive
  if (sound_on && sound_bank > 0)
    sound_set_survive(sound_bank, survive_p);
}

void dc_sound_set_vol(int script, int* yield, int* preturnint,
		      int sound_bank, int vol)
{
  if (sound_on && sound_bank > 0)
    sound_set_vol(sound_bank, vol);
}

void dc_sound_set_kill(int script, int* yield, int* preturnint,
		       int sound_bank)
{
  if (sound_on && sound_bank > 0)
    sound_set_kill(sound_bank);
}


void dc_save_game(int script, int* yield, int* preturnint, int game_slot)
{
  save_game(game_slot);
}

void dc_force_vision(int script, int* yield, int* preturnint, int vision)
{
  *pvision = vision;
  rinfo[script]->sprite = 1000;
  fill_whole_hard();
  draw_map_game();
}

void dc_fill_screen(int script, int* yield, int* preturnint, int palette_index)
{
  fill_screen(palette_index);
}

void dc_load_game(int script, int* yield, int* preturnint, int game_slot)
{
  kill_all_scripts_for_real();
  *preturnint = load_game(game_slot);
  log_info("load completed.");
  if (rinfo[script] == NULL)
    log_error("[DinkC] Script %d is suddenly null!", script);
  *pupdate_status = 1;
  draw_status_all();
  *yield = 1;
}

void dc_game_exist(int script, int* yield, int* preturnint, int game_slot)
{
  FILE *fp;
  if ((fp = paths_savegame_fopen(game_slot, "rb")) != NULL)
    {
      fclose(fp);
      *preturnint = 1;
    }
  else
    {
      *preturnint = 0;
    }
}

void dc_move_stop(int script, int* yield, int* preturnint,
		  int sprite, int direction, int destination_limit, int ignore_hardness_p)
{
  STOP_IF_BAD_SPRITE(sprite);
  spr[sprite].move_active = /*true*/1;
  spr[sprite].move_dir = direction;
  spr[sprite].move_num = destination_limit;
  spr[sprite].move_nohard = ignore_hardness_p;
  spr[sprite].move_script = script;
  log_debug("Move_stop: Sprite %d, dir %d, num %d", sprite, direction, destination_limit);
  *yield = 1;
}

void dc_load_sound(int script, int* yield, int* preturnint,
		   char* wav_file, int sound_index)
{
  if (sound_on)
    {
      log_info("getting %s..", wav_file);
      CreateBufferFromWaveFile(wav_file, sound_index);
    }
}

void dc_debug(int script, int* yield, int* preturnint,
	      char* text)
{
  /* Convert from Latin-1 (.c) to UTF-8 (SDL) since the message is
     shown on the screen in debug mode */
  char* buf = latin1_to_utf8(text);
  decipher_string(&buf, script);
  log_debug(buf);
  free(buf);
}

void dc_busy(int script, int* yield, int* preturnint,
	     int sprite)
{
  STOP_IF_BAD_SPRITE(sprite);
  *preturnint = does_sprite_have_text(nlist[0]);
  log_debug("Busy: Return int is %d and %d.  Nlist got %d.",
	    *preturnint, does_sprite_have_text(sprite), sprite);
}


void dc_make_global_int(int script, int* yield, int* preturnint,
			char* varname, int default_val)
{
  make_int(varname, default_val, 0, script);
}

void dc_inside_box(int script, int* yield, int* preturnint,
		   int x, int y, int left, int right, int top, int bottom)
{
  rect myrect;
  rect_set(&myrect, left, right, top, bottom);
  *preturnint = inside_box(x, y, myrect);
  log_debug("Inbox is int is %d and %d.  Nlist got %d.", *preturnint, x, y);
}

void dc_random(int script, int* yield, int* preturnint,
	       int range, int base)
{
  *preturnint = (rand() % range) + base;
}

void dc_initfont(int script, int* yield, int* preturnint,
		 char* fontname)
{
  initfont(fontname);
  log_info("Initted font %s", fontname);
}

void dc_set_mode(int script, int* yield, int* preturnint,
		 int newmode)
{
  mode = newmode;
  *preturnint = mode;
}

void dc_kill_shadow(int script, int* yield, int* preturnint,
		    int sprite)
{
  /* STOP_IF_BAD_SPRITE(sprite); */
  int jj;
  for (jj = 1; jj <= last_sprite_created; jj++)
    {
      if (spr[jj].brain == 15 && spr[jj].brain_parm == sprite)
	{
	  spr[jj].active = 0;
	}
    }
}

void dc_create_sprite(int script, int* yield, int* preturnint,
		      int x, int y, int brain, int sequence, int frame)
{
  *preturnint = add_sprite_dumb(x, y, brain, sequence, frame, 100/*size*/);
}

void dc_sp(int script, int* yield, int* preturnint,
	   int editor_sprite)
{
  int i = find_sprite(editor_sprite);
  if (i != 0) {
    log_debug("Sp returned %d.", i);
    *preturnint = i;
    return;
  }
  if (last_sprite_created == 1)
    log_warn("you can't call SP() from a screen-ref,"
	     " no sprites have been created yet.");
  *preturnint = 0; /* not found */
}

void dc_is_script_attached(int script, int* yield, int* preturnint,
			   int sprite)
{
  STOP_IF_BAD_SPRITE(sprite);
  *preturnint = spr[sprite].script;
}

void dc_get_sprite_with_this_brain(int script, int* yield, int* preturnint,
				   int brain, int sprite_ignore)
{
  int i;
  for (i = 1; i <= last_sprite_created; i++)
    {
      if (spr[i].brain == brain && i != sprite_ignore && spr[i].active == 1)
	{
	  log_debug("Ok, sprite with brain %d is %d", brain, i);
	  *preturnint = i;
	  return;
	}
    }
  *preturnint = 0; /* not found */
}

void dc_get_rand_sprite_with_this_brain(int script, int* yield, int* preturnint,
					int brain, int sprite_ignore)
{
  int i;
  int nb_matches = 0;
  for (i = 1; i <= last_sprite_created; i++)
    {
      if (spr[i].brain == brain && i != sprite_ignore && spr[i].active == 1)
	nb_matches++;
    }
  if (nb_matches == 0)
    {
      log_debug("Get rand brain can't find any brains with %d.", brain);
      *preturnint = 0;
      return;
    }
  
  int mypick = (rand() % nb_matches) + 1;
  int ii;
  int cur_match = 0;
  for (ii = 1; ii <= last_sprite_created; ii++)
    {
      if (spr[ii].brain == brain && ii != sprite_ignore && spr[ii].active == 1)
	{
	  cur_match++;
	  if (cur_match == mypick)
	    {
	      *preturnint = ii;
	      return;
	    }
	}
    }
  *preturnint = 0; /* not found */
}

/* BIG FAT WARNING: in DinkC, buttons are in [1, 10] (not [0, 9]) */
void dc_set_button(int script, int* yield, int* preturnint,
		   int button, int function)
{
  input_set_button_action(button-1, function);
}

void dc_hurt(int script, int* yield, int* preturnint,
	     int sprite, int damage)
{
  STOP_IF_BAD_SPRITE(sprite);

  if (dversion >= 108)
    {
      // With v1.07 hurt(&sthing, -1) would run hit(), with v1.08 it
      // doesn't (after redink1 tried to fix a game freeze bug that I
      // can't reproduce)
      if (damage < 0)
	return;
    }

  if (hurt_thing(sprite, damage, 0) > 0)
    random_blood(spr[sprite].x, spr[sprite].y-40, sprite);

  if (spr[sprite].nohit != 1
      && spr[sprite].script != 0
      && locate(spr[sprite].script, "HIT"))
    {
      if (rinfo[script]->sprite != 1000)
	{
	  *penemy_sprite = rinfo[script]->sprite;
	  //redink1 addition of missle_source stuff
	  if (dversion >= 108)
	    *pmissle_source = rinfo[script]->sprite;
	}
      kill_returning_stuff(spr[sprite].script);
      run_script(spr[sprite].script);
    }
}

void dc_screenlock(int script, int* yield, int* preturnint,
		   int param)
{
  if (dversion >= 108)
    {
      // returns the screenlock value to DinkC
      if (param == 0 || param == 1)
	screenlock = param;
      *preturnint = screenlock;
      /* Note: redink1's v1.08 always set returnint, even if too many
	 parameters were passed. Since this breaks the logic of DinkC
	 interpreter clarification (return a variable value when bad
	 parameters), we won't reproduce this particular bug
	 here. AFAICS no D-Mod abused 'screenlock' this way. */
    }
  else
    {
      screenlock = param;
    }
}


/****************/
/*  v1.08-only  */
/*              */
/****************/

void dc_sp_blood_num(int script, int* yield, int* preturnint, int sprite, int sparg)
{
  RETURN_NEG_IF_BAD_SPRITE(sprite);
  change_sprite (sprite, sparg, &spr[sprite].bloodnum);
  *preturnint = spr[sprite].bloodseq;
}

void dc_sp_blood_seq(int script, int* yield, int* preturnint, int sprite, int sparg)
{
  RETURN_NEG_IF_BAD_SPRITE(sprite);
  change_sprite (sprite, sparg, &spr[sprite].bloodseq);
  *preturnint = spr[sprite].bloodseq;
}

void dc_sp_clip_bottom(int script, int* yield, int* preturnint, int sprite, int sparg)
{
  RETURN_NEG_IF_BAD_SPRITE(sprite);
  change_sprite (sprite, sparg, &spr[sprite].alt.bottom);
  *preturnint = spr[sprite].alt.bottom;
}

void dc_sp_clip_left(int script, int* yield, int* preturnint, int sprite, int sparg)
{
  RETURN_NEG_IF_BAD_SPRITE(sprite);
  change_sprite (sprite, sparg, &spr[sprite].alt.left);
  *preturnint = spr[sprite].alt.left;
}

void dc_sp_clip_right(int script, int* yield, int* preturnint, int sprite, int sparg)
{
  RETURN_NEG_IF_BAD_SPRITE(sprite);
  change_sprite (sprite, sparg, &spr[sprite].alt.right);
  *preturnint = spr[sprite].alt.right;
}

void dc_sp_clip_top(int script, int* yield, int* preturnint, int sprite, int sparg)
{
  RETURN_NEG_IF_BAD_SPRITE(sprite);
  change_sprite (sprite, sparg, &spr[sprite].alt.top);
  *preturnint = spr[sprite].alt.top;
}

void dc_sp_custom(int script, int* yield, int* preturnint, char* key, int sprite, int val)
{
  RETURN_NEG_IF_BAD_SPRITE(sprite);
  if (spr[sprite].active == 0)
    {
      *preturnint = -1;
    }
  else
    {
      // Set the value
      if (val != -1)
	dinkc_sp_custom_set(spr[sprite].custom, key, val);
      *preturnint = dinkc_sp_custom_get(spr[sprite].custom, key);
    }
}


/**
 * Like sp_mx but use change_sprite_noreturn, so allow setting the
 * value to -1.
 */
void dc_sp_move_x(int script, int* yield, int* preturnint, int sprite, int dx)
  {
    STOP_IF_BAD_SPRITE(sprite);
    change_sprite_noreturn (sprite, dx, &spr[sprite].mx);
  }

/**
 * Like sp_my but use change_sprite_noreturn, so allow setting the
 * value to -1.
 */
void dc_sp_move_y(int script, int* yield, int* preturnint, int sprite, int dy)
  {
    STOP_IF_BAD_SPRITE(sprite);
    change_sprite_noreturn (sprite, dy, &spr[sprite].my);
  }

void dc_sp_freeze(int script, int* yield, int* preturnint, int sprite, int frozen_p)
{
  STOP_IF_BAD_SPRITE(sprite);
  // Set the value
  if (frozen_p == 0)
    spr[sprite].freeze = 0;
  else if (frozen_p == 1)
    spr[sprite].freeze = script;
  /* else -> invalid value */

  // Return the (normalized) value
  *preturnint = (spr[sprite].freeze > 0);
}


void dc_clear_editor_info(int script, int* yield, int* preturnint)
{
    int i;
    for (i = 0; i < 769; i++)
      {
	int j;
	for (j = 0; j < 100; j++)
	  {
	    play.spmap[i].seq[j] = 0;
	    play.spmap[i].frame[j] = 0;
	    play.spmap[i].type[j] = 0;
	    play.spmap[i].last_time = 0;
	  }
      }
    *preturnint = 1;
}

void dc_get_date_day(int script, int* yield, int* preturnint)
{
    char mytime[5];
    time_t ct;
    struct tm* time_now;
    time (&ct);
    time_now = localtime (&ct);
    strftime (mytime, 5, "%d", time_now);
    *preturnint = atoi (mytime);
}

void dc_get_date_month(int script, int* yield, int* preturnint)
{
    char mytime[5];
    time_t ct;
    struct tm* time_now;
    time (&ct);
    time_now = localtime (&ct);
    strftime (mytime, 5, "%m", time_now);
    *preturnint = atoi (mytime);
}

void dc_get_date_year(int script, int* yield, int* preturnint)
{
    char mytime[5];
    time_t ct;
    struct tm* time_now;
    time (&ct);
    time_now = localtime (&ct);
    strftime (mytime, 5, "%Y", time_now);
    *preturnint = atoi (mytime);
}

void dc_get_time_game(int script, int* yield, int* preturnint)
{
    time_t ct;
    time (&ct);
    *preturnint = play.minutes + (difftime (ct, time_start) / 60);
}

void dc_get_time_real(int script, int* yield, int* preturnint)
{
    char mytime[5];
    time_t ct;
    struct tm* time_now;
    time (&ct);
    time_now = localtime (&ct);
    strftime (mytime, 5, "%M", time_now);
    *preturnint = atoi (mytime);
    strftime (mytime, 5, "%H", time_now);
    *preturnint += 60 * atoi (mytime);
}

void dc_get_truecolor(int script, int* yield, int* preturnint)
{
    *preturnint = truecolor;
}

void dc_show_console(int script, int* yield, int* preturnint)
{
    console_active = 1;
}

void dc_show_inventory(int script, int* yield, int* preturnint)
{
    show_inventory = 1;
}

void dc_var_used(int script, int* yield, int* preturnint)
{
    int m = 0;
    int i;
    for (i = 1; i < MAX_VARS; i++)
      if (play.var[i].active == 1)
	m++;
    *preturnint = m;
}


void dc_loopmidi(int script, int* yield, int* preturnint, int loop_midi)
{
  loopmidi(loop_midi);
}


void dc_math_abs(int script, int* yield, int* preturnint, int val)
{
  *preturnint = abs(val);
}

void dc_math_sqrt(int script, int* yield, int* preturnint, int val)
{
  *preturnint = sqrt(abs(val));
}

void dc_math_mod(int script, int* yield, int* preturnint, int val, int div)
{
  *preturnint = (val % div);
}

void dc_make_global_function(int script, int* yield, int* preturnint, char* dcscript, char* procname)
{
  make_function(dcscript, procname);
}

void dc_set_save_game_info(int script, int* yield, int* preturnint, char* info)
{
  strncpy(save_game_info, info, LEN_SAVE_GAME_INFO);
  save_game_info[LEN_SAVE_GAME_INFO - 1] = '\0';
}

void dc_load_map(int script, int* yield, int* preturnint, char* mapdat_file, char* dinkdat_file)
{
  // load a new map/dink.dat
  strcpy(current_map, mapdat_file);
  strcpy(current_dat, dinkdat_file);
  load_info();
}

void dc_load_tile(int script, int* yield, int* preturnint, char* tileset_file, int tileset_index)
{
  // load new tiles
  if (tileset_index >= 1 && tileset_index <= GFX_TILES_NB_SETS)
    {
      //Load in the new tiles...
      tiles_load_slot(tileset_file, tileset_index);
      
      //Store in save game
      strncpy(play.tile[tileset_index].file, tileset_file, 50);
    }
  else
    {
      log_error("[DinkC] %s:%d:%s: dc_load_tile: invalid tileset index '%d'",
		rinfo[script]->name, rinfo[script]->debug_line, cur_funcname,
		tileset_index);
    }
}

void dc_map_tile(int script, int* yield, int* preturnint, int tile_position, int tile_index)
{
  // developers can change or see what tile is at any given position
  // Yeah... they can only modify valid tiles
  if (tile_position >= 1 && tile_position <= 96)
    {
      int max = GFX_TILES_NB_SQUARES - 1;

      if (tile_index >= 0 && tile_index <= max)
	pam.t[tile_position - 1].square_full_idx0 = tile_index;
      else
	log_error("[DinkC] %s:%d:%s: dc_map_tile: invalid tile index '%d'",
		  rinfo[script]->name, rinfo[script]->debug_line, cur_funcname,
		  tile_index);

      *preturnint = pam.t[tile_position - 1].square_full_idx0;
    }
}

void dc_map_hard_tile(int script, int* yield, int* preturnint, int tile_position, int hard_tile_index)
{
  // developers can retrieve/modify a hard tile
  // Yeah... they can only modify valid tiles
  if (tile_position >= 1 && tile_position <= 96)
    {
      //Only change the value if it is greater than 0...
      if (hard_tile_index > 0)
	pam.t[tile_position - 1].althard = hard_tile_index;
      *preturnint = pam.t[tile_position - 1].althard;
    }
}


void dc_load_palette(int script, int* yield, int* preturnint, char* bmp_file)
{
  // load a palette from any bmp
  if (gfx_palette_set_from_bmp(bmp_file) < 0)
    log_error("[DinkC] Couldn't load palette from '%s': %s", bmp_file, SDL_GetError());
  gfx_palette_get_phys(GFX_real_pal);
  
  //Store in save game
  strncpy(play.palette, slist[0], 50);
}

void dc_get_item(int script, int* yield, int* preturnint, char* dcscript)
{
  // get index of specified item
  *preturnint = 0;
  {
    int i = 0;
    for (; i < NB_ITEMS; i++)
      {
	if (play.item[i].active
	    && compare(play.item[i].name, dcscript))
	  {
	    *preturnint = i + 1;
	    break;
	  }
      }
  }
}

void dc_get_magic(int script, int* yield, int* preturnint, char* dcscript)
{
  // get index of specified magic spell
  *preturnint = 0;
  {
    int i = 0;
    for (; i < NB_MITEMS; i++)
      {
	if (play.mitem[i].active
	    && compare(play.mitem[i].name, dcscript))
	  {
	    *preturnint = i + 1;
	    break;
	  }
      }
  }
}

void dc_set_font_color(int script, int* yield, int* preturnint, int index, int r, int g, int b)
{
  // sets font color
  set_font_color(index, r, g, b);
}

void dc_get_next_sprite_with_this_brain(int script, int* yield, int* preturnint,
					int brain, int sprite_ignore, int sprite_start_with)
{
  // make Paul Pliska's life more fulfilling
  {
    int i = sprite_start_with;
    for (; i <= last_sprite_created; i++)
      {
	if ((spr[i].brain == brain) && (i != sprite_ignore))
	  if (spr[i].active == 1)
	    {
	      log_debug("Ok, sprite with brain %d is %d", brain, i);
	      *preturnint = i;
	      return;
	    }
      }
  }
  log_debug("Ok, sprite with brain %d is 0", brain);
  *preturnint = 0; /* not found */
}

void dc_set_smooth_follow(int script, int* yield, int* preturnint, int smooth_p)
{
  if (smooth_p == 0)
    smooth_follow = 0;
  else if (smooth_p == 1)
    smooth_follow = 1;
}
void dc_set_dink_base_push(int script, int* yield, int* preturnint, int base_sequence)
{
  dink_base_push = base_sequence;
}

void dc_callback_kill(int script, int* yield, int* preturnint, int callback_index)
{
  log_debug("setting callback random");
  kill_callback(callback_index);
}


/****************/
/*  Hash table  */
/*              */
/****************/

/* Map DinkC function with C function */
#define NB_COMMON_ARGS 3
struct binding 
{
  char* funcname; /* name of the function, as string */
  void* func;     /* pointer to the C function */
  int params[10]; /* DinkC specification of params e.g. {2,1,1,0,0,0,0,0,0,0} */
  enum dinkc_parser_state badparams_dcps; /* if the DinkC script has bad arguments, skip line or yield? */
  int badparams_returnint_p; /* overwrite returnint if bad arguments? */
  int badparams_returnint;   /* value for returnint if badparams_returnint_p is 1 */
};

/* Hash table of bindings, build dynamically (depending on 'dversion',
   not statically) */
Hash_table* bindings = NULL;

/* Auxiliary functions for hash */
static size_t dinkc_bindings_hasher(const void *x, size_t tablesize)
{
  return hash_string(((struct binding*)x)->funcname, tablesize);
  // We could also call 'hash_pjw' from module 'hash-pjw'
}

static bool dinkc_bindings_comparator(const void* a, const void* b)
{
  return !strcmp(((struct binding*)a)->funcname,
		 ((struct binding*)b)->funcname);
}

/**
 * Search a binding by function name
 */
struct binding* dinkc_bindings_lookup(dinkc_sp_custom hash, char* funcname)
{
  struct binding search;
  struct binding *result;
  char* lcfuncname = strdup(funcname);
  char* pc;
  for (pc = lcfuncname; *pc != '\0'; pc++)
    *pc = tolower(*pc);
  search.funcname = lcfuncname;

  result = hash_lookup(hash, &search);

  free(lcfuncname);
  return result;
}

/**
 * Add a new binding to hash table 'hash'.
 */
static void dinkc_bindings_add(Hash_table* hash, struct binding* pbd)
{
  void* slot = dinkc_bindings_lookup(hash, pbd->funcname);
  if (slot != NULL)
    {
      log_fatal("Internal error: attempting to redeclare DinkC function %s", pbd->funcname);
      exit(EXIT_FAILURE);
    }

  /* Copy uninitialized binding in hash table */
  struct binding* newslot = malloc(sizeof(struct binding));
  *newslot = *pbd;
  if (hash_insert(hash, newslot) == NULL)
    {
      log_fatal("Not enough memory to declare DinkC function %s", pbd->funcname);
      exit(EXIT_FAILURE);
    }
}


/**
 * Add a DinkC binding
 * 
 * Simple macro to allow using struct initializer e.g. {2,1,1,0....}
 * when declaring a DinkC function.
 */
#define DCBD_ADD(name, ...)                                 \
{                                                           \
  struct binding bd = { #name, dc_ ## name, __VA_ARGS__ };  \
  dinkc_bindings_add(bindings, &bd);                        \
}
/**
 * Map DinkC functions to C functions, with their arguments
 */
void dinkc_bindings_init()
{
  /* Set all string params pointers to NULL */
  int i = 0;
  for (; i < 10; i++)
    {
      /* alloc empty strings; will be replaced as needed in
	 get_parm(...) */
      slist[i] = strdup("");
    }

  Hash_tuning* default_tuner = NULL;
  int start_size = 400; /* ~nbfuncs*2 to try and avoid collisions */
  bindings = hash_initialize(start_size, default_tuner,
			     dinkc_bindings_hasher, dinkc_bindings_comparator,
			     free);


  /* funcname, params, badparams_dcps, badparams_returnint_p, badparams_returnint */

  DCBD_ADD(sp_active,                 {1,1,0,0,0,0,0,0,0,0}, DCPS_GOTO_NEXTLINE, 1, -1);
  DCBD_ADD(sp_attack_hit_sound,       {1,1,0,0,0,0,0,0,0,0}, DCPS_GOTO_NEXTLINE, 1, -1);
  DCBD_ADD(sp_attack_hit_sound_speed, {1,1,0,0,0,0,0,0,0,0}, DCPS_GOTO_NEXTLINE, 1, -1);
  DCBD_ADD(sp_attack_wait,            {1,1,0,0,0,0,0,0,0,0}, DCPS_GOTO_NEXTLINE, 1, -1);
  DCBD_ADD(sp_base_attack,            {1,1,0,0,0,0,0,0,0,0}, DCPS_GOTO_NEXTLINE, 1, -1);
  DCBD_ADD(sp_base_die,               {1,1,0,0,0,0,0,0,0,0}, DCPS_GOTO_NEXTLINE, 1, -1);
  DCBD_ADD(sp_base_hit,               {1,1,0,0,0,0,0,0,0,0}, DCPS_GOTO_NEXTLINE, 1, -1);
  DCBD_ADD(sp_base_idle,              {1,1,0,0,0,0,0,0,0,0}, DCPS_GOTO_NEXTLINE, 1, -1);
  DCBD_ADD(sp_base_walk,              {1,1,0,0,0,0,0,0,0,0}, DCPS_GOTO_NEXTLINE, 1, -1);
  DCBD_ADD(sp_brain,                  {1,1,0,0,0,0,0,0,0,0}, DCPS_GOTO_NEXTLINE, 1, -1);
  DCBD_ADD(sp_brain_parm,             {1,1,0,0,0,0,0,0,0,0}, DCPS_GOTO_NEXTLINE, 1, -1);
  DCBD_ADD(sp_brain_parm2,            {1,1,0,0,0,0,0,0,0,0}, DCPS_GOTO_NEXTLINE, 1, -1);
  DCBD_ADD(sp_defense,                {1,1,0,0,0,0,0,0,0,0}, DCPS_GOTO_NEXTLINE, 1, -1);
  DCBD_ADD(sp_dir,                    {1,1,0,0,0,0,0,0,0,0}, DCPS_GOTO_NEXTLINE, 1, -1);
  DCBD_ADD(sp_disabled,               {1,1,0,0,0,0,0,0,0,0}, DCPS_GOTO_NEXTLINE, 1, -1);
  DCBD_ADD(sp_distance,               {1,1,0,0,0,0,0,0,0,0}, DCPS_GOTO_NEXTLINE, 1, -1);
  DCBD_ADD(sp_exp,                    {1,1,0,0,0,0,0,0,0,0}, DCPS_GOTO_NEXTLINE, 1, -1);
  DCBD_ADD(sp_flying,                 {1,1,0,0,0,0,0,0,0,0}, DCPS_GOTO_NEXTLINE, 1, -1);
  DCBD_ADD(sp_follow,                 {1,1,0,0,0,0,0,0,0,0}, DCPS_GOTO_NEXTLINE, 1, -1);
  DCBD_ADD(sp_frame,                  {1,1,0,0,0,0,0,0,0,0}, DCPS_GOTO_NEXTLINE, 1, -1);
  DCBD_ADD(sp_frame_delay,            {1,1,0,0,0,0,0,0,0,0}, DCPS_GOTO_NEXTLINE, 1, -1);
  DCBD_ADD(sp_gold,                   {1,1,0,0,0,0,0,0,0,0}, DCPS_GOTO_NEXTLINE, 1, -1);
  DCBD_ADD(sp_hard,                   {1,1,0,0,0,0,0,0,0,0}, DCPS_GOTO_NEXTLINE, 1, -1);
  DCBD_ADD(sp_hitpoints,              {1,1,0,0,0,0,0,0,0,0}, DCPS_GOTO_NEXTLINE, 1, -1);
  DCBD_ADD(sp_move_nohard,            {1,1,0,0,0,0,0,0,0,0}, DCPS_GOTO_NEXTLINE, 1, -1);
  DCBD_ADD(sp_mx,                     {1,1,0,0,0,0,0,0,0,0}, DCPS_GOTO_NEXTLINE, 1, -1);
  DCBD_ADD(sp_my,                     {1,1,0,0,0,0,0,0,0,0}, DCPS_GOTO_NEXTLINE, 1, -1);
  DCBD_ADD(sp_noclip,                 {1,1,0,0,0,0,0,0,0,0}, DCPS_GOTO_NEXTLINE, 1, -1);
  DCBD_ADD(sp_nocontrol,              {1,1,0,0,0,0,0,0,0,0}, DCPS_GOTO_NEXTLINE, 1, -1);
  DCBD_ADD(sp_nodraw,                 {1,1,0,0,0,0,0,0,0,0}, DCPS_GOTO_NEXTLINE, 1, -1);
  DCBD_ADD(sp_nohit,                  {1,1,0,0,0,0,0,0,0,0}, DCPS_GOTO_NEXTLINE, 1, -1);
  DCBD_ADD(sp_notouch,                {1,1,0,0,0,0,0,0,0,0}, DCPS_GOTO_NEXTLINE, 1, -1);
  DCBD_ADD(sp_pframe,                 {1,1,0,0,0,0,0,0,0,0}, DCPS_GOTO_NEXTLINE, 1, -1);
  DCBD_ADD(sp_picfreeze,              {1,1,0,0,0,0,0,0,0,0}, DCPS_GOTO_NEXTLINE, 1, -1);
  DCBD_ADD(sp_pseq,                   {1,1,0,0,0,0,0,0,0,0}, DCPS_GOTO_NEXTLINE, 1, -1);
  DCBD_ADD(sp_que,                    {1,1,0,0,0,0,0,0,0,0}, DCPS_GOTO_NEXTLINE, 1, -1);
  DCBD_ADD(sp_range,                  {1,1,0,0,0,0,0,0,0,0}, DCPS_GOTO_NEXTLINE, 1, -1);
  DCBD_ADD(sp_reverse,                {1,1,0,0,0,0,0,0,0,0}, DCPS_GOTO_NEXTLINE, 1, -1);
  DCBD_ADD(sp_seq,                    {1,1,0,0,0,0,0,0,0,0}, DCPS_GOTO_NEXTLINE, 1, -1);
  DCBD_ADD(sp_size,                   {1,1,0,0,0,0,0,0,0,0}, DCPS_GOTO_NEXTLINE, 1, -1);
  DCBD_ADD(sp_sound,                  {1,1,0,0,0,0,0,0,0,0}, DCPS_GOTO_NEXTLINE, 1, -1);
  DCBD_ADD(sp_speed,                  {1,1,0,0,0,0,0,0,0,0}, DCPS_GOTO_NEXTLINE, 1, -1);
  DCBD_ADD(sp_strength,               {1,1,0,0,0,0,0,0,0,0}, DCPS_GOTO_NEXTLINE, 1, -1);
  DCBD_ADD(sp_target,                 {1,1,0,0,0,0,0,0,0,0}, DCPS_GOTO_NEXTLINE, 1, -1);
  DCBD_ADD(sp_timing,                 {1,1,0,0,0,0,0,0,0,0}, DCPS_GOTO_NEXTLINE, 1, -1);
  DCBD_ADD(sp_touch_damage,           {1,1,0,0,0,0,0,0,0,0}, DCPS_GOTO_NEXTLINE, 1, -1);
  DCBD_ADD(sp_x,                      {1,1,0,0,0,0,0,0,0,0}, DCPS_GOTO_NEXTLINE, 1, -1);
  DCBD_ADD(sp_y,                      {1,1,0,0,0,0,0,0,0,0}, DCPS_GOTO_NEXTLINE, 1, -1);

  DCBD_ADD(sp_kill,                   {1,1,0,0,0,0,0,0,0,0}, DCPS_GOTO_NEXTLINE, 1, -1);
  DCBD_ADD(sp_editor_num,             {1,0,0,0,0,0,0,0,0,0}, DCPS_GOTO_NEXTLINE, 1, -1);
  DCBD_ADD(sp_kill_wait,              {1,0,0,0,0,0,0,0,0,0}, DCPS_GOTO_NEXTLINE, 1, -1);
  DCBD_ADD(sp_script,                 {1,2,0,0,0,0,0,0,0,0}, DCPS_GOTO_NEXTLINE, 0, 0);
  /* sp_base_death is an alias for sp_base_die */
  struct binding bd_sp_base_death = *dinkc_bindings_lookup(bindings, "sp_base_die");
  bd_sp_base_death.funcname = "sp_base_death";
  dinkc_bindings_add(bindings, &bd_sp_base_death);

  DCBD_ADD(unfreeze,              {1,0,0,0,0,0,0,0,0,0}, DCPS_GOTO_NEXTLINE, 0, 0);
  DCBD_ADD(freeze,                {1,0,0,0,0,0,0,0,0,0}, DCPS_GOTO_NEXTLINE, 0, 0);
  DCBD_ADD(set_callback_random,   {2,1,1,0,0,0,0,0,0,0}, DCPS_GOTO_NEXTLINE, 0, 0);
  DCBD_ADD(set_dink_speed,        {1,0,0,0,0,0,0,0,0,0}, DCPS_GOTO_NEXTLINE, 0, 0);
  DCBD_ADD(reset_timer,          {-1,0,0,0,0,0,0,0,0,0}, -1                , 0, 0);
  DCBD_ADD(set_keep_mouse,        {1,0,0,0,0,0,0,0,0,0}, DCPS_GOTO_NEXTLINE, 0, 0);
  DCBD_ADD(add_item,              {2,1,1,0,0,0,0,0,0,0}, DCPS_GOTO_NEXTLINE, 0, 0);
  DCBD_ADD(add_magic,             {2,1,1,0,0,0,0,0,0,0}, DCPS_GOTO_NEXTLINE, 0, 0);
  DCBD_ADD(add_exp,               {1,1,0,0,0,0,0,0,0,0}, DCPS_GOTO_NEXTLINE, 0, 0);
  DCBD_ADD(kill_this_item,        {2,0,0,0,0,0,0,0,0,0}, DCPS_GOTO_NEXTLINE, 0, 0);
  DCBD_ADD(kill_this_magic,       {2,0,0,0,0,0,0,0,0,0}, DCPS_GOTO_NEXTLINE, 0, 0);
  DCBD_ADD(show_bmp,              {2,1,1,0,0,0,0,0,0,0}, DCPS_YIELD        , 0, 0);
  DCBD_ADD(copy_bmp_to_screen,    {2,0,0,0,0,0,0,0,0,0}, DCPS_GOTO_NEXTLINE, 0, 0);
  DCBD_ADD(wait_for_button,      {-1,0,0,0,0,0,0,0,0,0}, -1                , 0, 0);
  DCBD_ADD(stop_wait_for_button, {-1,0,0,0,0,0,0,0,0,0}, -1                , 0, 0);
  DCBD_ADD(draw_screen,          {-1,0,0,0,0,0,0,0,0,0}, -1                , 0, 0);
  DCBD_ADD(free_items,           {-1,0,0,0,0,0,0,0,0,0}, -1                , 0, 0);
  DCBD_ADD(free_magic,           {-1,0,0,0,0,0,0,0,0,0}, -1                , 0, 0);
  DCBD_ADD(kill_cur_item,        {-1,0,0,0,0,0,0,0,0,0}, -1                , 0, 0);
  DCBD_ADD(kill_cur_magic,       {-1,0,0,0,0,0,0,0,0,0}, -1                , 0, 0);
  DCBD_ADD(draw_status,          {-1,0,0,0,0,0,0,0,0,0}, -1                , 0, 0);
  DCBD_ADD(arm_weapon,           {-1,0,0,0,0,0,0,0,0,0}, -1                , 0, 0);
  DCBD_ADD(arm_magic,            {-1,0,0,0,0,0,0,0,0,0}, -1                , 0, 0);
  DCBD_ADD(load_screen,          {-1,0,0,0,0,0,0,0,0,0}, -1                , 0, 0);
  DCBD_ADD(say,                   {2,1,0,0,0,0,0,0,0,0}, DCPS_GOTO_NEXTLINE, 0, 0);
  DCBD_ADD(say_stop,              {2,1,0,0,0,0,0,0,0,0}, DCPS_GOTO_NEXTLINE, 0, 0);
  DCBD_ADD(say_stop_npc,          {2,1,0,0,0,0,0,0,0,0}, DCPS_GOTO_NEXTLINE, 0, 0);
  DCBD_ADD(say_stop_xy,           {2,1,1,0,0,0,0,0,0,0}, DCPS_GOTO_NEXTLINE, 0, 0);
  DCBD_ADD(say_xy,                {2,1,1,0,0,0,0,0,0,0}, DCPS_GOTO_NEXTLINE, 0, 0);
  DCBD_ADD(restart_game,         {-1,0,0,0,0,0,0,0,0,0}, -1                , 0, 0);
  DCBD_ADD(wait,                  {1,0,0,0,0,0,0,0,0,0}, DCPS_GOTO_NEXTLINE, 0, 0);
  DCBD_ADD(preload_seq,           {1,0,0,0,0,0,0,0,0,0}, DCPS_GOTO_NEXTLINE, 0, 0);
  DCBD_ADD(script_attach,         {1,0,0,0,0,0,0,0,0,0}, DCPS_GOTO_NEXTLINE, 0, 0);
  DCBD_ADD(draw_hard_sprite,      {1,0,0,0,0,0,0,0,0,0}, DCPS_GOTO_NEXTLINE, 0, 0);

  DCBD_ADD(activate_bow,        {-1,0,0,0,0,0,0,0,0,0}, DCPS_GOTO_NEXTLINE, 0, 0);
  DCBD_ADD(disable_all_sprites, {-1,0,0,0,0,0,0,0,0,0}, DCPS_GOTO_NEXTLINE, 0, 0);
  DCBD_ADD(draw_background,     {-1,0,0,0,0,0,0,0,0,0}, DCPS_GOTO_NEXTLINE, 0, 0);
  DCBD_ADD(draw_hard_map,       {-1,0,0,0,0,0,0,0,0,0}, DCPS_GOTO_NEXTLINE, 0, 0);
  DCBD_ADD(enable_all_sprites,  {-1,0,0,0,0,0,0,0,0,0}, DCPS_GOTO_NEXTLINE, 0, 0);
  DCBD_ADD(fade_down,           {-1,0,0,0,0,0,0,0,0,0}, DCPS_GOTO_NEXTLINE, 0, 0);
  DCBD_ADD(fade_up,             {-1,0,0,0,0,0,0,0,0,0}, DCPS_GOTO_NEXTLINE, 0, 0);
  DCBD_ADD(get_burn,            {-1,0,0,0,0,0,0,0,0,0}, DCPS_GOTO_NEXTLINE, 0, 0);
  DCBD_ADD(get_last_bow_power,  {-1,0,0,0,0,0,0,0,0,0}, DCPS_GOTO_NEXTLINE, 0, 0);
  DCBD_ADD(get_version,         {-1,0,0,0,0,0,0,0,0,0}, DCPS_GOTO_NEXTLINE, 0, 0);
  DCBD_ADD(kill_all_sounds,     {-1,0,0,0,0,0,0,0,0,0}, DCPS_GOTO_NEXTLINE, 0, 0);
  DCBD_ADD(kill_game,           {-1,0,0,0,0,0,0,0,0,0}, DCPS_GOTO_NEXTLINE, 0, 0);
  DCBD_ADD(kill_this_task,      {-1,0,0,0,0,0,0,0,0,0}, DCPS_GOTO_NEXTLINE, 0, 0);
  DCBD_ADD(scripts_used,        {-1,0,0,0,0,0,0,0,0,0}, DCPS_GOTO_NEXTLINE, 0, 0);
  DCBD_ADD(stopcd,              {-1,0,0,0,0,0,0,0,0,0}, DCPS_GOTO_NEXTLINE, 0, 0);
  DCBD_ADD(stopmidi,            {-1,0,0,0,0,0,0,0,0,0}, DCPS_GOTO_NEXTLINE, 0, 0);
  DCBD_ADD(turn_midi_off,       {-1,0,0,0,0,0,0,0,0,0}, DCPS_GOTO_NEXTLINE, 0, 0);
  DCBD_ADD(turn_midi_on,        {-1,0,0,0,0,0,0,0,0,0}, DCPS_GOTO_NEXTLINE, 0, 0);

  DCBD_ADD(count_item,               {2,0,0,0,0,0,0,0,0,0}, DCPS_GOTO_NEXTLINE, 1, -1);
  DCBD_ADD(count_magic,              {2,0,0,0,0,0,0,0,0,0}, DCPS_GOTO_NEXTLINE, 1, -1);
  DCBD_ADD(compare_sprite_script,    {1,2,0,0,0,0,0,0,0,0}, DCPS_GOTO_NEXTLINE, 0, 0);
  DCBD_ADD(compare_weapon,           {2,0,0,0,0,0,0,0,0,0}, DCPS_GOTO_NEXTLINE, 0, 0);
  DCBD_ADD(compare_magic,            {2,0,0,0,0,0,0,0,0,0}, DCPS_GOTO_NEXTLINE, 0, 0);
  DCBD_ADD(init,                     {2,0,0,0,0,0,0,0,0,0}, DCPS_GOTO_NEXTLINE, 1, -1);
  DCBD_ADD(dink_can_walk_off_screen, {1,0,0,0,0,0,0,0,0,0}, DCPS_GOTO_NEXTLINE, 1, -1);
  DCBD_ADD(push_active,              {1,0,0,0,0,0,0,0,0,0}, DCPS_GOTO_NEXTLINE, 1, -1);
  DCBD_ADD(stop_entire_game,         {1,0,0,0,0,0,0,0,0,0}, DCPS_GOTO_NEXTLINE, 1, -1);

  DCBD_ADD(editor_type,  {1,1,0,0,0,0,0,0,0,0}, DCPS_GOTO_NEXTLINE, 1, -1);
  DCBD_ADD(editor_seq,   {1,1,0,0,0,0,0,0,0,0}, DCPS_GOTO_NEXTLINE, 1, -1);
  DCBD_ADD(editor_frame, {1,1,0,0,0,0,0,0,0,0}, DCPS_GOTO_NEXTLINE, 1, -1);

  DCBD_ADD(move,                 {1,1,1,1,0,0,0,0,0,0}, DCPS_GOTO_NEXTLINE, 0, 0);
  DCBD_ADD(spawn,                {2,0,0,0,0,0,0,0,0,0}, DCPS_GOTO_NEXTLINE, 0, 0);
  DCBD_ADD(run_script_by_number, {1,2,0,0,0,0,0,0,0,0}, DCPS_GOTO_NEXTLINE, 0, 0);
  DCBD_ADD(playmidi,             {2,0,0,0,0,0,0,0,0,0}, DCPS_GOTO_NEXTLINE, 0, 0);
  DCBD_ADD(playsound,            {1,1,1,1,1,0,0,0,0,0}, DCPS_GOTO_NEXTLINE, 1, 0);
  DCBD_ADD(sound_set_survive,    {1,1,0,0,0,0,0,0,0,0}, DCPS_GOTO_NEXTLINE, 0, 0);
  DCBD_ADD(sound_set_vol,        {1,1,0,0,0,0,0,0,0,0}, DCPS_GOTO_NEXTLINE, 0, 0);
  DCBD_ADD(sound_set_kill,       {1,0,0,0,0,0,0,0,0,0}, DCPS_GOTO_NEXTLINE, 0, 0);

  DCBD_ADD(save_game,                 {1,0,0,0,0,0,0,0,0,0}, DCPS_GOTO_NEXTLINE, 0, 0);
  DCBD_ADD(force_vision,              {1,0,0,0,0,0,0,0,0,0}, DCPS_GOTO_NEXTLINE, 0, 0);
  DCBD_ADD(fill_screen,               {1,0,0,0,0,0,0,0,0,0}, DCPS_GOTO_NEXTLINE, 0, 0);
  DCBD_ADD(load_game,                 {1,0,0,0,0,0,0,0,0,0}, DCPS_GOTO_NEXTLINE, 0, 0);
  DCBD_ADD(game_exist,                {1,0,0,0,0,0,0,0,0,0}, DCPS_GOTO_NEXTLINE, 0, 0);
  DCBD_ADD(move_stop,                 {1,1,1,1,0,0,0,0,0,0}, DCPS_GOTO_NEXTLINE, 0, 0);
  DCBD_ADD(load_sound,                {2,1,0,0,0,0,0,0,0,0}, DCPS_GOTO_NEXTLINE, 0, 0);
  DCBD_ADD(debug,                     {2,0,0,0,0,0,0,0,0,0}, DCPS_GOTO_NEXTLINE, 0, 0);
  DCBD_ADD(busy,                      {1,0,0,0,0,0,0,0,0,0}, DCPS_GOTO_NEXTLINE, 0, 0);

  DCBD_ADD(make_global_int,            {2,1,0,0,0,0,0,0,0,0}, DCPS_GOTO_NEXTLINE, 0, 0);
  DCBD_ADD(inside_box,                 {1,1,1,1,1,1,0,0,0,0}, DCPS_GOTO_NEXTLINE, 0, 0);
  DCBD_ADD(random,                     {1,1,0,0,0,0,0,0,0,0}, DCPS_GOTO_NEXTLINE, 0, 0);
  DCBD_ADD(initfont,                   {2,0,0,0,0,0,0,0,0,0}, DCPS_GOTO_NEXTLINE, 0, 0);
  DCBD_ADD(set_mode,                   {1,0,0,0,0,0,0,0,0,0}, DCPS_GOTO_NEXTLINE, 0, 0);
  DCBD_ADD(kill_shadow,                {1,0,0,0,0,0,0,0,0,0}, DCPS_GOTO_NEXTLINE, 0, 0);
  DCBD_ADD(create_sprite,              {1,1,1,1,1,0,0,0,0,0}, DCPS_GOTO_NEXTLINE, 1, 0);
  DCBD_ADD(sp,                         {1,0,0,0,0,0,0,0,0,0}, DCPS_GOTO_NEXTLINE, 1, 0);
  DCBD_ADD(is_script_attached,         {1,0,0,0,0,0,0,0,0,0}, DCPS_GOTO_NEXTLINE, 0, 0);
  DCBD_ADD(get_sprite_with_this_brain,      {1,1,0,0,0,0,0,0,0,0}, DCPS_GOTO_NEXTLINE, 1, 0);
  DCBD_ADD(get_rand_sprite_with_this_brain, {1,1,0,0,0,0,0,0,0,0}, DCPS_GOTO_NEXTLINE, 1, 0);
  DCBD_ADD(set_button,                 {1,1,0,0,0,0,0,0,0,0}, DCPS_GOTO_NEXTLINE, 1, -1);
  DCBD_ADD(hurt,                       {1,1,0,0,0,0,0,0,0,0}, DCPS_GOTO_NEXTLINE, 1, -1);
  DCBD_ADD(screenlock,                 {1,0,0,0,0,0,0,0,0,0}, DCPS_GOTO_NEXTLINE, 1, -1);

  if (dversion >= 108)
    {
      DCBD_ADD(sp_blood_num,   {1,1,0,0,0,0,0,0,0,0}, DCPS_GOTO_NEXTLINE, 1, -1);
      DCBD_ADD(sp_blood_seq,   {1,1,0,0,0,0,0,0,0,0}, DCPS_GOTO_NEXTLINE, 1, -1);
      DCBD_ADD(sp_clip_bottom, {1,1,0,0,0,0,0,0,0,0}, DCPS_GOTO_NEXTLINE, 1, -1);
      DCBD_ADD(sp_clip_left,   {1,1,0,0,0,0,0,0,0,0}, DCPS_GOTO_NEXTLINE, 1, -1);
      DCBD_ADD(sp_clip_right,  {1,1,0,0,0,0,0,0,0,0}, DCPS_GOTO_NEXTLINE, 1, -1);
      DCBD_ADD(sp_clip_top,    {1,1,0,0,0,0,0,0,0,0}, DCPS_GOTO_NEXTLINE, 1, -1);

      DCBD_ADD(sp_custom,      {2,1,1,0,0,0,0,0,0,0}, DCPS_GOTO_NEXTLINE, 1, -1);

      DCBD_ADD(sp_move_x, {1,1,0,0,0,0,0,0,0,0}, DCPS_GOTO_NEXTLINE, 0, 0);
      DCBD_ADD(sp_move_y, {1,1,0,0,0,0,0,0,0,0}, DCPS_GOTO_NEXTLINE, 0, 0);
      DCBD_ADD(sp_freeze, {1,1,0,0,0,0,0,0,0,0}, DCPS_GOTO_NEXTLINE, 0, 0);


      DCBD_ADD(clear_editor_info, {-1,0,0,0,0,0,0,0,0,0}, DCPS_GOTO_NEXTLINE, 0, 0);
      DCBD_ADD(get_date_day,      {-1,0,0,0,0,0,0,0,0,0}, DCPS_GOTO_NEXTLINE, 0, 0);
      DCBD_ADD(get_date_month,    {-1,0,0,0,0,0,0,0,0,0}, DCPS_GOTO_NEXTLINE, 0, 0);
      DCBD_ADD(get_date_year,     {-1,0,0,0,0,0,0,0,0,0}, DCPS_GOTO_NEXTLINE, 0, 0);
      DCBD_ADD(get_time_game,     {-1,0,0,0,0,0,0,0,0,0}, DCPS_GOTO_NEXTLINE, 0, 0);
      DCBD_ADD(get_time_real,     {-1,0,0,0,0,0,0,0,0,0}, DCPS_GOTO_NEXTLINE, 0, 0);
      DCBD_ADD(get_truecolor,     {-1,0,0,0,0,0,0,0,0,0}, DCPS_GOTO_NEXTLINE, 0, 0);
      DCBD_ADD(show_console,      {-1,0,0,0,0,0,0,0,0,0}, DCPS_GOTO_NEXTLINE, 0, 0);
      DCBD_ADD(show_inventory,    {-1,0,0,0,0,0,0,0,0,0}, DCPS_GOTO_NEXTLINE, 0, 0);
      DCBD_ADD(var_used,          {-1,0,0,0,0,0,0,0,0,0}, DCPS_GOTO_NEXTLINE, 0, 0);

      DCBD_ADD(loopmidi,           {1,0,0,0,0,0,0,0,0,0}, DCPS_GOTO_NEXTLINE, 0, 0);

      DCBD_ADD(math_abs,                 {1,0,0,0,0,0,0,0,0,0}, DCPS_GOTO_NEXTLINE, 0, 0);
      DCBD_ADD(math_sqrt,                {1,0,0,0,0,0,0,0,0,0}, DCPS_GOTO_NEXTLINE, 0, 0);
      DCBD_ADD(math_mod,                 {1,1,0,0,0,0,0,0,0,0}, DCPS_GOTO_NEXTLINE, 0, 0);
      DCBD_ADD(make_global_function,     {2,2,0,0,0,0,0,0,0,0}, DCPS_GOTO_NEXTLINE, 0, 0);
      DCBD_ADD(set_save_game_info,       {2,0,0,0,0,0,0,0,0,0}, DCPS_GOTO_NEXTLINE, 0, 0);
      DCBD_ADD(load_map,                 {2,2,0,0,0,0,0,0,0,0}, DCPS_GOTO_NEXTLINE, 0, 0);
      DCBD_ADD(load_tile,                {2,1,0,0,0,0,0,0,0,0}, DCPS_GOTO_NEXTLINE, 0, 0);
      DCBD_ADD(map_tile,                 {1,1,0,0,0,0,0,0,0,0}, DCPS_GOTO_NEXTLINE, 1, -1);
      DCBD_ADD(map_hard_tile,            {1,1,0,0,0,0,0,0,0,0}, DCPS_GOTO_NEXTLINE, 1, -1);
      DCBD_ADD(load_palette,             {2,0,0,0,0,0,0,0,0,0}, DCPS_GOTO_NEXTLINE, 0, 0);
      DCBD_ADD(get_item,                 {2,0,0,0,0,0,0,0,0,0}, DCPS_GOTO_NEXTLINE, 1, -1);
      DCBD_ADD(get_magic,                {2,0,0,0,0,0,0,0,0,0}, DCPS_GOTO_NEXTLINE, 1, -1);
      DCBD_ADD(set_font_color,           {1,1,1,1,0,0,0,0,0,0}, DCPS_GOTO_NEXTLINE, 0, 0);
      DCBD_ADD(set_smooth_follow,        {1,0,0,0,0,0,0,0,0,0}, DCPS_GOTO_NEXTLINE, 1, -1);
      DCBD_ADD(set_dink_base_push,       {1,0,0,0,0,0,0,0,0,0}, DCPS_GOTO_NEXTLINE, 0, 0);
      DCBD_ADD(callback_kill,            {1,0,0,0,0,0,0,0,0,0}, DCPS_GOTO_NEXTLINE, 0, 0);
      DCBD_ADD(get_next_sprite_with_this_brain,  {1,1,1,0,0,0,0,0,0,0}, DCPS_GOTO_NEXTLINE, 1, 0);
    }
}

void dinkc_bindings_quit()
{
  if (bindings != NULL)
    hash_free(bindings);
  bindings = NULL;

  int i = 0;
  for (; i < 10; i++)
    {
      if (slist[i] != NULL)
	free(slist[i]);
      slist[i] = NULL;
    }
}


/******************/
/*  DinkC parser  */
/*                */
/******************/

void attach(void)
{
  /* Make sure the "system" variable exists - otherwise we might use a
     NULL pointer below */
  char* var_names[22] = { "&life", "&vision", "&result", "&speed",
		     "&timing", "&lifemax", "&exp", "&strength",
		     "&defense", "&gold", "&magic", "&level",
		     "&player_map", "&cur_weapon", "&cur_magic",
		     "&last_text", "&magic_level", "&update_status",
		     "&missile_target", "&enemy_sprite", "&magic_cost",
		     "&missle_source" };
  int n, i;
  for (n = 0; n < 22; n++)
    {
      if (!var_exists(var_names[n], 0)) /* 0 = global scope */
	make_int(var_names[n], 0, 0, -1);
      /* TODO: setting script to -1 is asking for troubles... */
    }

  for (i = 1; i < MAX_VARS; i++)
    {
      if (compare("&life", play.var[i].name)) plife = &play.var[i].var;
      if (compare("&vision", play.var[i].name)) pvision = &play.var[i].var;
      if (compare("&result", play.var[i].name)) presult = &play.var[i].var;
      if (compare("&speed", play.var[i].name)) pspeed = &play.var[i].var;
      if (compare("&timing", play.var[i].name))	ptiming = &play.var[i].var;
      if (compare("&lifemax", play.var[i].name)) plifemax = &play.var[i].var;
      if (compare("&exp", play.var[i].name)) pexper = &play.var[i].var;
      if (compare("&strength", play.var[i].name))  pstrength = &play.var[i].var;
      if (compare("&defense", play.var[i].name))  pdefense = &play.var[i].var;
      if (compare("&gold", play.var[i].name))  pgold = &play.var[i].var;
      if (compare("&magic", play.var[i].name))  pmagic = &play.var[i].var;
      if (compare("&level", play.var[i].name))  plevel = &play.var[i].var;
      if (compare("&player_map", play.var[i].name)) pmap = &play.var[i].var;
      if (compare("&cur_weapon", play.var[i].name)) pcur_weapon = &play.var[i].var;
      if (compare("&cur_magic", play.var[i].name)) pcur_magic = &play.var[i].var;
      if (compare("&last_text", play.var[i].name)) plast_text = &play.var[i].var;
      if (compare("&magic_level", play.var[i].name)) pmagic_level = &play.var[i].var;
      if (compare("&update_status", play.var[i].name)) pupdate_status = &play.var[i].var;
      if (compare("&missile_target", play.var[i].name)) pmissile_target = &play.var[i].var;
      if (compare("&enemy_sprite", play.var[i].name)) penemy_sprite = &play.var[i].var;
      if (compare("&magic_cost", play.var[i].name)) pmagic_cost = &play.var[i].var;
      if (compare("&missle_source", play.var[i].name)) pmissle_source = &play.var[i].var;
    }
}


/**
 * Process DinkC dialog choice stanza
 */
/*bool*/int talk_get(int script)
{
  char* line = NULL;
  int cur = 1;
  int retnum = 0;
  clear_talk();
  talk.newy = -5000;
  while(1)
    {
    redo:
      line = read_next_line(script);
      if (line == NULL)
	line = strdup(""); // compatibility
      
      strip_beginning_spaces(line);
      //Msg("Comparing to %s.", line);
      
      char* word = get_word(line, 1);
      if (compare(word, "set_y"))
        {
	  free(word);
	  word = get_word(line, 2);
	  talk.newy = atol(word);
	  free(word);
	  free(line);
	  goto redo;
        }
      
      if (compare(word, "set_title_color"))
        {
	  free(word);
	  word = get_word(line, 2);
	  talk.color = atol(word);
	  free(word);
	  free(line);
	  goto redo;
        }
      free(word);
      
      strip_beginning_spaces(line);
      if (compare(line, "\n"))
	{
	  free(line);
	  goto redo;
	}

      char* directive = NULL;
morestuff:
      directive = separate_string(line, 1, '(');
      strip_beginning_spaces(directive);
      
      if (compare(directive, "title_start"))
	{
	  free(line);
	  while((line = read_next_line(script)) != NULL)
	    {
	      strip_beginning_spaces(line);
	      free(directive);
	      
	      directive = separate_string(line, 1, '(');
	      if (directive != NULL)
		{
		  strip_beginning_spaces(directive);
		  
		  if (compare(directive, "title_end"))
		    {
		      replace_norealloc("\n\n\n\n", "\n \n", talk.buffer);
		      replace_norealloc("\n\n", "\n", talk.buffer);
		      free(directive);
		      free(line);
		      goto redo;
		    }
		}
	      
	      /* drop '\n', this messes translations */
	      line[strlen(line)-1] = '\0';
	      /* Translate text (before variable substitution) */
	      char* translation = i18n_translate(rinfo[script]->name, rinfo[script]->debug_line, line);
	      decipher_string(&translation, script);
	      int cur_len = strlen(talk.buffer);
	      strncat(talk.buffer, translation, TALK_TITLE_BUFSIZ - 1 - cur_len - 1);
	      free(translation);
	      /* put '\n' back */
	      strcat(talk.buffer, "\n");
	      talk.buffer[TALK_TITLE_BUFSIZ-1] = '\0';
	      free(line);
	    }
	  
	  free(directive);
	  goto redo;
	}
      
      if (compare(directive, "choice_end"))
	{
	  if (cur-1 == 0)
	    {
	      log_debug("Error: choice() has 0 options in script %s, offset %d.",
			rinfo[script]->name, rinfo[script]->current);
	      
	      free(directive);
	      free(line);
	      return /*false*/0;
	    }
	  //all done, lets jam
	  //Msg("found choice_end, leaving!");
	  talk.last = cur-1;
	  talk.cur = 1;
	  talk.active = /*true*/1;
	  talk.page = 1;
	  talk.cur_view = 1;
	  talk.script = script;
	  
	  free(directive);
	  free(line);
	  return /*true*/1;
	}
      free(directive);

      char* condition = separate_string(line, 1, '"');
      strip_beginning_spaces(condition);
      
      if (strlen(condition) > 2)
	{
	  //found conditional statement
	  if (strchr(condition, '(') == NULL)
	    {
	      log_error("[DinkC] Error with choice() statement in script %s, offset %d. (%s?)",
			rinfo[script]->name, rinfo[script]->current, condition);
	      
	      free(condition);
	      free(line);
	      return /*false*/0;
	    }
	  
	  char* temp = separate_string(condition, 2, '(');
	  free(condition);
	  condition = separate_string(temp, 1, ')');
	  free(temp);
	  
	  //Msg("Running %s through var figure..", check);
	  if (var_figure(condition, script) == 0)
	    {
	      log_debug("Answer is no.");
	      retnum++;
	      
	      free(condition);
	      free(line);
	      goto redo;
	      //said NO to statement
	    }
	  //Msg("Answer is yes.");
	  free(condition);
	  
	  /* Resume processing stripping the first condition (there
	     may be several conditions on a single dialog ligne, which
	     are AND'ed) */
	  char* p = strchr(line, ')') + 1;
	  int i = 0;
	  for (; *p != '\0'; i++, p++)
	    line[i] = *p;
	  line[i] = '\0';
	  goto morestuff;
	}
      free(condition);
      
      retnum++;
      char* text = separate_string(line, 2, '"');
      if (strlen(text) > 0)
	{
	  /* Translate text (before variable substitution) */
	  char* translation = i18n_translate(rinfo[script]->name, rinfo[script]->debug_line, text);
	  strip_beginning_spaces(translation);

	  decipher_savegame = retnum;
	  decipher_string(&translation, script);
	  decipher_savegame = 0;
	  strncpy(talk.line[cur], translation, TALK_LINE_BUFSIZ-1);
	  talk.line[cur][TALK_LINE_BUFSIZ-1] = '\0';
	  free(translation);
	}
      else
	{
	  /* Handle empty text separately because _("") has a special
	     meaning (returns .mo meta-data). */
	  strcpy(talk.line[cur], "");
	}
      free(text);
      talk.line_return[cur] = retnum;
      cur++;
      free(line);
    }
}


/**
 * Utility function for 'process_line', to separate and store the current procedure arguments.
 *
 * proc_name: named of the called function
 * script: script id
 * str_params: string to parse (what was after the function name)
 * spec: describe the function's parameters:
 *    1=int
 *    2=string
 *    0=no more args (10 args max)
 *
 * Known compatibility issue: passing no argument to a function
 * expecting 1 int argument is considered valid..
 *
 * Return: 0 if parse error, 1 if success
 */
int get_parms(char proc_name[20], int script, char *str_params, int* spec)
{
  /* Clean-up parameters */
  memset(nlist, 0, 10 * sizeof (int));
  {
    int i = 0;
    for (; i < 10; i++)
      slist[i][0] = '\0';
  }

  /* Safety */
  char* limit = str_params + strlen(str_params);

  strip_beginning_spaces(str_params);
  if (str_params[0] == '(')
    {
      //Msg("Found first (.");
      str_params++;
    }
  else
    {
      log_error("[DinkC] Missing '(' in %s, offset %d.", rinfo[script]->name, rinfo[script]->current);
      return 0;
    }

  int i = 0;
  for (; i < 10; i++)
    {
      strip_beginning_spaces(str_params);
      
      if (spec[i] == 1) // type=int
	{
	  // Get next parameter (until ',' or ')' is reached)
	  char* parm = NULL;
	  if (strchr(str_params, ',') != NULL)
	    parm = separate_string(str_params, 1, ',');
	  else if (strchr(str_params, ')') != NULL)
	    parm = separate_string(str_params, 1, ')');
	  else
	    parm = strdup("");

	  // move to next param
	  str_params += strlen(parm);

	  int intval = -1;
	  if (parm[0] == '&')
	    {
	      replace_norealloc(" ", "", parm);
	      intval = decipher(parm, script);
	    }
	  else
	    {
	      intval = atol(parm);
	    }
	  // store parameter of type 'int'
	  nlist[i] = intval;
	  free(parm);
	}
      else if (spec[i] == 2) // type=string
	{
	  // Checking for string
	  char* parm = NULL;
	  parm = separate_string(str_params, 2, '"');

	  // replace DinkC string parameter
	  free(slist[i]);
	  slist[i] = parm;

	  // move to next param
	  str_params += strlen(parm) + 2; // 2x"
	  if (str_params > limit) str_params = limit;
	}

      if ((i+1) == 10 || spec[i+1] == 0) // this was the last arg
	{
	  //finish
	  strip_beginning_spaces(str_params);
	  
	  if (str_params[0] == ')')
	    {
	      str_params++;
	    }
	  else
	    {
	      return 0;
	    }
	  strip_beginning_spaces(str_params);
	  return 1;
	}

      //got a parm, but there is more to get, lets make sure there is a comma there
      strip_beginning_spaces(str_params);

      if (str_params[0] == ',')
	{
	  str_params++;
	}
      else
	{
	  return 0;
	}
    }
  return 1;
}

/**
 * Are these 2 function signatures identical?
 */
static int signatures_eq_p(int* params1, int* params2)
{
  int i = 0;
  for (; i < 10; i++)
    if (params1[i] != params2[i])
      return 0;
  return 1;
}

/**
 * Process one line of DinkC and returns directive to the DinkC
 * interpreter.
 * 
 * Cf. doc/HACKING_dinkc.txt for understanding in progress ;)
 **/
enum dinkc_parser_state
process_line(int script, char *s, /*bool*/int doelse)
{
  char *h, *p;
  char* ev[3];
  
  memset(&ev, 0, sizeof(ev));

  if (rinfo[script]->level < 1)
    rinfo[script]->level = 1;

  h = s;
  if (h[0] == '\0')
    return 0;
  
  if ((h[0] == '/') && (h[1] == '/'))
    {
      //Msg("It was a comment!");
      goto bad;
    }

  /* Cut line */
  ev[0] = separate_string(h, 1, ' ');
  ev[1] = separate_string(h, 2, ' ');
  ev[2] = separate_string(h, 3, ' ');
  /* Prepare free on return */
#define PL_RETURN(intval) {free(ev[0]), free(ev[1]), free(ev[2]); return intval;}

  if (compare(ev[0], "VOID"))
    {
      if (rinfo[script]->proc_return != 0)
	{
	  run_script(rinfo[script]->proc_return);
	  kill_script(script);
	}
      PL_RETURN(DCPS_YIELD);
    }

  /* goto label? */
  if (ev[0][strlen(ev[0]) -1] == ':' && strlen(ev[1]) < 2)
    {
      if (dversion >= 108)
	{
	  /* Attempt to avoid considering:
			   say("bonus: 5 points", 1); // would not display any text at all!
			   as a label */
	  if (strncmp (ev[0], "say", 3) != 0)
	    {
	      PL_RETURN(0); //its a label
	    }
	}
      else
	{
	  PL_RETURN(0); //its a label
	}
    }

  /** Expression between parenthesis **/
  if (ev[0][0] == '(')
    {
      //this procedure has been passed a conditional statement finder
      //what kind of conditional statement is it?
      p = h;
      char* temp = separate_string(h, 2, ')');
      free(ev[0]);
      ev[0] = separate_string(h, 1, ')');

      // Msg("Ok, turned h %s to  ev1 %s.",h,ev[0]);
      p += strlen(ev[0]) + 1;

      strip_beginning_spaces(p);

      if (strchr(temp, '=') != NULL)
	{
	  h++;
	  strip_beginning_spaces(h);
	  process_line(script, h, /*false*/0);
	  replace_norealloc("==", "", temp);
	  char* expr = xmalloc(20 + 4 + strlen(temp) + 1);
	  sprintf(expr, "%d == %s", returnint, temp);
	  returnint = var_figure(expr, script);
	  strcpy(h, "\n");
	  free(expr);
	  free(temp);
	  PL_RETURN(0);
	}
      
      if (strchr(temp, '>') != NULL)
	{
	  h++;
	  strip_beginning_spaces(h);
	  process_line(script, h, /*false*/0);
	  replace_norealloc("==", "", temp);
	  char* expr = xmalloc(20 + 3 + strlen(temp) + 1);
	  sprintf(expr, "%d > %s", returnint, temp);
	  returnint = var_figure(expr, script);
	  strcpy(h, "\n");
	  free(expr);
	  free(temp);
	  PL_RETURN(0);
	}

      if (strchr(temp, '<') != NULL)
	{
	  h++;
	  strip_beginning_spaces(h);
	  process_line(script, h, /*false*/0);
	  replace_norealloc("==", "", temp);
	  char* expr = xmalloc(20 + 3 + strlen(temp) + 1);
	  sprintf(expr, "%d < %s", returnint, temp);
	  returnint = var_figure(expr, script);
	  strcpy(h, "\n");
	  free(expr);
	  free(temp);
	  PL_RETURN(0);
	}
      
      /* Beuc: This should be converted to a set of "if ... else
       * if... else if ..." and multi-character constants should be
       * removed. However, this may cause the interpreter to behave
       * differently, so be careful. */
      /* For now, I'll rewrite the code in an equivalent warning-free
       * inelegant way: strchr(str, 'ab') <=> strchr(str, 'b') */
      /* if (strchr (temp, '<=') != NULL) */
      if (strchr(temp, '=') != NULL)
	{
	  h++;
	  strip_beginning_spaces(h);
	  process_line(script, h, /*false*/0);
	  replace_norealloc("==", "", temp);
	  char* expr = xmalloc(20 + 4 + strlen(temp) + 1);
	  sprintf(expr, "%d <= %s", returnint, temp);
	  returnint = var_figure(expr, script);
	  strcpy(h, "\n");
	  free(expr);
	  free(temp);
	  PL_RETURN(0);
	}
      /* if (strchr (temp, '>=') != NULL) */
      if (strchr (temp, '=') != NULL)
	{
	  h++;
	  strip_beginning_spaces(h);
	  process_line(script, h, /*false*/0);
	  replace_norealloc("==", "", temp);
	  char* expr = xmalloc(20 + 4 + strlen(temp) + 1);
	  sprintf(expr, "%d >= %s", returnint, temp);
	  returnint = var_figure(expr, script);
	  strcpy(h, "\n");
	  free(expr);
	  free(temp);
	  PL_RETURN(0);
	}
      /* if (strchr (temp, '!=') != NULL) */
      if (strchr (temp, '=') != NULL)
	{
	  h++;
	  strip_beginning_spaces(h);
	  process_line(script, h, /*false*/0);
	  replace_norealloc("==", "", temp);
	  char* expr = xmalloc(20 + 4 + strlen(temp) + 1);
	  sprintf(expr, "%d != %s", returnint, temp);
	  returnint = var_figure(expr, script);
	  strcpy(h, "\n");
	  free(expr);
	  free(temp);
	  PL_RETURN(0);
	}
      free(temp);


      if (p[0] == ')')
	{
	  //its a procedure in the if statement!!!
	  h++;
	  p++;
	  char* line_copy = strdup(p);
	  process_line(script, h, /*false*/0);
	  log_debug("Returned %d for the returnint", returnint);
	  strcpy(s, line_copy); /* strlen(s) >= strlen(line_copy) */
	  free(line_copy);
	  h = s;
	  
	  PL_RETURN(0);
	}
      else
	{
	  h++;
	  
	  char* expr = separate_string(h, 1,')');
	  h += strlen(expr) + 1;
	  returnint = var_figure(expr, script);
	  free(expr);

	  strcpy_nooverlap(s, h);
	  
	  PL_RETURN(0);
	}
      
      strip_beginning_spaces(h);
      strip_beginning_spaces(ev[0]);

      s = h;
    } /* END expression between parenthesis */


  if (strchr(ev[0], '(') != NULL)
    {
      //Msg("Has a (, lets change it");
      free(ev[0]);
      ev[0] = separate_string(h, 1, '(');
      //Msg("Ok, first is now %s",ev[0]);
    }

  /** { Bloc } **/
  char first = ev[0][0];
  if (first == '{')
    {
      rinfo[script]->level++;
      //Msg("Went up level, now at %d.", rinfo[script]->level);
      h++;
      if (rinfo[script]->skipnext)
	{
	  /* Skip the whole { section } */
	  rinfo[script]->skipnext = /*false*/0;
	  rinfo[script]->onlevel = ( rinfo[script]->level - 1);
	}
      goto good;
    }
  
  if (first == '}')
    {
      rinfo[script]->level--;
      //Msg("Went down a level, now at %d.", rinfo[script]->level);
      h++;
      
      if (rinfo[script]->onlevel > 0 && rinfo[script]->level == rinfo[script]->onlevel)
	{
	  /* Finished skipping the { section }, preparing to run 'else' */
	  strip_beginning_spaces(h);
	  strcpy_nooverlap(s, h);
	  PL_RETURN(DCPS_DOELSE_ONCE);
	}
      goto good;
    }

  /* Fix if there are too many closing '}' */
  if (rinfo[script]->level < 0)
    {
      rinfo[script]->level = 0;
    }


  /* Note: that's the 2nd time we compare with "VOID" -
     cf. above. However ev[0] was modified in between, so this
     section may still be called if the first comparison didn't
     match. */
  if (compare(ev[0], "void"))
    {
      //     Msg("Next procedure starting, lets quit");
      strcpy_nooverlap(s, h);
      if (rinfo[script]->proc_return != 0)
	{
	  run_script(rinfo[script]->proc_return);
	  kill_script(script);
	}
      
      PL_RETURN(DCPS_YIELD);
    }

  
  /* Stop processing if we're skipping the current { section } */
  if (rinfo[script]->onlevel > 0 && rinfo[script]->level > rinfo[script]->onlevel)
    {
      PL_RETURN(0);
    }
    
  rinfo[script]->onlevel = 0;
    
  /* Skip the current line if the previous 'if' or 'else' said so */
  if (rinfo[script]->skipnext)
    {
      //sorry, can't do it, you were told to skip the next thing
      rinfo[script]->skipnext = /*false*/0;
      strcpy(s, "\n"); /* jump to next line */
      //PL_RETURN(3);
      PL_RETURN(DCPS_DOELSE_ONCE);
    }
    


  if (compare(ev[0], "void"))
    {
      log_error("[DinkC] Missing } in %s, offset %d.", rinfo[script]->name,rinfo[script]->current);
      strcpy_nooverlap(s, h);
      PL_RETURN(DCPS_YIELD);
    }
    
  /** if **/
  if (compare(ev[0], "if"))
    {
      h += strlen(ev[0]);
      strip_beginning_spaces(h);
	
      process_line(script, h, /*false*/0);
      // Result is 'returnint'
	
      if (returnint != 0)
	{
	  log_debug("If returned true");
	}
      else
	{
	  //don't do it!
	  rinfo[script]->skipnext = /*true*/1;
	  log_debug("If returned false, skipping next thing");
	}
	
      strcpy_nooverlap(s, h);
      //g("continuing to run line %s..", h);

      //PL_RETURN(5);
      PL_RETURN(DCPS_DOELSE_ONCE);
      /* state 5 should actually be state DCPS_CONTINUE, but keeping
	 it that way (e.g. with doelse=1 for the next line) for
	 compatibility, just in case somebody abused it */
    }

  if (compare(ev[0], "else"))
    {
      //Msg("Found else!");
      h += strlen(ev[0]);
		
      if (doelse)
	{
	  // Yes to else
	}
      else
	{
	  // No to else...
	  // they shouldn't run the next thing
	  rinfo[script]->skipnext = /*true*/1;
	}
      strcpy_nooverlap(s, h);
      PL_RETURN(1);
    }
    
  /** Dialog **/
  if (compare(ev[0], "choice_start"))
    {
      kill_text_owned_by(1);
      if (talk_get(script))
	{
	  // Question(s) gathered successfully
	  PL_RETURN(DCPS_YIELD);
	}
      PL_RETURN(0);
    }

  /** Jump **/
  if (compare(ev[0], "goto"))
    {
      locate_goto(ev[1], script);
      PL_RETURN(0);
    }

  /** Definition **/
  if (compare(ev[0], "int"))
    {
      int_prepare(h, script);
      h += strlen(ev[0]);

      if (strchr(h, '=') != NULL)
	{
	  strip_beginning_spaces(h);
	  //Msg("Found =...continuing equation");
	  strcpy_nooverlap(s, h);
	  PL_RETURN(DCPS_CONTINUE);
	}
      else
	{
	  PL_RETURN(DCPS_GOTO_NEXTLINE);
	}
    }

  /** "return;" and "return something;" **/
  if (compare(ev[0], "return;"))
    {
      log_debug("Found return; statement");
	
      if (rinfo[script]->proc_return != 0)
	{
	  bKeepReturnInt = 1; /* v1.08 */
	  run_script(rinfo[script]->proc_return);
	  kill_script(script);
	}
	
      PL_RETURN(DCPS_YIELD);
    }

  if (dversion >= 108)
    {
      /* "return", without trailing ';' */
      /* added so we can have return values and crap. */
      /* see also "return;" above */
      if (compare (ev[0], "return"))
	{
	  log_debug("Found return; statement");
	  h += strlen(ev[0]);
	  strip_beginning_spaces (h);
	  process_line (script, h, 0);
	  if (rinfo[script]->proc_return != 0)
	    {
	      bKeepReturnInt = 1;
	      run_script (rinfo[script]->proc_return);
	      kill_script (script);
	    }
	  PL_RETURN(DCPS_YIELD);
	}
    }

  /********************/
  /*  DinkC bindings  */
  /*                  */
  /********************/
    
  /** Lookup bindings **/
  char* funcname = ev[0];
  char* str_args = h + strlen(ev[0]);
  struct binding* pbd = NULL;
  pbd = dinkc_bindings_lookup(bindings, funcname);
    
  if (pbd != NULL)
    {
      /* Common arguments */
      int* yield = alloca(sizeof(int)*1);
      yield[0] = 0; /* don't yield by default) */

      /* Specific arguments */
      int* params = pbd->params;
      if (params[0] != -1) /* no args == no checks*/
	{
	  if (!get_parms(funcname, script, str_args, params))
	    {
	      /* Invalid parameters in the DinkC script - output an
		 error message */
	      int i = 0;
	      while (params[i] != 0 && i < 10)
		i++;
	      log_error("[DinkC] %s:%d: procedure '%s' takes %d parameters",
			rinfo[script]->name, rinfo[script]->debug_line,
			funcname, i);

	      /* Set 'returnint' if necessary */
	      if (pbd->badparams_returnint_p == 1)
		returnint = pbd->badparams_returnint;
	      /* Fallback parser state */
	      PL_RETURN(pbd->badparams_dcps);
	    }
	}
	
      /* Call C function */
      cur_funcname = pbd->funcname; /* for error messages */
      int sig_void[10]        = {-1,0,0,0,0,0,0,0,0,0};
      int sig_int[10]         =  {1,0,0,0,0,0,0,0,0,0};
      int sig_str[10]         =  {2,0,0,0,0,0,0,0,0,0};
      int sig_int_int[10]     =  {1,1,0,0,0,0,0,0,0,0};
      int sig_int_str[10]     =  {1,2,0,0,0,0,0,0,0,0};
      int sig_str_int[10]     =  {2,1,0,0,0,0,0,0,0,0};
      int sig_str_str[10]     =  {2,2,0,0,0,0,0,0,0,0};
      int sig_int_int_int[10] =  {1,1,1,0,0,0,0,0,0,0};
      int sig_str_int_int[10] =  {2,1,1,0,0,0,0,0,0,0};
      int sig_int_int_int_int[10] =  {1,1,1,1,0,0,0,0,0,0};
      int sig_int_int_int_int_int[10] =  {1,1,1,1,1,0,0,0,0,0};
      int sig_int_int_int_int_int_int[10] =  {1,1,1,1,1,1,0,0,0,0};

      /* {-1,0,0,0,0,0,0,0,0,0} */
      if (signatures_eq_p(pbd->params, sig_void))
	{
	  void (*pf)(int, int*, int*) = pbd->func;
	  (*pf)(script, yield, &returnint);
	}
      /* {1,0,0,0,0,0,0,0,0,0} */
      else if (signatures_eq_p(pbd->params, sig_int))
	{
	  void (*pf)(int, int*, int* , int) = pbd->func;
	  (*pf)(script, yield, &returnint , nlist[0]);
	}
      /* {2,0,0,0,0,0,0,0,0,0} */
      else if (signatures_eq_p(pbd->params, sig_str))
	{
	  void (*pf)(int, int*, int* , char*) = pbd->func;
	  (*pf)(script, yield, &returnint , slist[0]);
	}
      /* {1,1,0,0,0,0,0,0,0,0} */
      else if (signatures_eq_p(pbd->params, sig_int_int))
	{
	  void (*pf)(int, int*, int* , int, int) = pbd->func;
	  (*pf)(script, yield, &returnint , nlist[0], nlist[1]);
	}
      /* {1,2,0,0,0,0,0,0,0,0} */
      else if (signatures_eq_p(pbd->params, sig_int_str))
	{
	  void (*pf)(int, int*, int* , int, char*) = pbd->func;
	  (*pf)(script, yield, &returnint , nlist[0], slist[1]);
	}
      /* {2,1,0,0,0,0,0,0,0,0} */
      else if (signatures_eq_p(pbd->params, sig_str_int))
	{
	  void (*pf)(int, int*, int* , char*, int) = pbd->func;
	  (*pf)(script, yield, &returnint , slist[0], nlist[1]);
	}
      /* {2,2,0,0,0,0,0,0,0,0} */
      else if (signatures_eq_p(pbd->params, sig_str_str))
	{
	  void (*pf)(int, int*, int* , char*, int) = pbd->func;
	  (*pf)(script, yield, &returnint , slist[0], nlist[1]);
	}
      /* {1,1,1,0,0,0,0,0,0,0} */
      else if (signatures_eq_p(pbd->params, sig_int_int_int))
	{
	  void (*pf)(int, int*, int* , int, int, int) = pbd->func;
	  (*pf)(script, yield, &returnint , nlist[0], nlist[1], nlist[2]);
	}
      /* {2,2,1,0,0,0,0,0,0,0} */
      else if (signatures_eq_p(pbd->params, sig_str_int_int))
	{
	  void (*pf)(int, int*, int* , char*, int, int) = pbd->func;
	  (*pf)(script, yield, &returnint , slist[0], nlist[1], nlist[2]);
	}
      /* {1,1,1,1,0,0,0,0,0,0} */
      else if (signatures_eq_p(pbd->params, sig_int_int_int_int))
	{
	  void (*pf)(int, int*, int* , int, int, int, int) = pbd->func;
	  (*pf)(script, yield, &returnint , nlist[0], nlist[1], nlist[2], nlist[3]);
	}
      /* {1,1,1,1,1,0,0,0,0,0} */
      else if (signatures_eq_p(pbd->params, sig_int_int_int_int_int))
	{
	  void (*pf)(int, int*, int* , int, int, int, int, int) = pbd->func;
	  (*pf)(script, yield, &returnint , nlist[0], nlist[1], nlist[2], nlist[3], nlist[4]);
	}
      /* {1,1,1,1,1,1,0,0,0,0} */
      else if (signatures_eq_p(pbd->params, sig_int_int_int_int_int_int))
	{
	  void (*pf)(int, int*, int* , int, int, int, int, int, int) = pbd->func;
	  (*pf)(script, yield, &returnint , nlist[0], nlist[1], nlist[2], nlist[3], nlist[4], nlist[5]);
	}
      else
	{
	  log_fatal("Internal error: DinkC function %s has unknown signature",
		    pbd->funcname);
	  exit(EXIT_FAILURE);
	}
      cur_funcname = "";
      /* the function can manipulation returnint through argument #3 */
	
      if (*yield == 0)
	{
	  PL_RETURN(DCPS_GOTO_NEXTLINE);
	}
      else if (*yield == 1)
	{
	  PL_RETURN(DCPS_YIELD);
	}
      else
	{
	  log_fatal("Internal error: DinkC function %s requested invalid state %d",
		    pbd->funcname, *yield);
	  exit(EXIT_FAILURE);
	}
    }
    
  
  /***************/
  /** Operators **/
  /**           **/
  /***************/

  /* Beware: this works on ev[1], not ev[0]; position in the code is
     critical! */

  if (compare(ev[1], "="))
    {
      h += strlen(ev[0]);
      strip_beginning_spaces(h);
      h++;
      strip_beginning_spaces(h);
      var_equals(ev[0], ev[2], '=', script, h);
      strcpy_nooverlap(s, h);
      PL_RETURN(0);
    }
    
  if (compare(ev[1], "+="))
    {
      h += strlen(ev[0]);
      strip_beginning_spaces(h);
      h += 2;
      strip_beginning_spaces(h);
      var_equals(ev[0], ev[2], '+', script, h);
      strcpy_nooverlap(s, h);
      PL_RETURN(0);
    }
    
  if (compare(ev[1], "*="))
    {
      h += strlen(ev[0]);
      strip_beginning_spaces(h);
      h += 2;
      strip_beginning_spaces(h);
      var_equals(ev[0], ev[2], '*', script, h);
      strcpy_nooverlap(s, h);
      PL_RETURN(0);
    }
    
  if (compare(ev[1], "-="))
    {
      h += strlen(ev[0]);
      strip_beginning_spaces(h);
      h += 2;
      strip_beginning_spaces(h);
	
      var_equals(ev[0], ev[2], '-', script, h);
	
      strcpy_nooverlap(s, h);
      PL_RETURN(0);
    }
    
  if (compare(ev[1], "/")
      || (dversion >= 108 && compare(ev[1], "/=")))
    {
      h += strlen(ev[0]);
      strip_beginning_spaces(h);
      h++;
      strip_beginning_spaces(h);
	
      var_equals(ev[0], ev[2], '/', script, h);
	
      strcpy_nooverlap(s, h);
      PL_RETURN(0);
    }
    
  if (compare(ev[1], "*"))
    {
      h += strlen(ev[0]);
      strip_beginning_spaces(h);
      h++;
      strip_beginning_spaces(h);
	
      var_equals(ev[0], ev[2], '*', script, h);
	
      strcpy_nooverlap(s, h);
      PL_RETURN(0);
    }
    
    
  /***************************************/
  /** New DinkC user-defined procedures **/
  /**                                   **/
  /***************************************/
  if (dversion >= 108)
    {
      if (compare (ev[0], "external"))
	{
	  h += strlen(ev[0]);
	  int p[20] = { 2, 2, 1, 1, 1, 1, 1, 1, 1, 1 };
	  {
	    int i = 0;
	    for (; i < 10; i++)
	      slist[i][0] = '\0';
	  }
	  get_parms(ev[0], script, h, p);
	  if (strlen(slist[0]) > 0 && strlen(slist[1]) > 0)
	    {
	      int myscript1 = load_script(slist[0], rinfo[script]->sprite, 0);
	      if (myscript1 == 0)
		{
		  log_error("[DinkC] external: Couldn't find %s.c (for procedure %s)",
		       slist[0], slist[1]);
		  PL_RETURN(0);
		}
	      rinfo[myscript1]->arg1 = nlist[2];
	      rinfo[myscript1]->arg2 = nlist[3];
	      rinfo[myscript1]->arg3 = nlist[4];
	      rinfo[myscript1]->arg4 = nlist[5];
	      rinfo[myscript1]->arg5 = nlist[6];
	      rinfo[myscript1]->arg6 = nlist[7];
	      rinfo[myscript1]->arg7 = nlist[8];
	      rinfo[myscript1]->arg8 = nlist[9];
	      if (locate (myscript1, slist[1]))
		{
		  rinfo[myscript1]->proc_return = script;
		  run_script (myscript1);
		  PL_RETURN(DCPS_YIELD);
		}
	      else
		{
		  log_error("[DinkC] external: Couldn't find procedure %s in %s.",
			    slist[1], slist[0]);
		  kill_script (myscript1);
		}
	    }
	  strcpy (s, h);
	  PL_RETURN(0);
	}

      if (strchr (h, '(') != NULL)
	{
	  //lets attempt to run a procedure
	  int myscript = load_script (rinfo[script]->name, rinfo[script]->sprite, 0);
	  h += strlen(ev[0]);
	  int p[20] = { 1, 1, 1, 1, 1, 1, 1, 1, 1, 1 };
	  get_parms(ev[0], script, h, p);
	  if (locate(myscript, ev[0]))
	    {
	      /* Custom procedure in the current script */
	      rinfo[myscript]->arg1 = nlist[0];
	      rinfo[myscript]->arg2 = nlist[1];
	      rinfo[myscript]->arg3 = nlist[2];
	      rinfo[myscript]->arg4 = nlist[3];
	      rinfo[myscript]->arg5 = nlist[4];
	      rinfo[myscript]->arg6 = nlist[5];
	      rinfo[myscript]->arg7 = nlist[6];
	      rinfo[myscript]->arg8 = nlist[7];
	      rinfo[myscript]->arg9 = nlist[8];
	      rinfo[myscript]->proc_return = script;
	      run_script(myscript);
	      PL_RETURN(DCPS_YIELD);
	    }
	  else
	    {
	      /* Try custom global procedure */
	      int i = 0;
	      for (; i < 100; i++)
		{
		  /* Skip empty slots */
		  if (strlen (play.func[i].func) == 0)
		    continue;
		    
		  if (compare(play.func[i].func, ev[0]))
		    {
		      myscript = load_script(play.func[i].file, rinfo[script]->sprite, 0);
		      rinfo[myscript]->arg1 = nlist[0];
		      rinfo[myscript]->arg2 = nlist[1];
		      rinfo[myscript]->arg3 = nlist[2];
		      rinfo[myscript]->arg4 = nlist[3];
		      rinfo[myscript]->arg5 = nlist[4];
		      rinfo[myscript]->arg6 = nlist[5];
		      rinfo[myscript]->arg7 = nlist[6];
		      rinfo[myscript]->arg8 = nlist[7];
		      rinfo[myscript]->arg9 = nlist[8];
		      if (locate(myscript, ev[0]))
			{
			  rinfo[myscript]->proc_return = script;
			  run_script (myscript);
			  PL_RETURN(DCPS_YIELD);
			}
		      break;
		    }
		}
	      log_error("[DinkC] Procedure void %s( void ); not found in script %s. (word 2 was %s)",
			ev[0], ev[1], rinfo[myscript] != NULL ? rinfo[myscript]->name : "");
	      kill_script (myscript);
	    }
	    
	  /*seperate_string(h, 1,'(',line);
	      
	    int myscript = load_script(rinfo[script]->name, rinfo[script]->sprite, false);
	      
	    if (locate( myscript, line))
	    {
	    rinfo[myscript]->proc_return = script;
	    run_script(myscript);    
	    PL_RETURN(DCPS_YIELD);
	    } else
	    {
	    Msg("ERROR:  Procedure void %s( void ); not found in script %s. (word 2 was %s) ", line,
	    ev[1], rinfo[myscript]->name); 
	    kill_script(myscript);          
	    } */
	  PL_RETURN(0);
	}
    }
  else
    {
      /* v1.07 function that are implemented differently than in v1.08 */
      if (compare(ev[0], "external"))
	{
	  h += strlen(ev[0]);
	  int p[20] = {2,2,0,0,0,0,0,0,0,0};
	  if (get_parms(ev[0], script, h, p))
	    {
	      int myscript1 = load_script(slist[0], rinfo[script]->sprite, /*false*/0);
	      if (myscript1 == 0)
		{
		  log_error("[DinkC] external: Couldn't find %s.c (for procedure %s)", slist[0], slist[1]);
		  PL_RETURN(0);
		}
	      if (locate(myscript1, slist[1]))
		{
		  rinfo[myscript1]->proc_return = script;
		  run_script(myscript1);
		  PL_RETURN(DCPS_YIELD);
		}
	      else
		{
		  log_error("[DinkC] external: Couldn't find procedure %s in %s.", slist[1], slist[0]);
		  kill_script(myscript1);
		}
	    }
	  else
	    {
	      log_error("[DinkC] %s: procedure 'external' takes 2 parameters (offset %d)",
			rinfo[script]->name, rinfo[script]->current);
	    }
	  strcpy_nooverlap(s, h);
	  PL_RETURN(0);
	}

      if (strchr(h, '(') != NULL)
	{
	  //lets attempt to run a procedure
	  char* proc = separate_string(h, 1, '(');
	  int myscript = load_script(rinfo[script]->name, rinfo[script]->sprite, /*false*/0);

	  if (locate(myscript, proc))
	    {
	      rinfo[myscript]->proc_return = script;
	      run_script(myscript);
	      free(proc);
	      PL_RETURN(DCPS_YIELD);
	    }
	  else
	    {
	      log_error("[DinkC] Procedure void %s( void ); not found in script %s. (word 2 was %s) ",
			proc, ev[1], rinfo[myscript]->name);
	      kill_script(myscript);
	    }
	  free(proc);
	  PL_RETURN(0);
	}
	
      log_error("[DinkC] \"%s\" unknown in %s, offset %d.",
		ev[0], rinfo[script]->name,rinfo[script]->current);
      //in a thingie, ready to go
    }

bad:
  strcpy(s, "\n"); /* jump to next line */
  //PL_RETURN(0);
  PL_RETURN(DCPS_CONTINUE);
  
 good:
  strcpy_nooverlap(s, h);
  //s = h
  //Msg("ok, continuing with running %s..",s);
  PL_RETURN(DCPS_CONTINUE);
}
