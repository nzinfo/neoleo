/*
 * $Id: regions.c,v 1.16 2000/08/10 21:02:51 danny Exp $
 *
 * Copyright � 1990, 1992, 1993 Free Software Foundation, Inc.
 *
 * This file is part of Oleo, the GNU Spreadsheet.
 *
 * Oleo is free software; you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation; either version 2, or (at your option)
 * any later version.
 *
 * Oleo is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with Oleo; see the file COPYING.  If not, write to
 * the Free Software Foundation, 675 Mass Ave, Cambridge, MA 02139, USA.
 */


#include <iostream>
#include <string>
#include <stdlib.h>

#ifdef HAVE_CONFIG_H
#include "config.h"
#endif

#ifdef	WITH_DMALLOC
#include <dmalloc.h>
#endif

#include <string.h>
#include "funcdef.h"
#include <stdio.h>
#include "sysdef.h"
#include "global.h"
#include "cell.h"
#include "io-generic.h"
#include "io-abstract.h"
#include "io-utils.h"
#include "lists.h"
#include "ref.h"
#include "regions.h"
#include "io-term.h"
#include "window.h"
#include "cmd.h"
#include "sort.h"
#include "basic.h"

using std::cout;
using std::endl;

/* Apparently "all_rng" is read-only, therefore need not be put in Global. */
struct rng all_rng = {MIN_ROW, MIN_COL, MAX_ROW, MAX_COL};

/* Take a struct rng (R) and init its elements to R1 C1 R2 C2, making sure
   they are put in in the right order.
 */
void
set_rng (struct rng *r, CELLREF r1, CELLREF c1, CELLREF r2, CELLREF c2)
{
  if (r1 <= r2)
    {
      r->lr = r1;
      r->hr = r2;
    }
  else
    {
      r->lr = r2;
      r->hr = r1;
    }
  if (c1 <= c2)
    {
      r->lc = c1;
      r->hc = c2;
    }
  else
    {
      r->lc = c2;
      r->hc = c1;
    }
}

/* Flush all the cells in a region */
void
delete_region (struct rng *where)
{
  CELLREF rr, cc;
  CELL *pp;

  Global->modified = 1;

  find_cells_in_range (where);
  while ((pp = next_row_col_in_range (&rr, &cc)))
    {
      if (!pp->get_cell_formula() && !GET_TYP (pp))
	{
	  bzero(&(pp->cell_flags), sizeof(pp->cell_flags));
	  //pp->cell_font = 0;
	  continue;
	}
      cur_row = rr;
      cur_col = cc;
      my_cell = pp;
      flush_old_value ();
      pp->set_cell_formula(0);
      bzero(&(pp->cell_flags), sizeof(pp->cell_flags));
      //pp->cell_font = 0;
      push_refs (pp->cell_refs_from);
      io_pr_cell (rr, cc, pp);
    }
  my_cell = 0;
}

/* Turn on/off the locked bits in a region */
void
lock_region (struct rng *where, int locked)
{
  CELL *cp;

  Global->modified = 1;
  make_cells_in_range (where);
  while ((cp = next_cell_in_range ()))
    SET_LCK (cp, locked);
}

void
format_region (struct rng *where, int fmt, int just)
{
  CELL *cp;
  CELLREF rr, cc;

  Global->modified = 1;
  make_cells_in_range (where);
  while ((cp = next_row_col_in_range (&rr, &cc)))
    {
      if (fmt != -1) {
	SET_FORMAT (cp, fmt);	/* Only the format, not the precision !! */
      }
      if (just != -1)
	SET_JST (cp, just);
      io_pr_cell (rr, cc, cp);
    }
}

void
precision_region (struct rng *where, int precision)
{
  CELL *cp;
  CELLREF rr, cc;

  Global->modified = 1;
  make_cells_in_range (where);
  while ((cp = next_row_col_in_range (&rr, &cc)))
    {
      if (precision != -1)
	SET_PRECISION (cp, precision);
      io_pr_cell (rr, cc, cp);
    }
}

unsigned int print_width;



/*
   Set up regions for the move/copy functions.  This deals with default
   sizing of the target region, regions that don't fit, etc.

   This returns
	-1 if the regions overlap
	0 if there is a *real* error
	1 if the target is a multiple of the source
	2 if everything is OK.
 */

static int
set_to_region (struct rng *fm, struct rng *to)
{
  /* Delta {row,col} {from,to} */
  int drf, dcf;
  int drt, dct;
  int ret = 2;

  drf = fm->hr - fm->lr;
  drt = to->hr - to->lr;
  if (drt == 0)
    {
      if (to->lr > MAX_ROW - drf)
	{
	  io_error_msg ("The range won't fit this far down!");
	  return 0;
	}
      to->hr = to->lr + drf;
    }
  else if (drf != drt)
    {
      if ((drt + 1) % (drf + 1) == 0)
	ret = 1;
      else
	{
	  io_error_msg ("Rows %u:%u and %u:%u don't fit", fm->lr, fm->hr, to->lr, to->hr);
	  return 0;
	}
    }
  dcf = fm->hc - fm->lc;
  dct = to->hc - to->lc;
  if (dct == 0)
    {
      if (to->lc > MAX_COL - dcf)
	{
	  io_error_msg ("The range won't fit this far over!");
	  return 0;
	}
      to->hc = to->lc + dcf;
    }
  else if (dcf != dct)
    {
      if ((dct + 1) % (dcf + 1) == 0)
	ret = 1;
      else
	{
	  io_error_msg ("Cols %u:%u and %u:%u don't fit", fm->lc, fm->hc, to->lc, to->hc);
	  return 0;
	}
    }

  if (fm->lr == to->lr && fm->lc == to->lc)
    {
      io_error_msg ("Regions are in the same place");
      return 0;
    }

  if (((fm->lr <= to->lr && to->lr <= fm->hr) || (fm->lr <= to->hr && to->hr <= fm->hr))
      && ((fm->lc <= to->lc && to->lc <= fm->hc) || (fm->lc <= to->hc && to->hc <= fm->hc)))
    return -1;
  Global->modified = 1;
  return ret;
}

/* This is only complicated because it must deal with overlap, and it wants
   to be smart about copying empty space. . .
 */
void
move_region (struct rng *fm, struct rng *to)
{
  /* Delta {row,col} */
  int dr, dc;
  int nr, nc;
  int ov, dn;
  struct rng del_to_1, del_to_2;
  int do_2, dirs[2];
  int maxr, maxc;
  CELLREF cmax, rmax;
  int cdmax, rdmax;
  int must_repaint = 0;		/* If this move changes cell widths/heights */

  switch (set_to_region (fm, to))
    {
    case 0:
      return;

    case 1:
      io_error_msg ("Can't move source to multiple targets");
      return;

    case 2:
      del_to_1 = *to;

      do_2 = 0;
      dirs[0] = 1;
      dirs[1] = 1;

      /* del_fm_1= *fm; */
      break;

    default:
      /* They overlap.  There are eight ways that
		   they can overlap.  */
      if (to->lc == fm->lc && to->lr < fm->lr)
	{
	  /* State 1:  'from' on bottom */
	  del_to_1.lr = to->lr;
	  del_to_1.lc = to->lc;
	  del_to_1.hr = fm->lr - 1;
	  del_to_1.hc = to->hc;

	  do_2 = 0;
	  dirs[0] = 1;
	  dirs[1] = 1;

	  /* del_fm_1.lr=to->hr+1;	del_fm_1.lc=fm->lc;
			del_fm_1.hr=fm->hr;	del_fm_1.hc=fm->hc; */
	}
      else if (to->lc == fm->lc)
	{
	  /* State 2: 'from' on top */
	  del_to_1.lr = fm->hr + 1;
	  del_to_1.lc = to->lc;
	  del_to_1.hr = to->hr;
	  del_to_1.hc = to->hc;

	  do_2 = 0;
	  dirs[0] = -1;
	  dirs[1] = 1;

	  /* del_fm_1.lr=fm->lr;	del_fm_1.lc=fm->lc;
			del_fm_1.hr=to->lr-1;	del_fm_1.hc=fm->hc; */
	}
      else if (to->lr == fm->lr && to->lc < fm->lc)
	{
	  /* State 3: 'from' on right */
	  del_to_1.lr = to->lr;
	  del_to_1.lc = to->lc;
	  del_to_1.hr = to->hr;
	  del_to_1.hc = fm->lc - 1;

	  do_2 = 0;
	  dirs[0] = 1;
	  dirs[1] = 1;

	  /* del_fm_1.lr=fm->lr;	del_fm_1.lc=to->hc+1;
			del_fm_1.hr=fm->hr;	del_fm_1.hc=fm->hc; */
	}
      else if (to->lr == fm->lr)
	{
	  /* State 4: 'from' on left */
	  del_to_1.lr = to->lr;
	  del_to_1.lc = fm->hc + 1;
	  del_to_1.hr = to->hr;
	  del_to_1.hc = to->hc;

	  do_2 = 0;
	  dirs[0] = 1;
	  dirs[1] = -1;

	  /* del_fm_1.lr=fm->lr;	del_fm_1.lc=fm->lc;
			del_fm_1.hr=fm->hr;	del_fm_1.hc=to->lc-1; */
	}
      else if (fm->lr < to->lr && fm->lc < to->lc)
	{
	  /* State 5: From on topleft */

	  del_to_1.lr = to->lr;
	  del_to_1.lc = fm->hc + 1;
	  del_to_1.hr = fm->hr;
	  del_to_1.hc = to->hc;

	  del_to_2.lr = fm->hr + 1;
	  del_to_2.lc = to->lc;
	  del_to_2.hr = to->hr;
	  del_to_2.hc = to->hc;

	  do_2 = 1;
	  dirs[0] = -1;
	  dirs[1] = -1;

	  /* del_fm_1.lr=fm->lr;	del_fm_1.lc=fm->lc;
			del_fm_1.hr=to->lr-1;	del_fm_1.hc=fm->hc;

			del_fm_2.lr=to->lr;	del_fm_2.lc=fm->lc;
			del_fm_2.hr=fm->hr;	del_fm_2.hc=to->lc-1; */
	}
      else if (fm->lr < to->lr)
	{
	  /* State 6: 'from' on topright */
	  del_to_1.lr = to->lr;
	  del_to_1.lc = to->lc;
	  del_to_1.hr = fm->hr;
	  del_to_1.hc = fm->lc - 1;

	  del_to_2.lr = fm->hr + 1;
	  del_to_2.lc = to->lc;
	  del_to_2.hr = to->hr;
	  del_to_2.hc = to->hc;

	  do_2 = 1;
	  dirs[0] = -1;
	  dirs[1] = 1;

	  /* del_fm_1.lr=fm->lr;	del_fm_1.lc=fm->lc;
			del_fm_1.hr=to->lr-1;	del_fm_1.hc=fm->hc;

			del_fm_2.lr=to->lr;	del_fm_2.lc=to->hc+1;
			del_fm_2.hr=fm->hr;	del_fm_2.hc=fm->hc; */
	}
      else if (fm->lc < to->lc)
	{
	  /* State 7: 'from on bottomleft */
	  del_to_1.lr = to->lr;
	  del_to_1.lc = to->lc;
	  del_to_1.hr = fm->lr - 1;
	  del_to_1.hc = to->hc;

	  del_to_2.lr = fm->lr;
	  del_to_2.lc = fm->hc;
	  del_to_2.hr = to->hr;
	  del_to_2.hc = to->hc;

	  do_2 = 1;
	  dirs[0] = 1;
	  dirs[1] = -1;

	  /* del_fm_1.lr=fm->lr;	del_fm_1.lc=fm->lc;
			del_fm_1.hr=to->hr;	del_fm_1.hc=to->lc-1;

			del_fm_2.lr=to->hr+1;	del_fm_2.lc=fm->lc;
			del_fm_2.hr=to->hr+1;	del_fm_2.hc=to->lc-1; */
	}
      else
	{
	  /* State 8: 'from' on bottomright */
	  del_to_1.lr = to->lr;
	  del_to_1.lc = to->lc;
	  del_to_1.hr = fm->lr - 1;
	  del_to_1.hc = to->hc;

	  del_to_2.lr = fm->lr;
	  del_to_2.lc = to->lc;
	  del_to_2.hr = to->hr;
	  del_to_2.hc = fm->lc - 1;

	  do_2 = 1;
	  dirs[0] = 1;
	  dirs[1] = 1;

	  /* del_fm_1.lr=fm->lr;	del_fm_1.lc=to->hc+1;
			del_fm_1.hr=to->hr;	del_fm_1.hc=fm->hc;

			del_fm_2.lr=to->hr+1;	del_fm_2.lc=fm->lc;
			del_fm_2.hr=fm->hr;	del_fm_2.hc=fm->hc; */
	}
    }
  dn = to->hr - fm->hr;
  ov = to->hc - fm->hc;

  dr = fm->hr - fm->lr;
  dc = fm->hc - fm->lc;

  delete_region (&del_to_1);
  if (do_2)
    delete_region (&del_to_2);

  if (to->lr == MIN_ROW && to->hr == MAX_ROW)
    {
      shift_widths (ov, fm->lc, fm->hc);
      must_repaint = 1;
    }

  if (to->lc == MIN_COL && to->hc == MAX_COL)
    {
      shift_heights (dn, fm->lr, fm->hr);
      must_repaint = 1;
    }

  shift_outside (fm, dn, ov);

  rmax = highest_row ();
  if (rmax < fm->lr)
    rdmax = -1;
  else if (rmax > fm->hr)
    rdmax = dr;
  else
    rdmax = rmax - fm->lr;
  nr = (dirs[0] > 0) ? 0 : rdmax;
  maxr = (dirs[0] > 0) ? rdmax + 1 : -1;
  for (; nr != maxr; nr += dirs[0])
    {
      cmax = max_col (fm->lr + nr);
      if (cmax < fm->lc)
	cdmax = -1;
      else if (cmax > fm->hc)
	cdmax = dc;
      else
	{
	  cdmax = cmax - fm->lc;
	}
      nc = (dirs[1] > 0) ? 0 : cdmax;
      maxc = (dirs[1] > 0) ? cdmax + 1 : -1;
      for (; nc != maxc; nc += dirs[1])
	{
	  CELLREF rf, cf, rt, ct;
	  CELL *cpf;

	  rf = fm->lr + nr;
	  cf = fm->lc + nc;
	  rt = to->lr + nr;
	  ct = to->lc + nc;

	  cpf = find_cell (rf, cf);
	  cur_row = rt;
	  cur_col = ct;
	  my_cell = find_cell (cur_row, cur_col);
	  if ((!cpf || (
					  //!cpf->cell_font &&
		((cpf->cell_flags.cell_format == 0)
		&& (cpf->cell_flags.cell_precision == 0)
		&& (cpf->cell_flags.cell_justify == 0)
		&& (cpf->get_cell_type() == 0))
		&& (cpf->cell_flags.cell_lock == 0)
		&& !cpf->get_cell_formula()))
	      && !my_cell)
	    continue;

	  if (!cpf)
	    {
	      bzero(&(my_cell->cell_flags), sizeof(my_cell->cell_flags));
	      //my_cell->cell_font = 0;
	      my_cell->cell_refs_to = 0;
	      my_cell->set_cell_formula(0);
	      my_cell->cell_cycle = 0;
	      my_cell = 0;
	      continue;
	    }
	  if (!my_cell)
	    {
	      my_cell = find_or_make_cell (cur_row, cur_col);
	      cpf = find_cell (rf, cf);
	    }
	  else
	    flush_old_value ();

	  my_cell->cell_flags = cpf->cell_flags;
	  //my_cell->cell_font = cpf->cell_font;
	  my_cell->cell_refs_to = cpf->cell_refs_to;
	  my_cell->set_cell_formula(cpf->get_cell_formula());
	  my_cell->cell_cycle = cpf->cell_cycle;
	  my_cell->set_c_z(cpf->get_c_z());

	  bzero(&(cpf->cell_flags), sizeof(cpf->cell_flags));
	  //cpf->cell_font = 0;
	  cpf->cell_refs_to = 0;
	  cpf->set_cell_formula(0);
	  cpf->cell_cycle = 0;

	  push_cell (cur_row, cur_col);

 	  if (!must_repaint)
	    {
	      if (cpf)
		io_pr_cell (rf, cf, cpf);
	      if (my_cell)
		io_pr_cell (rt, ct, my_cell);
	    }
	  my_cell = 0;
	}
    }
  if (must_repaint)
    io_repaint ();
/* Perpetration of an interface change here.  If we really
 * need to get back to the old region, we can do it by
 * wrapping the move-region command in macros that set
 * and return to a hardy mark.  We might, however, want
 * to jump the moved text again, or reformat it in some
 * way, so the mark should travel with us.
 * 
 * to->lr and to->lc give the lowest column and row (northwest
 * corner) in the destination region, to->hr and to->hc five the
 * highest, or southeast corner.  The absolute value if their
 * difference tells us how far to move over and down from
 * northwest to mark the region just moved.  This way the new
 * region can be operated on as a chunk immediately
 *
 * --FB 1997.12.17
 */
  if (mkrow != NON_ROW) {
      mkrow = to->lr + abs(to->lr - to->hr);
      mkcol = to->lc + abs(to->lc - to->hc);
  }
  goto_region (to);
  return;
}

void
copy_region (struct rng *fm, struct rng *to)
{
  CELLREF rf, rt, cf, ct;

  if (set_to_region (fm, to) < 1)
    return;

  for (rf = fm->lr, rt = to->lr; (rt > 0) && (rt <= to->hr); rt++, rf++)
    {
      for (cf = fm->lc, ct = to->lc; (ct > 0) && (ct <= to->hc); ct++, cf++)
	{
	  copy_cell (rf, cf, rt, ct);

	  if (cf == fm->hc)
	    cf = fm->lc - 1;
	}
      if (rf == fm->hr)
	rf = fm->lr - 1;
    }
  if (mkrow != NON_ROW) {
      mkrow = to->lr + abs(to->lr - to->hr);
      mkcol = to->lc + abs(to->lc - to->hc);
  }
  goto_region (to);
}

void
copy_values_region (struct rng *fm, struct rng *to)
{
	CELLREF rf, rt, cf, ct;
	union vals dummy;
	CELL *cpf;
	using VALPTR=union vals *;

	if (set_to_region (fm, to) < 1)
		return;

	for (rf = fm->lr, rt = to->lr; rt <= to->hr; rt++, rf++)
	{
		for (cf = fm->lc, ct = to->lc; ct <= to->hc; ct++, cf++)
		{
			ValType new_type  = cpf ?  GET_TYP (cpf) : TYP_NUL;
			//VALPTR v = &dummy;
			cpf = find_cell (rf, cf);
			if(cpf) 
				dummy = cpf->get_c_z();
				//v = &(cpf->get_c_z());
			//}
			set_new_value (rt, ct, new_type, &dummy);

			if (cf == fm->hc)
				cf = fm->lc - 1;
		}
		if (rf == fm->hr)
			rf = fm->lr - 1;
	}
}

void
copy_row(int rf)
{
	if(rf == curow) return;
	struct rng rngf{ .lr= (CELLREF) rf, .lc=(CELLREF) 1, .hr = (CELLREF) rf, .hc = (CELLREF) MAX_COL};
	struct rng rngt{ .lr= (CELLREF) curow, .lc=(CELLREF) 1, .hr = (CELLREF) curow, .hc = (CELLREF) MAX_COL};
	copy_region(&rngf, &rngt);
}

struct rng sort_rng;
struct rng sort_ele;
struct cmp *sort_keys;
int sort_keys_alloc;
int sort_keys_num = 0;

static int srdiff, erdiff, scdiff, ecdiff;

#ifdef TEST
extern int debug;
#endif





