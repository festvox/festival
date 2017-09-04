/*************************************************************************/
/*                                                                       */
/*                Centre for Speech Technology Research                  */
/*                     University of Edinburgh, UK                       */
/*                       Copyright (c) 1996,1997                         */
/*                        All Rights Reserved.                           */
/*                                                                       */
/*  Permission is hereby granted, free of charge, to use and distribute  */
/*  this software and its documentation without restriction, including   */
/*  without limitation the rights to use, copy, modify, merge, publish,  */
/*  distribute, sublicense, and/or sell copies of this work, and to      */
/*  permit persons to whom this work is furnished to do so, subject to   */
/*  the following conditions:                                            */
/*   1. The code must retain the above copyright notice, this list of    */
/*      conditions and the following disclaimer.                         */
/*   2. Any modifications must be clearly marked as such.                */
/*   3. Original authors' names are not deleted.                         */
/*   4. The authors' names are not used to endorse or promote products   */
/*      derived from this software without specific prior written        */
/*      permission.                                                      */
/*                                                                       */
/*  THE UNIVERSITY OF EDINBURGH AND THE CONTRIBUTORS TO THIS WORK        */
/*  DISCLAIM ALL WARRANTIES WITH REGARD TO THIS SOFTWARE, INCLUDING      */
/*  ALL IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS, IN NO EVENT   */
/*  SHALL THE UNIVERSITY OF EDINBURGH NOR THE CONTRIBUTORS BE LIABLE     */
/*  FOR ANY SPECIAL, INDIRECT OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES    */
/*  WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN   */
/*  AN ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION,          */
/*  ARISING OUT OF OR IN CONNECTION WITH THE USE OR PERFORMANCE OF       */
/*  THIS SOFTWARE.                                                       */
/*                                                                       */
/*************************************************************************/
/*             Author :  Alistair Conkie                                 */
/*             Date   :  August 1996                                     */
/*-----------------------------------------------------------------------*/
/*                                                                       */
/* A free version of diphone selection and concatenation supported the   */
/* standard CSTR diphone dbs                                             */
/*                                                                       */
/*=======================================================================*/
#include <cstdio>
#include <cstdlib>
#include "festival.h"
#include "diphone.h"

static void bresenham_mod(DIPHONE_SPN *ps, DIPHONE_ACOUSTIC *as);
static void nochange_mod(DIPHONE_SPN *ps, DIPHONE_ACOUSTIC *as);

void di_frame_select(DIPHONE_DATABASE *database, DIPHONE_SPN *ps, DIPHONE_ACOUSTIC *as)
{
    /* resist the temptation to call this select :-)  */
    int i, j;
    int bias = 0;

    for(i=0;i<ps->p_sz;i++) 
    {
	ps->pm_req[i] = 0;
	ps->pm_giv[i] = 0;
    }

    /* count the needed pitch marks for each phoneme (or diph)  -> ps  */

    j = 0;			/* can j get too big ?  */
    for(i=0;i<as->p_sz;i++) 
    {
	if((as->cum_pitch[i]>=ps->cum_dur[j]) && 
	   (as->cum_pitch[i]<ps->cum_dur[j+1])) 
	    ps->pm_req[j] += 1;
	else if((as->cum_pitch[i]>=ps->cum_dur[j+1]) && 
		(as->cum_pitch[i]<ps->cum_dur[j+2])) 
	    ps->pm_req[++j] += 1;
	else
	{
	   *cdebug << "Diphone: some sort of pitch error" << endl;
	    ps->pm_req[++j] += 1;
       }
    }


    /* count the  given pitch marks for each phoneme (or diph) -> ps  */

    ps->pm_chg_diph[0] = 0;
    for(i=0;i<ps->p_sz-1;i++) 
    {
	ps->pm_giv[i] += database->pm[ps->ref[i]]->lmark;
	ps->pm_chg_diph[i+1] = database->pm[ps->ref[i]]->rmark;
	ps->pm_giv[i+1] += database->pm[ps->ref[i]]->rmark;
    }

    if (siod_get_lval("diphone_no_mods",NULL) != NIL)
	nochange_mod(ps,as);
    else
	bresenham_mod(ps,as);

    /* there is something still to do in terms of selecting frames  */
    /* 			diph_mark  				*/

    bias = 0;
    for(i=0;i<as->p_sz-1;i++) 
    {
	if ((as->phon_mark[i] + bias) < 0)
	    as->diph_mark[i] = as->phon_mark[i];
	else if ((as->phon_mark[i] + bias) < 
		 database->pm[as->diph_ref[i]]->nmark)
	    as->diph_mark[i] = as->phon_mark[i] + bias;
	else if (as->phon_mark[i] < 
		 database->pm[as->diph_ref[i]]->nmark)
	    as->diph_mark[i] = as->phon_mark[i];
	else  // It needs less than one pitch mark;
	    as->diph_mark[i] = database->pm[as->diph_ref[i]]->nmark-1;
	if ((as->diph_ref[i] != as->diph_ref[i+1]) &&
	    (i+1 != as->p_sz-1))
	    bias = - database->pm[as->diph_ref[i]]->rmark;
	if ((as->phon_ref[i] != as->phon_ref[i+1]) &&
	    (i+1 != as->p_sz-1))
	    bias = database->pm[as->diph_ref[i+1]]->lmark;
    }
}

static void nochange_mod(DIPHONE_SPN *ps, DIPHONE_ACOUSTIC *as)
{
    // Does no duration or pitch modifications, useful in testing
    int ph,i,j;

    j=0;
    for(ph=0;ph<ps->p_sz;ph++) 
    {
	for (i=0; i < ps->pm_giv[ph]; i++,j++)
	{
	    as->phon_ref[j] = ph;
	    as->phon_mark[j] = i;
	    if (i >= ps->pm_chg_diph[ph])
		as->diph_ref[j] = ps->ref[ph];
	    else
		as->diph_ref[j] = ps->ref[ph-1];
	}
    }
    as->p_sz = j;
}

static void bresenham_mod(DIPHONE_SPN *ps, DIPHONE_ACOUSTIC *as)
{
    int error = 0;
    int i,x,y,deltax,deltay, ph;

    as->p_tmp = 0;

    for(ph=0;ph<ps->p_sz;ph++) 
    {
	error = 0;
	deltax = ps->pm_req[ph];
	deltay = ps->pm_giv[ph];
	x = 0;
	y = 0;
	if (deltax < deltay) 
	{
	    for (i=0;i<deltay;i++) 
	    {
		error += deltax;
		if (error>=deltay) 
		{
		    as->phon_ref[as->p_tmp] = ph;
		    as->phon_mark[as->p_tmp] = i-1;
		    if ((i-1) >= ps->pm_chg_diph[ph])
			as->diph_ref[as->p_tmp] = ps->ref[ph];
		    else
			as->diph_ref[as->p_tmp] = ps->ref[ph-1];
		    as->p_tmp++;
		    /* printf("phon [%d], corr[%d] = %d\n",ph,x,i-1);  */
		    x++;
		    error -= deltay;
		}
	    }
	} else 
	{
	    for(i=0;i<deltax;i++) 
	    {
		as->phon_ref[as->p_tmp] = ph;
		as->phon_mark[as->p_tmp] = y;
		if(y >= ps->pm_chg_diph[ph])
		    as-> diph_ref[as->p_tmp] = ps->ref[ph];
		else
		    as-> diph_ref[as->p_tmp] = ps->ref[ph-1];
		as->p_tmp++;
		/* printf("phon [%d], corr[%d] = %d\n",ph,i,y);  */
		error += deltay;
		if(error>deltax)
		{
		    y++;
		    error -= deltax;
		}
	    }
	}
    }
}

