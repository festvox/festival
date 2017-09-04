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
#include "festival.h"
#include "diphone.h"

static int interpolated_freq(int k, DIPHONE_SPN *ps,int def_f0);
static int interpolate(int a,int b,int c,int d,int e);

void di_calc_pitch(DIPHONE_DATABASE *db, DIPHONE_SPN *ps, DIPHONE_ACOUSTIC *as)
{
    int j,k;
    int y;
    int l = 0;
    int k_old = 0;
    int k_fine = 0;
    int x = 0;

    for(j=0;j<ps->t_sz;j++) 
	ps->abs_targ[j] = (int)(ps->cum_dur[ps->targ_phon[j]] + 
	    ps->pc_targs[j]*ps->duration[ps->targ_phon[j]]/100.0);

    as->cum_pitch[0] = 0;
    for(k=0;k<ps->cum_dur[ps->p_sz];k+=100) 
    {
	y = interpolated_freq(k,ps,db->def_f0);
	x += 100*y;
	while(x>db->samp_freq) 
	{
	    k_fine = k + interpolate(x-100*y,0,x,100,db->samp_freq);
	    x -= db->samp_freq;
	    as->pitch[l] = k_fine-k_old;
	    as->cum_pitch[l+1] = as->pitch[l] + as->cum_pitch[l];
	    l++;
	    if (l == as->p_max)
	    {
		cerr << "Diphone: too many pitch marks\n";
		festival_error();
	    }
	    k_old = k_fine;
	}
    }
    as->p_sz = l;

    return;
}

static int interpolated_freq(int k, DIPHONE_SPN *ps,int def_f0)
{
    int i;
    int freq;

   if(!ps->t_sz)
        return(def_f0);
    else if(k<ps->abs_targ[0])
        return(ps->targ_freq[0]);
    else if(k>=ps->abs_targ[ps->t_sz-1])
        return(ps->targ_freq[ps->t_sz-1]);
    for(i=1;i<ps->t_sz;i++) {
        if((k<ps->abs_targ[i]) && (k>=ps->abs_targ[i-1]))
        {
            freq = interpolate(ps->abs_targ[i-1],
			       ps->targ_freq[i-1],
			       ps->abs_targ[i],
			       ps->targ_freq[i],k);
	    return(freq);
	} 
    }
    return(-1);			/* should never arrive here  */
}

static int interpolate(int a,int b,int c,int d,int e)
{
    int f;

    f = (c*b + d*e - e*b -a*d)/(c-a);

    return(f);
}
