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
/*             Date   :  1986                                            */
/*  This version was gifted by Alistair for this new                     */
/*  copyright, the original retains their original copyright             */
/*                                                                       */
/*************************************************************************/
#include <stdio.h>
#include "t2s.h"

static int interpolated_freq(int k, SPN *ps);
static int interpolate(int a,int b,int c,int d,int e);

void calc_pitch(SPN *ps, ACOUSTIC *as)
{
	int j,k;
	int y;
	int l = 0;
	int k_old = 0;
	int k_fine = 0;
	int x = 0;

	for(j=0;j<ps->t_sz;j++) 
		ps->abs_targ[j] = ps->cum_dur[ps->targ_phon[j]] + 
		ps->pc_targs[j]*ps->duration[ps->targ_phon[j]]/100.0;

	for(k=0;k<ps->cum_dur[ps->p_sz];k+=100) {
		y = interpolated_freq(k,ps);
		x += 100*y;
		while(x>SR) {
			k_fine = k + interpolate(x-100*y,0,x,100,10000);
			x -= SR;
			as->pitch[l++] = k_fine-k_old;
			if(l == as->p_max) {
				as_realloc(as->f_max,as->p_max*2,as);
			}
			k_old = k_fine;
                }
	}
	as->p_sz = l;
	as->pitch[0] += FR_SZ/2;  /* to compensate for mismatch  */
}

static int interpolated_freq(int k, SPN *ps)
{
	int i;
	int freq;

	if(!ps->t_sz)
	    return(DEF_F0);
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
	return(-1); /* should never arrive here  */
}

static int interpolate(int a,int b,int c,int d,int e)
{
	int f;

	f = (c*b + d*e - e*b -a*d)/(c-a);

	return(f);
}
