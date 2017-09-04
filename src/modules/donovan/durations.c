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
/*             Date   :  1996                                            */
/*  This version was gifted by Alistair for this new                     */
/*  copyright, the original retains their original copyright             */
/*                                                                       */
/*************************************************************************/
#include <stdio.h>
#include "t2s.h"

static int min(int a, int b);
static float local_fmax(float a, float b);

void durations(SPN *ps, ACOUSTIC *as)
{
	int durdist;
	int interdist;
	float multiplier_i;
	float proportion;
	int i;
	int j;
	
	for(i=0;i<ps->p_sz;i++) 
		ps->scale[i] = (float)ps->duration[i] /
		(float)((ps->pb[i+1]-ps->pb[i])*FR_SZ);
	
	ps->cum_dur[0] = 0;  /* do cumulative at same time  */
	for(i=0,j=0;i<as->f_sz;i++) {
		if(i == ps->pb[j]) {
			if(j != 0) {
				ps->cum_dur[j] = ps->duration[j-1] + ps->cum_dur[j-1];
			}
			as->duration[i] = FR_SZ;
			ps->duration[j] = FR_SZ; /* saves adding later  */
			j++;
		} else {
			durdist = min(i-ps->pb[j-1],ps->pb[j]-i);
			interdist = ps->pb[j] - ps->pb[j-1];
			proportion = (float)durdist/(float)interdist;
			multiplier_i = local_fmax(0.01,4.0*proportion*(ps->scale[j-1]-1.0)+1.0);
			as->duration[i] = FR_SZ*multiplier_i;
			ps->duration[j-1] += as->duration[i];
		}
	}
}

static int min(int a, int b)
{
	return((a<b)?a:b);
}

static float local_fmax(float a, float b)
{
	return((a>b)?a:b);
}

