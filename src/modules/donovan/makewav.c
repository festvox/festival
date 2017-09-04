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
/*             Author :  Steve Isard and Alistair Conkie                 */
/*                            1984 and 1996                              */
/*  This version was gifted by Steve and Alistair for this new           */
/*  copyright, the original retains their original copyright             */
/*                                                                       */
/*************************************************************************/
#include <math.h>
#include <stdio.h>
#include "t2s.h"

void makewave(CONFIG *config, ACOUSTIC *as)
{
	short *ptptr;
	short ptbuf[OUT_BUF+NCOEFFS];
	float rbuf[NCOEFFS],amp;
	int i,j,k,l;
	int voiced;
	int zz = 0;
	short *mcebuf;
	short u_cache, s_cache = 0;
	short wkspace[2] = {0,0};

	for(zz=0;zz<NCOEFFS;zz++) 
		ptbuf[zz] = 0;

	ptptr = &ptbuf[NCOEFFS];

	for(i=0;i<as->f_sz;i++) {

		mcebuf = &(as->mcebuf[i]->frame[0]);

		voiced = mcebuf[1]/2;

		if(voiced == 0) {
			amp = 2*(float)sqrt((double)mcebuf[0]);
		} else {
			amp = (float) sqrt((double) (mcebuf[0] * voiced) );
		}

		for(k=FR_DATA-NCOEFFS;k < FR_DATA;k++)
			rbuf[k-(FR_DATA-NCOEFFS)] = (float)mcebuf[k]/32767;

		rfctolpc(rbuf); /* convert to lpc coeffs */

		for(j=0;j < as->duration[i];j++,ptptr++) {
			register float v,*cptr;
			register short *backptr;
			short *endptr;
			float exc;

			exc = iexc(voiced,as,wkspace);
			v = (float)(exc == 0 ? 0 : amp*10*exc); /* 10 is NEW  */

			/* following loop depends on an initial NCOEFFS zeros preceding
                         * ptbuf to make *(--backptr) zero the first time through.
                         * We are depending on the ILS header before ptbuf to supply
                         * these zeros.  If waves are ever to be synthesized without
                         * an ILS header on the front, special provision will have to be
                         * made.
                         */

			endptr = ptptr - NCOEFFS;
			backptr = ptptr;
			cptr = rbuf + NCOEFFS; /* point at (empty) last cell of rbuf,
                                     * because we are going to use coeffs in
                                     * reverse order
			                                     */
			for(; backptr > endptr;)
				v += (float)( *(--backptr) * *(--cptr) );

			/* this is where we need to concern ourselves with */
			/* flushing the buffer from time to time  */
			ptbuf[zz++] = (short)(v);
			if(zz>=(OUT_BUF+NCOEFFS)) {
				for(l=zz-NCOEFFS;l<zz-2;l++) {
					ptbuf[l-OUT_BUF] = ptbuf[l];
				}
				u_cache = ptbuf[zz-1];
				ptbuf[NCOEFFS-1] = s_cache;
				for(l=NCOEFFS;l<zz;l++) {
					ptbuf[l] += (short)(0.9 * ptbuf[l-1]);
				}
				s_cache = ptbuf[zz-1];
				ptbuf[NCOEFFS-1] = u_cache;
				audio_play(&ptbuf[NCOEFFS],sizeof(short),OUT_BUF,config); 
				zz = NCOEFFS;
				ptptr = &ptbuf[NCOEFFS-1]; /* -1 since in for loop  */
			}
		}
	}
	ptbuf[NCOEFFS-1] = s_cache;
	for(l=NCOEFFS;l<zz;l++) {
		ptbuf[l] += (short)(0.9 * ptbuf[l-1]);
	}
	audio_play(&ptbuf[NCOEFFS],sizeof(short),zz-NCOEFFS,config); 
}
