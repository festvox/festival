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
/*             Author :  Steve Isard                                     */
/*             Date   :  1984                                            */
/*  This version was gifted by Steve for this new                        */
/*  copyright, the original retains their original copyright             */
/*                                                                       */
/*************************************************************************/
#include <math.h>
#include <stdio.h>
#include "t2s.h"

/* transform a set of reflection coefficients to linear prediction
 * coefficients using algorithm from HCL-H
 */

void rfctolpc(float *buf)
{
	float a,b;
	register float *cptr;
	register int n,k;

    /* HCL-H algorithm goes through coeffs in reverse of order in which they
     * appear here, working on buffer numbered from 1 upwards. To get same
     * effect, we make cptr point at space just after last coeff and let n and
     * k go down from -1 instead of up from 1.
     */
	cptr = buf + NCOEFFS; /* There should be NCOEFFS coeffs. Point just
                               * after last one.
                               */

	for(n = -1; n >= -NCOEFFS; n--) {
		*(cptr+n) = -(*(cptr+n));
		for(k = -1; 2*k >= n; k--) {
			a = *(cptr+k); 
			b = *(cptr+n-k);
			*(cptr+k) = a - b * *(cptr+n);
			*(cptr+n-k) = b - a * *(cptr+n);
		}
	}
}

