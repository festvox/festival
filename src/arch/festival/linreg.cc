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
/*                     Author :  Alan W Black                            */
/*                     Date   :  May 1996                                */
/*-----------------------------------------------------------------------*/
/*                                                                       */
/* A simple interpreter for Linear Regression models                     */
/*                                                                       */
/*=======================================================================*/
#include <cstdio>
#include "EST_unix.h"
#include "festival.h"

#define FFEATURE_NAME(X) (get_c_string(car(X)))
#define FFEATURE_WEIGHT(X) (get_c_float(car(cdr(X))))
#define FFEATURE_MAPCLASS(X) (car(cdr(cdr(X))))

EST_Val lr_predict(EST_Item *s, LISP lr_model)
{
    EST_Val v = 0.0;
    float answer;
    LISP f;
    const char *ffeature_name, *last_name="";

    answer = FFEATURE_WEIGHT(car(lr_model)); // Intercept;
    for (f=cdr(lr_model); CONSP(f); f=CDR(f))
    {
	ffeature_name = FFEATURE_NAME(CAR(f));
	if (!streq(ffeature_name,last_name))
	    v = ffeature(s,ffeature_name);
	if (siod_llength(CAR(f)) == 3)
	{   // A map class is specified
	    if (siod_member_str(v.string(),FFEATURE_MAPCLASS(CAR(f))) != NIL)
		answer += FFEATURE_WEIGHT(CAR(f));
	}
	else
	    answer += FFEATURE_WEIGHT(CAR(f)) * (float)v;
	last_name = ffeature_name;
    }

    return EST_Val(answer);
}

LISP l_lr_predict(LISP si, LISP lr_model)
{
    EST_Item *s = item(si);
    EST_Val answer;

    answer = lr_predict(s,lr_model);
    return flocons(answer.Float());
}

    

    
    
	

