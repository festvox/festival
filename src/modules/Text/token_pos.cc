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
/*             Author :  Alan W Black                                    */
/*             Date   :  March 1997                                      */
/*-----------------------------------------------------------------------*/
/*                                                                       */
/* This assigns a form of part of speech to tokens.  It is typically use */
/* to assign gross level pos for things before they are converted to     */
/* words, particularly, numbers (ordinals, digitized, years, numbers)    */
/*                                                                       */
/*=======================================================================*/
#include <cstdio>
#include "festival.h"
#include "tokenP.h"

LISP FT_Token_POS_Utt(LISP utt)
{
    // This module assigns token_pos feature to each token based on the
    // Assoc list of Regex to CART trees in token_pos_cart_trees.
    EST_Utterance *u = get_c_utt(utt);
    EST_Item *t;
    LISP trees,l;

    trees = siod_get_lval("token_pos_cart_trees",NULL);
    if (trees == NIL) return utt;

    for (t=u->relation("Token")->first(); t != 0; t = inext(t))
    {
	if (t->f("token_pos","0") == "0")
	    for (l=trees; l != NIL; l=cdr(l))  // find a tree that matches
	    {
		if (t->name().matches(make_regex(get_c_string(car(car(l))))))
		{
		    t->set_val("token_pos",
				 wagon_predict(t,car(cdr(car(l)))));
		    break;
		}
	    }
    }

    return utt;

}
