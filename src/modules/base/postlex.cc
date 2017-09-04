/*************************************************************************/
/*                                                                       */
/*                Centre for Speech Technology Research                  */
/*                     University of Edinburgh, UK                       */
/*                         Copyright (c) 1997                            */
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
/*                      Author :  Alan W Black                           */
/*                      Date   :  February 1997                          */
/*-----------------------------------------------------------------------*/
/*                                                                       */
/* Post-lexical rules: vowel reduction and contraction, R deleteion      */
/*                                                                       */
/* All this is far too specific, and should be parameterized better      */
/*    -- and gradually it is ...                                         */
/*                                                                       */
/*=======================================================================*/
#include <cstdio>
#include "festival.h"
#include "modules.h"

static void vowel_reduction(EST_Utterance *u);
static void r_reduction(EST_Utterance *u);
static void vowel_reduce(EST_Item *syl,LISP vow_table);

LISP FT_PostLex_Utt(LISP utt)
{
    // Do vowel reduction, destructively changes vowel segment values
    EST_Utterance *u = get_c_utt(utt);

    vowel_reduction(u);
    r_reduction(u);

    return utt;
}

static void r_reduction(EST_Utterance *u)
{
    // R reduction for mrpa (British English)
    EST_Item *s,*t;
    LISP r_red_tree;

    if (!streq(get_c_string(ft_get_param("PhoneSet")),"mrpa"))
	return;

    r_red_tree = siod_get_lval("postlex_mrpa_r_cart_tree",NULL);
    if (r_red_tree == NIL)
	return;

    for (s=u->relation("Segment")->first(); s != 0; s = t)
    {
	t = inext(s);
	if (wagon_predict(s,r_red_tree) == "delete")
	    s->unref_all();
    }
}

static void vowel_reduction(EST_Utterance *u)
{
    EST_Item *s;
    LISP red_tree, full_vow_table, vow_table=NIL;

    red_tree = siod_get_lval("postlex_vowel_reduce_cart_tree", NULL);
    full_vow_table = siod_get_lval("postlex_vowel_reduce_table",NULL);
    vow_table = 
	car(cdr(siod_assoc_str(get_c_string(ft_get_param("PhoneSet")),
			       full_vow_table)));
    if ((vow_table == NIL) || (red_tree == NIL))
	return;   // ain't anything to do
    
    for (s=u->relation("Syllable")->first(); s != 0; s = inext(s))
    {
	if (wagon_predict(s,red_tree) == "1")
	    vowel_reduce(s,vow_table);
    }
}

static void vowel_reduce(EST_Item *syl,LISP vow_table)
{
    // Reduce vowel in syl by looking it up in vow_table for 
    // appropriate vowel mapping
    EST_Item *seg;
    LISP vreduce=NIL;

    for (seg=daughter1(syl,"SylStructure"); seg; seg=inext(seg))
    {
	if (ph_is_vowel(seg->name()))
	{
	    vreduce = siod_assoc_str(seg->name(),vow_table);
	    if (vreduce != NIL)
		seg->set_name(get_c_string(car(cdr(vreduce))));
	    return;
	    // ignore any secondary vowels in syllable (should only be one)
	}
    }
}


