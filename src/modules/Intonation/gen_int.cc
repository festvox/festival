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
/*             Date   :  April 1996                                      */
/*-----------------------------------------------------------------------*/
/*                                                                       */
/* A general intonation method for implementing various simple rule      */
/* intonation systems.  It allows a list of targets to be predicted      */
/* in a way fully specified by the user without changing the C/C++ code  */
/* This was specifically designed to replace the simple intonation mode  */
/* monotone mode, and implemented generic ToBI type labels.              */
/*                                                                       */
/* This was to help Gregor Moehler do German ToBI as well as get a       */
/* we can use for a rule-based English ToBI for comparison with trained  */
/* versions                                                              */
/*                                                                       */
/*=======================================================================*/
#include <cstdio>
#include "festival.h"
#include "intonation.h"

static void check_targs(EST_Utterance *u);
static EST_Item *find_nearest_seg(EST_Utterance *u,float pos);

LISP FT_Int_Targets_General_Utt(LISP utt)
{
    // Predict F0 targets 
    EST_Utterance *u = get_c_utt(utt);
    EST_Item *s;
    EST_Item *seg;
    EST_Relation *targrel;
    LISP gen_params, targets, t;
    LISP tfunc;  // a lisp function that returns list of targets and values

    // Create some down step accents
    gen_params = siod_get_lval("int_general_params",
				  "no general intonation simple params");
    tfunc = get_param_lisp("targ_func",gen_params,NIL);
    if (tfunc == NIL)
    {
	cerr << "Int Target General: no target function specified" << endl;
	festival_error();
    }
    
    targrel = u->create_relation("Target");
    
    for (s=u->relation("Syllable")->first(); s != 0 ; s=inext(s))
    {
	targets = 
          leval(cons(tfunc,cons(utt,cons(siod(s),NIL))),NIL);
	// Add the given targets
	for (t=targets; t != NIL; t=cdr(t))
	{
	    seg = find_nearest_seg(u,get_c_float(car(car(t))));
	    add_target(u,seg,get_c_float(car(car(t))),
		       get_c_float(car(cdr(car(t)))));
	}
    }

    check_targs(u);

    return utt;
}

static EST_Item *find_nearest_seg(EST_Utterance *u,float pos)
{
    // Find the segment that this target falls within.
    // This naively searchs from the start of the segments,
    // this is not very efficient
    EST_Item *seg;

    for (seg=u->relation("Segment")->first(); seg != 0;seg=inext(seg))
    {
	if (seg->F("end") >= pos)
	    return seg;
    }

    cerr << "Int Target General: target past end of segments at " <<
	pos << endl;
    festival_error();
    return NULL;
}

static void check_targs(EST_Utterance *u)
{
    // Check targets are in order
    EST_Item *t;
    float l = 0.0;

    for (t=first_leaf(u->relation("Target")->first()); t != 0;t=next_leaf(t))
    {
	if (t->F("pos") < l)
	{
	    cerr << "Int Target General: targets out of order" << endl;
	    festival_error();
	}
	l = t->F("pos");
    }
}
       

