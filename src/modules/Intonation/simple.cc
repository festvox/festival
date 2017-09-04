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
/*             Author :  Alan W Black (and Paul Taylor)                  */
/*             Date   :  April 199[4|6]                                  */
/*-----------------------------------------------------------------------*/
/*                                                                       */
/* Simple intonation prediction: a hat shape on each content word        */
/*                                                                       */
/*=======================================================================*/
#include <cstdio>
#include "festival.h"
#include "intonation.h"

static void add_targets(EST_Utterance *u,EST_Item *syl, 
			float baseline,float peak);

LISP FT_Intonation_Simple_Utt(LISP utt)
{
    // Predict some accents
    EST_Utterance *u = get_c_utt(utt);
    EST_Item *s;
    LISP accent_tree;
    EST_Val paccent;

    *cdebug << "Simple intonation module" << endl;

    accent_tree = siod_get_lval("int_accent_cart_tree","no accent tree");

    u->create_relation("IntEvent");
    u->create_relation("Intonation");

    for (s=u->relation("Syllable")->first(); s != 0; s = inext(s))
    {	
	paccent = wagon_predict(s,accent_tree);
	if (paccent != "NONE")
	    add_IntEvent(u,s,paccent.string());
    }

    return utt;
}

LISP FT_Int_Targets_Simple_Utt(LISP utt)
{
    // Predict F0 targets 
    EST_Utterance *u = get_c_utt(utt);
    EST_Item *s, *p, start_word, end_word;
    float start,end,duration;
    float baseline, decline;
    EST_Item *start_syl, *end_syl;
    LISP simple_params;
    float f0_mean, f0_std;

    *cdebug << "Simple int targets module" << endl;

    // Create some down step accents
    simple_params = siod_get_lval("int_simple_params","no simple params");
    f0_mean = get_param_int("f0_mean",simple_params,110);
    f0_std = get_param_int("f0_std",simple_params,25);
    
    u->create_relation("Target");
    
    for (p=u->relation("Phrase")->first(); p != 0 ; p=inext(p))
    {
	baseline = f0_mean + (f0_std * 0.6);
	start = ffeature(p,"R:Phrase.daughter1.word_start");
	end = ffeature(p,"R:Phrase.daughtern.word_end");
	duration = end - start;
	decline = f0_std / duration;
	start_syl = daughter1(daughter1(p),"SylStructure");
	end_syl = daughtern(daughtern(p),"SylStructure");

	if (start_syl)
	    add_target(u,daughter1(start_syl,"SylStructure"),
	       ffeature(start_syl,"R:SylStructure.daughter1.segment_start"),
		       baseline);
	for (s=start_syl->as_relation("Syllable"); s != inext(end_syl); 
	     s = inext(s))
	{
	    if (ffeature(s,"accented") == 1)
		add_targets(u,s,baseline,f0_std);
	    baseline -= decline*(ffeature(s,"syllable_duration").Float());
	}

	if (end_syl)
	    add_target(u,daughtern(end_syl,"SylStructure"),
		   ffeature(end_syl,"R:SylStructure.daughtern.segment_end"),
		   f0_mean-f0_std);
    }

    return utt;
}

static void add_targets(EST_Utterance *u,EST_Item *syl, 
			float baseline,float peak)
{
    // Add a down stepped accent at this point 
    EST_Item *first_seg = daughter1(syl,"SylStructure");
    EST_Item *end_seg = daughter1(syl,"SylStructure");
    EST_Item *t=0,*vowel_seg;

    add_target(u,first_seg,ffeature(first_seg,"segment_start"),baseline);

    vowel_seg = end_seg; // by default
    for (t = first_seg; t != 0; t = inext(t))
	if (ph_is_vowel(t->name()))
	{
	    vowel_seg = t;
	    break;
	}
    add_target(u,vowel_seg,ffeature(vowel_seg,"segment_mid"),baseline+peak);
    add_target(u,end_seg,ffeature(end_seg,"segment_end"),baseline);
}
       

