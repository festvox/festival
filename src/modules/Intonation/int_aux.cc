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
/*                      Author :  Alan W Black                           */
/*                      Date   :  April 1996                             */
/*-----------------------------------------------------------------------*/
/*                                                                       */
/* Basic intonation utilities common between different                   */
/*                                                                       */
/*=======================================================================*/
#include <cstdio>
#include "festival.h"
#include "intonation.h"
#include "modules.h"
#include "lexicon.h"

static EST_String IntEventname("IntEvent");
static EST_String Targetname("Target");

EST_Item *add_target(EST_Utterance *u,EST_Item *seg, 
			    float pos, float val)
{

  // Check time is NOT the same as the last target, as this causes problems...

  float last_time;
  EST_Item* last_item = last_leaf(u->relation(Targetname)->first());
  if (last_item)
    last_time = last_item->f("pos");
  else 
    last_time = -1.0; // no last time.

  if(last_time == pos)
    {
      pos += 0.001;
      *cdebug << "Repeated f0 target time, fix your generation function!\n";
    }

  if (seg->as_relation(Targetname) == 0)
    u->relation(Targetname)->append(seg);
  EST_Item *item = append_daughter(seg,Targetname);
  
  item->set("f0",val);
  item->set("pos",pos);
  
  return item;

}


EST_Item *add_IntEvent(EST_Utterance *u,EST_Item *syl,
			      const EST_String &label)
{
    if (syl->as_relation("Intonation") == 0)
	u->relation("Intonation")->append(syl);
    EST_Item *item = u->relation(IntEventname)->append();
    item->set_name(label);
    append_daughter(syl,"Intonation",item);
    return item;
}

void targets_to_f0(EST_Relation &targ, EST_Track &f0, const float shift)
{
    float prev_f0=0.0;
    float prev_pos=0, m;
    EST_Item *s;
    int i;

    f0.resize(int(ceil(last_leaf(targ.first())->F("pos",0) / shift)), 1);
    f0.fill_time(shift);

    s = first_leaf(targ.first());

    // fill with zeros until first target;
    for (i = 0; i < f0.num_frames(); ++i)
    {
	if (f0.t(i) > s->F("pos",0))
	    break;
	f0.a(i) = 0.0;
    }

    prev_pos = s->F("pos",0);
    prev_f0 = s->F("f0",0);

    s = next_leaf(s);

    for (m=0.0,i = 0; i < f0.num_frames(); ++i)
    {
	if (s && f0.t(i) > s->F("pos"))
	{
	    prev_pos = s->F("pos");
	    prev_f0 = s->F("f0");
	    s = next_leaf(s);
	    if (s == 0)
		break;
	    m = (s->F("f0") - prev_f0)/ (s->F("pos") - prev_pos);
	}
	f0.a(i) = (m * (f0.t(i) - prev_pos)) + prev_f0;
    }

    for ( ; i < f0.num_frames(); ++i)
	f0.a(i) = 0.0;

}    

LISP FT_us_targets_to_f0(LISP lutt)
{
    EST_Utterance *utt = get_c_utt(lutt);
    EST_Track *f0 = new EST_Track;

    utt->create_relation("f0");
    EST_Item *f = utt->relation("f0")->append();

    f->set("name", "f0");
    f->set_val("f0", est_val(f0));

    targets_to_f0(*utt->relation("Target"), *f0, 0.01);

    return lutt;
}


LISP FT_Intonation_Default_Utt(LISP args);
LISP FT_Int_Targets_Default_Utt(LISP args);
LISP FT_Intonation_Simple_Utt(LISP args);
LISP FT_Int_Targets_Simple_Utt(LISP args);
LISP FT_Intonation_Tree_Utt(LISP args);
LISP FT_Int_Targets_LR_Utt(LISP args);
LISP FT_Int_Targets_LR_5_Utt(LISP args);
LISP FT_Int_Targets_General_Utt(LISP utt);
LISP FT_Int_Targets_Relation_Utt(LISP utt, LISP relname);

void festival_Intonation_init(void)
{

    festival_def_utt_module("Intonation_Default",FT_Intonation_Default_Utt,
    "(Intonation_Default UTT)\n\
  this method is such a bad intonation module that it does nothing at all.\n\
  This utterance module is called when the Parameter Int_Method is not\n\
  set or  set to Default.  This module is called through the Intonation\n\
  module. [see Default intonation]");

    init_subr_2("Int_Targets_Relation", FT_Int_Targets_Relation_Utt,
    "(Int_Targets_Relation UTT)");

    init_subr_1("targets_to_f0", FT_us_targets_to_f0,
    "(targets_to_f0 UTT)\n\
    Make f0 relation, and place an f0 contour in it, using F0 targets\n\
    from the Target Relation\n");

    festival_def_utt_module("Int_Targets_Default",FT_Int_Targets_Default_Utt,
    "(Int_Targets_Default UTT)\n\
  This module creates two Targets causing a simple downward continuous\n\
  F0 through the whole utterance.  The code is in an appropriate named file\n\
  called duffint.  This module is called when the Parameter\n\
  Int_Method is not set or set to Default.  This module is called through\n\
  the Int_Targets module.  Optional parameters for a start value (default\n\
  130) and end value (default 110) may be set in the variable\n\
  diffint_params.  This can be used to generate a monotone intonation\n\
  with a setting like (set! duffint_params '((start 100) (end 100))).\n\
  [see Default intonation]");
    festival_def_utt_module("Intonation_Simple",FT_Intonation_Simple_Utt,
    "(Intonation_Simple)\n\
  Assign accents to each content word, creating an IntEvent stream. This \n\
  utterance module is called when the Parameter Int_Method is set to \n\
  Simple.  This module is called through the Intonation module.\n\
  [see Simple intonation]");
    festival_def_utt_module("Int_Targets_Simple",FT_Int_Targets_Simple_Utt,
    "(Int_Targets_Simple UTT)\n\
  Naively add targets for hat shaped accents for each accent in the \n\
  IntEvent stream.  This module is called when the Parameter Int_Method is\n\
  set to Simple.  This module is called through the Int_Targets module.\n\
  [see Simple intonation]");
    festival_def_utt_module("Int_Targets_General",FT_Int_Targets_General_Utt,
    "(Int_Targets_General UTT)\n\
  Add targets based on the functions defined in int_general_params.  This\n\
  method allows quite detailed control over the general of targets per\n\
  syllable, see manual for details and examples.  This module is called\n\
  when the Parameter Int_Method is set to General.  This module is called\n\
  through the Int_Targets module. [see General intonation]");
    festival_def_utt_module("Intonation_Tree",FT_Intonation_Tree_Utt,
    "(Intonation_Tree UTT)\n\
  Use the CART trees in int_tone_cart_tree and int_accent_cart_tree to\n\
  create an IntEvent stream of tones and accents related to syllables.\n\
  This module is called through the Intonation module and is selected\n\
  when the Parameter Int_Method is ToBI. [see Tree intonation]");
    festival_def_utt_module("Int_Targets_LR",FT_Int_Targets_LR_Utt,
    "(Int_Targets_LR UTT)\n\
  Predict Target F0 points using linear regression from factors such as\n\
  accent, tone, stress, position in phrase etc.  This utterance module is\n\
  called through the module Int_Targets when the Parameter Int_Method is\n\
  set to ToBI, even though this technique is not restricted to the ToBI\n\
  labelling system. [see Tree intonation]");
    festival_def_utt_module("Int_Targets_5_LR",FT_Int_Targets_LR_5_Utt,
    "(Int_Targets_5_LR UTT)\n\
  Predict Target F0 points using linear regression from factors such as\n\
  accent, tone, stress, position in phrase etc.  This utterance module is\n\
  called through the module Int_Targets when the Parameter Int_Method is\n\
  set to ToBI, even though this technique is not restricted to the ToBI\n\
  labelling system. [see Tree intonation]");

}
    
