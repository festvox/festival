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
/*             Date   :  May 1996                                        */
/*-----------------------------------------------------------------------*/
/*                                                                       */
/* Tree-based prediction of intonation.  Uses accent and end             */
/* tone prediction trees, could be ToBI could be something               */
/* else, its up to the trees to decide ...                               */
/*                                                                       */
/* Accents and boundaries are predicted by CART tree while               */
/* the F0 targets are predicted by linear regression (as                 */
/* described in Black and Hunt ICSLP96)                                  */
/*                                                                       */
/*=======================================================================*/
#include <cstdio>
#include "festival.h"
#include "intonation.h"

enum lr_tpos {tp_start, tp_left, tp_mid, tp_right, tp_end};

static EST_String accent_specified(EST_Item *s);
static EST_String tone_specified(EST_Item *s);
static int after_pause(EST_Item *s);
static int before_pause(EST_Item *s);
static EST_Item *vowel_seg(EST_Item *syl);
static void init_int_lr_params(void);
static void add_target_at(EST_Utterance *u, EST_Item *seg,
			  float val,lr_tpos pos);
static float apply_lr_model(LISP model, EST_FVector &feats);
static void find_feat_values(EST_Item *s, LISP model,EST_FVector &feats);

static LISP Intonation_Endtone_Tree_Utt(LISP utt);  // ... mh 99-08-06
static LISP Intonation_Accent_Tree_Utt(LISP utt);

static float target_f0_mean = 0.0;
static float target_f0_std = 1.0;
static float model_f0_mean = 0.0;
static float model_f0_std = 1.0;

#define MZSCORE(X) (((X)-model_f0_mean)/model_f0_std)
#define UNTZSCORE(X) (((X)*target_f0_std)+target_f0_mean)
#define MAP_F0(X) (UNTZSCORE(MZSCORE(X)))

LISP FT_Intonation_Tree_Utt(LISP utt)
{
    // For each syllable predict intonation events.  Potentially
    // two forms, accents and ent tones
    EST_Utterance *u = get_c_utt(utt);

    u->create_relation("IntEvent");
    u->create_relation("Intonation");

    utt = Intonation_Endtone_Tree_Utt(utt);
    utt = Intonation_Accent_Tree_Utt(utt);

    return utt;
}

LISP Intonation_Accent_Tree_Utt(LISP utt)
{
    // For each syllable predict intonation events.
    // here only accents
    EST_Utterance *u = get_c_utt(utt);
    EST_Item *s;
    EST_String paccent;
    LISP accent_tree;

    accent_tree = siod_get_lval("int_accent_cart_tree","no accent tree");

    for (s=u->relation("Syllable")->first(); s != 0; s=inext(s))
    {
	if ((paccent = accent_specified(s)) == "0") // check if pre-specified
	    paccent = (EST_String)wagon_predict(s,accent_tree);
	if (paccent != "NONE")
	    add_IntEvent(u,s,paccent);
    }
    return utt;
}

LISP Intonation_Endtone_Tree_Utt(LISP utt)
{
    // For each syllable predict intonation events.
    // here only endtones
    EST_Utterance *u = get_c_utt(utt);
    EST_Item *s;
    EST_String ptone;
    LISP endtone_tree;

    endtone_tree = siod_get_lval("int_tone_cart_tree","no tone cart tree");

    for (s=u->relation("Syllable")->first(); s != 0; s=inext(s))
    {
	if ((ptone = tone_specified(s)) == "0")
	    ptone = (EST_String)wagon_predict(s,endtone_tree);
	if (ptone != "NONE")
	    add_IntEvent(u,s,ptone);
    }
    return utt;
}

static EST_String accent_specified(EST_Item *s)
{
    // If there is an explicit accent specifed on the related token
    // If there is check the syllable to see if its stress or a singleton
    EST_Item *word = parent(s,"SylStructure");
    if (!word) return "0";
    EST_Item *token = parent(word,"Token");
    EST_String paccent("0");
    if (token)
	paccent = (EST_String)ffeature(token,"accent");
	
    if (paccent == "0")
    {
	paccent = (EST_String)ffeature(word,"accent");
	if (paccent == "0")
	    return paccent;
    }
    if (ffeature(s,"stress") == "1")
    {   // only goes on first stressed syllable
	EST_Item *p;
	for (p=iprev(as(s,"SylStructure")); p != 0; p=iprev(p))
	    if (ffeature(s,"stress") == "1")
		return "NONE";  // specified but not on this syllable
	return paccent;  // first stressed syl in word
    }
    else if (daughter1(word)->length() == 1)
	return paccent;
    else
	return "NONE";  // pre-specified but inappropriate syllable in word
}

static EST_String tone_specified(EST_Item *s)
{
    // If there is an explicit accent specifed on the related token
    // If there is check the syllable to see if its strees or a singleton
    EST_Item *ss = s->as_relation("SylStructure");
    EST_Item *word = parent(ss);
    if (!word) return "0";
    EST_Item *token = parent(word,"Token");
    EST_String ptone("0");
    if (token)
	ptone = (EST_String)ffeature(token,"tone");

    if (ptone == "0")
    {
	ptone = (EST_String)ffeature(word,"tone");
	if (ptone == "0")
	    return ptone;
    }
    if (inext(ss) == 0)  // final syllable in word
	return ptone;
    else
	return "NONE";  // pre-specified but inappropriate syllable in word
}

LISP FT_Int_Targets_LR_Utt(LISP utt)
{
    // Predict F0 targets using Linear regression
    EST_Utterance *u = get_c_utt(utt);
    EST_Item *s;
    float pstart, pmid, pend;
    LISP start_lr, mid_lr, end_lr;

    init_int_lr_params();
    // Note the models must *all* be the same size
    start_lr = siod_get_lval("f0_lr_start","no f0 start lr model");
    mid_lr = siod_get_lval("f0_lr_mid","no f0 mid lr model");
    end_lr = siod_get_lval("f0_lr_end","no f0 end lr model");
    
    u->create_relation("Target");
    pend = 0;
    EST_FVector feats;
    feats.resize(siod_llength(start_lr));

    for (s=u->relation("Syllable")->first(); s != 0; s=inext(s))
    {
	find_feat_values(s,start_lr,feats);
	pstart = apply_lr_model(start_lr,feats);
	pstart = MAP_F0(pstart);
	if (after_pause(s))
	    add_target_at(u,daughter1(s,"SylStructure"),pstart,tp_start);
	else
	    add_target_at(u,daughter1(s,"SylStructure"),
			  (pstart+pend)/2.0,tp_start);

	pmid = apply_lr_model(mid_lr,feats);
	pmid = MAP_F0(pmid);
	add_target_at(u,vowel_seg(s),pmid,tp_mid);

	pend = apply_lr_model(end_lr,feats);
	pend = MAP_F0(pend);
	if (before_pause(s))
	    add_target_at(u,daughtern(s,"SylStructure"),pend,tp_end);
    }

    return utt;

}

LISP FT_Int_Targets_LR_5_Utt(LISP utt)
{
  // Predict F0 targets using Linear regression
  // This version uses 5 points rather than 3.
   EST_Utterance *u = get_c_utt(utt);
    EST_Item *s;
    float pstart, pleft, pmid, pright, pend;
    LISP start_lr, left_lr, mid_lr, right_lr, end_lr;

    init_int_lr_params();
    // Note the models must *all* be the same size
    start_lr = siod_get_lval("f0_lr_start","no f0 start lr model");
    left_lr = siod_get_lval("f0_lr_left","no f0 left lr model");
    mid_lr = siod_get_lval("f0_lr_mid","no f0 mid lr model");
    right_lr = siod_get_lval("f0_lr_right","no f0 right lr model");
    end_lr = siod_get_lval("f0_lr_end","no f0 end lr model");
    
    u->create_relation("Target");
    pend = 0;
    EST_FVector feats;
    feats.resize(siod_llength(start_lr));

    for (s=u->relation("Syllable")->first(); s != 0; s=inext(s))
    {
	find_feat_values(s,start_lr,feats);
	pstart = apply_lr_model(start_lr,feats);
	pstart = MAP_F0(pstart);
	if (after_pause(s))
	    add_target_at(u,daughter1(s,"SylStructure"),pstart,tp_start);
	else
	    add_target_at(u,daughter1(s,"SylStructure"),
			  (pstart+pend)/2.0,tp_start);

	pleft = apply_lr_model(left_lr,feats);
	pleft = MAP_F0(pleft);
	add_target_at(u,vowel_seg(s),pleft,tp_left);
	pmid = apply_lr_model(mid_lr,feats);
	pmid = MAP_F0(pmid);
	add_target_at(u,vowel_seg(s),pmid,tp_mid);
	pright = apply_lr_model(right_lr,feats);
	pright = MAP_F0(pright);
	add_target_at(u,vowel_seg(s),pright,tp_right);

	pend = apply_lr_model(end_lr,feats);
	pend = MAP_F0(pend);
	if (before_pause(s))
	    add_target_at(u,daughtern(s,"SylStructure"),pend,tp_end);
    }

    return utt;

}


#define FFEATURE_NAME(X) (get_c_string(car(X)))
#define FFEATURE_WEIGHT(X) (get_c_float(car(cdr(X))))
#define FFEATURE_MAPCLASS(X) (car(cdr(cdr(X))))

static void find_feat_values(EST_Item *s, LISP model,EST_FVector &feats)
{
    EST_Val v = 0.0;
    int i;
    LISP f;
    const char *ffeature_name, *last_name="";

    feats[0] = 1;
    for (i=1,f=cdr(model); CONSP(f); f=CDR(f),i++)
    {
	ffeature_name = FFEATURE_NAME(CAR(f));
	if (!streq(ffeature_name,last_name))
	    v = ffeature(s,ffeature_name);
	if (siod_llength(CAR(f)) == 3)
	{   // A map class is specified
	    if (siod_member_str(v.string(),FFEATURE_MAPCLASS(CAR(f))) != NIL)
		feats[i] = 1;
	    else
		feats[i] = 0;
	}
	else
	    feats[i] = (float)v;
	last_name = ffeature_name;
    }
}

static float apply_lr_model(LISP model, EST_FVector &feats)
{
    float answer = FFEATURE_WEIGHT(car(model));
    int i;
    LISP f;

    for(i=1,f=cdr(model); i<feats.n(); f=cdr(f),i++)
	answer += feats.a_no_check(i) * FFEATURE_WEIGHT(CAR(f));

    return answer;
}

static void init_int_lr_params(void)
{
    LISP params;

    params = siod_get_lval("int_lr_params","no lr params");

    target_f0_mean = get_param_float("target_f0_mean",params,0.0);
    target_f0_std = get_param_float("target_f0_std",params,1.0);
    model_f0_mean = get_param_float("model_f0_mean",params,0.0);
    model_f0_std = get_param_float("model_f0_std",params,1.0);
}


static void add_target_at(EST_Utterance *u, EST_Item *seg,
			  float val,lr_tpos pos)
{
    // Add a target to segment at position

    if (seg == 0)
    {
	cerr << "Int_Tree: failed to find seg related to syllable for target."
	    << endl;
	return;
    }
  
  if (pos == tp_start)
    add_target(u,seg,ffeature(seg,"segment_start").Float(),val);
  else if (pos == tp_left)
    add_target(u,seg,
	       0.5*(ffeature(seg,"segment_mid").Float()+
		    ffeature(seg,"segment_start").Float()),
	       val);
  else if (pos == tp_mid)
    add_target(u,seg,ffeature(seg,"segment_mid").Float(),val);
  else if (pos == tp_right)
    add_target(u,seg,
	       0.5*(ffeature(seg,"segment_mid").Float()+
		    seg->F("end")),
	       val);
  else if (pos == tp_end)
    add_target(u,seg,seg->F("end"),val);
  else
    {
	cerr << "add_target_at: unknown position type\n";
	festival_error();
    }
}

static int after_pause(EST_Item *s)
{
    // TRUE if segment immediately previous to this is a silence
    EST_Item *p;
    if (iprev(s) == 0)
	return TRUE;
    EST_Item *ss = s->as_relation("SylStructure");
    if (iprev(s) == iprev(ss))
	return FALSE;

    p = iprev(daughter1(ss)->as_relation("Segment"));
    if (p == 0)
	return TRUE;
    else if (ph_is_silence(p->name()))
	return TRUE;
    else
	return FALSE;
}

static int before_pause(EST_Item *s)
{
    // TRUE is segment immediately after this is a silence
    if (inext(s) == 0)
	return TRUE;
    EST_Item *ss = s->as_relation("SylStructure");
    EST_Item *n = inext(daughtern(ss)->as_relation("Segment"));
    if (ph_is_silence(n->name()))
	return TRUE;
    else 
	return FALSE;
}

static EST_Item *vowel_seg(EST_Item *syl)
{
    // return related to vowel segment
    EST_Item *p;

    for (p=daughter1(syl,"SylStructure"); p != 0; p=inext(p))
	if (ph_is_vowel(p->name()))
	    return p;

    // No vowel found, so return first daughter.
    return daughter1(syl,"SylStructure");
}
    
    
