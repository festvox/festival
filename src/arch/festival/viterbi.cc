/*************************************************************************/
/*                                                                       */
/*                Centre for Speech Technology Research                  */
/*                     University of Edinburgh, UK                       */
/*                         Copyright (c) 1999                            */
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
/*                 Authors:  Alan W Black                                */
/*                 Date   :  February 1999                               */
/*-----------------------------------------------------------------------*/
/*  Generic Viterbi search specifications through scheme                 */
/*                                                                       */
/*=======================================================================*/
#include <cstdio>
#include "festival.h"
#include "lexicon.h"

static EST_VTCandidate *gv_candlist(EST_Item *s,EST_Features &f);
static EST_VTPath *gv_npath(EST_VTPath *p,EST_VTCandidate *c,EST_Features &f);
static double gv_find_wfst_prob(EST_VTPath *p,EST_WFST *wfst,
				int n,int &state);
static double gv_find_ngram_prob(EST_VTPath *p,EST_Ngrammar *ngram,
				 int n,int &state,EST_Features &f);

LISP Gen_Viterbi(LISP utt)
{
    // For each syllable predict intonation events.
    EST_Utterance *u = utterance(utt);
    LISP params = siod_get_lval("gen_vit_params","no gen_vit_params");
    EST_Features f;
    EST_WFST *wfst = 0;
    EST_Ngrammar *ngram = 0;
    int num_states;

    // Add some defaults
    f.set("gscale_s",1.0);
    f.set("gscale_p",0.0);
    f.set("Relation","Syllable");
    f.set("return_feat","gen_vit_val");
    lisp_to_features(params,f);

    if (f.present("ngramname"))
    {
	ngram = get_ngram(f.S("ngramname"));
	num_states = ngram->num_states();
    }
    else
    {
	wfst = get_wfst(f.S("wfstname"));
	num_states = wfst->num_states();
    }

    EST_Viterbi_Decoder v(gv_candlist,gv_npath,num_states);
    v.f = f;

    v.initialise(u->relation(f.S("Relation")));
    v.search();
    v.result("gv_id");
    if (f.present("debug"))
    {
	v.copy_feature("nprob");
	v.copy_feature("prob");
	v.copy_feature("score");
	v.copy_feature("total_score");
    }

    // Map internal ids back to strings 
    for (EST_Item *p=u->relation(f.S("Relation"))->head(); p != 0; p=inext(p))
	if (wfst == 0)
	    p->set(f.S("return_feat"),ngram->get_vocab_word(p->I("gv_id")));
	else
	    p->set(f.S("return_feat"),wfst->in_symbol(p->I("gv_id")));

    return utt;
}

static EST_VTCandidate *gv_candlist(EST_Item *s,EST_Features &f)
{
    LISP p;
    LISP l;
    EST_VTCandidate *c;
    EST_VTCandidate *all_c = 0;
    EST_WFST *w = 0;
    EST_Ngrammar *n = 0;
    float prob;

    // Call user function to get candidate probabilities
    p = leval(cons(rintern(f.S("cand_function")),
		   cons(siod(s),NIL)),NIL);
    if (f.present("ngramname"))
	n = get_ngram(f.S("ngramname"));
    else
	w = get_wfst(f.S("wfstname"));

    for (l=p; l != NIL; l=cdr(l))
    {
	prob = get_c_float(car(cdr(car(l))));
	if (f.present("debug"))
	    s->set(EST_String("cand_")+get_c_string(car(car(l))),prob);
	if (prob != 0)
	{
	    c = new EST_VTCandidate;
	    if (w == 0)
		c->name = n->get_vocab_word(get_c_string(car(car(l))));
	    else
		c->name = w->in_symbol(get_c_string(car(car(l))));
	    c->score = log(prob);
	    c->s = s;
	    c->next = all_c;
	    all_c = c;
	}
    }
    return all_c;
}

static EST_VTPath *gv_npath(EST_VTPath *p,EST_VTCandidate *c,EST_Features &f)
{
    EST_VTPath *np = new EST_VTPath;
    double prob,lprob;
    EST_WFST *wfst = 0;
    EST_Ngrammar *ngram = 0;

    if (f.present("ngramname"))
	ngram = get_ngram(f.S("ngramname"));
    else
	wfst = get_wfst(f.S("wfstname"));

    np->c = c;
    np->from = p;
    int n = c->name.Int();
    if (wfst == 0)
	prob = gv_find_ngram_prob(p,ngram,n,np->state,f);
    else
	prob = gv_find_wfst_prob(p,wfst,n,np->state);

    prob = f.F("gscale_p") + (prob * (1-f.F("gscale_p")));

    if (prob == 0)
	lprob = log(0.00000001);
    else
	lprob = log(prob);
    
    if (p==0)
	np->score = (c->score+lprob);
    else
	np->score = (c->score+lprob) + p->score;
    
    if (f.present("debug"))
    {
	np->f.set("prob",prob);
	np->f.set("score",c->score);
	np->f.set("nprob",prob*(exp(c->score)));
	np->f.set("total_score",np->score);
    }

    return np;
}
    
static double gv_find_wfst_prob(EST_VTPath *p,EST_WFST *wfst,
				int n,int &state)
{
    float prob;
    int oldstate;

    if (p == 0)
	oldstate = wfst->start_state();
    else
	oldstate = p->state;
    state = wfst->transition(oldstate,n,n,prob);
    return prob;
}

static double gv_find_ngram_prob(EST_VTPath *p,EST_Ngrammar *ngram,
				 int n,int &state,EST_Features &f)
{
    int oldstate=0;
    double prob;

    if (p == 0)
    {
        // This could be done once before the search is called
	int order = ngram->order();
	int i;
	EST_IVector window(order);
	
	if (order > 1)
	    window.a_no_check(order-1) = n;
	if (order > 2)
	    window.a_no_check(order-2) = 
		ngram->get_vocab_word(f.S("p_word"));
	for (i = order-3; i>=0; i--)
	    window.a_no_check(i) =
		ngram->get_vocab_word(f.S("pp_word"));
	oldstate = ngram->find_state_id(window);
    }
    else
	oldstate = p->state;
    state = ngram->find_next_state_id(oldstate,n);
    const EST_DiscreteProbDistribution &pd = ngram->prob_dist(oldstate);
    if (pd.samples() == 0)
	prob = 0;
    else
	prob = (double)pd.probability(n);

    return prob;
}
    
