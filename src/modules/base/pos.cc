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
/*                      Date   :  August 1996                            */
/*-----------------------------------------------------------------------*/
/*                                                                       */
/* Various part-of-speech predciting modules                             */
/*                                                                       */
/*=======================================================================*/
#include <cstdio>
#include "festival.h"
#include "lexicon.h"

static EST_VTCandidate *pos_candlist(EST_Item *s,EST_Features &f);
static EST_VTPath *pos_npath(EST_VTPath *p,EST_VTCandidate *c,EST_Features &f);
static double find_np_prob(EST_VTPath *p,int n,int *state);

static EST_Ngrammar *pos_ngram = 0;

static EST_String zeroString("0");
static int p_word = 0;  // arbitrary numbers
static int n_word = 1;

LISP FT_Classic_POS_Utt(LISP utt)
{
    // Predict part of speech for word stream
    EST_Utterance *u = get_c_utt(utt);
    LISP pos_lex_name, pos_ngram_name;
    LISP lastlex, pos_p_start_tag, pos_pp_start_tag;
    
    *cdebug << "Classic POS module\n";

    pos_lex_name = siod_get_lval("pos_lex_name",NULL);
    if (pos_lex_name == NIL)
	return utt;   // not set so ignore it
    pos_ngram_name = siod_get_lval("pos_ngram_name","no pos ngram name");
    pos_p_start_tag = siod_get_lval("pos_p_start_tag","no prev start tag");
    pos_pp_start_tag = siod_get_lval("pos_pp_start_tag","no prev prev start tag");
    
    lastlex = lex_select_lex(pos_lex_name);

    if ((pos_ngram = get_ngram(get_c_string(pos_ngram_name))) == 0)
    {
	cerr << "POS: no ngram called \"" <<
	    get_c_string(pos_ngram_name) << "\" defined" << endl;
	festival_error();
    }

    p_word = pos_ngram->get_vocab_word(get_c_string(pos_p_start_tag));
    n_word = pos_ngram->get_vocab_word(get_c_string(pos_pp_start_tag));

    EST_Viterbi_Decoder v(pos_candlist,pos_npath,pos_ngram->num_states());
    
    v.initialise(u->relation("Word"));
    v.search();
    v.result("pos_index");

    lex_select_lex(lastlex);

    EST_Item *w;
    EST_String pos;
    LISP l;
    // Map pos tagset to desired set
    LISP pos_map = siod_get_lval("pos_map",NULL);
    for (w=u->relation("Word")->first(); w != 0; w = inext(w))
    {
	// convert pos index into string value
	pos = pos_ngram->get_vocab_word(w->f("pos_index").Int());
	w->set("pos",pos);
	for (l=pos_map; l != NIL; l=cdr(l))
	    if (siod_member_str(pos,car(car(l))) != NIL)
	    {
		w->set("pos",get_c_string(car(cdr(car(l)))));
		break;
	    }
    }

    return utt;
}

static EST_VTCandidate *pos_candlist(EST_Item *s,EST_Features &f)
{
    // Return list of possible pos based on a priori probabilities
    LISP pd,l;
    EST_Item *token;
    EST_VTCandidate *c;
    EST_VTCandidate *all_c = 0;
    EST_String actual_pos;
    (void)f;
    
    if (((actual_pos = s->S("pos","0")) != "0") ||
	(((token = parent(s,"Token")) != 0) &&
	 ((actual_pos = token->S("pos","0")) != "0")))
    {
	// There is an explicit pos specified, so respect it
	pd = cons(make_param_float(actual_pos,1.0),NIL);
	c = new EST_VTCandidate;
	c->name = pos_ngram->get_vocab_word(actual_pos);
	c->score = 1.0;
	c->s = s;
	c->next = 0;
	return c;
    }

    LISP e = lex_lookup_word(s->name(),NIL);
    pd = car(cdr(e));
    
    if (pd == NIL)
    {
	const char *chr = s->name();
	if (strchr("0123456789",chr[0]) != NULL)
	    e = lex_lookup_word("_number_",NIL); // I *know* there is an entry
	else
	    e = lex_lookup_word("_OOV_",NIL); // I *know* there is an entry
	pd = car(cdr(e));
    }

    // Build a candidate for each entry in prob distribution
    for (l=pd; l != NIL; l=cdr(l))
    {
	c = new EST_VTCandidate;
	c->name = pos_ngram->get_vocab_word(get_c_string(car(car(l))));
	c->score = get_c_float(car(cdr(car(l))));
	c->s = s;
	c->next = all_c;
	all_c = c;
    }
    
    return all_c;
}

static EST_VTPath *pos_npath(EST_VTPath *p,EST_VTCandidate *c,EST_Features &f)
{
    // Build a potential new path from previous path and this candidate
    EST_VTPath *np = new EST_VTPath;
//    static EST_String lscorename("lscore");
    double prob;
    double lprob;
    (void)f;
    
    np->c = c;
    np->from = p;
    int n = c->name.Int();
    prob = find_np_prob(p,n,&np->state);
    if (prob == 0)
	lprob = log(0.00000001);
    else
	lprob = log(prob);
    
//    np->set_feature(lscorename,(c->score+lprob));
    if (p==0)
	np->score = (c->score+lprob);
    else
	np->score = (c->score+lprob) + p->score;
    
    return np;
}

static double find_np_prob(EST_VTPath *p,int n,int *state)
{
    int oldstate=0;

    if (p==0)
    {   // This could be done once before the search is called
	int order = pos_ngram->order();
	EST_IVector window(order);
	int i;

	window.a_no_check(order-1) = n;
	window.a_no_check(order-2) = p_word;
	for (i = order-3; i>=0; i--)
	    window.a_no_check(i) = n_word;
	oldstate = pos_ngram->find_state_id(window);
    }
    else
	oldstate = p->state;
    *state = pos_ngram->find_next_state_id(oldstate,n);
    const EST_DiscreteProbDistribution &pd = pos_ngram->prob_dist(oldstate);
    if (pd.samples() == 0)
	return 0;
    else
	return (double)pd.probability(n);
}
