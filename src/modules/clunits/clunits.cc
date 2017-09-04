/*************************************************************************/
/*                                                                       */
/*                   Carnegie Mellon University and                      */
/*                Centre for Speech Technology Research                  */
/*                     University of Edinburgh, UK                       */
/*                       Copyright (c) 1998-2001                         */
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
/*  THE UNIVERSITY OF EDINBURGH, CARNEGIE MELLON UNIVERSITY AND THE      */
/*  CONTRIBUTORS TO THIS WORK DISCLAIM ALL WARRANTIES WITH REGARD TO     */
/*  THIS SOFTWARE, INCLUDING ALL IMPLIED WARRANTIES OF MERCHANTABILITY   */
/*  AND FITNESS, IN NO EVENT SHALL THE UNIVERSITY OF EDINBURGH, CARNEGIE */
/*  MELLON UNIVERSITY NOR THE CONTRIBUTORS BE LIABLE FOR ANY SPECIAL,    */
/*  INDIRECT OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES WHATSOEVER          */
/*  RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN  AN ACTION   */
/*  OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF     */
/*  OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.       */
/*                                                                       */
/*************************************************************************/
/*             Author :  Alan W Black                                    */
/*             Date   :  April 1998                                      */
/*-----------------------------------------------------------------------*/
/*                                                                       */
/*  Yet another unit selection method.                                   */
/*                                                                       */
/*  Using an acoustic measure find the distance between all units in the */
/*  db.  Try to minimise the mean difference between units in a cluster  */
/*  using CART technology, based on features like phonetic and prosodic  */
/*  context.  This gives a bunch of CARTs for each unit type in the db   */
/*  which are acoustically close.  Use these as candidates and optimise  */
/*  a path through them minimising join using a viterbi search.          */
/*                                                                       */
/*  Advantages:                                                          */
/*    requires little or no measurements at selection time               */
/*    allows for clear method of pruning                                 */
/*    no weights need to be generated (well, except where they do)       */
/*    will optimise appropriately with varying numbers of example units  */
/*                                                                       */
/*  Disadvantages:                                                       */
/*    Units can't cross between clusters                                 */
/*                                                                       */
/*  Implementation of Black, A. and Taylor, P. (1997). Automatically     */
/*  clustering similar units for unit selection in speech synthesis      */
/*  Proceedings of Eurospeech 97, vol2 pp 601-604, Rhodes, Greece.       */
/*                                                                       */
/*  postscript: http://www.cs.cmu.edu/~awb/papers/ES97units.ps           */
/*  http://www.cs.cmu.edu/~awb/papers/ES97units/ES97units.html           */
/*                                                                       */
/*  Comments:                                                            */
/*                                                                       */
/*  This is a new implementation using the newer unit selection/signal   */
/*  processing archtecture in festival                                   */
/*                                                                       */
/*  This is still in development but become more stable.  It is robust   */
/*  for many cases, though a lot depends on the db and parameters        */
/*  you use                                                              */
/*                                                                       */
/*  This had significant new work (and bug fixes) done on it when awb    */
/*  moved to CMU                                                         */
/*                                                                       */
/*=======================================================================*/
#include <cstdlib>
#include "EST_math.h"
#include "festival.h"
#include "clunits.h"

static EST_String static_unit_prev_move = "unit_prev_move";
static EST_String static_unit_this_move = "unit_this_move";
static EST_String static_jscore = "local_join_cost";
static EST_String static_tscore = "local_target_cost";
static EST_String static_cscore = "cummulative_unit_score";

static void setup_clunits_params();
static EST_VTCandidate *TS_candlist(EST_Item *s,EST_Features &f);
static EST_VTPath *TS_npath(EST_VTPath *p,EST_VTCandidate *c,EST_Features &f);
static float naive_join_cost(CLunit *unit0, CLunit *unit1,
			     EST_Item *s,
			     float &u0_move,
			     float &u1_move);
static float optimal_couple(CLunit *u0,
			    CLunit *u1,
			    float &u0_move,
			    float &u1_move,
			    int type,
			    float different_prev_pen,
			    float non_consecutive_pen);
static void cl_parse_diphone_times(EST_Relation &diphone_stream, 
				   EST_Relation &source_lab);

VAL_REGISTER_CLASS_NODEL(vtcand,EST_VTCandidate);
VAL_REGISTER_CLASS_NODEL(clunit,CLunit);

LISP selection_trees = NIL;
LISP clunits_params = NIL;
static int optimal_coupling = 0;
static int extend_selections = 0;
static int clunits_debug = 0;
static int clunits_log_scores = 0;
static int clunits_smooth_frames = 0;
float continuity_weight = 1;
float f0_join_weight = 0.0;
float different_prev_pen = 1000.0;
float non_consecutive_pen = 100.0;
static EST_String clunit_name_feat = "name";

static CLDB *cldb;

static LISP clunits_select(LISP utt)
{
    // Select units from db using CARTs to index into clustered unit groups
    EST_Utterance *u = get_c_utt(utt);
    EST_Item *s, *f;

    cldb = check_cldb();  // make sure there is one loaded
    setup_clunits_params();

    f = u->relation("Segment")->head();
    for (s=f; s; s=inext(s))
	s->set_val("clunit_name",ffeature(s,clunit_name_feat));

    if (f)
    {
	EST_Viterbi_Decoder v(TS_candlist,TS_npath,-1);
	v.set_big_is_good(FALSE);  // big is bad

	v.initialise(u->relation("Segment"));
	v.search();
	if (!v.result("unit_id"))
	{
	    cerr << "CLUNIT: failed to find path\n";
	    return utt;
	}
	v.copy_feature(static_unit_this_move);
	v.copy_feature(static_unit_prev_move);
	v.copy_feature(static_jscore);
	v.copy_feature(static_tscore);
	v.copy_feature(static_cscore);
    }

    return utt;
}

static LISP clunits_get_units(LISP utt)
{
    // Create unit stream and loading params
    EST_Utterance *u = get_c_utt(utt);
    EST_Relation *units,*ss;
    EST_Item *s;

    cldb = check_cldb();  // make sure there is one loaded

    units = u->create_relation("Unit");
    for (s=u->relation("Segment")->head(); s != 0; s=inext(s))
    {
	EST_Item *unit = units->append();
	CLunit *db_unit = clunit(s->f("unit_id"));
	float st,e;
	unit->set_name(db_unit->name);
	unit->set("fileid",db_unit->fileid);
	// These should be modified from the optimal coupling
	if ((iprev(s)) && (s->f_present("unit_this_move")))
	    st = s->F("unit_this_move");
	else
	    st = db_unit->start;
	if (inext(s) && (inext(s)->f_present("unit_prev_move")))
	    e = inext(s)->F("unit_prev_move");
	else
	    e = db_unit->end;
	if ((e-st) < 0.011)
	    e = st + 0.011;
	unit->set("start",st);
	unit->set("middle",db_unit->start);
	unit->set("end",e);
	unit->set("unit_start",st);
	unit->set("unit_middle",db_unit->start);
	unit->set("unit_end",e);
	unit->set("seg_start",db_unit->start);
	unit->set("seg_end",db_unit->end);
	cldb->load_coefs_sig(unit);
	if (clunits_debug)
	    printf("unit: %s fileid %s start %f end %f\n",
		   (const char *)db_unit->name,
		   (const char *)db_unit->fileid,
		   st,e);
    }

    // Make it look as much like the diphones as possible for
    // the rest of the code
    ss = u->create_relation("SourceSegments");
    for (s = u->relation("Segment")->head(); s != 0 ; s = inext(s))
    {
	EST_Item *d = ss->append();
	d->set_name(ffeature(s,"clunit_name"));
    }

    cl_parse_diphone_times(*units,*ss);

    return utt;
}

static void cl_parse_diphone_times(EST_Relation &diphone_stream, 
				   EST_Relation &source_lab)
{
    EST_Item *s, *u;
    EST_Track *pm;
    int e_frame, m_frame = 0;
    float dur_1 = 0.0, dur_2 = 0.0, p_time;
    float t_time = 0.0, end;
    p_time = 0.0;
    
    for (s = source_lab.head(), u = diphone_stream.head(); u;
         u = inext(u), s = inext(s))
    {
	pm = track(u->f("coefs"));
	if (pm == 0)
	{
	    cerr << "CLUNIT: couldn't get pitchmarks for " << u->name() << endl;
	    festival_error();
	}
	
	e_frame = pm->num_frames() - 1;
	m_frame = u->I("middle_frame");

	dur_1 = pm->t(m_frame);
	dur_2 = pm->t(e_frame) - dur_1;
	
	s->set("end", (dur_1 + p_time));
	p_time = s->F("end") + dur_2;

	end = dur_1 + dur_2 + t_time;
	t_time = end;
	u->set("end", t_time);
    }
    if (s)
	s->set("end", (dur_2 + p_time));
}

static LISP clunits_simple_wave(LISP utt)
{
    // Naive joining of waveforms
    EST_Utterance *u = get_c_utt(utt);
    EST_Wave *w = new EST_Wave;
    EST_Wave *w1 = 0;
    EST_Item *witem = 0;
    EST_Item *s;
    int size,i,k,c;

    for (size=0,s=u->relation("Unit")->head(); s != 0; s = inext(s))
	size += wave(s->f("sig"))->num_samples();

    if (u->relation("Unit")->head())
    {   // This will copy the necessary wave features across
	s = u->relation("Unit")->head();
	*w = *(wave(s->f("sig")));
    }
    i = w->num_samples();
    w->resize(size); // its maximum size
    for (s=inext(u->relation("Unit")->head()); s; s=inext(s))
    {
	w1 = wave(s->f("sig"));
	// Find last zero crossing
	for (c=0; ((i > 0) && (c < 40)); c++,i--)
	    if (((w->a_no_check(i) < 0) && (w->a_no_check(i-1) >= 0)) ||
		((w->a_no_check(i) >= 0) && (w->a_no_check(i-1) < 0)))
		break;
	if (c == 40) i += 40;
	// Find next zero crossing
	for (c=0,k=1; ((k < w1->num_samples()) && (c < 40)); k++,i++)
	    if (((w1->a_no_check(k) < 0) && (w1->a_no_check(k-1) >= 0)) ||
		((w1->a_no_check(k) >= 0) && (w1->a_no_check(k-1) < 0)))
		break;
	if (c == 40) k -= 40;
	for (; k < w1->num_samples(); k++,i++)
	    w->a_no_check(i) = w1->a_no_check(k);
    }
    w->resize(i);

    witem = u->create_relation("Wave")->append();
    witem->set_val("wave",est_val(w));

    return utt;
}

static LISP clunits_windowed_wave(LISP utt)
{
    // windowed join, no prosodic modification
    EST_Utterance *u = get_c_utt(utt);
    EST_Wave *w = new EST_Wave;
    EST_Wave *w1 = 0;
    EST_Track *t1 = 0;
    EST_Item *witem = 0;
    EST_Item *s;
    int size,i,k,wi,samp_idx, l_samp_idx;
    int width, lwidth;
    EST_Wave *www=0;

    for (size=0,s=u->relation("Unit")->head(); s != 0; s = inext(s))
	size += wave(s->f("sig"))->num_samples();

    if (u->relation("Unit")->head())
    {   // This will copy the necessary wave features across
	s = u->relation("Unit")->head();
	www = wave(s->f("sig"));
	*w = *www;
    }
    w->resize(size); // its maximum size
    wi=0;
    lwidth = width = 0;
    for (s=u->relation("Unit")->head(); s; s=inext(s))
    {
	w1 = wave(s->f("sig"));
	t1 = track(s->f("coefs"));

	l_samp_idx = 0;
	for (i=0; i < t1->num_frames()-1; i++)
	{
	    samp_idx = (int)(t1->t(i)*w->sample_rate());
	    width = samp_idx - l_samp_idx;
	    if (clunits_smooth_frames && (i==0) && (lwidth != 0))
		width = (width+lwidth)/2;  // not sure if this is worth it
	    wi += width;
	    for (k=-width; ((k<width)&&((samp_idx+k)<w1->num_samples())) ;k++)
		w->a(wi+k) += 
		    (int)(0.5*(1+cos((PI/(double)(width))*(double)k))*
			w1->a(samp_idx+k));
	    l_samp_idx = samp_idx;
	}
	lwidth = width;
    }
    w->resize(wi);

    witem = u->create_relation("Wave")->append();
    witem->set_val("wave",est_val(w));

    return utt;
}

static LISP clunits_smoothedjoin_wave(LISP utt)
{
    // Actually not very smoothed yet, just joined
    EST_Utterance *u = get_c_utt(utt);
    EST_Wave *w = new EST_Wave;
    EST_Wave *w1 = 0;
    EST_Item *witem = 0;
    EST_Item *s;
    int size,i,wi;
    int samp_end, samp_start;
    EST_Wave *www=0;

    for (size=0,s=u->relation("Unit")->head(); s != 0; s = inext(s))
    {
	samp_end = s->I("samp_end");
	samp_start = s->I("samp_start");
	size += samp_end-samp_start;
    }

    if (u->relation("Unit")->head())
    {   // This will copy the necessary wave features across
	s = u->relation("Unit")->head();
	www = wave(s->f("sig"));
	*w = *www;
    }
    w->resize(size); // its maximum size
    wi=0;
    for (s=u->relation("Unit")->head(); s; s=inext(s))
    {
	samp_end = s->I("samp_end");
	samp_start = s->I("samp_start");
	w1 = wave(s->f("sig"));
/*	printf("%s %s %f %f %d %d\n",
	       (const char *)s->S("name"),
	       (const char *)s->S("fileid"),
	       (float)samp_start/(float)w->sample_rate(),
	       (float)samp_end/(float)w->sample_rate(),
	       w1->num_samples(),
	       samp_end); */
	for (i=samp_start; i<samp_end; i++,wi++)
	    w->a_no_check(wi) = w1->a_no_check(i);
/*	printf("%d %f\n",wi,(float)wi/(float)w->sample_rate()); */
    }
    w->resize(wi);

    witem = u->create_relation("Wave")->append();
    witem->set_val("wave",est_val(w));

    return utt;
}

static void setup_clunits_params()
{
    // Set up params
    clunits_params = siod_get_lval("clunits_params",
				    "CLUNITS: no parameters set for module");
    optimal_coupling = get_param_int("optimal_coupling",clunits_params,0);
    different_prev_pen = get_param_float("different_prev_pen",clunits_params,1000.0);
    non_consecutive_pen = get_param_float("non_consectutive_pen",clunits_params,100.0);
    extend_selections = get_param_int("extend_selections",clunits_params,0);
    continuity_weight = get_param_float("continuity_weight",clunits_params,1);
    f0_join_weight = get_param_float("f0_join_weight",clunits_params,0.0);
    clunits_debug = get_param_int("clunits_debug",clunits_params,0);
    clunits_log_scores = get_param_int("log_scores",clunits_params,0);
    clunits_smooth_frames = get_param_int("smooth_frames",clunits_params,0);
    clunit_name_feat = get_param_str("clunit_name_feat",clunits_params,"name");
    selection_trees = 
	siod_get_lval("clunits_selection_trees",
		      "CLUNITS: clunits_selection_trees unbound");
}

static EST_VTCandidate *TS_candlist(EST_Item *s,EST_Features &f)
{
    // Return a list of candidate units for target s
    // Use the appropriate CART to select a small group of candidates
    EST_VTCandidate *all_cands = 0;
    EST_VTCandidate *c, *gt;
    LISP tree,group,l,pd,cc,ls;
    EST_String name;
    EST_String lookingfor;
    CLunit *u;
    int bbb,ccc;
    float cluster_mean;
    (void)f;
    bbb=ccc=0;

    lookingfor = s->S("clunit_name");
    ls = siod(s);

    cc = siod_get_lval("clunits_cand_hooks",NULL);
    if (cc)
	pd = apply_hooks(siod_get_lval("clunits_cand_hooks",NULL),
			 ls);
    else
    {
	tree = car(cdr(siod_assoc_str(lookingfor,selection_trees)));
	pd = wagon_pd(s,tree);
    }
    if (pd == NIL)
    {
	cerr << "CLUNITS: no predicted class for " << 
	    s->S("clunit_name") << endl;
	festival_error();
    }
    group = car(pd);
    cluster_mean = get_c_float(car(cdr(pd)));
    
    for (bbb=0,l=group; l != NIL; l=cdr(l),bbb++)
    {
	c = new EST_VTCandidate;
	name = s->S("clunit_name")+"_"+get_c_string(car(car(l)));
	u = cldb->get_unit(name);
	if (u == 0)
	{
	    cerr << "CLUNITS: failed to find unit " << name <<
		" in index" << endl;
	    festival_error();
	}
	cldb->load_join_coefs(u);
	c->name = est_val(u);
	c->s = s;
	// Mean distance from others in cluster (could be precalculated)
	c->score = get_c_float(car(cdr(car(l))))-cluster_mean;
	c->score *= c->score;
	// Maybe this should be divided by overall mean of set
	// to normalise this figure (?)

	c->next = all_cands;
	all_cands = c;
    }

    if (extend_selections)
    {
	// An experiment, for all candidates of the previous 
	// item whose following is of this phone type, include
	// them as a candidate
	EST_Item *ppp = iprev(s);
	if (ppp)
	{
	    EST_VTCandidate *lc = vtcand(ppp->f("unit_cands"));
	    for (ccc=0 ; lc && (ccc < extend_selections); lc = lc->next)
	    {
		CLunit *unit = clunit(lc->name);
		CLunit *next_unit;

		if (unit->next_unit)
		    next_unit = unit->next_unit;
		else
		    continue;
		EST_String ss;
		ss = next_unit->name.before("_");
		if (ss.matches(".*_.*_.*"))
		{
		    ss += "_";
		    ss += next_unit->name.after("_").before("_");
		}
/*		printf("%s %s\n",(const char *)ss, (const char *)lookingfor); */
		for (gt=all_cands; gt; gt=gt->next)
		    if (clunit(gt->name)->name == next_unit->name)
			break;  /* got this one already */
		if ((ss == lookingfor) && (gt == 0))
		{  // its the right type so add it
		    c = new EST_VTCandidate;
		    c->name = est_val(next_unit);
		    cldb->load_join_coefs(next_unit);
		    c->s = s;
		    c->score = 0; 
		    c->next = all_cands;
		    all_cands = c;
		    bbb++;
		    ccc++;
		}
	    }
	}

	s->set_val("unit_cands",est_val(all_cands));
    }
    if (clunits_debug)
	printf("cands %d (extends %d) %s\n",bbb,ccc,(const char *)lookingfor);
    return all_cands;
}

static EST_VTPath *TS_npath(EST_VTPath *p,EST_VTCandidate *c,EST_Features &f)
{
    // Combine candidate c with previous path updating score 
    // with join cost
    float cost;
    EST_VTPath *np = new EST_VTPath;
    CLunit *u0, *u1;
    float u0_move=0.0, u1_move=0.0;
    (void)f;

    np->c = c;
    np->from = p;
    if ((p == 0) || (p->c == 0))
	cost = 0;  // nothing previous to join to
    else
    {
	u0 = clunit(p->c->name);
	u1 = clunit(c->name);
//	printf("u0 %s u1 %s\n",
//	       (const char *)u0->name,
//	       (const char *)u1->name);
	if (optimal_coupling)
	    cost = optimal_couple(u0,u1,u0_move,u1_move,
				  optimal_coupling,
				  different_prev_pen,
				  non_consecutive_pen);
	else // naive measure
	    cost = naive_join_cost(u0,u1,c->s,u0_move,u1_move);
	// When optimal_coupling == 2 the moves will be 0, just the scores
	// are relevant
	if (optimal_coupling == 1)
	{
	    np->f.set(static_unit_prev_move,u0_move); // new (prev) end
	    np->f.set(static_unit_this_move,u1_move); // new start
	}
    }
//    printf("cost %f continuity_weight %f\n", cost, continuity_weight);
    cost *= continuity_weight;
    np->state = c->pos;  // "state" is candidate number
    if (clunits_log_scores && (cost != 0))
	cost = log(cost);

    np->f.set(static_jscore,cost);
    np->f.set(static_tscore,c->score);
    if (p==0)
	np->score = (c->score+cost);
    else
	np->score = (c->score+cost) + p->score;
    np->f.set(static_cscore,np->score);

    if (clunits_debug > 1)
	printf("joining cost %f\n",np->score);
    return np;
}

static float optimal_couple(CLunit *u0,
			    CLunit *u1,
			    float &u0_move,
			    float &u1_move,
			    int type,
			    float different_prev_pen,
			    float non_consecutive_pen
			    )
{
    // Find combination cost of u0 to u1, checking for best
    // frame up to n frames back in u0 and u1.
    // Note this checks the u0 with u1's predecessor, which may or may not
    // be of the same type
    // There is some optimisation here in unit coeff access
    EST_Track *u0_cep, *u1_p_cep;
    float dist, best_val;
    int i,eee;
    int u0_st, u0_end;
    int u1_p_st, u1_p_end;
    int best_u0, best_u1;
    CLunit *u1_p;
    float f;

    u1_p = u1->prev_unit;

    u0_move = u0->end;
    if (u1_p == 0)
	u1_move = 0;
    else
	u1_move = u1_p->end;

    if (u1_p == u0)  // they are consecutive
	return 0.0;
    if (u1_p == 0)   // hacky condition, when there is no previous we'll
	return 0.0;  // assume a good join (should be silence there)
    
    if (u1_p->join_coeffs == 0)
	cldb->load_join_coefs(u1_p);
    // Get indexes into full cep for utterances rather than sub ceps
    u0_cep = u0->join_coeffs;
    u1_p_cep = u1_p->join_coeffs;

    u0_end = u0_cep->num_frames();
    u1_p_end = u1_p_cep->num_frames();

    if (!streq(u1_p->base_name,u0->base_name))
    {   /* prev(u1) is a different phone from u0 so don't slide */
	f = different_prev_pen;
	u0_st = u0_cep->num_frames()-1;
	u1_p_st = u1_p_cep->num_frames()-1;
    }
    else if (type == 2)
    {   /* we'll only check the edge for the join */
	u0_st = u0_cep->num_frames()-1;
	u1_p_st = u1_p_cep->num_frames()-1;
	f = 1;
    }
    else
    {
	u0_st = (int)(u0_cep->num_frames() * 0.33);
	u1_p_st = (int)(u1_p_cep->num_frames() * 0.33);
	f = 1;
    }

    best_u0=u0_end;
    best_u1=u1_p_end;
    best_val = HUGE_VAL;

    // Here we look for the best join without sliding the windows
    if ((u0_end-u0_st) < (u1_p_end-u1_p_st))
	eee = u0_end-u0_st;
    else
	eee = u1_p_end-u1_p_st;
    for (i=0; i < eee; i++)
    {
	dist = frame_distance(*u0_cep,i+u0_st,
			      *u1_p_cep,i+u1_p_st,
			      cldb->cweights,
			      f0_join_weight);
	if (dist < best_val)
	{
	    best_val = dist;
	    best_u0 = i+u0_st;
	    best_u1 = i+u1_p_st;
	}
    }
#if 0
    // This tries *all* possible matches in the pair, its slow
    // and has a tendency to shorten things more than you'd like
    // so we just use the more simple test above.
    int j;
    for (i=u0_st; i < u0_end; i++)
    {
	for (j=u1_p_st; j < u1_p_end; j++)
	{
	    dist = frame_distance(*u0_cep,i,
				  *u1_p_cep,j,
				  cldb->cweights);
	    if (dist < best_val)
	    {
		best_val = dist;
		best_u0 = i;
		best_u1 = j;
	    }
	}
    }
#endif

    if (type == 1)
    {
	u0_move = u0_cep->t(best_u0);
	u1_move = u1_p_cep->t(best_u1);
    }

    return non_consecutive_pen+(best_val*f);
}

static float naive_join_cost(CLunit *unit0, CLunit *unit1,
			     EST_Item *s,
			     float &u0_move,
			     float &u1_move)
{
    // A naive join cost, because I haven't ported the info yet

    u0_move = unit0->end;
    u1_move = unit1->start;

    if (unit0 == unit1)
	return 0;
    else if (unit1->prev_unit->name == unit0->name)
	return 0;
    else if (ph_is_silence(s->name()))
	return 0;
    else if (ph_is_stop(s->name()))
	return 0.2;
    else if (ph_is_fricative(s->name()))
	return 0.3;
    else
	return 1.0;
}

static LISP cldb_load_all_coeffs(LISP filelist)
{
    LISP f;

    cldb = check_cldb();
    for (f=filelist; f; f=cdr(f))
    {
	cldb->get_file_coefs_sig(get_c_string(car(f)));
	cldb->get_file_join_coefs(get_c_string(car(f)));
    }

    return NIL;
}

void festival_clunits_init(void)
{
    // Initialization for clunits selection

    proclaim_module("clunits",
            "Copyright (C) University of Edinburgh and CMU 1997-2010\n");

    gc_protect(&clunits_params);
    gc_protect(&selection_trees);

    festival_def_utt_module("Clunits_Select",clunits_select,
    "(Clunits_Select UTT)\n\
  Select units from current databases using cluster selection method.");

    festival_def_utt_module("Clunits_Get_Units",clunits_get_units,
    "(Clunits_Get_Units UTT)\n\
  Construct Unit relation from the selected units in Segment and extract\n\
  their parameters from the clunit db.");

    festival_def_utt_module("Clunits_Simple_Wave",clunits_simple_wave,
    "(Clunits_Simple_Wave UTT)\n\
  Naively concatenate signals together into a single wave (for debugging).");

    festival_def_utt_module("Clunits_Windowed_Wave",clunits_windowed_wave,
    "(Clunits_Windowed_Wave UTT)\n\
  Use hamming window over edges of units to join them, no prosodic \n\
  modification though.");

    festival_def_utt_module("Clunits_SmoothedJoin_Wave",clunits_smoothedjoin_wave,
    "(Clunits_SmoothedJoin_Wave UTT)\n\
  smoothed join.");

    init_subr_1("clunits:load_db",cl_load_db,
    "(clunits:load_db PARAMS)\n\
  Load index file for cluster database and set up params, and select it.");

    init_subr_1("clunits:select",cldb_select,
    "(clunits:select NAME)\n\
  Select a previously loaded cluster database.");

    init_subr_1("clunits:load_all_coefs",cldb_load_all_coeffs,
    "(clunits:load_all_coefs FILEIDLIST)\n\
  Load in coefficients, signal and join coefficients for each named\n\
  fileid.  This is can be called at startup to to reduce the load time\n\
  during synthesis (though may make the image large).");

    init_subr_0("clunits:list",cldb_list,
    "(clunits:list)\n\
  List names of currently loaded cluster databases.");

    init_subr_2("acost:build_disttabs",make_unit_distance_tables,
    "(acost:build_disttabs UTTTYPES PARAMS)\n\
  Built matrices of distances between each ling_item in each each list\n\
  of ling_items in uttypes.   Uses acoustic weights in PARAMS and save\n\
  the result as a matrix for later use.");

    init_subr_2("acost:utt.load_coeffs",acost_utt_load_coeffs,
    "(acost:utt.load_coeffs UTT PARAMS)\n\
  Load in the acoustic coefficients into UTT and set the Acoustic_Coeffs\n\
  feature for each segment in UTT.");

    init_subr_3("acost:file_difference",ac_distance_tracks,
    "(acost:file_difference FILENAME1 FILENAME2 PARAMS)\n\
  Load in the two named tracks and find the acoustic difference over all\n\
  based on the weights in PARAMS.");

    init_subr_2("cl_mapping", l_cl_mapping,
      "(cl_mapping UTT PARAMS)\n\
  Impose prosody upto some percentage, and not absolutely.");

}
