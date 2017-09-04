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
/*             Author :  Paul Taylor                                     */
/*             Date   :  June 1998                                       */
/*-----------------------------------------------------------------------*/
/*                                                                       */
/*             Metrical Tree based Phonology system                      */
/*                                                                       */
/*=======================================================================*/

#include <cmath>
#include "festival.h"
//#include "development/EST_FeatureData.h"
#include "../UniSyn/us_features.h"

/**** FUNCTIONS FOR UNISYN BUILD ****/
LISP FT_add_trans_seg(LISP l_utt, LISP lf_seg);
LISP FT_add_trans_duration(LISP l_utt, LISP lf_seg);
LISP FT_add_trans_metrical_tree(LISP l_utt, LISP lf_input, LISP lf_output);
LISP FT_add_xml_relation(LISP l_utt, LISP xml_file);
LISP FT_add_trans_word(LISP l_utt, LISP lf_word);
LISP FT_add_trans_intonation(LISP l_utt, LISP lf_int, LISP l_add_words);
LISP FT_add_f0_points(LISP l_utt, LISP lf_f0);
LISP FT_add_coefs(LISP l_utt, LISP lf_f0);

LISP FT_syl_to_word_intonation(LISP l_utt);
LISP FT_intonation_diagnostics(LISP l_ref, LISP l_test, LISP l_rel_name, LISP l_valid);
/**** END FUNCTIONS FOR UNISYN BUILD ****/

void extend_tree(EST_Item *m, EST_Item *p, const EST_String &terminal,
		 const EST_String &second_tree);

void parse_words(EST_Utterance &utt);

LISP FT_add_trans_word(LISP l_utt, LISP lf_word, LISP keep_times);

LISP FT_US_add_intonation(LISP utt);
LISP FT_focus_nth_item(LISP utt, LISP lrel, LISP w);
LISP FT_focus_nth_tree_item(LISP utt, LISP lrel, LISP w);
LISP FT_foot_nth_item(LISP utt, LISP w);

void make_prosodic_tree(EST_Item *m, EST_Item *p);

void phrase_factor(EST_Utterance &u, const EST_String &base_stream, 
		    const EST_String &mettree);

void stress_factor1(EST_Utterance &u, const EST_String &base_stream, 
		    const EST_String &m);
void stress_factor2(EST_Utterance &u, const EST_String &base_stream, 
		    const EST_String &m);

void main_stress(EST_Item *s);
void footing(EST_Item *n1);
void add_monotone_targets(EST_Utterance &u, float start_f0,
				    float end_f0);

void parse_words(EST_Utterance &utt);

void syntax_to_metrical_words(EST_Utterance &utt);

void binaryize_tree(EST_Utterance &utt, const EST_String &base_tree,
		    const EST_String &new_tree);

/*void assign_phone_z_scores(EST_Utterance &u, const EST_String &seg_name);

void promote_vowel_z_score(EST_Utterance &u, const EST_String &st_name,
		      const EST_String &syl_name);


void promote_mean_z_score(EST_Utterance &u, const EST_String &st_name,
		      const EST_String &syl_name);

void find_cvc_words(EST_Utterance &u);
*/
void tilt_to_f0(EST_Utterance &u);
void scale_tilt(EST_Relation &ev, float shift, float scale);

void vowel_tilt_to_abs_tilt(EST_Utterance &u);
void targets_to_f0(EST_Relation &targ, EST_Track &f0, const float shift);

void syntax_metrical_tree(EST_Utterance &utt, const EST_String &wordfile);


void tilt_to_f0(EST_Relation &intonation, EST_Relation &f0);

void legal_metrical_tree(EST_Item *s);

LISP FT_focus_nth_item(LISP utt, LISP lrel, LISP w)
{
    EST_Utterance *u = get_c_utt(utt);
    int f = get_c_int(w);
    EST_String relname = get_c_string(lrel);
    EST_Item *n;
    int i;

    cout << "Focusing item " << f << " in relation " << relname << endl;

    for (i = 1, n = u->relation(relname)->head(); n; n = inext(n), ++i)
	if (i == f)
	    break;

    if (n == 0)
    {
	cerr << "Error: Can't focus node " << f << 
	    " in a relation with only " << i << " items\n";
	return NIL;
    }

    main_stress(n->as_relation("MetricalTree"));
    if (siod_get_lval("mettree_debug", NULL) != NIL)
	u->save("focus.utt", "est");

    return utt;
}

LISP FT_focus_nth_tree_item(LISP utt, LISP lrel, LISP w)
{
    EST_Utterance *u = get_c_utt(utt);
    EST_String dir;
    EST_String relname = get_c_string(lrel);
    EST_Item *n;
    LISP l;

    n = u->relation(relname)->head();
    for (l = w; (l != NIL) && (n); l = cdr(l))
    {
	dir = get_c_string(car(l));
	cout << "dir = " << dir << endl;
	if (dir == "l")
	    n = daughter1(n);
	else
	if (dir == "r")
	    n = daughter2(n);
	else
	    cerr << "Bad instruction: " << dir << endl;
    }

    main_stress(n->as_relation("MetricalTree"));
    if (siod_get_lval("mettree_debug", NULL) != NIL)
	u->save("focus.utt", "est");

    return utt;
}

LISP FT_foot_nth_item(LISP utt, LISP w)
{
    EST_Utterance *u = get_c_utt(utt);
    int f = get_c_int(w);
    (void) f;
    (void)u;
    EST_Item *n;
    int i;

    cout << "Footing item " << f << endl;

    for (i = 1, n = u->relation("Syllable")->head(); n; n = inext(n), ++i)
	if (i == f)
	    break;

    if (n == 0)
    {
	cerr << "Error: Can't foot node " << f << 
	    " in a relation with only " << i << " items\n";
	return NIL;
    }

    footing(n->as_relation("MetricalTree"));

    if (siod_get_lval("mettree_debug", NULL) != NIL)
	u->save("foot.utt", "est");

    return utt;
}

LISP FT_US_add_intonation(LISP utt)
{
    (void) utt;
    EST_Utterance *u = get_c_utt(utt);
    EST_String base_int;
    
    /*    lt = siod_get_lval("us_acc_thresh", NULL);
	  t = (lt == NIL) ? DEF_THRESH : get_c_float(lt);
	  
	  lt = siod_get_lval("us_base_int", NULL);
	  if (lt == NIL)
	  base_int = "Syllable";
	  else
	  {
	  char *x = get_c_string(lt);
	  base_int = x;
	  }
	  
	  add_intonation(*u, base_int, t);
	  */
    
    float start_f0 = get_c_float(siod_get_lval("us_start_f0", NULL));
    float end_f0 = get_c_float(siod_get_lval("us_end_f0", NULL));
    
    u->create_relation("f0");
    EST_Track *f0 = new EST_Track;
    EST_Item *a = u->relation("f0")->append();
    a->set_val("f0",est_val(f0));
    
    add_monotone_targets(*u, start_f0, end_f0);
    
    targets_to_f0(*u->relation("Target"), *f0, 0.01);
    
    return utt;
}

LISP FT_extend_tree(LISP l_utt, LISP largs)
{
    EST_Item *p, *m;
    EST_Utterance *u = get_c_utt(l_utt);

    EST_String new_tree = get_c_string(car(largs));
    EST_String first_tree = get_c_string(car(cdr(largs)));
    EST_String second_tree = get_c_string(car(cdr(cdr(largs))));
    EST_String terminal = get_c_string(car(cdr(cdr(cdr(largs)))));

    u->create_relation(new_tree);

    for (m = u->relation(first_tree)->head(); m; m = inext(m))
    {
	p = u->relation(new_tree)->append(m);
	extend_tree(m, p, terminal, second_tree);
    }
    return l_utt;
}

static void add_keep_nodes(EST_Item *n, EST_String keep)
{
    if (n == 0)
	return;

    n->set(keep, 1);
    
    for (EST_Item *p = daughter1(n); p; p = inext(p))
	add_keep_nodes(p, keep);
}

static void remove_sisters(EST_Item *n)
{
    EST_Item *m;
    EST_Item *p = parent(n);
    if (p == 0)
	return;

    for (EST_Item *s = daughter1(p); s; s = m)
    {
	m = inext(s);
	if (s != n)
	    s->unref_all();
    }
    move_sub_tree(n, p);
    remove_sisters(p);
}

LISP FT_copy_sub_tree(LISP l_utt, LISP l_id, LISP l_relation)
{
    EST_Utterance *u = get_c_utt(l_utt);
    EST_Utterance *new_utt = new EST_Utterance;

    new_utt = u;

    EST_Item *n = new_utt->id(get_c_string(l_id))->
	as_relation(get_c_string(l_relation));

    remove_sisters(n);

    // reset n - should now be a root node.
    n = new_utt->id(get_c_string(l_id))->
	as_relation(get_c_string(l_relation));

    // remove other root nodes.
    EST_Item *s, *m;

    for (s = new_utt->relation(get_c_string(l_relation))->head();
	 s; s = m)
    {
	m = inext(s);
	if (s != n)
	    s->unref_all();
    }

    n = new_utt->id(get_c_string(l_id))->
	as_relation(get_c_string(l_relation));

    add_keep_nodes(n, "keep");

    for (s = new_utt->relation("Segment")->head(); s; s = m)
    {
	m = inext(s);
	if (!s->f_present("keep"))
	    s->unref_all();
    }

    for (s = new_utt->relation("Syllable")->head(); s; s = m)
    {
	m = inext(s);
	if (!s->f_present("keep"))
	    s->unref_all();
    }

    for (s = new_utt->relation("Word")->head(); s; s = m)
    {
	m = inext(s);
	if (!s->f_present("keep"))
	    s->unref_all();
    }

/*    for (s = new_utt->relation("Segment")->head(); 
	 s->S("id") != first_leaf(n)->S("id"); s = m)
    {
	m = inext(s);
	cout << "deleting segment :" << s->S("name") << endl;
	s->unref_all();
    }

    for (s = inext(last_leaf(n)->as_relation("Segment")); s; s = m)
    {
	m = inext(s);
	cout << "deleting segment :" << s->S("name") << endl;
	s->unref_all();
    }
*/

    cout << "h1\n";
    LISP n_utt;
    cout << "h1\n";
    n_utt = siod(new_utt);
    cout << "h1\n";

    return n_utt;
}

void add_syllable_name(EST_Item *syl, const EST_String &fname);
void add_non_terminal_features(EST_Item *s,
			      EST_Features &f);

LISP FT_add_match_features(LISP l_utt)
{
    EST_Item *p;
    EST_Utterance *u = get_c_utt(l_utt);

    for (p = u->relation("Word")->head(); p; p = inext(p))
	p->set("match", p->S("name"));

    for (p = u->relation("Segment")->head(); p; p = inext(p))
	p->set("match", p->S("name"));
	
    for (p = u->relation("Syllable")->head(); p; p = inext(p))
	add_syllable_name(p, "match");

    EST_Features tf;
    tf.set_function("end", "standard+unisyn_leaf_end");
    tf.set_function("start", "standard+unisyn_leaf_start");
    tf.set_function("dur", "standard+duration");
    
    tf.set("time_path", "ProsodicTree");
    tf.set("time_path", "ProsodicTree");

    add_non_terminal_features(u->relation("ProsodicTree")->head(), tf);
    
    return l_utt;
}

LISP FT_tilt_to_f0(LISP l_utt, LISP l_f0_name)
{
    EST_String f0_name = get_c_string(l_f0_name);
    EST_Utterance *u = get_c_utt(l_utt);

    EST_Relation *f0 = u->create_relation(f0_name);

    tilt_to_f0(*u->relation("Intonation"), *f0);
    return l_utt;
}

LISP FT_scale_tilt(LISP l_utt, LISP l_shift, LISP l_scale)
{
    scale_tilt(*(get_c_utt(l_utt)->relation("Intonation")), 
	       get_c_float(l_shift), get_c_float(l_scale));
    return l_utt;
}

LISP FT_vowel_tilt_to_abs_tilt(LISP l_utt)
{
    vowel_tilt_to_abs_tilt(*get_c_utt(l_utt));
    return l_utt;
}

LISP FT_phrase_factor(LISP l_utt, LISP l_base_name, LISP l_met_name)
{
    phrase_factor(*get_c_utt(l_utt), get_c_string(l_base_name), 
		  get_c_string(l_met_name));
    return l_utt;
}

LISP FT_stress_factor(LISP l_utt, LISP l_rel_name, LISP l_num)
{
    if (get_c_int(l_num) == 1)
	stress_factor1(*get_c_utt(l_utt), get_c_string(l_rel_name), 
		       "LexicalMetricalTree");
    else
	stress_factor2(*get_c_utt(l_utt), get_c_string(l_rel_name),
		       "LexicalMetricalTree");
    return l_utt;
}

LISP FT_smooth_f0(LISP l_utt, LISP l_rel_name, LISP l_num)
{
    if (get_c_int(l_num) == 1)
	stress_factor1(*get_c_utt(l_utt), get_c_string(l_rel_name), 
		       "LexicalMetricalTree");
    else
	stress_factor2(*get_c_utt(l_utt), get_c_string(l_rel_name),
		       "LexicalMetricalTree");
    return l_utt;
}

LISP FT_legal_metrical_tree(LISP l_utt)
{
    EST_Utterance *u = get_c_utt(l_utt);
    EST_Item *s;

    for (s = u->relation("MetricalTree")->head(); s; s= inext(s))
	legal_metrical_tree(s);

    return l_utt;
}
void auto_metrical_lex(EST_Utterance &utt);

LISP FT_unisyn_lex(LISP l_utt)
{
    EST_Utterance *u = get_c_utt(l_utt);
    auto_metrical_lex(*u);

    return l_utt;
}


/*LISP FT_MetricalTree_Utt(LISP l_utt)
{
    EST_Utterance *u = get_c_utt(l_utt);

    auto_metrical_tree(*u);
    auto_metrical_lex(*u);

    return l_utt;
}
*/



LISP FT_syntax_to_metrical_words(LISP l_utt)
{
    syntax_to_metrical_words(*get_c_utt(l_utt));
    return l_utt;
}

LISP FT_binaryize_tree(LISP l_utt, LISP old_tree, LISP new_tree)
{
    binaryize_tree(*get_c_utt(l_utt), get_c_string(old_tree), 
		   get_c_string(new_tree));
    return l_utt;
}

LISP FT_parse_words(LISP l_utt)
{
    parse_words(*get_c_utt(l_utt));
    return l_utt;
}

void festival_UniSyn_phonology_init(void)
{
    init_subr_1("US_add_intonation", FT_US_add_intonation,
    "(US_add_intonation UTT)");

    init_subr_1("vowel_tilt_to_abs_tilt",  FT_vowel_tilt_to_abs_tilt, ".");

    init_subr_2("tilt_to_f0", FT_tilt_to_f0, ".");

    init_subr_3("scale_tilt", FT_scale_tilt, 
		"(scale_tilt UTT shift scale)\n"
		"Add shift Hz to each event and increase range by a factor\n"
		"of scale. (scale UTT 0.0 1.0) leaves the tilt parameters\n"
		"unaffected\n");

/*    init_subr_3("promote_vowel_z_score", FT_promote_vowel_z_score,
    "(promote_vowel_z_score UTT RELATION)\n\
     Focus nth item in relation.");

    init_subr_3("promote_mean_z_score", FT_promote_mean_z_score,
    "(promote_mean_z_score UTT RELATION)\n\
     Focus nth item in relation.");

    init_subr_2("assign_phone_z_scores", FT_assign_phone_z_scores,
    "(assign_phone_z_scores UTT RELATION RELATION)\n\
     Focus nth item in relation.");
*/

    init_subr_3("focus_nth_item", FT_focus_nth_item,
    "(focus_nth_item UTT RELATION WordNumber)\n\
     Focus nth item in relation.");

    init_subr_3("focus_nth_tree_item", FT_focus_nth_tree_item,
    "(focus_tree_nth_item UTT RELATION (l r path)\n\
     Focus nth item in relation.");

    init_subr_3("copy_sub_tree", FT_copy_sub_tree,
    "(foot_nth_item UTT WordNumber)\n\
     Foot nth item in relation.");

    init_subr_2("foot_nth_item", FT_foot_nth_item,
    "(foot_nth_item UTT WordNumber)\n\
     Foot nth item in relation.");

    init_subr_1("legal_metrical_tree", FT_legal_metrical_tree,
    "(legal_metrical_tree UTT))\n\
     load data.");

    init_subr_1("add_match_features", FT_add_match_features,
    "(legal_metrical_tree UTT))\n\
     load data.");

    init_subr_2("extend_tree", FT_extend_tree,
    "(extend_tree utt (new_tree first_tree second_tree terminal))");

    init_subr_3("phrase_factor", FT_phrase_factor,
    "(phrase_factor utt)\n\
     Apply phrase factor algorithm.");

    init_subr_3("stress_factor", FT_stress_factor,
    "(stress_factor utt RELATION VERSION)\n\
     Apply version VERSION of stress factor algorithm to RELATION.");

    init_subr_1("syntax_to_metrical_words", FT_syntax_to_metrical_words, 
    "(add_trans_intonation UTT Tilt file)\n\
     Foot nth item in relation.");

    init_subr_3("binaryize_tree", FT_binaryize_tree,
    "(add_trans_intonation UTT Tilt file)\n\
     Foot nth item in relation.");

    init_subr_1("parse_words", FT_parse_words,
    "(add_trans_intonation UTT Tilt file)\n\
     Foot nth item in relation.");

    init_subr_1("unisyn_lex", FT_unisyn_lex,
    "(unisyn_lex UTT Tilt file)\n\
     Foot nth item in relation.");


/**** FUNCTIONS FOR UNISYN BUILD ****/

    init_subr_3("add_trans_intonation", FT_add_trans_intonation,
    "(add_trans_intonation UTT Tilt_file ADD_WORDS)\n\
     Foot nth item in relation.");

    init_subr_2("add_f0_points", FT_add_f0_points,
    "(add_f0_points UTT f0 file)\n\
     Foot nth item in relation.");

    init_subr_2("add_coefs", FT_add_coefs,
    "(add_coefs UTT f0 file)\n\
     Foot nth item in relation.");

    init_subr_2("add_trans_segment", FT_add_trans_seg,
    "(add_trans_segment UTT label_file)\n\
     Add segment information\n\
     Foot nth item in relation.");

    init_subr_2("add_trans_duration", FT_add_trans_duration,
    "(add_trans_duration UTT label_file)\n\
     Foot nth item in relation.");

    init_subr_3("add_trans_word", FT_add_trans_word,
    "(add_trans_word UTT word_label_file)\n\
     Foot nth item in relation.");

    init_subr_2("add_xml_relation", FT_add_xml_relation,
    "(add_xml_relation UTT xml_file))\n\
     load data.");

    init_subr_1("syl_to_word_intonation", FT_syl_to_word_intonation,
    "(add_xml_relation UTT xml_file))\n\
     load data.");

    init_subr_4("intonation_diagnostics", FT_intonation_diagnostics, 
    "(intonation_diagnostics UTT
     Foot nth item in relation.");

    // semi redundant
    init_subr_3("add_trans_metrical_tree", FT_add_trans_metrical_tree,
    "(add_trans_intonation UTT Tilt file)\n\
     Foot nth item in relation.");



/**** END FUNCTIONS FOR UNISYN BUILD ****/
}

/*
LISP FT_assign_phone_z_scores(LISP l_utt, LISP l_rel_name)
{
    assign_phone_z_scores(*get_c_utt(l_utt), get_c_string(l_rel_name));
    return l_utt;
}

LISP FT_promote_mean_z_score(LISP l_utt, LISP l_st_name, LISP l_syl_name)
{
    promote_mean_z_score(*get_c_utt(l_utt), get_c_string(l_st_name), 
		     get_c_string(l_syl_name));
    return l_utt;
}

LISP FT_promote_vowel_z_score(LISP l_utt, LISP l_st_name, LISP l_syl_name)
{
    promote_vowel_z_score(*get_c_utt(l_utt), get_c_string(l_st_name), 
		     get_c_string(l_syl_name));
    return l_utt;
}
*/
