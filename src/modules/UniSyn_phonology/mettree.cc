/*************************************************************************/
/*                                                                       */
/*                Centre for Speech Technology Research                  */
/*                     University of Edinburgh, UK                       */
/*                         Copyright (c) 1998                            */
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
/*             Author :  Alan W Black and Paul Taylor                    */
/*             Date   :  February 1998                                   */
/*-----------------------------------------------------------------------*/
/*                                                                       */
/*  An implementation of Metrical Tree Phonology                         */
/*                                                                       */
/*=======================================================================*/

#include <cmath>
#include "festival.h"
#include "lexicon.h"
#include "../UniSyn/us_features.h"

EST_Features phone_def;

float local_cost(const EST_Item *s1, const EST_Item *s2);

static void mettree_add_words(EST_Utterance &u);

void construct_metrical_tree(EST_Utterance &word);
void add_end_silences(EST_Relation &segment);
void StrListtoString(EST_StrList &l, EST_String &s, EST_String sep=" ");

void parse_wsj_syntax(void);
static void apply_nsr(EST_Utterance &u, const EST_String &tree);
#if 0
static void remove_punctuation(EST_Utterance &u);
static void add_intonation(EST_Utterance &u, const EST_String &base_int, 
			   float threshold);
#endif

void subword_list(EST_Item *w, EST_Relation &syllable, 
		  EST_Relation &metricaltree);

void add_non_terminal_features(EST_Relation &r, 
			       EST_Features &f);

void stress_factor1(EST_Utterance &u, const EST_String &base_stream, 
		    const EST_String &m);
void stress_factor2(EST_Utterance &u, const EST_String &base_stream, 
		    const EST_String &m);

void phrase_factor(EST_Utterance &u);

void main_stress(EST_Item *s);

void end_to_dur(EST_Relation &r);

void footing(EST_Item *n1);

LISP FT_Classic_Phrasify_Utt(LISP args);
LISP FT_Classic_POS_Utt(LISP args);
LISP FT_PParse_Utt(LISP args);
LISP FT_MultiParse_Utt(LISP utt);
void MultiParse(EST_Utterance &u);

void add_feature_string(EST_Relation &r, const EST_String &fname,
			const EST_String &f);

void add_monotone_targets(EST_Utterance &u, float start_f0,
				    float end_f0);


void clear_feature(EST_Relation &r, const EST_String &name); 

typedef
float (*local_cost_function)(const EST_Item *item1,
			     const EST_Item *item2);



void subword_phonology(EST_Utterance &word);
void lex_to_phones(EST_Utterance &u, const EST_String &relname);

bool dp_match(const EST_Relation &lexical,
	      const EST_Relation &surface,
	      EST_Relation &match,
	      local_cost_function lcf,
	      EST_Item *null_syl);

//bool dp_match(EST_Relation &a, EST_Relation &b, EST_Relation &c,
//	      local_cost_function lcf, const EST_String &null_sym);
void lex_to_phones(const EST_String &name, const EST_String &pos,
		   EST_Relation &phone);
	      
void subword_metrical_tree(EST_Relation &syllable, 
			   EST_Relation &metricaltree);

			   
int syllabify_word(EST_Item *nw, EST_Relation &phone, 
		    EST_Relation &sylstructure, EST_Relation &syl, int flat);

void subword_metrical_tree(EST_Item *w, EST_Relation &syllable, 
			   EST_Relation &metricaltree);

EST_Item *prev_match(EST_Item *n)
{
    EST_Item *p = iprev(n);
    if (p == 0)
	return 0;

    if (daughter1(p->as_relation("Match")) == 0)
	prev_match(p);

    return daughter1(p->as_relation("Match"));
}

void insert_schwa(EST_Item *n)
{
    EST_Item *p, *s;
    float pp_end = 0;
    float schwa_length = 0.01;

    if ((p = prev_match(n)) == 0)
    {
	cout << "Couldn't insert dummy schwa after " << *n << endl;
	return;
    }

    p = p->as_relation("SurfacePhone");
    pp_end = (iprev(p) != 0) ? iprev(p)->F("end",0.0) : 0.0;

    s = p->insert_after();

    s->set("name", "ax");
    s->set("stress_num", "0");

    if ((p->F("end",0) - pp_end) < schwa_length)
	schwa_length = p->F("dur") / 2.0;

    s->set("end", p->F("end",0));
    p->set("end", p->F("end",0) - schwa_length);
    s->set("start", p->F("end",0));

    s->set("df", phone_def.A("ax"));

//    cout << "end 1:" << s->f("end") << endl;
//    cout << "end 2:" << p->f("end") << endl;
}

void add_initial_silence(EST_Relation &lexical, EST_Relation &surface, 
	       EST_Relation &match)
{
    EST_Item *s, *p, *n, *m;

    s = lexical.head();
    if ((s->f("name") != "pau") && (s->f("name") != "sil"))
    {
	p = s->insert_before();
	p->set("name", "pau");
	p->set("df", phone_def.A("pau"));

	n = surface.head();
	if ((n->f("name") == "pau") || (n->f("name") == "sil"))
	{
	    m = match.head()->insert_before(p);
	    m->append_daughter(n);
	}

    }
}

void add_even_segment_times(EST_Item *w, EST_Relation &phone)
{
    EST_Item *s;
    int i;
    float start,dur=0,n,div;

    start = w->F("start");
    dur = w->F("end") - start;
    n = (float)phone.length();
    div = dur/n;

    for (i = 0, s = phone.head(); s; s = inext(s), ++i)
    {
	s->set("start", start + div * (float) i);
	s->set("end", start + div * (float) (i + 1));
    }
}

#if 0
static void add_trans_phrase_phrase(EST_Utterance &utt)
{
    EST_Item *s, *t, *a, *r;
    EST_Item *first_accent = 0, *last_accent = 0;
    bool exist;

    // This looks insanely comlicated, but all it really does is
    // add phrase_start and phrase_end items to the root node of
    // each metrical tree and then places these in the right position
    // in the intonation relation.

    utt.create_relation("IntonationPhrase");

    for (s = utt.relation("MetricalTree", 1)->head(); s; s = inext(s))
	{
	    for (r = first_leaf_in_tree(s); 
		 r != next_leaf(last_leaf_in_tree(s)); r = next_leaf(r))
		if (r->in_relation("IntonationSyllable"))
		    {
			if (first_accent == 0)
			    first_accent = 
				parent(r->as_relation("IntonationSyllable"))
				->as_relation("Intonation");
			last_accent = 
			    parent(r->as_relation("IntonationSyllable"))
			    ->as_relation("Intonation");
		    }

	    exist = false;
	    //	    cout << "\nroot node: " <<*s << endl;

	    if (first_accent)
		{
		    cout << "first accent: " << *first_accent << endl;
		    a = iprev(first_accent);

		    if (a->S("name","") != "phrase_start")
			a = first_accent->insert_before();
		}
	    else
		{
		    if (a == 0)
			a = utt.relation("Intonation")->prepend();
		    else
			a = a->insert_after();
		}

	    if (a->S("name","") != "phrase_start" ) // i.e. its a new one
		{
		    a->set("name", "phrase_start");
		    a->set("ev:f0", 100);
		}
	    // re-write position as relative to metrical tree
	    a->set("position", usf_int_start);
	    // add this as daughter to root node
	    t = utt.relation("IntonationPhrase")->append(s);
	    t->append_daughter(a);
	    exist = false;
	    //cout << "appended phrase end\n";

	    if (last_accent)
		{
		    cout << "last accent: " << *last_accent << endl;
		    a = inext(last_accent);
		    if (a->S("name","") != "phrase_end")
			a = last_accent->insert_after();
		}
	    else
		a = a->insert_after();

	    if (a->S("name","") != "phrase_end")
		{
		    a->set("name", "phrase_end");
		    a->set("ev:f0", 100);
		}
	    // re-write position as relative to metrical tree
	    a->set("position", usf_int_end);

	    // add this as daughter to root node
	    t->append_daughter(a);
	    //cout << "appended phrase start\n";
	    first_accent = 0; // trigger for first time operation of loop
	}

    // now join any other marked phrase_start/ends to intermediate
    // nodes in metrical tree.

    /*    for (s = u.relation("Intonation", 1)->head(); s; s = inext(s))
	{
	    if (!s->in_relation("IntonationPhrase") && 
		!s->in_relation("IntonationSyllable"))
		{
		    pos = s->F("position");
	
	    
		
	}
    */

}
#endif


void add_single_phrase(EST_Utterance &utt, EST_Item *t)
{
    EST_Item *s=0, *a, *p;
    float pos, max, d = 0, start = 0.0;

    pos = t->F("time");
    max = 100000.0;

    for (p = utt.relation("Syllable")->head(); p; p = inext(p))
    {
	if (t->S("name") == "phrase_end")
	    d = fabs(pos - p->F("end"));
	else
	    d = fabs(pos - start);

	if (d < max)
	{
	    max = d;
	    s = p;
	}
	start = p->F("end");
    }

/*    if (s)
	 cout << "joining syllable " << *s << endl;
    else
	 cout << "No legal syllable " << endl;
     cout << "to " << *t << endl;

     cout << "d = " << d << endl;
*/

    if (!s->in_relation("IntonationSyllable"))
	a = utt.relation("IntonationSyllable")->append(s);
    else
	a = s->as_relation("IntonationSyllable");
    a->append_daughter(t);
    t->set("time_path", "IntonationSyllable");
    t->set_function("position", "standard+unisyn_tilt_phrase_position");
}

void add_times(EST_Relation &lexical, EST_Relation &surface, 
	       EST_Relation &match)
{
    (void) surface;
    (void) match;
    EST_Item *s, *t, *p;
    float prev_end, inc, first_end, last_end;
    int i;

    // first pass, copy times as appropriate, and find first 
    // and last defined ends
    // This is hacky and certainly won't work for many cases

    first_end = -1.0;
    prev_end = 0.0;
    last_end = 0.0;

//    cout << "surface: " << surface << endl;

    for (s = lexical.head(); s; s = inext(s))
    {
	if ((t = daughter1(s->as_relation("Match"))) != 0)
	{
	    s->set("end", t->F("end"));
	    s->set("start", t->F("start"));

	    last_end = t->F("end");
	    if (first_end < 0.0)
		first_end = t->F("end");
	}
    }

     if (!lexical.head()->f_present("end"))
     {
	 lexical.head()->set("end", first_end / 2.0);
	 lexical.head()->set("start", 0.0);
     }

     if (!lexical.tail()->f_present("end"))
     {
	 lexical.tail()->set("end", last_end + 0.01);
	 lexical.tail()->set("start", last_end);
     }

    for (s = lexical.head(); s; s = inext(s))
    {
	if (!s->f_present("end"))
	{
//	    cout << "missing end feature for " << *s << endl;
	    for (i = 1, p = s; p; p = inext(p), ++i)
		if (p->f_present("end"))
		    break;
	    inc = (p->F("end") - prev_end) / ((float) i);
//	    cout << "inc is : " << inc << endl;

//	    cout << "stop phone is " << *p << endl;

	    for (i = 1; s !=p ; s = inext(s), ++i)
	    {
		s->set("end", (prev_end + ((float) i * inc)));
		s->set("start", (prev_end + ((float) (i - 1 )* inc)));
	    }
	}
	prev_end = s->F("end");
    }
}	    


static void met_error(EST_Item *s)
{
    cerr << "Illegally named daughters of metrical node\n"
	 << "daughter1 : " << *daughter1(s) << endl
	 << "daughter2 : " << *daughter2(s) << endl;
    EST_error("");
}

void legal_metrical_tree(EST_Item *s)
{
    if (s == 0)
	return;

    if ((daughter1(s) == 0) || (daughter2(s) == 0))
	return;

    if ((daughter1(s)->S("MetricalValue") == "s") 
	&& (daughter2(s)->S("MetricalValue") != "w"))
	met_error(s);
    else if ((daughter1(s)->S("MetricalValue") == "w") 
	&& (daughter2(s)->S("MetricalValue") != "s"))
	met_error(s);
    else if ((daughter1(s)->S("MetricalValue") != "w") 
	     && (daughter1(s)->S("MetricalValue") != "s"))
	met_error(s);

    legal_metrical_tree(daughter1(s));
    legal_metrical_tree(daughter2(s));
}

void parse_words(EST_Utterance &utt)
{
    utt.create_relation("Token");

    FT_Classic_POS_Utt(siod(&utt));
    FT_Classic_Phrasify_Utt(siod(&utt));
    MultiParse(utt);

    utt.relation("Syntax")->remove_item_feature("pos_index");
    utt.relation("Syntax")->remove_item_feature("pos_index_score");
    utt.relation("Syntax")->remove_item_feature("phr_pos");
    utt.relation("Syntax")->remove_item_feature("pbreak_index");
    utt.relation("Syntax")->remove_item_feature("pbreak_index_score");
    utt.relation("Syntax")->remove_item_feature("pbreak");
    utt.relation("Syntax")->remove_item_feature("blevel");
    utt.relation("Syntax")->remove_item_feature("prob");
}

void binaryize_tree(EST_Item *t)
{
    // terminating condition
    if (daughter1(t) == 0)
	return;

    // nodes with single children should be merged
    if (daughter2(t) == 0)
    {
//	cout << "Single daughter: " << *t << endl;
	EST_Item *d = daughter1(t);
	move_sub_tree(d, t);
    }

    for (EST_Item *p = daughter1(t); p; p = inext(p))
	binaryize_tree(p);
}

void binaryize_tree(EST_Utterance &utt, const EST_String &base_tree,
		    const EST_String &new_tree)
{
    utt.create_relation(new_tree);
    copy_relation(*utt.relation(base_tree), *utt.relation(new_tree));

    for (EST_Item *p = utt.relation(new_tree)->head(); p; p = inext(p))
	binaryize_tree(p);
}

void syntax_to_metrical_words(EST_Utterance &utt)
{
    utt.create_relation("MetricalWord");
    // copy syntax tree while merging single daughter nodes
    binaryize_tree(utt, "Syntax", "MetricalWord");
    // add strong and weak values
    apply_nsr(utt, "MetricalWord");
}

void add_metrical_functions(EST_Utterance &utt)
{
    // Note that we don't add "start" functions here as this depends on
    // pause behaviour
    add_feature_function(*utt.relation("Syllable"), 
			 "vowel_start", 
			 "unisyn_vowel_start");
    
    add_feature_function(*utt.relation("Syllable"), 
			 "end", "standard+unisyn_leaf_end");
    add_feature_function(*utt.relation("Syllable"), 
			 "start", "standard+unisyn_leaf_start");

    for (EST_Item *s = utt.relation("Syllable")->head(); s; s = inext(s))
	s->set("time_path", "SylStructure");
    
    EST_Features tf;
    tf.set_function("end", "standard+unisyn_leaf_end");
    tf.set_function("start","standard+unisyn_leaf_start");
    tf.set_function("dur","standard+unisyn_duration");
    
    tf.set("time_path", "MetricalTree");
    tf.set("time_path", "MetricalTree");

//    add_non_terminal_features(*utt.relation("MetricalTree"), tf);
  
    tf.set("time_path", "SylStructure");
    add_non_terminal_features(*utt.relation("SylStructure"), tf);
    
    add_feature_function(*utt.relation("Segment"), 
			 "dur",
			 "standard+duration");

    
}

void auto_metrical_lex(EST_Utterance &utt)
{
    LISP l_pdef;

    utt.create_relation("Syllable");
    utt.create_relation("Segment");

    l_pdef = siod_get_lval("darpa_fs", NULL);
    lisp_to_features(l_pdef, phone_def);
    
    mettree_add_words(utt);
    
    LISP lt = siod_get_lval("us_base_int", NULL);
    EST_String base_int;
    if (lt == NIL)
	base_int = "Syllable";
    else
    {
	const char *x = get_c_string(lt);
	base_int = x;
    }
    
    // add_end_silences(*utt.relation("Segment"));
    
    add_metrical_functions(utt);
}

void extend_tree(EST_Item *m, EST_Item *p, const EST_String &terminal,
		 const EST_String &second_tree)
{
    EST_Item *d, *e;

    if (!daughter1(m))
    {
	if (m->in_relation(terminal)) // ie. really hit the bottom
	    return;
	m = m->as_relation(second_tree); // swap to a new tree
    }

    for (d = daughter1(m); d; d = inext(d))
    {
	e = p->append_daughter(d);
        extend_tree(d, e, terminal, second_tree);
    }
}


static void nsr(EST_Item *n)
{
    EST_Item *left, *right;
    left = daughter1(n);
    right = daughter2(n);
    if (left == 0)
	return;
    else
    {
	nsr(left);
	left->set("MetricalValue","w");
    }
    
    if (right == 0)
	return;
    else
    {
	nsr(right);
	right->set("MetricalValue","s");
    }
}

static void apply_nsr(EST_Utterance &u, const EST_String &tree)
{
    EST_Item *n;
    
    for (n = u.relation(tree)->head(); n; n = inext(n))
	nsr(n);
}

EST_Item *other_daughter(EST_Item *parent, EST_Item *daughter)
{
    return (daughter1(parent) == daughter) ? daughter2(parent) :
    daughter1(parent);
}

static void stress_factor1(EST_Item *s, int max_depth)
{
    EST_Item *a;
    EST_String val, pad;
    char *str;
    long n, i;
    float max;
    
    val = "";
    
    for (a = s; parent(a); a = parent(a))
	if (a->f("MetricalValue") == "s")
	    val += "2";
	else
	    val += "0";
    
    //    cout << "\nSyllable " << s << " has value " << val << endl;
    
    if (val.length() < max_depth)
	for (pad = "", i = 0; i < (max_depth - val.length()); ++i)
	    pad += "2";
    
    val += pad;
    //    cout << "Syllable " << s << " has padded value " << val << endl;
    
    str = strdup(val);
    max = pow(3.0, (float)max_depth) - 1.0;
    n = strtol(str, (char **)NULL, 3);
    //    cout << "decimal value: " << n;
    //    cout << " normalised: " << (float)n/max << endl;
    s->set("StressFactor1", ((float)n/max));
}

EST_Item * find_apex(EST_Item *n, int &num_nodes)
{
    EST_Item *p;
    p = parent(n);
    if (p == 0)
	return n;
    if (daughter2(p) == n)
	return find_apex(p, ++num_nodes);
    
    return p;
}

void find_leaf(EST_Item *n, int &num_nodes)
{
    if (n == 0)
	return;
    find_leaf(daughter1(n), ++num_nodes);
}

static void phrase_factor(EST_Item &syl, const EST_String &met_name)
{
    EST_Item *p;
    EST_String val, pad;
    int num_nodes = 1;
    
    //    cout << "Terminal Syl = " << syl << " f:" << syl.f << endl;
    
    p = find_apex(syl.as_relation(met_name), num_nodes);
    //    cout << "up nodes: " << num_nodes;
    //    cout << "Apex = " << *p << endl;
    find_leaf(daughter2(p), num_nodes);
    //    cout << " downp nodes: " << num_nodes << endl;
    
    syl.set("PhraseIndex", num_nodes);
}

static int max_tree_depth(EST_Utterance &u, const EST_String &base_stream,
			  const EST_String &mettree)
{
    EST_Item *s, *a;
    int depth;
    int max_depth = 0;
    
    for (s = u.relation(base_stream)->head(); s; s = inext(s))
    {
	depth = 0;
	for (a = s->as_relation(mettree); parent(a); a = parent(a))
	    ++depth;
	if (depth > max_depth)
	    max_depth = depth;
    }
    return max_depth;
}

void stress_factor1(EST_Utterance &u, const EST_String &base_stream,
		    const EST_String &mettree)
{
    EST_Item *s;
    int max_depth = max_tree_depth(u, base_stream, mettree);
    
    for (s = u.relation(base_stream)->head(); s; s = inext(s))
	stress_factor1(s->as_relation(mettree), max_depth);
}


EST_Item *strong_daughter(EST_Item *n)
{
    if (daughter1(n) == 0)
	return 0;
    return (daughter1(n)->f("MetricalValue") == "s") 
	? daughter1(n) : daughter2(n);
}

EST_Item *weak_daughter(EST_Item *n)
{
    if (daughter1(n) == 0)
	return 0;
    return (daughter1(n)->f("MetricalValue") == "w") 
	? daughter1(n) : daughter2(n);
}

static void fill_mini_tree(EST_Item *s, int val)
{
    if (s->f("MetricalValue") == "s")
	s->set("StressVal", val);
    else
	s->set("StressVal", 0);
    if (strong_daughter(s))
	fill_mini_tree(strong_daughter(s), val);
    
    if (weak_daughter(s))
	fill_mini_tree(weak_daughter(s), val - 1);
}

void stress_factor2(EST_Utterance &u, const EST_String &base_stream, 
		    const EST_String &mettree)
{
    EST_Item *s;
    int sv = -1;
    float b;
    (void) base_stream;
    
    s = u.relation(mettree)->head();
    fill_mini_tree(s, sv);
    
    // normalise values
    sv = 0;
    for (s = u.relation(base_stream)->head(); s; s = inext(s))
	sv = Lof(s->I("StressVal"), sv);
    
    cout << "Max Stress: " << sv << endl;
    
    for (s = u.relation(base_stream)->head(); s; s = inext(s))
    {
	b = (float)(s->I("StressVal") - sv + 1);
	if (s->f("MetricalValue") == "s")
	    s->set("StressFactor2", (b / float(sv)) * -1.0);
	else
	    s->set("StressFactor2", 0);
    }
}

void phrase_factor(EST_Utterance &u, const EST_String &base_stream, 
		   const EST_String &mettree)
{
    EST_Item *s;
    float max_pf = 0;
    
    for (s = u.relation(base_stream)->head(); s; s = inext(s))
	phrase_factor(*s, mettree);
    
    for (s = u.relation(base_stream)->head(); s; s = inext(s))
	if (s->I("PhraseIndex") > max_pf)
	    max_pf = s->I("PhraseIndex");
    
    for (s = u.relation(base_stream)->head(); s; s = inext(s))
    {
	s->set("PhraseFactor",
		(float)s->I("PhraseIndex")/max_pf);
	//	cout << *s << " pf = " << 
	//	    s->F("PhraseFactor") << endl;
    }
    
}

#if 0
static void remove_punctuation(EST_Utterance &u)
{
    // The syntactic grammar has unary rules for the preterminals
    // these would make the mtettrical tree have an extra layer
    // at the word level.  So here we remove that extra layer
    EST_Item *w;
    EST_Item *a, *b, *c, *od;
    
    for (w = u.relation("Word")->head(); w != 0; w = inext(w))
    {
	if (w->f("pos") == "punc")
	{
	    a = w->as_relation("Syntax");
	    b = parent(a);
	    c = parent(b);
	    od = other_daughter(c, b);
	    remove_item(b, "Syntax");
	    move_sub_tree(od, c);
	    remove_item(w, "Word");
	}
    }
}

static void add_intonation(EST_Utterance &u, const EST_String &base_stream,
			   float threshold)
{
    EST_Item *e, *s;
    
    cout << "Threshold = " << threshold << endl;
    
    for (s = u.relation(base_stream)->head(); s; s = inext(s))
    {
	if (s->F("StressFactor") > threshold)
	{
	    //	    cout << *s <<" **stress factor:" << s->F("StressFactor") << endl;
	    e = u.relation("IntSyl")->append();
	    e->insert_below(s);
	    e->set_name("Accent");
	    e->set("prominence", s->F("StressFactor"));
	    u.relation("Intonation")->append(e);
	}
    }
}

#endif

void add_monotone_targets(EST_Utterance &u, float start_f0, 
				 float end_f0)
{
    EST_Item *t;
    float end;
    
    end = u.relation("Segment")->tail()->f("end");
    
    cout << "Phone ends\n";
    cout << *u.relation("Segment");
    
    cout << "last position is :" << end << endl;
    
    u.create_relation("Target");
    
    t = u.relation("Target")->append();
    t->set("f0", start_f0);
    t->set("pos", 0.0);
    
    // temporary - should disappear when awb changes code
    //    t->set("name", ftoString(start_f0));
    //    t->set("end", 0.0);
    
    t = u.relation("Target")->append();
    t->set("f0", end_f0);
    t->set("pos", end);
    
    // temporary - should disappear when awb changes code
    //    t->set("name", ftoString(end_f0));
    //    t->set("end", end);
}

static void mettree_add_words(EST_Utterance &u)
{
    EST_Utterance word;
    EST_Item *w;
    
    word.create_relation("Word");
    word.create_relation("Segment");
    word.create_relation("SylStructure");
    word.create_relation("Syllable");
    word.create_relation("WordStructure");
    
    for (w = u.relation("Word")->head(); w != 0; w = inext(w))
    {
	word.clear_relations();
	
	cout << "N:";
	cout << w->f("name") << " " << w->f("pos", "") << endl;
	lex_to_phones(w->f("name"), w->f("pos", "0"), 
		      *word.relation("Segment"));
	
	EST_Item *nw = word.relation("Word")->append();
	nw->set("name", w->S("name"));
	
	syllabify_word(nw, *word.relation("Segment"),
		       *word.relation("SylStructure"),
		       *word.relation("Syllable"), 0);
	
	subword_metrical_tree(nw, *word.relation("Syllable"),
			      *word.relation("WordStructure"));

	utterance_merge(u, word, w, word.relation("Word")->head());
    }
}

void add_metrical_nodes(EST_Utterance &u, EST_Item *n, LISP lpos);

EST_String strip_vowel_num(EST_String p)
{
    if (p.contains(RXint))
	p = p.before(RXint);
    return p;
}


static void percolate(EST_Item *start)
{
    EST_Item *n;
    
    for (n = start; n; n = parent(n))
    {
	//	cout << "altering sister\n";
	if (iprev(n) != 0)
	    iprev(n)->set("MetricalValue", "w");
	else if (inext(n) != 0)
	    inext(n)->set("MetricalValue", "w");
    }
}


void main_stress(EST_Item *s)
{
    EST_Item *n;
    
    for (n = s; parent(n); n = parent(n))
  	n->set("MetricalValue", "s");
    
    n = s;
    percolate(n);
}

void footing(EST_Item *n1)
{
    EST_Item *n2, *n3, *n4, *p1, *p3, *r;
    
    r = parent(n1);		// root node
    p1 = daughter2(r);
    n2 = daughter1(p1);
    n3 = daughter2(p1);
    
    if (p1 == 0)
    {
	cerr << "Error: Empty 3rd node after " << *n1 << " in footing\n";
	return;
    }
    if (n2 == 0)
    {
	cerr << "Error: Empty 3rd node after " << *n1 << " in footing\n";
	return;
    }
    if (n3 == 0)
    {
	cerr << "Error: Empty 3rd node after " << *n1 << " in footing\n";
	return;
    }
    
    cout << "n1: " << *n1 << endl << endl;
    cout << "n2: " << *n2 << endl << endl;
    cout << "n3: " << *n3 << endl << endl;
    cout << "p1: " << *p1 << endl << endl;
    
    p3 = n1->insert_parent();
    n1 = daughter1(p3);
    n4 = p3->append_daughter();
    
    move_sub_tree(n2, n4);
    move_sub_tree(n3, p1);
    
    p3->set("MetricalValue", "w");
    p3->set("Altered_a", "DONE_W");
    
    n1->set("MetricalValue", "s");
    n1->set("Altered_b", "DONE_S");
}


#if 0
LISP FT_metrical_data(LISP lf_word, LISP lf_seg, LISP lf_int)
   {
   
   EST_Utterance word, *u = new EST_Utterance;
   EST_Relation phone;
   EST_Item *s, *p, *w, *nw, *n;
   EST_StrList plist;
   float phone_start, mid;
   LISP lutt;
   EST_Track fz;
   int i;
   
   u->create_relation("Word");
   u->create_relation("Segment");
   u->create_relation("Syllable");
   u->create_relation("MetricalTree");
   u->create_relation("LexicalMetricalTree");
   u->create_relation("SurfacePhone");
   u->create_relation("Surface");
   u->create_relation("Intonation");
   u->create_relation("IntonationSyllable");
   
   EST_String segfile = get_c_string(lf_seg);
   EST_String wordfile = get_c_string(lf_word);
   
   if (u->relation("Word")->load(wordfile) != format_ok)
   {
   cerr << "Couldn't load file " << get_c_string(lf_word) << endl;
   festival_error();
   }
   
   if ((segfile != "dummy") &&(u->relation("Segment")->
   load(get_c_string(lf_seg)) != format_ok))
   {
   cerr << "Couldn't load file " << get_c_string(lf_seg) << endl;
   festival_error();
   }
   
   if (lf_int != NIL)
   if (u->relation("Intonation")->load(get_c_string(lf_int)) != format_ok)
   {
   cerr << "Couldn't load file " << get_c_string(lf_int) << endl;
   festival_error();
   }
   
   u->f.set("fileroot", basename(wordfile, "*"));
   
   //    cout << "Words: " << *u->relation("Word");
   
   if (segfile != "dummy")
   phonemic_trans(*u->relation("Segment"));
   //    u->relation("Intonation")->load(get_c_string(lf_int));
   
   // tmp hack
   float prev_end = 0.0;
   
   for (w = u->relation("Word")->head(); w != 0; w = n)
   {
   n = inext(w);
   //	w->set("start", prev_end);
   w->f_remove("end");
   //	prev_end = w->F("end");
   if ((w->f("name") == "sil") || (w->f("name") == "pau"))
   u->relation("Word")->remove_item(w);
   }
   
   gc_protect(&lutt);
   lutt = siod_make_utt(u);
   
   cout << *u->relation("Word") << endl;
   
   FT_POS_Utt(lutt);
   FT_Phrasify_Utt(lutt);
   MultiParse(*u);
   
   //    remove_punctuation(*u);
   
   // Copy Syntax tree into a new Metrical Tree
   copy_relation(*u->relations.val("Syntax"), 
   *u->relations.val("MetricalTree"));
   // flatten preterminal unary rules
   flatten_preterminals(*u);
   
   apply_nsr(*u);
   
   copy_relation(*u->relations.val("MetricalTree"),
   *u->relations.val("LexicalMetricalTree"));
   
   word.create_relation("Word");
   word.create_relation("Match");
   word.create_relation("NewMatch");
   word.create_relation("Segment");
   word.create_relation("SurfacePhone");
   word.create_relation("LexicalSylStructure");
   word.create_relation("SurfaceSylStructure");
   word.create_relation("LexicalSyllable");
   word.create_relation("SurfaceSyllable");
   
   word.create_relation("LexicalMetricalTree");
   word.create_relation("SurfaceMetricalTree");
   
   phone_start = 0.0;
   
   // Note starts are hardwired here because feature function thing
   // isn't fully operational and because deleting silence messes
   // it up.
   
   //    u->save("zz_parse.utt", "est");
   
   if (segfile != "dummy")
   {
   for (s = u->relation("Segment")->head(); s; s = inext(s))
   {
   s->set("start", phone_start);
   phone_start = s->F("end");
   }
   phone_start = 0.0;
   
   s = u->relation("Segment")->head();
   if ((s->f("name") == "pau") || (s->f("name") == "sil"))
   {
   w = u->relation("SurfacePhone")->append();
   w->set("name", "pau");
   w->set("end",  s->F("end"));
   w->set("start",  s->F("start"));
   }
   }
   
   //    cout <<"Surface 1:" << *u->relation("SurfacePhone") << endl;
   
   for (i = 0, w = u->relation("Word")->head(); w != 0; w = inext(w), ++i)
   {
   word.clear_relations();
   
   lex_to_phones(w->f("name"), w->f("pos"), 
   *word.relation("Segment"));
   
   if (segfile == "dummy")
   *word.relation("SurfacePhone") = *word.relation("Segment");
   else
   trans_to_phones(w, *u->relation("Segment"), 
   *word.relation("SurfacePhone"));
   
   //	cout << "lex phones: " << *word.relation("LexicalPhone") << endl;
   //	cout << "sur phones: " << *word.relation("SurfacePhone") << endl;
   
   if (siod_get_lval("mettree_phones_debug", NULL) != NIL)
   {
   cout << "phones for word" << *w << endl;
   cout << *word.relation("SurfacePhone") << endl;
   }
   
   nw = word.relation("Word")->append();
   nw->set("name", w->S("name"));
   
   syllabify_word(nw, *word.relation("LexicalPhone"),
   *word.relation("LexicalSylStructure"),
   *word.relation("LexicalSyllable"));
   
   subword_metrical_tree(nw, *word.relation("LexicalSyllable"),
   *word.relation("LexicalMetricalTree"));
   
   if (siod_get_lval("mettree_debug", NULL) != NIL)
   word.save("word_lex.utt", "est");
   
   //	copy_relation(*word.relation("LexicalMetricalTree"), 
   //		      *word.relation("HackMT"));
   
   EST_Item xx;
   dp_match(*word.relation("LexicalPhone"), 
   *word.relation("SurfacePhone"),
   *word.relation("Match"), local_cost, &xx);
   
   if (syllabify_word(nw, *word.relation("SurfacePhone"),
   *word.relation("SurfaceSylStructure"),
   *word.relation("SurfaceSyllable")) < 1)
   {
   cerr << "Pronuciation for \"" << w->S("name") 
   << "\" doesn't contain a vowel: " << 
   *word.relation("SurfacePhone") << endl;
   //	    festival_error();
   }
   
   fix_syllables(nw, word);
   
   subword_metrical_tree(nw, *word.relation("SurfaceSyllable"),
   *word.relation("SurfaceMetricalTree"));
   
   
   if (siod_get_lval("mettree_debug_word", NULL) != NIL)
   word.save("word_dp.utt", "est");
   
   if (siod_get_lval("mettree_debug_word", NULL) != NIL)
   if (get_c_int(siod_get_lval("mettree_debug_word", NULL)) == i)
   word.save("word_nth.utt", "est");
   
   
   utterance_merge(*u, word, w, "LexicalMetricalTree");
   }
   
   //    u->save("zz_parse2.utt", "est");
   
   //    u->save("test.utt");
   
   //    cout <<"Surface 2:" << *u->relation("SurfacePhone") << endl;
   
   add_initial_silence(*u->relation("LexicalPhone"), 
   *u->relation("SurfacePhone"), 
   *u->relation("Match"));
   
   //    cout <<"Surface 3:" << *u->relation("SurfacePhone") << endl;
   
   add_times(*u->relation("LexicalPhone"), *u->relation("SurfacePhone"), 
   *u->relation("Match"));
   
   u->relation("LexicalPhone")->f.set("timing_style", "segment");
   u->relation("SurfacePhone")->f.set("timing_style", "segment");
   //    u->relation("Word")->f.set("timing_style", "segment");
   
   u->relation("LexicalSyllable")->f.set("timing_style", "segment");
   u->relation("LexicalSyllable")->f.set("time_path", 
   "LexicalSylStructure");
   
   //    u->relation("LexicalSylStructure")->f.set("timing_style", "segment");
   //    u->relation("LexicalSylStructure")->f.set("time_relation", 
   //					 "LexicalPhone");
   
   
   u->relation("LexicalMetricalTree")->f.set("timing_style", "segment");
   
   
   //    if (lf_int != NIL)
   //	add_feature_function(*u->relation("LexicalSyllable"),"vowel_start", 
   //			     vowel_start_time);
   
   //    add_feature_function(*u->relation("LexicalPhone"), "start",
   //			 ff_start_time);
   //    add_feature_function(*u->relation("SurfacePhone"), "start",
   //			 ff_start_time);
   //    add_feature_function(*u->relation("SurfacePhone"), "dur",
   //			 duration_time);
   
   //    add_feature_function(*u->relation("LexicalSyllable"),"end", leaf_end_time);
   
   EST_Features tf;
   tf.set("time_path", "LexicalMetricalTree");
   tf.set("end", leaf_end_time);
   
   //    add_feature_string(*u->relation("LexicalMetricalTree"), "time_path", 
   //		       "LexicalMetricalTree");
   //    add_feature_string(*u->relation("LexicalSylStructure"), "time_path", 
   //		       "LexicalSylStructure");
   //
   
   add_non_terminal_features(*u->relation("LexicalMetricalTree"), 
   tf);
   
   tf.set("time_path", "LexicalSylStructure");
   
   add_non_terminal_features(*u->relation("LexicalSylStructure"), 
   tf);
   
   
   //    add_feature_function(*u->relation("LexicalSyllable"),"start",
   //			 ff_start_time);
   //    add_feature_function(*u->relation("LexicalSyllable"),"dur", duration_time);
   
   
   //    cout << "ADDED Features to phone\n\n";
   //    cout << *(u->relation("LexicalPhone")) << endl << endl;
   
   //    cout << "ADDED Features\n\n";
   //    cout << *(u->relation("LexicalSyllable"));
   
   //    cout << "\nfinished\n\n";
   
   //    if (lf_int != NIL)
   //	add_trans_intonation(*u);
   
   //    cout <<"Lexical 3:" << *u->relation("LexicalPhone") << endl;
   
   //    end_to_dur(*u->relation("SurfacePhone"));
   //    end_to_dur(*u->relation("LexicalPhone"));
   
   
   
   
   //    cout <<"Lexical 3:" << *u->relation("LexicalPhone") << endl;
   
   //    clear_feature(*u->relation("SurfacePhone"), "end");
   //    clear_feature(*u->relation("LexicalPhone"), "end");
   
   if (siod_get_lval("mettree_debug", NULL) != NIL)
   u->save("met_data.utt", "est");
   
   gc_unprotect(&lutt);
   
   //    u->save("zz_parse3.utt", "est");
   
   return lutt;
   }
#endif
