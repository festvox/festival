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

#include <cstdio>
#include <cstdlib>
#include <cmath>
#include <fstream>
#include "festival.h"

#include "../UniSyn/us_features.h"

void merge_features(EST_Item *from, EST_Item *to, int keep_id);
void insert_schwa(EST_Item *n);

extern EST_Features phone_def;
void subword_metrical_tree(EST_Item *w, EST_Relation &syllable, 
			   EST_Relation &metricaltree);

void lex_to_phones(const EST_String &name, const EST_String &pos,
		   EST_Relation &phone);
void trans_to_phones(EST_Item *w, EST_Relation &trans,
		     EST_Relation &phone);

void fix_syllables(EST_Item *nw, EST_Utterance &word);

typedef
float (*local_cost_function)(const EST_Item *item1,
			     const EST_Item *item2);

void add_metrical_functions(EST_Utterance &utt);
bool dp_match(const EST_Relation &lexical,
	      const EST_Relation &surface,
	      EST_Relation &match,
	      local_cost_function lcf,
	      EST_Item *null_syl);
float local_cost(const EST_Item *s1, const EST_Item *s2);

void add_times(EST_Relation &lexical, EST_Relation &surface, 
	       EST_Relation &match);

void add_initial_silence(EST_Relation &lexical, EST_Relation &surface, 
	       EST_Relation &match);

void subword_metrical_tree(EST_Relation &syllable, 
			   EST_Relation &metricaltree);

void add_metrical_functions(EST_Utterance &utt);

int syllabify_word(EST_Item *nw, EST_Relation &phone, 
		    EST_Relation &sylstructure, EST_Relation &syl, int flat);

void add_even_segment_times(EST_Item *w, EST_Relation &phone);
void lex_to_phones(EST_Utterance &u, const EST_String &relname);
void phonemic_trans(EST_Relation &trans);

void add_single_phrase(EST_Utterance &utt, EST_Item *t);

LISP FT_add_trans_metrical_tree(LISP l_utt, LISP lf_input, LISP lf_output);
#if 0
static void add_trans_phrase(EST_Utterance &utt, const EST_String &i_name,
			  const EST_String &s_name);
#endif

float local_cost(const EST_Item *s1, const EST_Item *s2)
{
    float insertion_cost = get_c_int(siod_get_lval("met_insertion", NULL));
    float deletion_cost = get_c_int(siod_get_lval("met_deletion", NULL));
    float substitution_cost = 
	get_c_int(siod_get_lval("met_substitution", NULL));

    EST_String null_sym = "nil";

    // otherwise cost is either insertion cost, or cost_matrix value
    if (s1->name() == s2->name())
	return 0;
    else
    {
	if (s1->name() == null_sym)
	    return insertion_cost;
	else if (s2->name() == null_sym)
	    return deletion_cost;
	else 
	    return substitution_cost;
    }
}

void trans_to_phones(EST_Item *w, EST_Relation &trans, EST_Relation &phone)
{
    int prev_phone;
    EST_Item *t, *p;
    int r;

    prev_phone = iprev(w) ? iprev(w)->I("phon_ref") : -1;
    r = w->I("phon_ref");

    for (t = trans.head(); t; t = inext(t))
    {
	if ((t->f("name") == "sil") || (t->f("name") == "pau"))
	    continue;
	if ((t->I("ref") > prev_phone) && (t->I("ref") <= r))
	{
	    p = phone.append();
	    p->set("name", t->S("name"));
	    p->set_val("end", t->f("end"));
	    p->set_val("start", t->f("start"));
	    p->set("df", phone_def.A(p->S("name")));
	}	
    }
}

void add_trans_intonation(EST_Utterance &utt, const EST_String &i_name,
			  const EST_String &s_name, int add_words)
{
    EST_Item *s, *w, *t, *a, *b;
    EST_String wref;
    int s_num;
    EST_String w_num;
    EST_String is_name = i_name + s_name;

    utt.relation(i_name)->f.set("intonation_style", "tilt");

    utt.create_relation(is_name);
    cout << "created : " << is_name << endl;

    // optional feature to add intonation events to words rather than syllables
    if (add_words)
	utt.create_relation("IntonationWord");

    for (t = utt.relation(i_name, 1)->head(); t; t = unext(t))
	{
	    t->f_remove("end");
	    if (!t->f_present("word_ref"))
		add_single_phrase(utt, t);
	    else
		{
		    w_num = t->S("word_ref");
		    s_num = t->I("syl_num");

		    for (w = utt.relation("Word", 1)->head(); w; w = inext(w))
		    {
			if (w->S("id") == w_num)
			    break;
		    }
		    if (w == 0)
			{
			    cerr << "Error: couldn't find word ref " << endl;
			    cerr << "For intonation event " << *t << endl;
			    festival_error();
			}
		    if (add_words)
		    {
			if (!w->in_relation("IntonationWord"))
			    b = utt.relation("IntonationWord")->append(w);
			else
			    b = w->as_relation("IntonationWord");
			b->append_daughter(t);
		    }

		    //	cout << "matching word: " << w->name() << endl;
		    if ((b = w->as_relation("WordStructure")) == 0)
			EST_error("Item is not in WordStructure\n");
		    if ((s = nth_leaf(b, s_num)) == 0)
		    {
			cerr << "Intonation element " << *t << 
			    "\nis linked to syllable " << s_num  <<
			    " but word \"" << w->S("name") << "\""
			    " has only " << num_leaves(b) << " syllables\n";
		    }
		    //	cout << "here is s\n";
		    //	cout << "matching syllable: " << *s << endl;

		    if (!s->in_relation(is_name))
			a = utt.relation(is_name)->append(s);
		    else
			a = s->as_relation(is_name);
		    a->append_daughter(t);

//		    cout << "s1: " << s->S("id", "XX") << endl;
		    s = s->as_relation(s_name);
		    if (s == 0)
			cerr << "Syllable with id " << nth_leaf(b, s_num)->S("id") << "exists "
			    "but is not in syllable relation. Suspect corrupted "
			    "lexical conversion\n";

//		    cout << "s2: " << s->S("id", "XX") << endl;

		    // change to relative positions if not already specified
		    if (!t->f_present("rel_pos"))
			t->set("rel_pos", t->F("time") - s->F("vowel_start"));

		    t->set("time_path", is_name);
		    t->set_function("time", 
				    "standard+unisyn_tilt_event_position");
//		    cout << "end syl:" << endl;
		    t->f_remove("word_ref");
		    t->f_remove("syl_num");
		}
	}
//    add_trans_phrase(utt, i_name, s_name);
}

void syl_to_word_intonation(EST_Utterance &utt)
{
    EST_Item *s, *w, *t=0, *b;

    utt.create_relation("IntonationWord");

    for (s = utt.relation("Syllable", 1)->head(); s; s = inext(s))
    {
	if (!s->in_relation("IntonationSyllable"))
	    continue;
    
	w = root(s, "WordStructure");
	
	if (w == 0)
	{
	    cerr << "Error: couldn't find word ref " << endl;
	    cerr << "For intonation event " << *t << endl;
	    festival_error();
	}
	if (!w->in_relation("IntonationWord"))
	    b = utt.relation("IntonationWord")->append(w);
	else
	    b = w->as_relation("IntonationWord");

	for (t = daughter1(s->as_relation("IntonationSyllable")); t; t = inext(t))
	    b->append_daughter(t);
    }
}

static bool legal_daughter(EST_Item *r, const EST_String &iname,
			   const EST_StrList &valid)
{
    if (!r->in_relation(iname))
	return false;
    if (strlist_member(valid, daughter1(r->as_relation(iname))->S("name", "")))
	return true;
    return false;
}

void intonation_diagnostics(EST_Utterance &ref, EST_Utterance &test, 
			    const EST_String &rel, const EST_StrList &valid)
{
    EST_Item *r, *t;
    EST_String iname = "Intonation" + rel;

    for (r = ref.relation(rel, 1)->head(), t = test.relation(rel, 1)->head(); r && t; 
	 r = inext(r), t = inext(t))
    {
	if (legal_daughter(r, iname, valid) && legal_daughter(t, iname, valid))
	    t->set("i_status", "COR");
	else if (legal_daughter(r, iname, valid) && (!legal_daughter(t, iname, valid)))
	    t->set("i_status", "DEL");
	else if (!legal_daughter(r, iname, valid) && legal_daughter(t, iname, valid))
	    t->set("i_status", "INS");
//	else 
//	    t->set("i_status", "0");
    }
}

#if 0
static void add_trans_phrase(EST_Utterance &utt, const EST_String &i_name,
			  const EST_String &s_name)
{
    EST_Item *s, *t, *a, *p;
    float pos, max, d;
    EST_String is_name = i_name + s_name;

    for (t = utt.relation(i_name, 1)->head(); t; t = inext(t))
	{
	    if (t->in_relation(is_name))
		continue;
	    pos = t->F("time");
	    max = 100000.0;

	    cout << "here 1\n";

	    for (p = utt.relation(s_name)->head(); p; p = inext(p))
		{
		    if (t->S("name","0") == "phrase_end")
			d = fabs(pos - p->end());
		    else
			d = fabs(pos - p->start());
		    if (d < max)
			{
			    max = d;
			    s = p;
			}
		}
	    a = utt.relation(is_name)->append(t);
	}
}
#endif

LISP FT_add_trans_intonation(LISP l_utt, LISP lf_int, LISP l_add_words)
{
    EST_String int_file = get_c_string(lf_int);
    EST_Utterance *u = get_c_utt(l_utt);
    EST_Relation lab;
    EST_Item *s, *n;
    int add_words = (l_add_words == NIL) ? 0 : 1;

    u->create_relation("Intonation");
  
    if (lab.load(int_file) != format_ok)
	EST_error("Couldn't load file %s\n", (const char *) int_file);

    for (s = lab.head(); s; s = inext(s))
      {
	n = u->relation("Intonation")->append();
	merge_features(n, s, 1);
	if (n->S("name") =="afb")
	    n->set("name", "a");
	else if (n->S("name") == "m")
	{
	    n->set("name", "a");
	    n->set("minor", 1);
	}
	else if ((n->S("name") == "a") || (n->S("name") == "arb") 
		 || (n->S("name") == "rb") || (n->S("name") == "phrase_end") 
		 || (n->S("name") == "phrase_start") 
		 || (n->S("name") == "fb") )  // tmp check (awb)
	    continue;
	else
	    EST_error("Illegal intonation name \"%s\"\n", (const char *) n->S("name"));
      }

    add_trans_intonation(*u, "Intonation", "Syllable", add_words);
    return l_utt;
}

LISP FT_add_trans_word(LISP l_utt, LISP lf_word, LISP keep_times)
{
    EST_String word_file = get_c_string(lf_word);
    EST_Utterance *u = get_c_utt(l_utt);
    EST_Relation lab;
    EST_Item *s, *n;
    float p_end = 0;

    u->create_relation("Word");
  
    if (lab.load(word_file) != format_ok)
	EST_error("Couldn't load file %s\n", (const char *) word_file);

    for (s = lab.head(); s; s = inext(s))
    {
	s->set("start", p_end);
	p_end = s->F("end");
	if ((s->S("name") == "pau") || (s->S("name") == "sil"))
	    continue;
	n = u->relation("Word")->append();
	merge_features(n, s, 0);
	if (keep_times == NIL)
	{
	    n->f_remove("end");
	    n->f_remove("start");
	}
    }

    return l_utt;
}

LISP FT_add_f0_points(LISP l_utt, LISP lf_f0)
{
    EST_String f0_file = get_c_string(lf_f0);
    EST_Utterance *u = get_c_utt(l_utt);
    EST_Track f0;
    EST_Item *s;
    float prev_mid, next_mid;

    if (f0.load(f0_file) != format_ok)
	EST_error("Couldn't load file %s\n", (const char *) f0_file);

    for (s = u->relation("Segment")->head(); s; s = inext(s))
    {
	prev_mid = iprev(s) ? 
	    (iprev(s)->F("end") + iprev(s)->F("start"))/2.0 : 0.0;
	next_mid = inext(s) ? 
	    (inext(s)->F("end") + inext(s)->F("start"))/2.0 : 0.0;

	s->set("prev_mid_f0", f0.a(f0.index(prev_mid)));
	s->set("start_f0", f0.a(f0.index(s->F("start"))));
	s->set("mid_f0", f0.a(f0.index((s->F("end") + s->F("start"))/2.0)));
	s->set("end_f0", f0.a(f0.index(s->F("end"))));
	s->set("next_mid_f0", f0.a(f0.index(next_mid)));
    }

    return l_utt;
}

LISP FT_add_coefs(LISP l_utt, LISP lf_coef)
{
    EST_String coef_file = get_c_string(lf_coef);

    EST_Utterance *u = get_c_utt(l_utt);
    EST_Track coef;
    EST_Item *s;
    float prev_mid, next_mid;
    EST_FVector *frame;

    cout << "loading\n";
    if (coef.load(coef_file) != format_ok)
	EST_error("Couldn't load file %s\n", (const char *) coef_file);
    cout << "done\n";

    frame = new EST_FVector;
    frame->fill(0.0); // special case for first frame.

    for (s = u->relation("Segment")->head(); s; s = inext(s))
    {
	prev_mid = iprev(s) ? 
	    (iprev(s)->F("end") + iprev(s)->F("start"))/2.0 : 0.0;
	next_mid = inext(s) ? 
	    (inext(s)->F("end") + inext(s)->F("start"))/2.0 : 0.0;

	frame = new EST_FVector;
	coef.copy_frame_out(coef.index((s->F("end") + s->F("start"))/2.0), 
				       *frame);
	s->set_val("mid_coef", est_val(frame));

	frame = new EST_FVector;
	coef.copy_frame_out(coef.index(s->F("end")), *frame);
	s->set_val("end_coef", est_val(frame));

	frame = new EST_FVector;
	coef.copy_frame_out(coef.index(s->F("start")), *frame);
	s->set_val("start_coef", est_val(frame));

	frame = new EST_FVector;
	coef.copy_frame_out(coef.index(prev_mid), *frame);
	s->set_val("prev_mid_coef", est_val(frame));

	frame = new EST_FVector;
	coef.copy_frame_out(coef.index(next_mid), *frame);
	s->set_val("next_mid_coef", est_val(frame));
    }

    return l_utt;

//	imid = coef.index((s->F("end") + s->F("start"))/2.0);
//	iend = coef.index(s->F("end"));

}

LISP FT_add_xml_relation(LISP l_utt, LISP xml_file)
{
    EST_Utterance *u, tmp;

    u = get_c_utt(l_utt);

    tmp.clear();
    tmp.load(get_c_string(xml_file));

    EST_Features::Entries p;

    for (p.begin(tmp.relations); p; ++p)
    {
	relation(p->v)->remove_item_feature("actuate");
	relation(p->v)->remove_item_feature("estExpansion");
	relation(p->v)->remove_item_feature("xml:link");
	relation(p->v)->remove_item_feature("href");
	relation(p->v)->remove_item_feature("show");
    }

    utterance_merge(*u, tmp, "id");

    return l_utt;
}

void fix_syllables(EST_Item *nw, EST_Utterance &word)
{
    EST_Item *t, *n, *s, *m;

    if (word.relation("Syllable")->length() == word.relation("SurfaceSyllable")->length())
	return;
    
    cout << "Word \"" << word.relation("Word")->head()->name() << "\" has " 
	<< word.relation("Syllable")->length() << 
	    " lexical syllables and " <<
		word.relation("SurfaceSyllable")->length() <<
		" surface syllables\n";

    for (s = word.relation("Syllable")->head(); s; s = inext(s))
    {
	t = s->as_relation("SylStructure");
	n = syl_nucleus(t);

	m = daughter1(n->as_relation("Match"));
	if (m == 0)
	    insert_schwa(n->as_relation("Segment"));
    }

    word.relation("SylStructure")->clear();
    word.relation("Syllable")->clear();
    word.relation("Match")->clear();

    syllabify_word(nw, *word.relation("Segment"),
		       *word.relation("SylStructure"),
		       *word.relation("Syllable"), 0);

//    syllabify_word(nw, *word.relation("SurfacePhone"),
//		       *word.relation("SurfaceSylStructure"),
//		       *word.relation("SurfaceSyllable"));

    EST_Item xx;

    dp_match(*word.relation("Segment"), 
	      *word.relation("SurfacePhone"),
	      *word.relation("Match"), local_cost, &xx);

}


/* Add segment durations from file */
void add_trans_duration(EST_Utterance &utt, const EST_String &segfile)
{
    EST_Utterance word;
    EST_Relation phone, lab;
    EST_Item *s, *n;
    EST_StrList plist;
    float phone_start;
    EST_Item xx;

    if (lab.load(segfile) != format_ok)
	EST_error("Couldn't load file %s\n", (const char *) segfile);

    phone_start = 0.0;
    utt.create_relation("LabelSegment");
    utt.create_relation("Match");

    for (s = lab.head(); s; s = inext(s))
    {
	if (!phone_def.present(s->S("name")))
	    EST_error("Phone %s is not defined in phone set\n", (const char *)
		      s->S("name"));
	n = utt.relation("LabelSegment")->append();
	merge_features(n, s, 1);
	n->set("start", phone_start);
	phone_start = s->F("end");
	n->set("dur", n->F("end") -  n->F("start"));
    }
    
    dp_match(*utt.relation("Segment"), *utt.relation("LabelSegment"),
	     *utt.relation("Match"), local_cost, &xx);

    add_times(*utt.relation("Segment"), *utt.relation("LabelSegment"),
	     *utt.relation("Match"));

    for (s = utt.relation("Segment")->head(); s; s = inext(s))
    {
	s->set("target_dur", (s->F("end") - s->F("start")));
	s->f_remove("end");
	s->f_remove("dur");
	s->f_remove("start");
    }
}

static void add_silences(EST_Utterance &utt,EST_Item *w)
{
    EST_Item *s;
    int r;

    if (w == 0)  // insert initial silence
    {
	s = utt.relation("LabelSegment")->head();
	if (s->name() == "pau")
	{
	    EST_Item *sil = utt.relation("Segment")->append();
	    sil->set("name","pau");
	    sil->set("start",s->F("start"));
	    sil->set("end",s->F("end"));
	}
	return;
    }

    cout << "Looking at inserting\n";
    // Intermeditate silences
    r = w->I("phon_ref");
    for (s=utt.relation("LabelSegment")->head(); s; s=inext(s))
    {
	if (r == s->I("ref"))
	{
	    if (inext(s)->name() == "pau")
	    {
		cout << "actually inserting\n";
		EST_Item *sil = utt.relation("Segment")->append();
		sil->set("name","pau");
		sil->set("start",s->F("end"));
		sil->set("end",inext(s)->F("end"));
	    }
	    return;
	}
    }
}

void add_trans_seg(EST_Utterance &utt, const EST_String &segfile)
{
    EST_Utterance word;
    EST_Relation phone, lab;
    EST_Item *s, *w, *nw, *n;
    EST_StrList plist;
    float phone_start;
    LISP lutt;
    int i;
    LISP l_pdef;

    l_pdef = siod_get_lval("darpa_fs", NULL);
    lisp_to_features(l_pdef, phone_def);
    
    utt.create_relation("LabelSegment");
    utt.create_relation("tmpSegment");
    utt.create_relation("Syllable");
    utt.create_relation("Segment");
    utt.create_relation("WordStructure");

    if (lab.load(segfile) != format_ok)
	EST_error("Couldn't load file %s\n", (const char *) segfile);

    phone_start = 0.0;
    
    for (s = lab.head(); s; s = inext(s))
    {
	if (!phone_def.present(s->S("name")))
	    EST_error("Phone %s is not defined in phone set\n", (const char *)
		      s->S("name"));
	n = utt.relation("LabelSegment")->append();
//	cout << "append ls id " << n->S("id") << endl;
	merge_features(n, s, 1);
//	cout << "keep ls id " << n->S("id") << endl;
	n->set("start", phone_start);
	phone_start = s->F("end");
    }
    
//    phonemic_trans(*utt.relation("LabelSegment"));
/*    for (w = utt.relation("Word")->head(); w != 0; w = n)
    {
	n = inext(w);
	w->f_remove("end");
	if ((w->f("name") == "sil") || (w->f("name") == "pau"))
	    utt.relation("Word")->remove_item(w);
    }
*/    
    gc_protect(&lutt);
    
    word.create_relation("Word");
    word.create_relation("Match");
    word.create_relation("NewMatch");
    word.create_relation("Segment");
    word.create_relation("tmpSegment");
    word.create_relation("SylStructure");
    word.create_relation("Syllable");
    word.create_relation("WordStructure");
    
    // Note starts are hardwired here because feature function thing
    // isn't fully operational and because deleting silence messes
    // it up.
    
/*    s = utt.relation("LabelSegment")->head();
    if ((s->f("name") == "pau") || (s->f("name") == "sil"))
    {
	w = utt.relation("SurfacePhone")->append();

	w->set("name", "pau");
	w->set("end",  s->F("end"));
	w->set("start",  s->F("start"));
	w->set("df", phone_def.A("pau"));
    }
*/

    add_silences(utt,0);

    for (i = 0, w = utt.relation("Word")->head(); w != 0; w = inext(w), ++i)
    {
	word.clear_relations();
	word.f.set("max_id", 0);
	cout << "word: " << *w << endl;
	lex_to_phones(w->f("name"), w->f("pos", ""), 
		      *word.relation("Segment"));
	trans_to_phones(w, *utt.relation("LabelSegment"), 
			*word.relation("tmpSegment"));
	
	nw = word.relation("Word")->append();
	nw->set("name", w->S("name"));
	
	syllabify_word(nw, *word.relation("Segment"),
		       *word.relation("SylStructure"),
		       *word.relation("Syllable"), 0);
	
//	subword_list(nw, *word.relation("Syllable"),
//			      *word.relation("MetricalTree"));
	
	if (siod_get_lval("mettree_debug", NULL) != NIL)
	    word.save("word_lex.utt", "est");
	
	EST_Item xx;
	dp_match(*word.relation("Segment"), *word.relation("tmpSegment"),
		 *word.relation("Match"), local_cost, &xx);

	
//	fix_syllables(nw, word);

	subword_metrical_tree(nw, *word.relation("Syllable"), 
			      *word.relation("WordStructure"));
//	cout << "C2\n";
	
	if (siod_get_lval("mettree_debug_word", NULL) != NIL)
	    word.save("word_dp.utt", "est");
	
	if (siod_get_lval("mettree_debug_word", NULL) != NIL)
	    if (get_c_int(siod_get_lval("mettree_debug_word", NULL)) == i)
		word.save("word_nth.utt", "est");
	
	word.remove_relation("SurfaceSylStructure");
	word.remove_relation("SurfaceMetrcialTree");
	word.remove_relation("SurfaceSyllable");
	//cout << "32\n";
	EST_String wid = w->S("id");
	utterance_merge(utt, word, w, word.relation("Word")->head());
	
	w->set("id", wid);

	add_silences(utt,w);
    }
    cout << "time2\n";
    
//    utt.save("test.utt");
    
    
/*    add_initial_silence(*utt.relation("Segment"), 
			*utt.relation("SurfacePhone"), 
			*utt.relation("Match"));
			*/
    
    add_times(*utt.relation("Segment"), *utt.relation("LabelSegment"), 
	      *utt.relation("Match"));
    
    //    utt.relation("Word")->f.set("timing_style", "segment");
    //    cout << "here d\n";
    
//    add_feature_function(*utt.relation("SurfacePhone"), "dur",
//		 usf_duration);
    
    add_metrical_functions(utt);

    // if silences aren't wanted we still have to build with them so that
    // start times before pauses are done properly.
    if (!siod_get_lval("unisyn_build_with_silences",NULL))
	for (s = inext(utt.relation("Segment")->head());s;s = inext(s))
	    if ((iprev(s)->S("name") != "pau") && (iprev(s)->S("name") != "sil"))
	    	s->set_function("start", "standard+unisyn_start");
	    else
	    	utt.relation("Segment")->remove_item(iprev(s));



    utt.relation("Segment")->remove_item_feature("stress_num");
    utt.relation("Word")->remove_item_feature("phon_ref");
    
    utt.remove_relation("tmpSegment");
    
    if (siod_get_lval("mettree_debug", NULL) != NIL)
        utt.save("met_data.utt", "est");
    
    gc_unprotect(&lutt);
     cout << "here c\n";
}


LISP FT_add_trans_seg(LISP l_utt, LISP lf_seg)
{
    add_trans_seg(*get_c_utt(l_utt), get_c_string(lf_seg));
    return l_utt;
}

LISP FT_add_trans_duration(LISP l_utt, LISP lf_seg)
{
    add_trans_duration(*get_c_utt(l_utt), get_c_string(lf_seg));
    return l_utt;
}

LISP FT_syl_to_word_intonation(LISP l_utt)
{
    syl_to_word_intonation(*get_c_utt(l_utt));
    return l_utt;
}

LISP FT_intonation_diagnostics(LISP l_ref, LISP l_test, LISP l_rel_name, LISP l_valid)
{
    EST_StrList valid;

    siod_list_to_strlist(l_valid, valid);
    intonation_diagnostics(*get_c_utt(l_ref), *get_c_utt(l_test), 
			   get_c_string(l_rel_name), valid);

    return NIL;
}


/*LISP FT_metrical_data(LISP lf_word, LISP lf_seg, LISP lf_int, LISP lf_met)
{
    EST_Utterance *u = new EST_Utterance;
    LISP l_utt = siod_make_utt(u);
    EST_String word_file = get_c_string(lf_word);

    u->f.set("fileroot", basename(word_file, "*"));

    if (lf_met)
    {
	if (siod_get_lval("us_xml_metrical_trees", NULL) != NIL)
	    data_metrical_tree(*u, get_c_string(lf_met), "xml");
	else
	    data_metrical_tree(*u, get_c_string(lf_met), "");
    }
    else
	syntax_metrical_tree(*u, get_c_string(lf_word));

    if (lf_int)
	data_metrical_lex(*get_c_utt(l_utt), get_c_string(lf_seg), 
			  get_c_string(lf_int));
    else
	data_metrical_lex(*get_c_utt(l_utt), get_c_string(lf_seg), "");

    return l_utt;
}
*/



/* Specific to s-expression weather.
   Should be replaced when metrical trees go into XML.

void data_metrical_tree(LISP l, EST_Item *met_parent, EST_Relation &word)
{
    EST_String mv, name;
    int id, phon_ref;
    LISP a;
    EST_Item *m;

    //cout << "full entry\n";
//    lprint(l);
    //cout << "now parsing\n";

    mv = get_c_string(car(l));
    //cout << "adding node strength: " << mv << endl;
    // root nodes are added in calling routine.
    if (mv != "r")
	m = met_parent->append_daughter();
    else
	m = met_parent;
    m->set("MetricalValue", mv);
    
    if (siod_atomic_list(cdr(l)))
	{
	    // cout << "atomic cdr is: ";
//	    lprint(cdr(l));
	    a = cdr(l);
	    name = get_c_string(car(a));
	    phon_ref = get_c_int(car(cdr(a)));
	    id = get_c_int(car(cdr(cdr(a))));
	    m->set("name", name);
	    m->set("phon_ref", phon_ref);
	    m->set("id", id);
	    word.append(m);
	    
	    //cout << "adding " << name << " on id: " << id << endl;
	    return;
	}
    //cout << "\ndoing left branch\n";
    data_metrical_tree(car(cdr(l)), m, word);
    //cout << "\ndoing right branch\n";
    data_metrical_tree(car(cdr(cdr(l))), m, word);
}
*/

int lisp_tree_to_xml(ofstream &outf, LISP l)
{
    int id;
    LISP a;
    EST_String mv;

    //cout << "full entry\n";
//    lprint(l);
    //cout << "now parsing\n";

    mv = get_c_string(car(l));

    outf << "<elem metrical_value=\"" << mv << "\"";
    
    if (siod_atomic_list(cdr(l)))
	{
	    // cout << "atomic cdr is: ";
//	    lprint(cdr(l));
	    a = cdr(l);
	    id = get_c_int(car(cdr(cdr(a))));
	    outf << " href=\"&w;#id(" << id << ")\" />\n";
	    return 0;
	}
    else
	outf << ">\n";

    if (lisp_tree_to_xml(outf, car(cdr(l))))
	outf << "</elem>\n";
    if (lisp_tree_to_xml(outf, car(cdr(cdr(l)))))
	outf << "</elem>\n";

    return 1;
}

LISP FT_add_trans_metrical_tree(LISP l_utt, LISP lf_input, LISP lf_output)
{
    EST_Utterance *utt;
    LISP lmet, l;
    EST_Item *m;

    utt = get_c_utt(l_utt);
    
    utt->create_relation("Word");
    utt->create_relation("Token");
    utt->create_relation("MetricalTree");

    lmet = vload(get_c_string(lf_input), 1);


    ofstream outf;
    outf.open(get_c_string(lf_output));

    outf << "<?xml version='1.0'?>\n";
    outf << "<!DOCTYPE relation SYSTEM \"relation.dtd\" [\n";
    outf << "<!ATTLIST elem metrical_value (s | r | w) #REQUIRED>]>\n";

    outf << "<relation name=\"MetricalTree\" structure-type=\"tree\">n";

//	lprint(lmet);

	// have to ensure that next id of build nodes is greater than
	// any in the file. This should be done properly sometime.
    utt->set_highest_id(10000);
	
    for (l = lmet; l ; l = cdr(l))
    {
	m = utt->relation("MetricalTree")->append();
//	    cout << "\nNew Tree\n";
	if (lisp_tree_to_xml(outf, car(l)))
	    outf << "</elem>" << endl;
	}

    outf << "</relation>\n";

    return l_utt;
}
