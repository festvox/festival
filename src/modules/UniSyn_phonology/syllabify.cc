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

extern EST_Features phone_def;

static int nucleus_count(EST_Relation &phone)
{
    int v = 0;
    for (EST_Item *l = phone.head(); l; l = inext(l))
	if (l->S("df.syllabic") == "+")
	    ++v;

    return v;
}

static bool legal_c1(EST_Item *x)
{
    return (x->S("df.syllabic") == "-");
}

static bool legal_c2(EST_Item *c1, EST_Item *x)
{
    if (x->S("df.syllabic") == "+")
	return false;

    if ((x->name() == "s") && ((c1->name() == "l") || (c1->name() ==
		       "w") || (c1->name() == "p") || (c1->name() ==
		       "t") || (c1->name() == "k") || (c1->name() ==
		       "m") || (c1->name() == "n") || (c1->name() ==
		       "r") || (c1->name() == "f"))) 
	return true;


    if ((c1->S("df.manner") == "approximant") &&
	 (x->S("df.manner") == "stop") || (x->name() == "th") 
					|| (x->name() == "f"))
//    if (ph_is_semivowel(c1->name()) && (ph_is_stop(x->name()) 
//					|| (x->name() == "th") 
//					|| (x->name() == "f")))
    {
	if (x->name() == "y")
	    return false;

	if ((c1->name() == "l") && ((x->name() == "t") || 
				    (x->name() == "d") || (x->name() == "th")))
	    return false;

	if ((c1->name() == "w") && ((x->name() == "p") || 
				    (x->name() == "b") || (x->name() == "f")))
	    return false;
	return true;
    }
    // for "vroom"
    if ((c1->name() == "r") && (x->name() == "v"))
	return true;
    return false;
}
static bool legal_c3(EST_Item * c1, EST_Item * c2, EST_Item *pos)
{
  (void) c1;
    if (pos->name() != "s")
	return false;
    if ((c2->S("df.manner") == "stop") && (c2->S("df.voicing") == "-"))
	return true;
    return false;
}

EST_Item *make_onset(EST_Item *syl_struct_root, EST_Item *nucleus, int flat)
{
    EST_Item *c1, *c2, *c3;
    EST_Item *p, *onset;

    // if first syllable in word, put all prevocalic segments in onset
    if ((iprev(syl_struct_root) == 0) && iprev(nucleus))
    {
	if (flat)
	    onset = syl_struct_root;
	else
	{
	    onset = daughter1(syl_struct_root)->insert_before();
	    onset->set("sylval", "Onset");
	}
	// tmpeorary hack because of lack of prepend daughter fn.
	EST_Item *s;
	for (s = iprev(nucleus); iprev(s); s = iprev(s));
	for (c1 = s; c1 != nucleus; c1 = inext(c1))
	    onset->append_daughter(c1);
	return 0;
    }

    c1 = iprev(nucleus);
    if (c1 == 0)
	return c1;

//    if (ph_is_vowel(c1->name()))
//	return inext(c1);
    if (c1->S("df.syllabic") == "+")
	return inext(c1);

//    if (c1->S("df.type") == "vowel")
//	return inext(c1);

    if (flat)
	onset = syl_struct_root;
    else
    {
	onset = daughter1(syl_struct_root)->insert_before();
	onset->set("sylval", "Onset");
    }

    // add first consonant
    if (legal_c1(c1))
	p = onset->append_daughter(c1);
    else
	return nucleus;

    // add second consonant
    c2 = iprev(c1);
    if (c2 == 0)
	return 0;
    if (legal_c2(c1, c2))
	p = p->insert_before(c2);
    else
	return c1;

    // add third consonant (s)
    c3 = iprev(c2);
    if (c3 == 0)
	return 0;

    // add third consonant
//    if (legal_c3(c1->name(), c2->name(), c3->name()))
//	p = p->insert_before(c3);
    if (legal_c3(c1, c2, c3))
	p = p->insert_before(c3);
    else
	return c2;

    return c3;
}

static void make_nucleus(EST_Item *syl_struct_root, EST_Item *nucleus, 
			 int flat)
{
    EST_Item *m;

    if (flat)
	m = syl_struct_root;
    else
    {
	// add rhyme
	m = syl_struct_root->append_daughter();
	m->set("sylval", "Rhyme");
    
	// add nucleus
	m = m->append_daughter();
	m->set("sylval", "Nucleus");
    }

    m->append_daughter(nucleus);
}

static void make_coda(EST_Item *first_coda, EST_Item *first_onset, 
		      EST_Item *syl_struct_root, int flat)
{
    EST_Item *m=0;;

    if ((first_coda != 0) && (first_coda != first_onset))
    {
	if (flat)
	    m = syl_struct_root;
	else
	{
	    m = daughter1(syl_struct_root);
	    if (m->f("sylval") != "Rhyme")
		m = daughter2(syl_struct_root);

	    m = m->append_daughter();
	    m->set("sylval", "Coda");
	}
    }

    for (; (first_coda != 0) && (first_coda != first_onset); 
	 first_coda = inext(first_coda))
	m->append_daughter(first_coda);
}

// if "flat" is set to 1 a tree syllable structure is built, otherwise
// a flat list like structure is built.

int syllabify_word(EST_Item *w, EST_Relation &phone, 
		   EST_Relation &sylstructure, 
		   EST_Relation &syllable,
		   int flat)
{
    EST_Item *prev_syl, *this_syl, *l, *this_struct, *prev_struct;
    EST_Item *first_onset, *first_coda = 0;
    EST_String v;

//    cout << "phones: " << phone << endl;

    int count = nucleus_count(phone);

    prev_struct =first_onset = 0;
    
    if (count < 1)
	return 0;

    for (prev_syl = 0, l = phone.head(); l; l = inext(l))
    {
//	cout << "type " << l->S("name") << ": " << l->S("df.type") << endl;
	if (l->S("df.syllabic") == "+")
//	if (ph_is_vowel(l->name()))
	{ 

	    if (count == 1)
		this_syl = syllable.append(w);
	    else
		this_syl = syllable.append();
	    this_struct = sylstructure.append(this_syl);
	    this_syl->set("stress_num", l->I("stress_num"));

	    // note: it must be in this order
//	    cout << "this struct: " << *this_syl << endl;
//	    cout << "this struct: " << *this_struct << endl;
	    make_nucleus(this_struct, l, flat);
    
	    first_onset = make_onset(this_struct, l, flat);

	    make_coda(first_coda, first_onset, prev_struct, flat);

	    prev_syl = this_syl;
	    prev_struct = this_struct;
	    first_coda = inext(l);
	}
    }

    make_coda(first_coda, first_onset, prev_struct, flat);
    return count;
}

void fix_lex_string(LISP lpos, EST_StrList &s)
{
    LISP a, b, c;
    EST_String p;

    for (a = car(lpos); a != NIL; a = cdr(a))
    {
//	cout << "1:\n";
//	lprint(a);
//	for (b = car(a); b != NIL; b = cdr(b))
//	 {
	b = car(a);
//	cout << "0:\n";
//	lprint(b);
	for (c = car(b); c != NIL; c = cdr(c))
	{
	    p = get_c_string(car(c));
	    if (ph_is_vowel(p))
		p += "0";
	    s.append(p);
	}
    }
//    cout << "def list: " << s << endl;
}

//Adds phoneme name to syllable as a string
void add_syllable_name(EST_Item *syl, const EST_String &fname)
{
    EST_Item *s, *p;
    
    s = syl->as_relation("SylStructure");

    for (p = first_leaf_in_tree(s); p != next_leaf(last_leaf_in_tree(s));
	 p = next_leaf(p))
	if (p ==  first_leaf_in_tree(s))
	    s->set(fname, p->S("name"));
	else
	    s->set(fname, s->S(fname) + " " + p->S("name"));
}

void lex_to_phones(const EST_String &name, const EST_String &pos,
		   EST_Relation &phone)
{
    LISP entry, lpos;
    EST_Item *p;
    EST_StrList lex_def;
    EST_String lex_phone;

    if (pos != "0")
	lpos = rintern(pos);
    else 
	lpos = NIL;
    entry = lex_lookup_word(name, lpos);

    lpos = cdr(cdr(entry));

    if (!siod_atomic_list(car(lpos)))
	fix_lex_string(lpos, lex_def);
    else
	siod_list_to_strlist(car(lpos), lex_def);
    
    for (EST_Litem *sl = lex_def.head(); sl; sl = inext(sl))
    {
	p = phone.append();
	lex_phone = lex_def(sl);
	if (lex_phone.contains(RXint))
	{
	    p->set("name", lex_phone.before(RXint));
	    p->set("stress_num", lex_phone.at(RXint));
	}
	else
	    p->set("name", lex_phone);
	
	// df = "distinctive features"
	if (phone_def.present(p->S("name")))
	    p->set("df", phone_def.A(p->S("name")));
	else
	    EST_error("Word \"%s\" has illegal phoneme \"%s\" in lexicon\n",
		      (const char *)name, (const char *)p->S("name"));
    }
}


/*static bool legal_c3(EST_String c1, EST_String c2, EST_String pos)
{
  (void) c1;
    if (pos != "s")
	return false;
    if (ph_is_stop(c2) && (!ph_is_voiced(c2)))
	return true;
    return false;
}
*/



/*static int vowel_count(EST_StrList &full)
{
    int v = 0;
    EST_Litem *l;

    for (l = full.head(); l; l = next(l))
	if (ph_is_stress_vowel(full(l)))
	    ++v;
    return v;
}


static bool ph_is_s(const EST_String &p)
{
    return (p == "s") ? true : false;
}
*/

/*void subword_phonology(EST_Utterance &word)
{
    word.create_relation("MetricalTree");
    word.create_relation("SylStructure");
    word.create_relation("Syllable");
    
    syllabify_word(word);
    subword_metrical_tree(word);
    
    if (siod_get_lval("mettree_debug", NULL) != NIL)
	word.save("word.utt", "est");
}
*/

/*bool vowel(EST_String p)
{
    if (p.contains(RXint))
	p = p.before(RXint);
    return ph_is_vowel(p);
}
*/

// Add phones, and sylstructure for a single syllable 


/*bool met_node_is_leaf(EST_Item *met_node)
{
    return met_node->in_relation("Syllable");
}
*/


/*static int sonority(EST_String p)
{
    if (p.contains(RXint))
	p = p.before(RXint);
    if (ph_is_vowel(p))
	return 6;
    if (ph_is_liquid(p) || ph_is_approximant(p))
	return 5;
    if (ph_is_nasal(p))
	return 4;
    if (ph_is_fricative(p) && (!ph_is_s(p)))
	return 3;
    if (ph_is_stop(p))
	return 2;
    return 1;
}

// Parse arbitrary phone string with numbered vowels into
// syllable, phone and sylstructure relations

static bool ph_is_semivowel(EST_String c1)
{
    return (ph_is_liquid(c1) || (ph_is_approximant(c1)));
}

static bool ph_is_s(EST_String c1)
{
    return (c1 == "s");
}
*/
/*static bool ph_is_stress_vowel(EST_String p)
{
    if (p.contains(RXint))
	p = p.before(RXint);
//    cout << "p = " << p << endl;
    return (ph_is_vowel(p));
}


static int vowel_count(EST_Relation &phone)
{
    int v = 0;
    for (EST_Item *l = phone.head(); l; l = inext(l))
	if (ph_is_vowel(l->name()))
	    ++v;
    return v;
}
*/
/*static void syllabify_word(EST_Utterance &word)
{
    EST_Item *prev_syl, *this_syl, *l;
    EST_Item *first_onset, *first_coda = 0;
    EST_String v;

    first_onset = 0;
    
    if (nucleus_count(*word.relation("Phone")) < 1)
    {
	cerr << "Error: Pronunciation for " << 
	    *(word.relation("Word")->head()) << " does not contain vowel\n";
	festival_error();
    }

    for (prev_syl = 0, l = word.relation("Phone")->head(); l; l = inext(l))
    {
	cout << "syl: " << l->S("name") << ": " << l->S("df.syllabic", 1) 
	    << endl;
	if (l->S("df.syllabic") == "+")
	{   
	    this_syl = word.relation("Syllable")->append();
	    word.relation("SylStructure")->append(this_syl);
	    this_syl->set("stress_num", l->I("stress_num"));

	    // note: it must be in this order
	    make_nucleus(this_syl->as_relation("SylStructure"), l);
    
	    first_onset = make_onset(this_syl->as_relation("SylStructure"),l);

	    make_coda(first_coda, first_onset, 
		      prev_syl->as_relation("SylStructure"));

	    prev_syl = this_syl;
	    first_coda = inext(l);
	}
    }

    make_coda(first_coda, first_onset, prev_syl->as_relation("SylStructure"));
}
*/


/*void convert_cmu_lex_to_utt(EST_Utterance &u, EST_Item *w)
{
    LISP entry, lpos;
    EST_String pos;
    EST_Utterance word;
    EST_Item *nw, *p;
    EST_StrList lex_def;
    
    pos = w->f("pos");
    if (pos != "0")
	lpos = rintern(pos);
    else 
	lpos = NIL;
    entry = lex_lookup_word(w->name(), lpos);
    
    lprint(entry);
    
    lpos = cdr(cdr(entry));
    
    printf("lpos\n");
    lprint(lpos);
    
    word.create_relation("Word");
    word.create_relation("MetricalTree");
    word.create_relation("SylStructure");
    word.create_relation("Syllable");
    word.create_relation("Phone");
    
    nw = word.relation("Word")->append();
    nw->set("name", w->S("name"));
    
    //    parse_lex_string(word, car(lpos));
    EST_String def, lex_phone;
    
    //    cout << "DEF\n";
    lprint(lpos);
    
    
    // make phone relation
    siod_list_to_strlist(car(lpos), lex_def);
    
    for (EST_Litem *sl = lex_def.head(); sl; sl = inext(sl))
    {
	p = word.relation("Phone")->append();
	lex_phone = lex_def(sl);
	if (lex_phone.contains(RXint))
	{
	    p->set("name", lex_phone.before(RXint));
	    p->set("stress_num", lex_phone.at(RXint));
	}
	else
	    p->set("name", lex_phone);
    }
    
    syllabify_word(word);
    
    //    cout << "before MT: " << *w << " F:" << w->f << endl;
    //    cout << "before MT: " << *(word.relation("Syllable")->head()->Info()) 
    //	<< " F:" << word.relation("Syllable")->head()->Info()->f << endl;
    
    subword_metrical_tree(word);
    
    // temporary hack because single syllable words don't get a metrical
    // node
    //    if ((word.relation("MetricalTree")->head() == 0) &&
    //	(word.relation("Syllable")->head() != 0))
    //	word.relation("MetricalTree")->append(word.relation("Syllable")->head()->Info());
    
    if (siod_get_lval("mettree_debug", NULL) != NIL)
	word.save("word.utt", "est");
    //    u.save("before.utt", "est");
    //    cout << "before merge: " << *w << " F:" << w->f << endl;
    //    cout << "before syl: " << *(word.relation("Syllable")->head()->Info()) 
    //	<< " F:" << word.relation("Syllable")->head()->Info()->f << endl;
    
    utterance_merge(u, word, w, "MetricalTree");
    //    cout << "after merge: " << *w << " F:" << w->f << endl;
    //    cout << "after syl: " << *(word.relation("Syllable")->head()->Info()) 
    //	<< " F:" << word.relation("Syllable")->head()->Info()->f << endl;
    
    //    u.save("after.utt", "est");
}

*/

/*static void subword_metrical_tree(EST_Utterance &word)
{
    EST_Item *s;
    EST_Item *new_leaf;
    
    //    cout << endl<< endl << *(word.relation("Word")->head()->Info()) << endl;
    
    //    cout << "head: " << word.relation("Syllable")->head() << endl;
    //    cout << "head: " << *(word.relation("Syllable")->head()->Info()) << endl;
    //    cout << "pre foot iteration:" << *(s->Info()) << " stress: " << s->Info()->f("stress_num") << endl << endl;
    
    // absorb initial unstressed syllables
    for (s = word.relation("Syllable")->head(); 
	 s && (s->f("stress_num") == 0); s = inext(s))
    {
	//	cout << "**1 syl:" << *s << endl;
	new_leaf = word.relation("MetricalTree")->append(s);
	new_leaf->set("MetricalValue", "w");
    }
    
    //    cout << "utt to now 1c: " << word << endl;
    // In a multi-syllable word
    
    if (inext(word.relation("Syllable")->head()))
    {
	//s = word.relation("Syllable")->head(); 
	//	cout << "**2 syl:" << *s << endl;
	for (; s;)
	{	
	    cout << "**3 syl:" << *s << endl;
	    cout << "foot iteration\n" << *s << endl << endl;
	    new_leaf = word.relation("MetricalTree")->append(s);
	    new_leaf->set("MetricalValue", "s");
	    //	    s = make_foot(new_leaf, inext(s));
	}
    }
    
    else if (s)			// For single syllable words
    {	
	//	cout << "adding single node\n" << *s << endl << endl;
	new_leaf = word.relation("MetricalTree")->append(s);
	//	cout << "added node\n" << *s << endl << endl;
    }
    
    //    cout << "utt to now 2: " << word << endl;
    
    if (siod_get_lval("mettree_debug", NULL) != NIL)    
	word.save("sub_word.utt", "est");
    
    s = word.relation("MetricalTree")->head(); 
    
    //    make_super_foot(s, inext(s));
    if (siod_get_lval("mettree_debug", NULL) != NIL)
	word.save("super_foot.utt", "est");
    
    //    all_stress(word);
}
*/




