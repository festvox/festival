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
#include "EST_error.h"
#include "us_features.h"

void add_feature_function(EST_Relation &r, const EST_String &fname,
			  const EST_String &funcname)
{
    for (EST_Item *p = r.head(); p; p = inext(p))
	p->set_function(fname, funcname);
}

void add_non_terminal_features(EST_Item *s,
			      EST_Features &f)
{
    EST_Features::Entries a;

    for (EST_Item *p = s; p; p = inext(p))
    {
	if (daughter1(p) != 0)
	{
	    add_non_terminal_features(daughter1(p), f);
	    for (a.begin(f); a; ++a)
		p->set_val(a->k, a->v);
	}
    }
}

void add_non_terminal_features(EST_Relation &r, 
			       EST_Features &f)
{
    add_non_terminal_features(r.head(), f);
}


EST_Item *named_daughter(EST_Item *syl, const EST_String &fname, 
			 const EST_String &fval)
{
    if ((daughter1(syl) != 0) && (daughter1(syl)->f(fname) == fval))
	return daughter1(syl);
    if ((daughter2(syl) != 0) && (daughter2(syl)->f(fname) == fval))
	return daughter2(syl);
    return 0;
}

EST_Item *syl_nucleus(EST_Item *syl_struct)
{
    EST_Item *t;
    if (syl_struct == 0)
	return 0;

    if ((t = named_daughter(syl_struct, "sylval", "Rhyme")) != 0)
    {
//	cout << "rhyme: " << *t << endl;
	t = named_daughter(t, "sylval", "Nucleus");
//	cout << "nucleus: " << *t << endl;
	return daughter1(t);
    }

    return 0;
}


EST_Item *nth(EST_Relation &r, int n)
{
    int i = 1;
    for (EST_Item *s = r.head(); s; s = inext(s), ++i)
	if (n == i)
	    return s;

    cerr << "Couldn't find item " << n << " in relation " << r.name() 
	<< " of length " << 	r.length() << endl;
    festival_error();
    return 0;
}

EST_Item *nth_leaf(EST_Item *r, int n)
{
    int i = 1;
    EST_Item *p;

    for (p = first_leaf_in_tree(r);
	 p != next_leaf(last_leaf_in_tree(r)); p = next_leaf(p), ++i)
	if (n == i)
	    return p;

    cerr << "Couldn't find leaf " << n << " in relation " 
	<< r->relation()->name() <<endl;
    // remove this as we want to catch errors for now.
//    festival_error();
    return 0;
}

#if 0

EST_Val usf_duration(EST_Item *s)
{
    return s->F("end") - s->F("start");
}

EST_Val usf_start(EST_Item *s)
{
    s = s->as_relation("Segment");
    //cout << "in usf_start\n";
    //cout << *s << endl;
    /*    EST_Relation *r = s->relation();
    if (r->f.S("timing_style") != "segment")
	EST_warning("Attempted to use start() feature function "
		  "in non segment relation\n");
    */
    return  (prev(s) == 0) ? 0.0 : prev(s)->F("end");
}

EST_Val usf_tilt_phrase_position(EST_Item *s)
{
    EST_String rel_name = s->S("time_path");
    EST_Item *t, *a;

    if ((t = s->as_relation(rel_name)) == 0)
	{
	    cerr << "item: " << *s << endl;
	    EST_error("No relation %s for item\n", (const char *) rel_name);
	}

    a = parent(t);

    cout << "us features phrase pos\n";
    //cout << "dereferencing syllable: " << *a << endl;
    cout << "start: " << a->F("start") << endl;
    cout << "end: " << a->F("end") << endl;

    if (s->S("name") == "phrase_start")
        return a->F("start");
    else
        return a->F("end");
}

EST_Val usf_tilt_event_position(EST_Item *s)
{
    EST_String rel_name = s->S("time_path");
    EST_Item *t, *a;

    if ((t = s->as_relation(rel_name)) == 0)
	EST_error("No relation %s for item\n", (const char *) rel_name);

    a = parent(t);

    cout << "us features tilt pos\n";
    cout << "dereferencing syllable: " << *a << endl;
    cout << "vowel_start: " << a->F("vowel_start") << endl;
   cout << "start: " << a->F("start") << endl;
    cout << "end: " << a->F("end") << endl;
    
    return a->F("vowel_start") + s->F("rel_pos",0.0);
}

EST_Val usf_leaf_end(EST_Item *s)
{
    if (!s->f_present("time_path"))
	EST_error("Attempted to use leaf end() feature function on "
		  "item with no time_path feature set: %s\n", 
		  (const char *)s->relation()->name());

    EST_String rel_name = s->S("time_path");
    EST_Item *t, *a;

    if ((t = s->as_relation(rel_name)) == 0)
	EST_error("No relation %s for item\n", (const char *) rel_name);

    a = last_leaf_in_tree(t);
    return a->F("end");
}

EST_Val usf_leaf_start(EST_Item *s)
{
    if (!s->f_present("time_path"))
	EST_error("Attempted to use leaf start() feature function on "
		  "item with no time_path feature set: %s\n", 
		  (const char *)s->relation()->name());

    EST_String rel_name = s->S("time_path");
    EST_Item *t, *a;

    if ((t = s->as_relation(rel_name)) == 0)
	EST_error("No relation %s for item\n", (const char *) rel_name);

    a = first_leaf_in_tree(t);
    //    cout << "this is the first node of the tree\n";
    //cout << *a << endl;
    return a->F("start");
}

EST_Val usf_int_start(EST_Item *s)
{
    EST_String rel_name = "IntonationPhrase";
    EST_Item *t, *a;

    if ((t = s->as_relation(rel_name)) == 0)
	EST_error("No relation %s for item\n", (const char *) rel_name);

    a = first_leaf_in_tree(parent(t)->as_relation("MetricalTree"));
    return a->F("start");
}

EST_Val usf_int_end(EST_Item *s)
{
    EST_String rel_name = "IntonationPhrase";
    EST_Item *t, *a;

    if ((t = s->as_relation(rel_name)) == 0)
	EST_error("No relation %s for item\n", (const char *) rel_name);

    a = last_leaf_in_tree(parent(t)->as_relation("MetricalTree"));
    return a->F("end");
}

#endif

EST_Val usf_vowel_start(EST_Item *s)
{
    if (!s->f_present("time_path"))
	EST_error("Attempted to use vowel_time() feature function "
		  "in relation with no time_relation feature defined\n");

    EST_String rel_name = s->S("time_path");
 
    EST_Item *n = syl_nucleus(s->as_relation(rel_name));

    n = n->as_relation("Segment");

    return n->F("start");
}

void register_unisyn_features(void)
{
//    register_featfunc("unisyn_duration", usf_duration);
//    register_featfunc("unisyn_start", usf_start);
    register_featfunc("unisyn_vowel_start", usf_vowel_start);
//    register_featfunc("unisyn_leaf_end", usf_leaf_end);
//    register_featfunc("unisyn_leaf_start", usf_leaf_start);
//    register_featfunc("unisyn_int_end", usf_int_end);
//    register_featfunc("unisyn_int_start", usf_int_start);
//    register_featfunc("unisyn_tilt_event_position", usf_tilt_event_position);
//    register_featfunc("unisyn_tilt_phrase_position", usf_tilt_phrase_position);
}
