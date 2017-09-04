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
#include "us_duration.h"

float phone_z_score(const EST_String &p, float dur)
{	
    float mean, sd;
    mean = met_duration.val(p).val("mean");
    sd = met_duration.val(p).val("sd");
    return ((dur - mean) / sd);
}

void clear_feature(EST_Relation &r, const EST_String &name)
{
    for (EST_Item *p = r.head(); p ; p = inext(p))
	p->f_remove(name);
}

// void dur_to_end(EST_Relation &r)
// moved to UniSyn us_diphone.cc

void end_to_dur(EST_Relation &r)
{
    float prev_end = 0;

    for (EST_Item *p = r.head(); p ; p = inext(p))
    {
	p->set("dur", p->F("end") - prev_end);
	prev_end = p->F("end");
    }
}

void assign_phone_z_scores(EST_Utterance &u, const EST_String &seg_name)
{
    EST_Item *s;

    end_to_dur(*u.relation(seg_name));

    for (s = u.relation(seg_name)->head(); s; s = inext(s))
	s->set("z_score", phone_z_score(s->f("name"), s->F("dur")));
}

void promote_mean_z_score(EST_Utterance &u, const EST_String &st_name,
		      const EST_String &syl_name)
{
    EST_Item *p, *s, *l;
    float z, n;


    for (s = u.relation(syl_name)->head(); s; s = inext(s))
    {
	p = s->as_relation(st_name);
	z = 0.0;
	for (n = 1, l = first_leaf_in_tree(p); l!= last_leaf_in_tree(p); 
	     l = next_leaf(l), n += 1.0)
	    z += l->F("z_score");

	z += l->F("z_score");
	z = z / n;
	s->set("m_z_score", z);
	

//	n = named_daughter(s->as_relation(st_name), "sylval", "Rhyme");
//       n = daughter1(named_daughter(n, "sylval", "Nucleus"));
//	s->set("z_score", n->F("z_score"));

    }
}

void promote_vowel_z_score(EST_Utterance &u, const EST_String &st_name,
		      const EST_String &syl_name)
{
    EST_Item *n, *s;

    for (s = u.relation(syl_name)->head(); s; s = inext(s))
    {
	n = named_daughter(s->as_relation(st_name), "sylval", "Rhyme");
	n = daughter1(named_daughter(n, "sylval", "Nucleus"));
	s->set("z_score", n->F("z_score"));
    }
}


// set everything to its phone's mean duration
LISP FT_met_dur_predict_1(LISP lutt, LISP lrel)
{
    EST_Utterance *utt = get_c_utt(lutt);
    EST_String rel = get_c_string(lrel);
    EST_Item *p;

    clear_feature(*utt->relation(rel), "dur");
    clear_feature(*utt->relation(rel), "end");

    for (p = utt->relation(rel)->head(); p ; p = inext(p))
	p->set("dur", met_duration.val(p->f("name")).F("mean"));

    cout << "dur end\n";

    dur_to_end(*utt->relation(rel));

    return lutt;
}

LISP FT_met_dur_predict_2(LISP lutt, LISP lrel)
{
    EST_Utterance *utt = get_c_utt(lutt);
    EST_String rel = get_c_string(lrel);

    clear_feature(*utt->relation(rel), "dur");
    clear_feature(*utt->relation(rel), "end");

    for (EST_Item *p = utt->relation(rel)->head(); p ; p = inext(p))
	p->set("dur", 0.2);

    dur_to_end(*utt->relation(rel));

    return lutt;
}

typedef
float (*local_cost_function)(const EST_Item *item1,
			     const EST_Item *item2);

float local_cost(const EST_Item *s1, const EST_Item *s2);

bool dp_match(const EST_Relation &lexical,
	      const EST_Relation &surface,
	      EST_Relation &match,
	      local_cost_function lcf,
	      EST_Item *null_syl);

void add_times(EST_Relation &lexical, EST_Relation &surface, 
	       EST_Relation &match);

LISP FT_nat_dur_predict(LISP lutt, LISP lrel_name, LISP llab_file)
{
    EST_Utterance *utt = get_c_utt(lutt);
    EST_String rel_name = get_c_string(lrel_name);
    EST_String lab_file = get_c_string(llab_file);
    EST_Relation lab, *segment, match, *ulab, *umatch;

    utt->create_relation("Match");
    utt->create_relation("Lab");

    if (utt->relation("Lab")->load(lab_file) != format_ok)
	festival_error();
//    if (lab.load(lab_file) != format_ok)
//	festival_error();

    EST_Item xx;

    segment = utt->relation(rel_name);
    ulab = utt->relation("Lab");
    umatch = utt->relation("Match");

    clear_feature(*segment, "dur");
    clear_feature(*segment, "end");

    dp_match(*segment, *ulab, *umatch, local_cost, &xx);
    add_times(*segment, *ulab, match);

    utt->remove_relation("Match");
    utt->remove_relation("Lab");

//    dp_match(*segment, lab, match, local_cost, &xx);
//    add_times(*segment, lab, match);

    return lutt;
}

