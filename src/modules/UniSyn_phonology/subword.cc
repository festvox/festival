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

#include "festival.h"

void main_stress(EST_Item *s);

void subword_metrical_tree(EST_Relation &syllable, 
			   EST_Relation &metricaltree);

EST_Item *make_foot(EST_Item *w, EST_Item *met_node, EST_Item *next_syl_node);
void subword_metrical_tree(EST_Item *w, EST_Relation &syllable, 
			   EST_Relation &metricaltree);

static void all_stress(EST_Relation &syllable, EST_Relation &mettree);

static void make_super_foot(EST_Item *w, EST_Item *met_node, 
			    EST_Item *next_syl_node);

// Note: this function doesn't work as it should: currently a
// monosyllabic word ends up being the same item as the syllable, when
// a single daughter item would be better.

void subword_list(EST_Item *w, EST_Relation &syllable, 
		  EST_Relation &metricaltree)
{
    EST_Item *s, *n;

    n = metricaltree.append(w);

    if (inext(syllable.head()) == 0)
	return;

    for (s = syllable.head(); s ; s = inext(s))
    {
	cout << "appending syl\n";
	n->append_daughter(s);
    }
}

void subword_metrical_tree(EST_Item *w, EST_Relation &syllable, 
			   EST_Relation &metricaltree)
{
    EST_Item *s;
    EST_Item *new_leaf;
    
    // single syllable
    if (inext(syllable.head()) == 0)
    {
	new_leaf = metricaltree.append(w);
	return;
    }
    
    // absorb initial unstressed syllables
    for (s = syllable.head(); s && (s->f("stress_num") == 0); s = inext(s))
    {
	new_leaf = metricaltree.append(s);
	new_leaf->set("MetricalValue", "w");
    }
    
    while (s)
    {
	new_leaf = metricaltree.append(s);
	new_leaf->set("MetricalValue", "s");
	s = make_foot(w, new_leaf, inext(s));
    }
    
    if (siod_get_lval("mettree_debug", NULL) != NIL)
	metricaltree.utt()->save("foot.utt", "est");
    
    s = metricaltree.head(); 
    make_super_foot(w, s, inext(s));
    
    if (siod_get_lval("mettree_debug", NULL) != NIL)
	metricaltree.utt()->save("super_foot.utt", "est");
    
    all_stress(syllable, metricaltree);
}


EST_Item *make_foot(EST_Item *w, EST_Item *met_node, EST_Item *next_syl_node)
{
    EST_Item *new_parent;
    EST_Item *fl;
    
    if (next_syl_node == 0)
	return 0;
    
    if (next_syl_node->f("stress_num") == 0)
    {
	met_node->set("MetricalValue", "s");    
	next_syl_node->set("MetricalValue", "w");
	
	fl = first_leaf(met_node);
	
	if (inext(next_syl_node))
	    new_parent = met_node->insert_parent();
	else
	{
	    if (iprev(fl))
		new_parent = met_node->insert_parent();
	    else
	    {
		//		cout << "making met node word node in foot\n";
		//		cout << "foot root:" << *w << endl;
		new_parent = met_node->insert_parent();
//		new_parent = met_node->insert_above(w);
		merge_item(new_parent, w);
		//		cout << "foot root:" << *w << endl;
		//		cout << "foot root:" << *new_parent << endl;
	    }
	    
	}
	new_parent->append_daughter(next_syl_node);
	
	next_syl_node = make_foot(w, new_parent, inext(next_syl_node));
    }
    return next_syl_node;
}

// construct left branching unlabelled tree using feet roots as terminals
static void make_super_foot(EST_Item *w, EST_Item *met_node, 
			    EST_Item *next_syl_node)
{
    EST_Item *new_parent;
    
    if (next_syl_node == 0)
	return;
    
    // make sure root node is w, i.e. word
    if (inext(next_syl_node))
	new_parent = met_node->insert_parent();
    else
    {
	//	cout << "inserted word as root in super foot:" << *w << endl;
	new_parent = met_node->insert_parent();
	
	// KTH this crashes in linux
	merge_item(new_parent, w);
	//	cout << "after inserted word as root in super foot:" << *w << endl;
	//	cout << "after inserted word as root in super foot:" << 
	//	*new_parent << endl;
	//	w = new_parent->as_relation("Word");
    }
    
    new_parent->append_daughter(next_syl_node);
    
    make_super_foot(w, new_parent, inext(new_parent));
}


static void all_stress(EST_Relation &syllable, EST_Relation &mettree)
{
    EST_Item *n, *s;
    int stress_num = -1;
    
    for (s = syllable.head(); s; s = inext(s))
	if (s->I("stress_num",0) > stress_num)
	    stress_num = s->I("stress_num");
    
    //    cout << "max stress num:" << stress_num << endl;
    
    for (; stress_num > 0; --stress_num)
    {
	for (s = syllable.head(); s; s = inext(s))
	    if (s->I("stress_num",0) == stress_num)
		break;
	
	if (s == 0)
	{
	    cerr << "No main stress found in definition of lexical entry\n";
	    festival_error();
	}
	
	n = s->as_relation(mettree.name());
	main_stress(n);
    }
}

