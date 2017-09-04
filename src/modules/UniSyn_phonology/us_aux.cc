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




int num_daughters(EST_Item *s)
{
    EST_Item *d1, *dn, *d;

    d1 = daughter1(s);
    if (d1 == 0)
	return 0;

    dn = daughtern(s);
    
    int n = 1;
    for (d = d1; d != dn; d = inext(d))
	++n;
    return n;
}


void phones_in_word(EST_Item *w, const EST_String &met_name, 
		    const EST_String &ss_name, const EST_String &seg_name,
		    EST_Item **first_p, EST_Item **last_p)
{
    EST_Item *s, *t;

    cout << "&word: " << w << endl;
    cout << "in relation: " << w->in_relation(met_name) << endl;
    w = w->as_relation(met_name);
    cout << "&word: " << w << endl;
    
    if (w == 0)
      EST_error("Word isn't in metrical tree\n");

    cout << "word: " << *w << endl;
    cout << "d1: " << daughter1(w) << endl;
    if (daughter1(w))
	cout << "*d1: " << daughter1(w) << endl;
    cout << "d2: " << daughter2(w) << endl;
    if (daughter2(w))
	cout << "*d2: " << daughter2(w) << endl;
    *first_p = 0;

    for (s = first_leaf_in_tree(w); s != next_leaf(last_leaf_in_tree(w));
	 s = next_leaf(s))
    {
	cout << "leaf: " << *s << endl;
	cout << "in ss relation: " << s->in_relation(ss_name) << endl;
//	cout << "relations: " << s->relations() << endl;

	cout << "first leaf: " << *first_leaf_in_tree(s->as_relation(ss_name)) << endl;
	cout << "last leaf: " << *last_leaf_in_tree(s->as_relation(ss_name)) 
	    << endl;
	for (t = first_leaf_in_tree(s->as_relation(ss_name)); 
	     t !=next_leaf(last_leaf_in_tree(s->as_relation(ss_name))); 
	     t = next_leaf(t))
	{
	    *last_p = t->as_relation(seg_name);
	    cout << "phone: " << *t << endl;
	    if (*first_p == 0)
		*first_p = t->as_relation(seg_name);
	}
    }

    cout << "word: " << *w << endl;
    cout << "first: " << **first_p << " last " << **last_p << endl;

    cout << "\n\n";
}

#if 0
static EST_Item *onset(EST_Item * ss_root)
{
    return (daughter1(ss_root)->f("sylval") == "Onset")? daughter1(ss_root) : 0;
}

static EST_Item *rhyme(EST_Item * ss_root)
{
    return (daughter2(ss_root) == 0) ? daughter1(ss_root) : daughter2(ss_root);
}

static EST_Item *nucleus(EST_Item * ss_root)
{
    return daughter1(rhyme(ss_root));
}

static EST_Item *coda(EST_Item * ss_root)
{
    return daughter2(rhyme(ss_root));
}

#endif
