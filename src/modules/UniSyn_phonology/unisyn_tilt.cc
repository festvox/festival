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
/*             Date   :  July 1998                                       */
/*-----------------------------------------------------------------------*/
/*                                                                       */
/*                   Tilt synthesis code                                 */ 
/*                                                                       */
/*=======================================================================*/

#include <cstdio>
#include "festival.h"
#include "EST_tilt.h"
#include "../UniSyn/us_features.h"

void vowel_tilt_to_abs_tilt(EST_Utterance &u)
{
    EST_Item *s, *t;
    float pos;

    for (t = u.relation("Intonation")->head(); t; t = inext(t))
    {
	if (t->as_relation("IntonationSyllable"))
	{
	    s = daughter1(t->as_relation("IntonationSyllable"));
	    s = s->as_relation("Syllable");
	    pos = t->F("tilt:position");
	    t->set("position", pos);
	    //	    cout << "pos: " << t->fF("position") << " rel:" <<
	    //t->fF("rel_pos") << " vowel start:" << s->fF("vowel_start") 
	    //<< endl;
	}
    }
}

void tilt_to_f0(EST_Relation &intonation, EST_Relation &f0)
{
    EST_Track *fz = new EST_Track;

    tilt_synthesis(*fz, intonation, 0.01, 0);

    fz->save("tilt.f0", "est");

    EST_Item *t = f0.append();
    t->set("name", "f0");
    t->set_val("f0", est_val(fz));
}


