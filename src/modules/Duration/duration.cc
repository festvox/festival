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
/*                      Author :  Alan W Black                           */
/*                      Date   :  April 1996                             */
/*-----------------------------------------------------------------------*/
/*                                                                       */
/* Duration averages and default and tree                                */
/*                                                                       */
/*=======================================================================*/
#include <cstdio>
#include "festival.h"
#include "durationP.h"

LISP FT_Duration_Ave_Utt(LISP utt)
{
    // Predict average duration on segments
    EST_Utterance *u = get_c_utt(utt);
    EST_Item *s;
    float end=0.0, dur;
    LISP ph_durs,ldur;
    float stretch;

    *cdebug << "Duration Average module\n";

    ph_durs = siod_get_lval("phoneme_durations","no phoneme durations");

    for (s=u->relation("Segment")->first(); s != 0; s = inext(s))
    {
	ldur = siod_assoc_str(s->name(),ph_durs);
	stretch = dur_get_stretch_at_seg(s);
	if (ldur == NIL)
	{
	    cerr << "Phoneme: " << s->name() << " have no default duration "
		<< endl;
	    dur = 0.100;
	}
	else
	    dur = get_c_float(car(cdr(ldur)));
	end += (dur*stretch);
	s->set("end",end);
    }

    return utt;
}

LISP FT_Duration_Def_Utt(LISP utt)
{
    // Predict fixed duration on segments
    EST_Utterance *u = get_c_utt(utt);
    EST_Item *s;
    float end=0.0;
    float stretch;

    *cdebug << "Duration Default module\n";

    for (s=u->relation("Segment")->first(); s != 0; s = inext(s))
    {
	stretch = dur_get_stretch_at_seg(s);
	end += 0.100*stretch;
	s->set("end",end);
    }

    return utt;
}

LISP FT_Duration_Tree_Utt(LISP utt)
{
    // Predict duration on segments using CART tree
    EST_Utterance *u = get_c_utt(utt);
    EST_Item *s;
    float end=0.0, dur,stretch;
    LISP tree;
    EST_Val pdur;

    *cdebug << "Duration Tree module\n";

    tree = siod_get_lval("duration_cart_tree","no duration cart tree");

    for (s=u->relation("Segment")->first(); s != 0; s = inext(s))
    {
	pdur = wagon_predict(s,tree);
	stretch = dur_get_stretch_at_seg(s);
	if (pdur == 0.0)
	{
	    cerr << "Phoneme: " << s->name() << " tree predicted 0.0 changing it"
		<< endl;
	    dur = 0.050;
	}
	else
	    dur = (float)pdur;
	dur *= stretch;
	end += dur;
	s->set("end",end);
    }

    return utt;
}

#define PH_AVE(X) (get_c_float(car(cdr(X))))
#define PH_STD(X) (get_c_float(car(cdr(cdr(X)))))

LISP FT_Duration_Tree_ZScores_Utt(LISP utt)
{
    // Predict duration on segments using CART tree
    EST_Utterance *u = get_c_utt(utt);
    EST_Item *s;
    float end=0.0, dur,stretch;
    LISP tree,dur_info,ph_info;
    float pdur;
    float ave, std;

    *cdebug << "Duration Tree ZScores module\n";

    tree = siod_get_lval("duration_cart_tree","no duration cart tree");
    dur_info = siod_get_lval("duration_ph_info","no duration phone info");

    for (s=u->relation("Segment")->first(); s != 0; s = inext(s))
    {
	pdur = wagon_predict(s,tree);
	ph_info = siod_assoc_str(s->name(),dur_info);
	stretch = dur_get_stretch_at_seg(s);
	if (ph_info == NIL)
	{
	    cerr << "Phoneme: " << s->name() << " has no duration info\n";
            ave = 0.080;
            std = 0.020;
	}
        else
        {
            ave = PH_AVE(ph_info);
            std = PH_STD(ph_info);
        }
	if ((pdur > 3) || (pdur < -3))
	{
            //	    cerr << "Duration tree extreme for " << s->name() << 
            //		" " << pdur << endl;
	    pdur = ((pdur < 0) ? -3 : 3);
	}
	s->set("dur_factor",pdur);
	dur = ave + (pdur*std);
	dur *= stretch;
	if (dur < 0.010)
	    dur = 0.010;  // just in case it goes wrong
	end += dur;
	s->set("end",end);
    }

    return utt;
}
