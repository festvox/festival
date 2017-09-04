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
/* Duff intonation                                                       */
/*                                                                       */
/*=======================================================================*/
#include <cstdio>
#include "festival.h"
#include "intonation.h"

LISP FT_Intonation_Default_Utt(LISP utt)
{
    return utt;
}

LISP FT_Int_Targets_Default_Utt(LISP utt)
{
    // Predict intonation from labels etc (producing F0 contour)
    EST_Utterance *u = get_c_utt(utt);
    EST_Item *s;
    EST_Relation *seg;
    LISP params;
    float start,end;
    
    *cdebug << "Intonation duff module\n";

    // should create some random targets
    params = siod_get_lval("duffint_params",NULL);
    start = get_param_float("start",params,130.0);
    end = get_param_float("end",params,110.0);
    u->create_relation("Target");

    seg = u->relation("Segment");

    if (seg->length() == 0)
	return utt;

    add_target(u,seg->first(),0,start);
    s = seg->rlast();
    add_target(u,s,(float)ffeature(s,"segment_end"),end);

    return utt;
}

LISP FT_Int_Targets_Relation_Utt(LISP utt, LISP relname)
{
    // Predict intonation from labels etc (producing F0 contour)
    EST_Utterance *u = get_c_utt(utt);
    EST_Track *pm = 0;
    LISP params;
    float start,end;
    int n_frames;
    
    *cdebug << "Intonation duff module\n";

    // should create some random targets
    params = siod_get_lval("duffint_params",NULL);
    start = get_param_float("start",params,130.0);
    end = get_param_float("end",params,110.0);

    pm = track(u->relation(get_c_string(relname))->head()->f("coefs"));

    float pp = 1.0/start;
    //    float end_time = ((float)pm->num_frames()) * pp;
    float end_time = pm->end();

    n_frames = (int)(ceil)(end_time/pp);
    cout << "n_frames: " << n_frames << endl;
    cout << "end_time: " << end_time << endl;

    EST_Track *f0 = new EST_Track;
    f0->resize(n_frames, 1);
    f0->fill_time(0.01);

    float m = (end-start) /end_time;
    float c = start;

    for (int i = 0; i < n_frames; ++ i)
	f0->a(i) = (m * ((float) i) * 0.01) + c;

    u->create_relation("f0");
    EST_Item *item = u->relation("f0")->append();
    item->set_val("f0", est_val(f0));

    return utt;
}

