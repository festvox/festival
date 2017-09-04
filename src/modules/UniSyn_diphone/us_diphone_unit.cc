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
/*                                                                       */
/*                 Author: Paul Taylor                                   */
/*                   Date: 1998, 1999                                    */
/* --------------------------------------------------------------------- */
/*            LPC residual synthesis alternative version                 */
/*                                                                       */
/*************************************************************************/

#include "siod.h"
#include "EST.h"
#include "us_diphone.h"
#include "Phone.h"

extern USDiphIndex *diph_index;

void dur_to_end(EST_Relation &r)
{
    float prev_end = 0;

    for (EST_Item *p = r.head(); p ; p = inext(p))
    {
	p->set("end", p->F("dur") + prev_end);
	prev_end = p->F("end");
    }
}

void add_end_silences(EST_Relation &segment, EST_Relation &target)
{
    EST_Item *t, *n;
    float shift = 0.0;
    const float pause_duration = 0.1;

    t = segment.head();
    if (!ph_is_silence(t->f("name")))
    {
	n = t->insert_before();
	n->set("name", ph_silence());
	n->set("dur", pause_duration);
	shift += pause_duration;
    }

    t = segment.tail();
    if (!ph_is_silence(t->S("name")))
    {
	n = t->insert_after();
	n->set("name", ph_silence());
	n->set("dur", pause_duration);
	shift += pause_duration;
    }
    dur_to_end(segment);

    target.tail()->set("pos", (target.tail()->F("pos") + shift));
}

void add_end_silences(EST_Relation &segment)
{
  EST_Item *t, *n;
  
  t = segment.head();
  if (!ph_is_silence(t->S("name")))
    {
      n = t->insert_before();
      n->set("name", ph_silence());
    }
  
  t = segment.tail();
  if (!ph_is_silence(t->S("name")))
    {
      n = t->insert_after();
      n->set("name", ph_silence());
    }
}

void parse_diphone_times(EST_Relation &diphone_stream, 
			 EST_Relation &source_lab)
{
    EST_Item *s, *u;
    EST_Track *pm;
    int e_frame, m_frame = 0;
    float dur_1 = 0.0, dur_2 = 0.0, p_time;
    float t_time = 0.0, end;
    p_time = 0.0;

    for (s = source_lab.head(), u = diphone_stream.head(); u; u = inext(u), 
             s = inext(s))
    {
	pm = track(u->f("coefs"));
	
	e_frame = pm->num_frames() - 1;
	m_frame = u->I("middle_frame");

        if (m_frame < 0) m_frame=0;
	dur_1 = pm->t(m_frame);
        if (e_frame < m_frame) e_frame=m_frame;
	dur_2 = pm->t(e_frame) - dur_1;
	
	s->set("source_end", (dur_1 + p_time));
	
	p_time = s->F("source_end") + dur_2;

	end = dur_1 + dur_2 + t_time;
	t_time = end;
	u->set("end", t_time);
    }
    if (s)
	s->set("source_end", (dur_2 + p_time));
}

void load_separate_diphone(int unit, bool keep_full, 
			   const EST_String &cut_type)
{
    // Load in the coefficients and signame for this diphone
    // It caches the results in the diphone index entry, though
    // someone else may clear them.  Note the full file is loaded
    // each time which isn't optimal if there are multiple diphones
    // is the same file
    int samp_start, samp_end;
    int pm_start, pm_end, pm_middle;
    EST_Track full_coefs, dcoefs, *coefs;
//    float q_start, q_middle, q_end;
    
    if (full_coefs.load(diph_index->coef_dir + "/" 
			+ diph_index->diphone[unit].S("filename")
			+ diph_index->coef_ext) != format_ok)
    {
	cerr << "US DB: failed to read coefs file from " <<
	    diph_index->coef_dir + "/" 
		+ diph_index->diphone[unit].S("filename")
		    + diph_index->coef_ext << endl;
	EST_error("");
    }

    pm_start = full_coefs.index(diph_index->diphone[unit].f("start"));
    pm_middle = full_coefs.index(diph_index->diphone[unit].f("middle"));
    pm_end = full_coefs.index(diph_index->diphone[unit].f("end"));

    // option for taking half a diphone only
    if (cut_type == "first_half")
	pm_end = pm_middle;
    else if (cut_type == "second_half")
	pm_start = pm_middle;
    
    // find time of mid-point, i.e. boundary between phones
    full_coefs.sub_track(dcoefs, pm_start, pm_end - pm_start + 1, 0, EST_ALL);
    // Copy coefficients so the full coeffs can be safely deleted
    coefs = new EST_Track(dcoefs);
    for (int j = 0; j < dcoefs.num_frames(); ++j)
	coefs->t(j) = dcoefs.t(j) - full_coefs.t(Gof((pm_start - 1), 0));

    diph_index->diphone[unit].set("first_dur", 
				   full_coefs.t(pm_middle) - 
				   full_coefs.t(pm_start));

    diph_index->diphone[unit].set("second_dur", 
				   full_coefs.t(pm_end) - 
				   full_coefs.t(pm_middle));

    if (keep_full)
    {
	EST_Track *f = new EST_Track;
	*f = full_coefs;
	diph_index->diphone[unit].set_val("full_coefs",est_val(f));
    }

    diph_index->diphone[unit].set_val("coefs", est_val(coefs));
    diph_index->diphone[unit].set("middle_frame", pm_middle - pm_start -1);
    
    EST_Wave full_sig, sub_sig;

    if (diph_index->sig_dir == "none")
	return;

    if (full_sig.load(diph_index->sig_dir + "/" 
		      + diph_index->diphone[unit].f("filename")
		      + diph_index->sig_ext) != format_ok)
    {
	cerr << "US DB: failed to read signal file from " <<
	    diph_index->sig_dir + "/" 
		+ diph_index->diphone[unit].f("filename")
		    + diph_index->sig_ext << endl;
	EST_error("");
    }
    
    // go to the periods before and after
    samp_start = (int)(full_coefs.t(Gof((pm_start - 1), 0))
		       * (float)full_sig.sample_rate());
    if (pm_end+2 < full_coefs.num_frames())
	pm_end++;

    samp_end = (int)(full_coefs.t(pm_end) * (float)full_sig.sample_rate());
    full_sig.sub_wave(sub_sig, samp_start, samp_end - samp_start + 1);
    EST_Wave *sig = new EST_Wave(sub_sig);

    diph_index->diphone[unit].set_val("sig", est_val(sig));

    if (keep_full)
    {
	EST_Wave *s = new EST_Wave;
	*s = full_sig;
	diph_index->diphone[unit].set_val("full_sig", est_val(s));
    }
}

void load_full_diphone(int unit)
{
    // Load in the coefficients and signame for this diphone
    // It caches the results in the diphone index entry, though
    // someone else may clear them.  Note the full file is loaded
    // each time which isn't optimal if there are multiple diphones
    // is the same file
    int pm_start, pm_end, pm_middle;
    EST_Track *full_coefs;

    full_coefs = new EST_Track;
    
    if (full_coefs->load(diph_index->coef_dir + "/" 
			+ diph_index->diphone[unit].f("filename")
			+ diph_index->coef_ext) != format_ok)
    {
	cerr << "US DB: failed to read coefs file from " <<
	    diph_index->coef_dir + "/" 
		+ diph_index->diphone[unit].f("filename")
		    + diph_index->coef_ext << endl;
	EST_error("");
    }
    
    pm_start = full_coefs->index(diph_index->diphone[unit].f("start"));
    pm_middle = full_coefs->index(diph_index->diphone[unit].f("middle"));
    pm_end = full_coefs->index(diph_index->diphone[unit].f("end"));
    
    diph_index->diphone[unit].set_val("full_coefs", est_val(full_coefs));

    EST_Wave *full_sig = new EST_Wave;

    if (full_sig->load(diph_index->sig_dir + "/" 
		      + diph_index->diphone[unit].f("filename")
		      + diph_index->sig_ext) != format_ok)
    {
	cerr << "US DB: failed to read signal file from " <<
	    diph_index->sig_dir + "/" 
		+ diph_index->diphone[unit].f("filename")
		    + diph_index->sig_ext << endl;
	EST_error("");
    }
    diph_index->diphone[unit].set_val("full_sig", est_val(full_sig));
}
