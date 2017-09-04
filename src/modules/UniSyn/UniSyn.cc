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
/*                   Date: February 1998                                 */
/* --------------------------------------------------------------------- */
/*             Waveform Generation Scheme Interface File                 */
/*                                                                       */
/*************************************************************************/
#include "siod.h"
#include "EST.h"
#include "UniSyn.h"
#include "us_synthesis.h"
#include "Phone.h"

VAL_REGISTER_TYPE(ivector,EST_IVector)
VAL_REGISTER_TYPE(wavevector,EST_WaveVector);

SIOD_REGISTER_TYPE(wavevector, EST_WaveVector);

void map_to_relation(EST_IVector &map, EST_Relation &r, 
		     const EST_Track &source_pm, 
		     const EST_Track &target_pm);

EST_Features *scheme_param(const EST_String& param, const EST_String &path)
{
    EST_Features *f, *p;

    f = feats(siod_get_lval(param, "Couldn't find scheme paramete named: " 
			    + param));

    p = (path == "") ? f : &f->A(path);
    return p;
}


LISP FT_us_linear_smooth_amplitude( LISP lutt )
{
  EST_Utterance *utt = get_c_utt( lutt );

  us_linear_smooth_amplitude( utt );

  return lutt;
}


static LISP FT_wavevector_get_wave( LISP l_wavevector, LISP l_framenum )
{
  EST_WaveVector *wv = wavevector( l_wavevector );
  int i = get_c_int( l_framenum );

  if( i<0 || i>wv->length() )
    EST_error( "index out of bounds" );

  return siod( &((*wv)[i]) );
}


LISP FT_us_unit_concat(LISP lutt)
{
    EST_String window_name;
    float window_factor;
    bool window_symmetric;

    EST_Features *f = scheme_param("Param", "unisyn");

    window_name = f->S("window_name");
    window_factor = f->F("window_factor");

    window_symmetric = (f->I("window_symmetric",1) == 0) ? false : true;

    us_unit_concat(*get_c_utt(lutt), window_factor, window_name, false, window_symmetric);
    return lutt;
}

LISP FT_us_unit_raw_concat(LISP lutt)
{
    us_unit_raw_concat(*get_c_utt(lutt));
    return lutt;
}


LISP FT_us_energy_normalise(LISP lutt, LISP lrname)
{
    EST_Utterance *utt = get_c_utt(lutt);
    EST_String rname = get_c_string(lrname);

    us_energy_normalise(*utt->relation(rname));
    return lutt;
}

LISP FT_us_generate_wave(LISP lutt, LISP l_f_method, LISP l_o_method)
{
    EST_String filter_method = get_c_string(l_f_method);
    EST_String ola_method = get_c_string(l_o_method);
    EST_Utterance *utt = get_c_utt(lutt);

    EST_Features *f = scheme_param("Param", "unisyn");
    if(f->I("window_symmetric",1) == 0){
      ola_method = "asymmetric_window";
    }
    us_generate_wave(*utt, filter_method, ola_method);

    return lutt;
}

LISP FT_us_mapping(LISP lutt, LISP method)
{
    us_mapping(*get_c_utt(lutt), get_c_string(method));
    return lutt;
}

LISP FT_us_get_copy_wave(LISP lutt, LISP l_sig_file,  LISP l_pm_file, 
			  LISP l_seg_file)
{
    EST_Utterance *utt = get_c_utt(lutt);
    EST_Relation seg;
    EST_String sig_file = get_c_string(l_sig_file);
    EST_String seg_file = get_c_string(l_seg_file);
    EST_String pm_file = get_c_string(l_pm_file);

    EST_Track *pm = new EST_Track;
    EST_Wave *sig = new EST_Wave;

    if (pm->load(pm_file) != format_ok)
	return NIL;

    if (sig->load(sig_file) != format_ok)
	return NIL;

    if (seg.load(seg_file) != format_ok)
	return NIL;

    if (!ph_is_silence(seg.tail()->f("name")))
    {
	EST_Item *n = seg.tail()->insert_after();
	n->set("name", ph_silence());
	n->set("end", iprev(seg.tail())->F("end") + 0.1);
    }

    us_get_copy_wave(*utt, *sig, *pm, seg);
    return lutt;
}


LISP FT_f0_to_pitchmarks(LISP lutt, LISP l_f0_name, LISP l_pm_name, 
			 LISP l_end_time)
{
    EST_Utterance *utt = get_c_utt(lutt);
    int num_channels=0;
    const float default_f0 = 100.0;
    EST_Relation *f0_rel=0, *pm_rel=0;
    EST_Track *f0=0, *pm=0;
    EST_Item *a;

    float end_time = (l_end_time == NIL) ? -1 : get_c_float(l_end_time);

    f0_rel = utt->relation(get_c_string(l_f0_name), 1);
    pm_rel = utt->create_relation(get_c_string(l_pm_name));

    f0 = track(f0_rel->head()->f("f0"));
    pm = new EST_Track;

    a = pm_rel->append();
    a->set_val("coefs", est_val(pm));
    a = pm_rel->append();

    if (utt->relation_present("SourceCoef"))
    {
	EST_Track *source_coef = 
	    track(utt->relation("SourceCoef")->head()->f("coefs"));
	num_channels = source_coef->num_channels();
    }

    f0_to_pitchmarks(*f0, *pm, num_channels, default_f0, end_time);

    return lutt;
}

LISP FT_map_to_relation(LISP lutt, LISP lsource_name, LISP ltarget_name,
			LISP lrel_name)
{
    EST_Utterance *utt = get_c_utt(lutt);
    EST_Track *source_pm = 0;
    EST_Track *target_pm = 0;
    EST_IVector *map = 0;
    target_pm = 
	track(utt->relation(get_c_string(ltarget_name))->head()->f("coefs"));
    source_pm = 
	track(utt->relation(get_c_string(lsource_name))->head()->f("coefs"));
    map = ivector(utt->relation("US_map")->head()->f("map"));

    utt->create_relation(get_c_string(lrel_name));

    map_to_relation(*map, *utt->relation(get_c_string(lrel_name)), 
		    *source_pm, *target_pm);

    return NIL;
}

void festival_UniSyn_init(void)
{
    proclaim_module("UniSyn");

    register_unisyn_features();

    init_subr_2( "wavevector.getwave", FT_wavevector_get_wave,
    "(wavevector.getwave WAVEVECTOR FRAMENUM)\n\
    retrieves an EST_Wave frame (int FRAMENUM) from a wavevector."); 

    init_subr_1("us_linear_smooth_amplitude", FT_us_linear_smooth_amplitude,
    "(us_linear_smooth_amplitude UTT)\n\
     Perform linear amplitute smoothing on diphone joins.");

    init_subr_1("us_unit_raw_concat", FT_us_unit_raw_concat,
    "(us_init_raw_concat UTT).");

    init_subr_2("us_energy_normalise", FT_us_energy_normalise,
    "(us_ps_synthesis UTT SIGPR)\n\
    Synthesize utterance UTT using signal processing technique SIGPR \n\
    for the UniSyn pitch-synchronous synthesizer.");
    
    init_subr_3("us_generate_wave", FT_us_generate_wave,
    "(us_td_synthesis UTT FILTER_METHOD OLA_METHOD)\n\
    Synthesize utterance UTT using signal processing technique SIGPR \n\
    for the UniSyn pitch-synchronous synthesizer.");
    
    init_subr_2("us_mapping", FT_us_mapping,
    "(us_mapping UTT method)\n\
    Synthesize utterance UTT using signal processing technique SIGPR \n\
    for the UniSyn pitch-synchronous synthesizer.");

    init_subr_1("us_unit_concat", FT_us_unit_concat,
    "(us_unit_concat UTT)\n\
     Concat coef and wave information in unit stream into a single \n\
     Frames structure storing the result in the Frame relation");

    init_subr_4("us_f0_to_pitchmarks", FT_f0_to_pitchmarks,
    "(us_f0_to_pitchmarks UTT F0_relation PM_relation END_TIME)\n\
    From the F0 contour in F0_relation, create a set of pitchmarks\n\
    in PM_relation. If END_TIME is not nil, Extra pitchmarks will be \n\
    created at the default interval up to this point");

    init_subr_4("map_to_relation", FT_map_to_relation,
    "(map_to_relation UTT Source_relation Target_relation new_relation)\n\
    From the F0 contour in F0_relation, create a set of pitchmarks\n\
    in PM_relation. If END_TIME is not nil, Extra pitchmarks will be \n\
    created at the default interval up to this point");
    
    init_subr_4("us_get_copy_wave", FT_us_get_copy_wave,
    "(warp_utterance UTT (Wavefile Pitchmark_file))\n\
    Change waveform to match prosodic specification of utterance.");


#ifdef HAVE_US_TDPSOLA_TM
    us_init_tdpsola();
#endif

}

/*

    init_subr_2("us_F0targets_to_pitchmarks", FT_us_F0targets_to_pitchmarks,
    "(us_F0targets_to_pitchmarks UTT Segment_Relation)\n\
     Make set of pitchmarks according to F0 target specification");

LISP FT_merge_pitchmarks(LISP lutt, LISP l_pm1, LISP l_pm2, 
			 LISP l_guide_name)
{
    EST_Utterance *utt = get_c_utt(lutt);

    EST_Track *pm1 = 
	track(utt->relation(get_c_string(l_pm1), 1)->head()->f("coefs", 1));
    EST_Track *pm2 = 
	track(utt->relation(get_c_string(l_pm2), 1)->head()->f("coefs", 1));

    EST_Relation *guide = utt->relation(get_c_string(l_guide_name), 1);

    EST_Relation *pm_rel = utt->create_relation("TargetCoefs");

    EST_Track *target_pm = new EST_Track;

    EST_Item *a = pm_rel->append();
    a->fset_val("coefs", est_val(target_pm));

    merge_pitchmarks(*get_c_utt(lutt), *pm1, *pm2, *target_pm, *guide);

    return lutt;
}
LISP FT_warp_pitchmarks(LISP lutt, LISP l_pm_file, LISP l_seg_file)
{
    EST_Utterance *utt = get_c_utt(lutt);

    EST_String pm_file = get_c_string(l_pm_file);
    EST_String seg_file = get_c_string(l_seg_file);

    EST_Track *pm = new EST_Track;
    EST_Relation seg;

    if (pm->load(pm_file) != format_ok)
	return NIL;

    if (seg.load(seg_file) != format_ok)
	return NIL;

    warp_pitchmarks(*utt, pm, seg, *utt->relation("Segment"));

    return lutt;
}

    init_subr_3("us_warp_pitchmarks", FT_warp_pitchmarks,
    "(warp_utterance UTT (Wavefile Pitchmark_file))\n\
    Change waveform to match prosodic specification of utterance.");

LISP FT_us_load_utt_segments(LISP l_utt, LISP l_filename)
{
    EST_String filename = get_c_string(l_filename);
    EST_Utterance tu;
    EST_Utterance *u = get_c_utt(l_utt);
    EST_Item *s, *t;

    if (tu.load(filename) != format_ok)
      festival_error();

    u->relation("Segment")->clear();

    for (s = tu.relation("Segment")->head(); s; s = inext(s))
      {
	t = u->relation("Segment")->append();
	t->fset("name", s->fS("name"));
	t->fset("end", s->fS("end"));
      }

    return l_utt;
}

void us_F0targets_to_pitchmarks(EST_Utterance &utt, 
				const EST_String &seg_relation);

LISP FT_us_F0targets_to_pitchmarks(LISP lutt, LISP lseg)
{
    EST_String s = (lseg == NIL) ? "" :  get_c_string(lseg);
    us_F0targets_to_pitchmarks(*get_c_utt(lutt), s);

    return lutt;
}


*/
