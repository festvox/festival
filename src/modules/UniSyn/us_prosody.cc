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
/*                   Date: 6 Jan 1998                                    */
/* --------------------------------------------------------------------- */
/*              UniSyn prosody manipulation functions                    */
/*                                                                       */
/*************************************************************************/

#include "us_synthesis.h"
#include "Phone.h"

//static void add_end_silences(EST_Relation &segment);
//static void add_end_silences(EST_Relation &segment, EST_Relation &target);

void pitchmarks_to_f0(EST_Track &pm, EST_Track &fz, float shift)
{
    int i;
    float period;

    fz.resize((int)(pm.end()/shift), 1);
    fz.fill_time(shift);

    for (i = 0; i < fz.num_frames() -1 ; ++i)
    {
        period = get_time_frame_size(pm, pm.index_below(fz.t(i)));
	fz.a(i) = 1.0 /period;
    }
}

void f0_to_pitchmarks(EST_Track &fz, EST_Track &pm, int num_channels,
		      float default_f0, float target_end)
{
    int i;
    float max = 0.0;
    float fz_end;

    // Its impossible to guess the length of the pitchmark array before 
    // hand. Here we find the upper limit and resize at the end
    for (i = 0; i < fz.num_frames(); ++i)
    {
        if (fz.a_no_check(i) < 0)
            fz.a_no_check(i) = 0;
        if (fz.a_no_check(i) > 500)
            fz.a_no_check(i) = fz.a_no_check(i-1);
        if (fz.a_no_check(i) > max)
            max = fz.a_no_check(i);
#if 0
	if (fz.a_no_check(i) < 0)
	    fz.a_no_check(i) = 0;
	if (fz.a_no_check(i) > 750)
	    fz.a_no_check(i) = 749;
	if (fz.a_no_check(i) > max)
	    max = fz.a_no_check(i);
#endif
    }

    // Coefficients will also be placed in here, so its best allocate
    // space for their channels now
    fz_end = fz.end();
    pm.resize(int(max * (Gof(fz_end, target_end))) + 10, num_channels);


    int fz_len = fz.length();
    float t1 = 0.0; //first pitchmark convention
    float t2;

    float f1 = fz.a_no_check(0); //arbitrary init 
    float f2; 
    
    double area = 0.5; // init value
    int pm_i = 0;
    int pm_len = pm.length();
    for( int i=0; i<fz_len; i++ ){
      t2 = fz.t( i );
      f2 = fz.a_no_check( i );

      float slope = (f2-f1)/(t2-t1);
      area += (t2 - t1) * 0.5 * (f1 + f2);
      while( (area >= 1.0) && (pm_i < pm_len) ){
	area -= 1.0;
	float discriminant = f2*f2 - 2.0 * area * slope;
	if (discriminant < 0.0) discriminant = 0.0;
	pm.t(pm_i++) = t2 - 2.0 * area / (f2 + sqrt (discriminant));
      }
      t1 = t2;
      f1 = f2;
    }

    float default_shift = 1.0 / default_f0;
    if (target_end > fz_end)
      for (; t1 < target_end; ++pm_i)
	t1 = pm.t(pm_i) = t1 + default_shift;
    
    pm.resize(pm_i-1, num_channels);
}



/* Convert an F0 contour into a set of pitchmarks. This is done by the
obvious iterative function.

Space before the first defined F0 value is filled with regularly space
pitchmarks at intervals 1/def_f0. If the target_end value is
specified, more default pitchmarks are placed after the end of the
last f0 value until time target_end has been reached.
*/

void f0_to_pitchmarks_orig(EST_Track &fz, EST_Track &pm, int num_channels,
		      float default_f0, float target_end)
{
    int i;
    float max = 0.0, prev_pm = 0.0, val;
    float fz_end;

//    cout << "fz end: " << fz.end() << endl;
//    cout << "fz n fg: " << fz.num_frames() << endl;

    // Its impossible to guess the length of the pitchmark array before 
    // hand. Here we find the upper limit and resize at the end
    for (i = 0; i < fz.num_frames(); ++i)
    {
	if (fz.a_no_check(i) < 0)
	    fz.a_no_check(i) = 0;
	if (fz.a_no_check(i) > 750)
	    fz.a_no_check(i) = 749;
	if (fz.a_no_check(i) > max)
	    max = fz.a_no_check(i);
    }

    // Coefficients will also be placed in here, so its best allocate
    // space for their channels now
    fz_end = fz.end();
    pm.resize(int(max * (Gof(fz_end, target_end))) + 10, num_channels);

//    cout << "fz end: " << fz.end() << endl;
//    cout << "fz n fg: " << fz.num_frames() << endl;
//    cout << "pmn fg: " << pm.num_frames() << endl;

    for (i = 0; prev_pm < fz_end; ++i)
    {
	val = fz.a(prev_pm) > 0.0 ? fz.a(prev_pm) : default_f0;
	pm.t(i) = prev_pm + (1.0 / val);
	prev_pm = pm.t(i);
    }

    if (target_end > fz_end)
	for (; prev_pm < target_end; ++i)
	{
	    pm.t(i) = prev_pm + (1.0 / default_f0);
	    prev_pm = pm.t(i);
	}

    pm.resize(i - 1, num_channels);
}

// not sure if this is useful
void linear_pitchmarks(EST_Track &source_pm, EST_Track &target_pm, 
		       float start_f0, float end_f0)
{
    int i;
    float m, length, pitch;
    target_pm.resize(source_pm.num_frames(), source_pm.num_channels());

    length = (float)source_pm.num_frames() / (end_f0 - start_f0);

    target_pm.t(0) = 0.0;
    m = (end_f0 - start_f0) / length;

    for(i = 1; i < target_pm.num_frames(); ++i)
    {
	pitch = (((float)i / (float) target_pm.num_frames())
		 * (end_f0 - start_f0)) + start_f0;
	target_pm.t(i) = target_pm.t(i - 1) + (1 /pitch);
    }
}    

// not sure if this is useful	      
void stretch_f0_time(EST_Track &f0, float stretch, 
		     float s_last_time, float t_last_time)
{
    for (int i = 0 ; i < f0.num_frames(); ++i)
    {
//	cout << i << " o t:" << f0.t(i) << endl;
	f0.t(i) = ((f0.t(i) - s_last_time) * stretch) + t_last_time;
//	cout << i << " m t:" << f0.t(i) << endl;
    }
}

// make target F0 from source F0, with same F0 values as original,
// but durations specified by target_seg.

/*
void us_F0targets_to_pitchmarks(EST_Utterance &utt, 
				const EST_String &seg_relation)
{
    utt.create_relation("TargetCoef");
    EST_Track *target_coef = new EST_Track;
    EST_Item *end_seg;
    int num_channels = 0;
    float end;

    if (utt.relation_present("SourceCoef"))
    {
	EST_Track *source_coef = 
	    track(utt.relation("SourceCoef")->head()->f("coefs"));
	num_channels = source_coef->num_channels();
    }

    if (seg_relation == "")
	end_seg = utt.relation("Segment", 1)->last();
    else
	end_seg = utt.relation(seg_relation, 1)->last();

    if (end_seg)
	end = end_seg->F("end");
    else
	end = 0;

    targets_to_pitchmarks(*(utt.relation("Target")), *target_coef, 
			  num_channels,end);

    EST_Item *item = utt.relation("TargetCoef")->append();
    item->set("name", "coef");
    item->set_val("coefs",est_val(target_coef));

}

void targets_to_pitchmarks(EST_Relation &targ, EST_Track &pitchmarks, 
			   int num_channels,float end)
{
    EST_Item *s;
    float time, f0, prev_time, prev_f0, m, max;
    int i;

    // Its impossible to guess the length of the pitchmark array before 
    // hand. Here we find the upper limit and resize at the end
    for (max = 0.0, s = targ.first_leaf(); s; s = next_leaf(s))
	if (s->F("f0") > max)
	    max = s->F("f0");

    pitchmarks.resize((int)(max * 1.1 * end)+1, num_channels);

    prev_time = 0;
    prev_f0 = targ.first_leaf() ? targ.first_leaf()->F("f0") : 120;
    pitchmarks.t(0) = 0.0;

    for (i = 1, s = targ.first_leaf(); s; s = next_leaf(s))
    {
	time = s->f("pos");
	f0 = s->F("f0");

	if (f0 < 30)  // to protect against with duff IntTarget algorithms
	    continue; 
	if (time == prev_time)
	    continue;
	else if (time < prev_time)
	{
	    cerr << "UniSyn: warning target in wrong order at " << prev_time;
	    cerr << " ignored" << endl;
	    continue;
	}
	m = (f0 - prev_f0) / (time - prev_time);


	{
	    f0 = (m * (pitchmarks.t(i - 1) - prev_time)) + prev_f0;
	    pitchmarks.t(i) = pitchmarks.t(i - 1) + 1.0/f0;
	}
	prev_time = time;
	prev_f0 = f0;
    }
    // Ensure pitch marks go to the end of the utterance
    // This will effectively mean the last half diphone will be extend over
    // the whol final segment.  This will only be reasonable if the
    // final segment is a silence.
    for (; pitchmarks.t(i - 1) < end; ++i)
	pitchmarks.t(i) = pitchmarks.t(i - 1) + 1.0/prev_f0;
    pitchmarks.resize(i, pitchmarks.num_channels());
}    
*/


/*static void add_end_silences(EST_Relation &segment, EST_Relation &target)
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
    if (!ph_is_silence(t->f("name")))
    {
	n = t->insert_after();
	n->set("name", ph_silence());
	n->set("dur", pause_duration);
	shift += pause_duration;
    }
    dur_to_end(segment);

    target.tail()->set("pos", (target.tail()->F("pos") + shift));
}

void merge_pitchmarks(EST_Utterance &u, EST_Track &pm1, 
		      EST_Track &pm2, EST_Track &target_pm,
		      EST_Relation &guide)
{
    EST_Item *s;
    float s_end, s_start;
    int s_i_start, s_i_end;
    int i, j = 0;
    (void) u;

    target_pm.resize(1000000, 0);
    s_start = 0.0;

    for (s = guide.head(); s; s = inext(s))
	{
	    s_end = s->F("end", 1);
	    if (s->fI("use_pm") == 1)
		{
		    s_i_start = pm1.index_below(s_start);
		    s_i_end = pm1.index_below(s_end);
		    for (i = s_i_start; i < s_i_end; ++i, ++j)
			target_pm.t(j) = pm1.t(i);
		}
	    else
		{
		    s_i_start = pm2.index_below(s_start);
		    s_i_end = pm2.index_below(s_end);
		    for (i = s_i_start; i < s_i_end; ++i, ++j)
			target_pm.t(j) = pm2.t(i);
		}
	    s_start = s_end;
	}
}

void warp_f0(EST_Track &source_f0, EST_Relation &source_seg,
	     EST_Track &target_f0, EST_Relation &target_seg)
{
    EST_Item *s, *t;
    float prev_source_end = 0.0, prev_target_end = 0.0;
    EST_Track part;
    int frame_start, frame_end;
    float stretch, t_last_time = 0, s_last_time = 0;
    EST_Relation match("Match");
    EST_Item xx;
    EST_Track str;
    int i = 0;

    dp_match(target_seg, source_seg, match, local_cost, &xx);

    target_f0 = source_f0;
    frame_start = 0;
    frame_end = 0;

    str.resize(target_seg.length(), 1);

    cout << "tag: " << target_seg << endl;

    for (t = target_seg.head(); t; t = inext(t))
    {
	s = daughter1(t,"Match");
	if (s == 0) // ie extra phone in target specification
	    continue;

	frame_end = source_f0.index(s->f("end"));
	if ((frame_end - frame_start) < 1)
	{
	    cout << "Warning no frames for: " << *t << endl;
	    continue;
	}
	target_f0.sub_track(part, frame_start, (frame_end - frame_start + 1),
			    0, EST_ALL);

	stretch = (t->F("end") - prev_target_end) / 
		   (s->F("end") - prev_source_end);

	str.a(i) = stretch;
	str.t(i++) = t->F("end");

	cout << "\nstretch: " << stretch << endl;
	cout << "source: " << *s << endl;
	cout << "target: " << *t << endl;
	cout << "frames: " << frame_start << " " << frame_end << endl;

	stretch_f0_time(part, stretch, s_last_time, t_last_time);

	prev_target_end = t->f("end");
	prev_source_end = s->f("end");
	frame_start = frame_end + 1;
	t_last_time = part.end();
	s_last_time = source_f0.t(frame_end);
	cout << "last time = " << s_last_time << " " << t_last_time << endl;
    }
    target_f0.resize(frame_end, 1);
    target_f0.a(target_f0.num_frames() - 1) = 100;
    str.save("zz_stretch");
}

void warp_pitchmarks(EST_Utterance &utt, EST_Track *source_pm, 
		    EST_Relation &source_seg, EST_Relation &target_seg)
{
    EST_Track source_f0, target_f0, *target_pm;

    target_pm = new EST_Track;

    cout << "tag: "<< target_seg << endl;

    add_end_silences(target_seg);


    cout << "tag 2: "<< target_seg << endl;

    pitchmarks_to_f0(*source_pm, source_f0, 0.01);

    cout << "tag 3: "<< target_seg << endl;

    warp_f0(source_f0, source_seg, target_f0, target_seg);

    f0_to_pitchmarks(target_f0, *target_pm);

    utt.create_relation("TargetCoef");
    utt.create_relation("SourceSegments");

    *utt.relation("SourceSegments") = source_seg;

    EST_Item *item = utt.relation("TargetCoef")->append();

    target_f0.save("tt_tar.f0", "est");
    target_seg.save("tt_tar.lab");
    source_seg.save("tt_sou.lab");
    source_f0.save("tt_sou.f0", "est");

    target_pm->save("target_coef_a.pm","est");
    item->set("name", "coefs");
    item->set_val("coefs", est_val(target_pm));
}

float local_cost(const EST_Item *s1, const EST_Item *s2)
{
    float insertion_cost = get_c_int(siod_get_lval("met_insertion", NULL));
    float deletion_cost = get_c_int(siod_get_lval("met_deletion", NULL));
    float substitution_cost = 
	get_c_int(siod_get_lval("met_substitution", NULL));

    EST_String null_sym = "nil";

    // otherwise cost is either insertion cost, or cost_matrix value
    if (s1->name() == s2->name())
	return 0;
    else
    {
	if (s1->name() == null_sym)
	    return insertion_cost;
	else if (s2->name() == null_sym)
	    return deletion_cost;
	else 
	    return substitution_cost;
    }
}
typedef
float (*local_cost_function)(const EST_Item *item1,
			     const EST_Item *item2);

bool dp_match(const EST_Relation &lexical,
	      const EST_Relation &surface,
	      EST_Relation &match,
	      local_cost_function lcf,
	      EST_Item *null_syl);



*/

/*static void add_end_silences(EST_Relation &segment)
{
    EST_Item *t, *n;

    t = segment.head();
    if (!ph_is_silence(t->f("name")))
    {
	n = t->insert_before();
	n->set("name", ph_silence());
    }

    t = segment.tail();
    if (!ph_is_silence(t->f("name")))
    {
	n = t->insert_after();
	n->set("name", ph_silence());
    }
}

*/
