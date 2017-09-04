/*************************************************************************/
/*                                                                       */
/*                   Carnegie Mellon University and                      */
/*                Centre for Speech Technology Research                  */
/*                     University of Edinburgh, UK                       */
/*                       Copyright (c) 1998-2001                         */
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
/*  THE UNIVERSITY OF EDINBURGH, CARNEGIE MELLON UNIVERSITY AND THE      */
/*  CONTRIBUTORS TO THIS WORK DISCLAIM ALL WARRANTIES WITH REGARD TO     */
/*  THIS SOFTWARE, INCLUDING ALL IMPLIED WARRANTIES OF MERCHANTABILITY   */
/*  AND FITNESS, IN NO EVENT SHALL THE UNIVERSITY OF EDINBURGH, CARNEGIE */
/*  MELLON UNIVERSITY NOR THE CONTRIBUTORS BE LIABLE FOR ANY SPECIAL,    */
/*  INDIRECT OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES WHATSOEVER          */
/*  RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN  AN ACTION   */
/*  OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF     */
/*  OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.       */
/*                                                                       */
/*************************************************************************/
/*             Author :  Alan W Black                                    */
/*             Date   :  April 1998                                      */
/*-----------------------------------------------------------------------*/
/*                                                                       */
/* Find the "acoustic" distance between two units.  There are a number   */
/* of various params to this but it falls downs to a weighted summed     */
/* difference of parameters for the two units.                           */
/*                                                                       */
/* A linear interpolation of the smallest to largest is done, and an     */
/* optional penality is factored.                                        */
/*                                                                       */
/* This is independent of any particular Unit Database                   */
/*                                                                       */
/*=======================================================================*/
#include <cstdlib>
#include "festival.h"
#include "clunits.h"

static void find_unit_distances(LISP units, const EST_String &fname);

static float duration_penalty_weight=1.0;
static float f0_penalty_weight=0.0;
static EST_FVector weights;
static LISP get_stds_per_unit = NIL;
static EST_String directory = "";

LISP acost_utt_load_coeffs(LISP utt, LISP params)
{
    // Load all the coefficient file and create sub_track for each 
    // segment
    EST_Utterance *u = get_c_utt(utt);
    EST_Track *track = new EST_Track;
    EST_String coefffilename = 
	EST_String(get_param_str("db_dir",params,"./"))+
	    get_param_str("coeffs_dir",params,"coeffs/")+
		u->f("fileid").string()+
		    get_param_str("coeffs_ext",params,".coeffs");
    float ac_left_context = get_param_float("ac_left_context",params,0.0);
    EST_String segrelation = 
        EST_String(get_param_str("clunit_relation",params,"Segment"));

    if (track->load(coefffilename) != format_ok)
    {
	cerr << "ACOST: failed to read track from \"" << 
	    coefffilename << "\"" << endl;
	festival_error();
    }
    cl_maybe_fix_pitch_c0(track);
    // Add the whole track to a new relation
    EST_Item *c_si = u->create_relation("Acoustic_Coeffs")->append();
    c_si->set_val("Acoustic_Coeffs", est_val(track));

    // Now add subtracks for each segment
    for (EST_Item *s=u->relation(segrelation)->first(); s != 0; s=inext(s))
    {
	EST_Track *st = new EST_Track;
	float start = ffeature(s,"segment_start");
	float end = ffeature(s,"segment_end");
	if (iprev(s))
	    start -= ac_left_context*
		ffeature(s,"p.segment_duration").Float();
	int startf = track->index(start);
	int nframes = track->index(end)-startf;
	if (startf >= track->num_frames())
	{
	    cerr << "ACOST: utterances longer than coeffs file \n  " <<
		coefffilename << endl;
	    festival_error();
	}
	else if ((startf + nframes) > track->num_frames())
	    nframes = track->num_frames() - startf;
	track->sub_track(*st,startf,nframes,0);
	s->set_val("Acoustic_Coeffs",est_val(st));
    }
    return utt;
}

LISP make_unit_distance_tables(LISP unittypes, LISP params)
{
    // Build distance tables for given lists of lists of items
    // Each item should have a coeffs feature (a track).  Also
    // the order the items are give is the order they will
    // be put into the saved distance matrix.  Care should be
    // taken to ensure you save related features in the same order
    LISP ut;

    for (ut=unittypes; ut != NIL; ut = cdr(ut))
    {
	acost_dt_params(params);

	EST_String unit_name = get_c_string(car(car(ut)));
	EST_String fname = 
	    EST_String(get_param_str("db_dir",params,"./"))+
		get_param_str("disttabs_dir",params,"disttabs/")+
		    unit_name+".disttab";
	cout << "Making unit distance table for " << unit_name <<
	    " (" << siod_llength(cdr(car(ut))) << ")" << endl;
	find_unit_distances(cdr(car(ut)),fname);
    }

    return NIL;
}

LISP ac_distance_tracks(LISP filename1, LISP filename2, LISP lweights)
{
    // Return Unit type distance between two full files.
    EST_Track a,b;

    if (a.load(get_c_string(filename1)) != format_ok)
    {
	cerr << "CLUNITS: distance tracks: \"" << 
	    get_c_string(filename1) << "\" unloadable." << endl;
	festival_error();
    }
    if (b.load(get_c_string(filename2)) != format_ok)
    {
	cerr << "CLUNITS: distance tracks: \"" << 
	    get_c_string(filename2) << "\" unloadable."
	    << endl;
	festival_error();
    }

    LISP l;
    int i;
    float dist;
    
    duration_penalty_weight = get_c_float(car(lweights));

    EST_FVector tweights(siod_llength(cdr(lweights)));
    for (l=cdr(lweights),i=0; l!=NIL;l=cdr(l),i++)
	tweights[i] = get_c_float(car(l));

    dist = ac_unit_distance(a,b,tweights);

    return flocons(dist);
}

void acost_dt_params(LISP params)
{
    // Extract parameters from description
    LISP lweights,l;
    int i;

    directory = get_param_str("disttab_dir",params,"disttabs");
    lweights = get_param_lisp("ac_weights",params,NIL);

    weights.resize(siod_llength(lweights));
    for (l=lweights,i=0; l != NIL; l=cdr(l),i++)
	weights[i] = get_c_float(car(l));
    duration_penalty_weight = get_param_float("dur_pen_weight",params,1.0);
    f0_penalty_weight = get_param_float("f0_pen_weight",params,0.0);
    get_stds_per_unit = get_param_lisp("get_stds_per_unit",params,NIL);

}    

static void cumulate_ss_frames(EST_Track *a,EST_SuffStats *ss_frames)
{
    // Gather sufficient statistics on the parameters in each frame
    int i,j;
    double p;
    
    for (i=0; i < a->num_frames(); i++)
	for (j=0; j < a->num_channels(); j++)
	{
	    p = a->a_no_check(i,j);
	    if (!finite(p))
	    {
		p = 1.0e5;
		a->a_no_check(i,j) = p;
	    }
	    ss_frames[j] += p;
	}
}

static EST_Track *acost_get_coefficients(EST_Item *si)
{
    EST_Val c = si->f("Acoustic_Coeffs");

    if (c == 0)
    {
	cerr << "ACOST: failed to find coefficients on items\n";
	festival_error();
    }
    return track(c);
}

static void find_unit_distances(LISP units, const EST_String &fname)
{
    // Find all distances between units of type.
    int i,j;
    LISP u,v;
    EST_FMatrix dist(siod_llength(units),siod_llength(units));
    EST_SuffStats *ss_frames = new EST_SuffStats[weights.length()];

    // Find stddev for this unit
    for (i=0,u=units; u != 0; u=cdr(u),i++)
    {
	dist.a_no_check(0,i) = 0;
	if (get_stds_per_unit != NIL)  // need to calculate stds for this unit
	{
	    EST_Item *si = get_c_item(car(u));
	    EST_Track *a=acost_get_coefficients(si);
            if (a->num_channels() != weights.length())
            {
                cerr << "ACOST: number of weights " <<
                    weights.length() << " does not match mcep param width "
                     << a->num_channels() << endl;
                festival_error();
            }
	    cumulate_ss_frames(a,ss_frames);
	}
    }

    if (get_stds_per_unit != NIL)  // modify weights with stds
	for (i=0; i < weights.length(); i++)
	    weights[i] /= (ss_frames[i].stddev() * ss_frames[i].stddev());

    for (i=1,u=cdr(units); u != 0; u=cdr(u),i++)
    {
	EST_Track *a=acost_get_coefficients(get_c_item(car(u)));
	// Only fill lower half of matrix
	for (v=units,j=0; j < i; j++,v=cdr(v))
	{
	    EST_Track *b=acost_get_coefficients(get_c_item(car(v)));
	    dist.a_no_check(i,j) = ac_unit_distance(*a,*b,weights);
	}
	for ( ; j < dist.num_rows(); j++)
	     dist.a_no_check(i,j) = 0.0;
    }

    delete [] ss_frames;

    if (dist.save(fname,"est_ascii") != write_ok)
    {
	cerr << "ACOST: failed to save distance data in \"" <<
	    fname << endl;
	festival_error();
    }
}

float ac_unit_distance(const EST_Track &unit1,
		       const EST_Track &unit2,
		       const EST_FVector wghts)
{
    // Find distance between two units, unit1 will be smaller than unit2
    float distance = 0.0;
    int i,j,k;
    float fj,incr,dur_penalty;
    float cost,diff;
    float score;
    int nc = unit1.num_channels();

    if (unit1.end() > unit2.end())
	return ac_unit_distance(unit2,unit1,wghts);  // unit1 is smaller
    if (unit1.num_frames() == 0)
//	return 1.0e20; // HUGE_VAL is too HUGE
	return 100;

    if ((unit1.num_channels() != unit2.num_channels()) ||
	(unit1.num_channels() != wghts.length()))
    {
	cerr << "ac_unit_distance: unit1 (" << unit1.num_channels() << 
	    "), unit2 (" << unit2.num_channels() << ") and wghts (" <<
		wghts.length() << ") are of different size" << endl;
	festival_error();
    }

//    printf("unit1 nf %d end %f unit2 nf %d end %f \n",
//	   unit1.num_frames(),
//	   unit1.end(),
//	   unit2.num_frames(),
//	   unit2.end());
    incr = unit1.end()/ unit2.end();
    for (fj=0.0,j=i=0; i < unit2.num_frames(); i++,fj+=incr)
    {
	while ((j < unit1.num_frames()-1) && (unit1.t(j) < unit2.t(i)*incr) )
	    j++;
//	printf("unit1 j %d %f unit2 %d %f %f\n",
//	       j,unit1.t(j),
//	       i,unit2.t(i),
//	       unit2.t(i)*incr);
	cost = f0_penalty_weight *
	    fabs((unit1.t(j)-((j > 0) ? unit1.t(j-1) : 0))-
		 (unit2.t(i)-((i > 0) ? unit2.t(i-1) : 0)));
	    
	for (k=0; k < nc; k++)
	    if (wghts.a_no_check(k) != 0.0)
	    {
		diff = unit2.a_no_check(i,k)-unit1.a_no_check(j,k);
		diff *= diff;
		cost += diff*wghts.a_no_check(k);
	    }
	distance += cost;
    }

    dur_penalty = (float)unit2.end()/(float)unit1.end();

    score = (distance/(float)i)+(dur_penalty*duration_penalty_weight);

    return score;
}

// Maybe can reduce this to an FVector pulled out of the track 
float frame_distance(const EST_Track &a, int ai,
		     const EST_Track &b, int bi,
		     const EST_FVector &wghts,
		     float f0_weight)
{
    float cost = 0.0,diff;

    if ((a.num_channels() != b.num_channels()) ||
	(a.num_channels() != wghts.length()))
    {
	cerr << "frame_distance: unit1, unit2 and wghts" <<
	    " are of different size" << endl;
	festival_error();
    }
	
    if ((ai < 0) ||
	(ai >= a.num_frames()) ||
	(bi < 0) ||
	(bi >= b.num_frames()))
    {
	cerr << "frame_distance: frames out of range" << endl;
	festival_error();
    }
    
    if (f0_weight > 0)
    {
	cost = f0_weight *
	    fabs((a.t(ai)-((ai > 0) ? a.t(ai-1) : 0))-
		 (b.t(bi)-((bi > 0) ? b.t(bi-1) : 0)));
    }

    for (int k=0; k < a.num_channels(); k++)
    {
	if (wghts.a_no_check(k) != 0.0)
	{
	    diff = a.a_no_check(ai,k)-b.a_no_check(bi,k);
	    diff *= wghts.a_no_check(k);
	    cost += diff*diff;
	}
    }

    return sqrt(cost);
}

