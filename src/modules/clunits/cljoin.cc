/*************************************************************************/
/*                                                                       */
/*                  Language Technologies Institute                      */
/*                     Carnegie Mellon University                        */
/*                      Copyright (c) 1999-2004                          */
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
/*                 Author: Alan W Black                                  */
/*                   Date: July 2001                                     */
/* --------------------------------------------------------------------- */
/*  Some more interesting join methods                                   */
/*       based on the mapping code in UniSyn                             */
/*                                                                       */
/*************************************************************************/

#include "EST_error.h"
#include "us_synthesis.h"
#include "festival.h"

#if 0
static int awb_voiced(EST_Track &pm,int i)
{
    // Hacky guess at voicedness
    return TRUE;
    if ((i < 2) || (i+3 > pm.num_frames()))
	return FALSE;
    else if (((pm.t(i) - pm.t(i-1)) == (pm.t(i+1) - pm.t(i))) &&
	     ((pm.t(i-1) - pm.t(i-2)) == (pm.t(i) - pm.t(i-1))))
	return FALSE;
    else
	return TRUE;
}

static float frame_duration(EST_Track &pm, int i)
{
    if (i <= 0)
	return frame_duration(pm,i+1);
    else if (i >= pm.num_frames())
	return frame_duration(pm,pm.num_frames()-1);
    else
	return pm.t(i)-pm.t(i-1);
}

static float awb_smoothed(EST_Track &pm, int i)
{
    // Returned smoothed pitch period at i

    return (frame_duration(pm,i-4)+
	    frame_duration(pm,i-3)+
	    frame_duration(pm,i-2)+
	    frame_duration(pm,i-1)+
	    frame_duration(pm,i)+
	    frame_duration(pm,i+1)+
	    frame_duration(pm,i+2))/7.0;
}
#endif


static void make_segment_varied_mapping(EST_Relation &source_lab, 
					EST_Track &source_pm, 
					EST_Track &target_pm, 
					EST_IVector &map,
					float dur_impose_factor,
					float f0_impose_factor) 
{
    int n_i, s_i, u_i, u_frames;
    int spp;
    float stime, ttime, ltime, dratio, n_frames;
    int max_frames;
    EST_Item *u;
    EST_Track ntarget_pm;

    ntarget_pm = target_pm;
    if (target_pm.num_frames() > source_pm.num_frames())
	max_frames = target_pm.num_frames()+100;
    else
	max_frames = source_pm.num_frames()+100;

    ntarget_pm.resize(max_frames,target_pm.num_channels());
    map.resize(max_frames);
    
//    printf("source_lab relations is %s\n",(const char *)source_lab.name());

    if (target_pm.t(target_pm.num_frames() - 1) < 
	source_lab.tail()->F("end",0))
    {
	EST_warning("Target pitchmarks end before end of target segment "
		    "timings (%f vs %f). Expect a truncated utterance\n",
		    target_pm.t(target_pm.num_frames() - 1),
	            source_lab.tail()->F("end",0.0));
    }

    n_i = 0;
    s_i = 0;
    ltime = 0;
    for (u = source_lab.head(); u; u = inext(u))
    {
	u_frames = u->I("num_frames");
//	stime = source_pm.t(s_i+u_frames-1) - source_pm.t(s_i);
	stime = u->F("unit_duration");
	ttime = ffeature(u,"segment_duration");
	if (streq("+",(EST_String)ffeature(u,"ph_vc")))
	    dratio = stime / (stime + ((ttime-stime)*dur_impose_factor));
	else
	    dratio = 1;
	n_frames = (float)u_frames / dratio;
/*	printf("unit %s dratio %f %d %f time %f, stime %f\n",
	       (const char *)u->name(),dratio,u_frames,n_frames,
	       ttime,stime); */
	
 	for (u_i = 0; u_i < n_frames; u_i++,n_i++)
	{
	    spp = (int)((float)u_i*dratio);
	    
	    if (s_i + spp == 0)
		ntarget_pm.t(n_i) = ltime;
	    else
		ntarget_pm.t(n_i) = ltime + source_pm.t(s_i+spp)-
		    source_pm.t(s_i+spp-1);
	    map[n_i] = s_i+spp;
	    ltime = ntarget_pm.t(n_i);
	    if (n_i+1 == ntarget_pm.num_frames())
		break;
	}
	s_i += u_frames;

    }

    ntarget_pm.resize(n_i,ntarget_pm.num_channels());

/*    printf("target_pm.end() = %f ntarget_pm.end() = %f\n",
      target_pm.end(), ntarget_pm.end()); */
    target_pm = ntarget_pm;
/*    printf("target_pm.end() = %f ntarget_pm.end() = %f\n",
      target_pm.end(), ntarget_pm.end()); */
    if (n_i == 0)
	map.resize(0);  // nothing to synthesize
    else
	map.resize(n_i - 1);
}

void cl_mapping(EST_Utterance &utt, LISP params)
{
    EST_Relation *target_lab;
    EST_IVector *map;
    EST_Track *source_coef=0, *target_coef=0;
    float dur_impose_factor, f0_impose_factor;

    source_coef = track(utt.relation("SourceCoef")->head()->f("coefs"));
    target_coef = track(utt.relation("TargetCoef")->head()->f("coefs"));
    target_lab = utt.relation("Segment");
    
    map = new EST_IVector;

    dur_impose_factor = get_param_float("dur_impose_factor",params,0.0);
    f0_impose_factor = get_param_float("f0_impose_factor",params,0.0);
    
    make_segment_varied_mapping(*target_lab, *source_coef,
				*target_coef, *map,
				dur_impose_factor,
				f0_impose_factor);

    utt.create_relation("US_map");
    EST_Item *item = utt.relation("US_map")->append();
    item->set_val("map", est_val(map));

}

LISP l_cl_mapping(LISP utt, LISP params)
{
    EST_Utterance *u = get_c_utt(utt);

    cl_mapping(*u,params);

    return utt;
}



