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
/*            LPC residual synthesis alternative version                 */
/*                                                                       */
/*************************************************************************/

#include "us_synthesis.h"
#include "UniSyn.h"
#include "siod.h"
#include "EST_sigpr.h"
#include "EST_error.h"
#include <cmath>
#include "EST_inline_utils.h"

void us_generate_wave(EST_Utterance &utt, 
		     const EST_String &filter_method,
		     const EST_String &ola_method)
{
    EST_IVector *map, *frame_pm_indices;
    EST_WaveVector *frames;
    EST_Track *source_coef, *target_coef;
    EST_Wave *sig, *res;
    EST_FVector gain;

    frames = wavevector(utt.relation("SourceCoef", 1)->head()->f("frame"));

    source_coef = track(utt.relation("SourceCoef", 1)->head()->f("coefs"));
    target_coef = track(utt.relation("TargetCoef", 1)->head()->f("coefs"));

    map = ivector(utt.relation("US_map", 1)->head()->f("map"));

    sig = new EST_Wave;
  
    if ( ola_method == "asymmetric_window" ){
      frame_pm_indices = ivector(utt.relation("SourceCoef", 1)->head()->f("pm_indices"));
      asymmetric_window_td_synthesis( *frames, *target_coef, *sig, *map, *frame_pm_indices );
    }
    else if ( ola_method == "synth_period" )
      td_synthesis2(*frames, *target_coef, *sig, *map);
    else
      td_synthesis(*frames, *target_coef, *sig, *map);
    
    if (filter_method == "lpc")
      {
	map_coefs(*source_coef, *target_coef, *map);
        // Save residual for external prcessing 
        res = new EST_Wave;
        res->copy(*sig);
        utt.relation("TargetCoef", 1)->head()->set_val("residual",est_val(res));
	// fast version
	lpc_filter_fast(*target_coef, *sig, *sig);
	// slower version (but cleaner)
	//lpc_filter_1(*target_coef, *sig, *sig);
      }
    
    add_wave_to_utterance(utt, *sig, "Wave");
}


void map_coefs(EST_Track &source_coef, EST_Track &target_coef, 
	       EST_IVector &map)
{
    int i, j;
    int m;

    if (source_coef.num_channels() != target_coef.num_channels())
	EST_error("Different numbers of channels in LPC resynthesis: "
		  "source %d, target %d\n", source_coef.num_channels(),
		  target_coef.num_channels());

    if (map.n() > target_coef.num_frames())
	m = target_coef.num_frames();
    else
	m = map.n();

    for (i = 0; i < m; ++i)
	for (j = 0; j < target_coef.num_channels(); ++j)
	    target_coef.a_no_check(i, j) = 
		source_coef.a_no_check(map.a_no_check(i), j);
    // There can be one or two frames at the end of target_coed without
    // a map.  Here we zero them, blindly assuming they are in silence
    for ( ; i < target_coef.num_frames(); i++)
	for (j = 0; j < target_coef.num_channels(); ++j)
	    target_coef.a_no_check(i, j) = 0;
}

void td_synthesis(EST_WaveVector &frames,
		  EST_Track &target_pm, EST_Wave &target_sig,
		  EST_IVector &map)
{
    int t_start;
    int i, j;
    float sr;
    int last_sample=0;
    int map_n = map.n();
    
    if( (frames.length()>0) && (map_n>0) ){
      sr = (float)frames(0).sample_rate();
      last_sample = (int)(rint(target_pm.end()*sr)) +
	((frames(frames.length()-1).num_samples() - 1)/2);//window_signal guarantees odd
      target_sig.resize(last_sample+1);
      target_sig.fill(0);
      target_sig.set_sample_rate((int)sr);
 
      for( i=0; i<map_n; ++i )
      {
          int mapped_i;
          if (map.a_no_check(i) < frames.length())
              mapped_i = map.a_no_check(i);
          else
              mapped_i = frames.length()-1;
	const EST_Wave &frame = frames(mapped_i);
	const int frame_num_samples = frame.num_samples();
	t_start = (int)(rint(target_pm.t(i)*sr)) -
	  ((frame_num_samples - 1)/2);//window_signal guarantees odd
	
#if defined(EST_DEBUGGING)
	cerr << t_start << " " 
	     << (frame_num_samples-1)/2 + t_start << " "
	     << frame_num_samples << "\n";
#endif

	for( j=0; j<frame_num_samples; ++j )
	  if( j+t_start>=0 )
	    target_sig.a_no_check(j + t_start) += frame.a_no_check(j);
      }    
    }
}

void asymmetric_window_td_synthesis(EST_WaveVector &frames,
				    EST_Track &target_pm, 
				    EST_Wave &target_sig,
				    EST_IVector &map,
				    EST_IVector &frame_pm_indices)
{
  int t_start;
  int i, j;
  float sr;
  int last_sample=0;
  int map_n = map.n();
  


#if defined(EST_DEBUGGING)
  cerr << "(maplength framelength pm_indiceslength) "
       << map_n << " "
       << frames.n() << " "
       << frame_pm_indices.n() << endl;
#endif


  if( (frames.length()>0) && (map_n>0) ){
    sr = (float)frames(0).sample_rate();

    last_sample = (int)(rint(target_pm.end()*sr)) +
      (frames(map(map_n-1)).num_samples() - frame_pm_indices(map(map_n-1)) - 1);

    target_sig.resize(last_sample+1, EST_ALL, 0); //0 -> don't set values
    target_sig.fill(0);
    target_sig.set_sample_rate((int)sr);
    
    for( i=0; i<map_n; ++i ){
      const int source_index = map.a_no_check(i);
      const EST_Wave &frame = frames(source_index);
      const int frame_num_samples = frame.num_samples();
      t_start = (int)(rint(target_pm.t(i)*sr)) - frame_pm_indices(source_index);
      

#if defined(EST_DEBUGGING)
      cerr << t_start << " " 
	   << frame_pm_indices(source_index) << " " 
	   << frame_pm_indices(source_index) + t_start << " "
	   << frame_num_samples << "\n";
#endif      


      //this initialisation for j intends to ensure (j+t_start)>=0
      // (it might be less than one where prosodic modification 
      // has moved the first pitch mark relative to wave start)
      for( j=-min(0,t_start); j<frame_num_samples; ++j )
      //for( j=0; j<frame_num_samples; ++j )
	target_sig.a_no_check(j + t_start) += frame.a_no_check(j);
    }    
  }
}


void td_synthesis2(EST_WaveVector &frames,
		   EST_Track &target_pm, EST_Wave &target_sig,
		   EST_IVector &map)
{
    int t_start;
    int i, j;
    float sr;
    int last_sample=0;
    int window_start;
    int s_period;
    EST_TBuffer<float> window;
    EST_FVector f;

    float s_window_factor= Param().F("unisyn.window_factor", 1.0);

    if (frames.length()> 0)
	sr = (float)frames(0).sample_rate();
    else
	sr = 16000; // sort of, who cares, its going to be a 0 sample waveform

    if (map.n() > 0)
	last_sample = (int)(target_pm.end() * sr) + 
	    (frames(map(map.n()-1)).num_samples() / 2);

    target_sig.resize(last_sample);
    target_sig.fill(0);
    target_sig.set_sample_rate((int)sr);
    
    for (i = 0; i < map.n(); ++i)
    {
	const EST_Wave &frame = frames(map(i));

	s_period = 
	    (int)(get_frame_size(target_pm, i, (int)sr) * s_window_factor);
//	cout << "period: " << s_period << endl;

	// start of window is mid point of analysis window
	// minus local synth period
	window_start = (frame.num_samples() / 2) - s_period;
	
	EST_Window::window_signal(frame, "hanning", window_start, 
				  s_period * 2, f, 1);

	t_start = ((int)(target_pm.t(i) * sr)
		   - (f.n() / 2));

	for (j = 0; j < f.n(); ++j)
	    if (j + t_start>=0)
		target_sig.a_no_check(j + t_start) += (short)f.a_no_check(j);
    }    
}



/*static void debug_options(EST_Relation &source_lab, EST_Relation &unit,
			  EST_Track &source_coef,
			  EST_Track &target_coef, EST_TVector <EST_Wave> &frames)
{
    EST_IVector map;
    EST_Wave sig;
    EST_Relation pm_lab;

    if (siod_get_lval("us_debug_save_source", NULL) != NIL)
    {
	make_segment_double_mapping(source_lab, source_coef, source_lab, 
			     source_coef, map);
	td_synthesis(source_coef, frames, source_coef, sig, map);
	sig.save("source_sig.wav", "nist");
    }

    if (siod_get_lval("us_debug_save_source_coefs", NULL) != NIL)
	source_coef.save("source_coefs.pm", "est");

    if (siod_get_lval("us_debug_save_target_pm", NULL) != NIL)
	target_coef.save("target_coefs.pm");

    if (siod_get_lval("us_debug_save_source_lab", NULL) != NIL)
	source_lab.save("source.lab");

    if (siod_get_lval("us_debug_save_target_unit_lab", NULL) != NIL)
	unit.save("target_unit.lab");

}
*/

