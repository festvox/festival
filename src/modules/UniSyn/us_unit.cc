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
/*                 Acoustic Unit Concatenation                           */
/*                                                                       */
/*************************************************************************/


#include "siod.h"
#include "EST_sigpr.h"
#include "EST_wave_aux.h"
#include "EST_track_aux.h"
#include "EST_ling_class.h"
#include "us_synthesis.h"
#include <cmath>

#include "Phone.h"

void merge_features(EST_Item *from, EST_Item *to, int keep_id);

void dp_time_align(EST_Utterance &utt, const EST_String &source_name,
		   const EST_String &target_name, 
		   const EST_String &time_name,
		   bool do_start);

void concatenate_unit_coefs(EST_Relation &unit_stream, EST_Track &source_lpc);
void us_unit_raw_concat(EST_Utterance &utt);

void window_units(EST_Relation &unit_stream, 
		  EST_TVector<EST_Wave> &frames, 
		  float window_factor, 
		  EST_String window_name,
		  bool window_symmetric,
		  EST_IVector *pm_indices=0);

bool dp_match(const EST_Relation &lexical,
	      const EST_Relation &surface,
	      EST_Relation &match,
	      float ins, float del, float sub);

void map_match_times(EST_Relation &target, const EST_String &match_name,
	       const EST_String &time_name, bool do_start);


static void window_frame(EST_Wave &frame, EST_Wave &whole, float scale, 
			 int start, int end, EST_WindowFunc *window_function,
			 int centre_index=-1)
{
  int i, j, send;
  EST_TBuffer<float> window;
  int window_length = (end-start)+1; 

  if (frame.num_samples() != (window_length))
    frame.resize(window_length);
  frame.set_sample_rate(whole.sample_rate());
  // Ensure we have a safe end
  if (end < whole.num_samples())
    send = end;
  else
    send = whole.num_samples();

  
  int print_centre;
  if ( centre_index < 0 ){
    window_function( window_length, window, -1 );
    print_centre = (window_length-1)/2+start;
  }
  else{
    window_function( window_length, window, (centre_index-start));
    print_centre = centre_index;
  }


#if defined(EST_DEBUGGING)
  cerr << "(start centre end window_length wholewavelen) " 
       << start << " " 
       << print_centre << " " 
       << end   << " "
       << window_length << " "
       << whole.num_samples() << endl;    
#endif  


  // To allow a_no_check access we do this in three stages
  for (i = 0, j = start; j < 0; ++i, ++j)
    frame.a_no_check(i) = 0;
  for ( ; j < send; ++i, ++j)
    frame.a_no_check(i) = (int)((float)whole.a_no_check(j) * window(i) * scale);
  for ( ; j < end; ++j,++i)
    frame.a_no_check(i) = 0;


#if defined(EST_DEBUGGING)
  // It's not always very nice to resynthesise speech from
  // inserted zeros!  These checks should alert the user (me ;)
  if( start<0 )
    EST_warning( "padded start of pitch period with zeros (index %d)", i );
  
  if( end>whole.num_samples() )
    EST_warning( "padded end of pitch period with zeros (frame %d)", i );      
#endif
}


// The window_signal function has been changed in several ways:
//
// *) The function now has an asymmetric window mode.  
//
//    In this mode, asymmetric windows are used from pitchmark at t-1
//    to pitchmark at time t+1, with the maximum value of 1.0 at
//    pitchmark at time t.
//
// *) In the original symmetric mode:
//
//    The first change is to ensure the window frames always have an
//    odd number of samples (a convention for how to handle rounding
//    problems when converting from times (float) to sample numbers
//    (int)).  The centre sample corresponds to the pitch mark time.
//
//    The second change is that the estimate of local pitch period is
//    always based in current and *previous* pitchmark.  In the case
//    of the first pitch mark in track pm, the previous pitchmark is
//    assumed to be at zero time.  Hopefully, this won't break much.
//    However, if this convention is not used everywhere else that
//    it's needed and some things break, then arguably those
//    things need to be fixed to adhere to this same convention...
void window_signal(EST_Wave &sig, EST_Track &pm, 
		   EST_WaveVector &frames, int &i, float scale, 
		   float window_factor, 
		   EST_WindowFunc *window_function,
		   bool window_symmetric,
		   EST_IVector *pm_indices=0)
{
    float first_pos, period=0.0;
    float prev_pm, current_pm;
    int first_sample, centre_sample, last_sample;
    int sample_rate = sig.sample_rate();
    int pm_num_frames = pm.num_frames();
    
    // estimate first period as pitchmark time itself (i.e. assume a previous
    // pitchmark at 0.0 time, waveform sample 0)
    prev_pm = 0.0;


    if( window_symmetric )
      {
	if (pm_num_frames < 1 )
	  EST_error( "Attempted to Window around less than 1 pitchmark" );

	for( int j=0; j<pm_num_frames; ++j, ++i ){
	  current_pm = pm.t(j);
	  period = current_pm - prev_pm;
	  centre_sample = (int)rint( current_pm*(float)sample_rate );
	  
	  first_pos = prev_pm - (period * (window_factor-1.0));
	  first_sample = (int)rint( first_pos*(float)sample_rate );
	  
	  last_sample  = (2*centre_sample)-first_sample;
	  
	  window_frame(frames[i], sig, scale, first_sample, last_sample, window_function);
	  
	  prev_pm = current_pm;
	}
      }
    else{
      if( pm_indices == 0 )
	EST_error( "required pitchmark indices EST_IVector is null" ); 

      int j;

      // Rob's experiment to see if we can handle small bits of speech with no pitchmarks.
      // We just 0 the frames in this case.
      
      if (pm_num_frames < 1 ) 
	{
	  EST_warning( "Attempted to Window around less than 1 pitchmark" );
	}
      else
	{
	  for( j=0; j<pm_num_frames-1; ++j, ++i ){
	    current_pm = pm.t(j);
	    period = current_pm - prev_pm;
	    centre_sample = (int)rint( current_pm*(float)sample_rate );
	    
	    first_pos = prev_pm - (period * (window_factor-1.0));
	    first_sample = (int)rint( first_pos*(float)sample_rate );
	
	    float next_pm = pm.t(j+1);
	    float last_pos = next_pm + ((next_pm-current_pm)*(window_factor-1.0)); 
	    last_sample = (int)rint( last_pos*(float)sample_rate );
	    
	    window_frame(frames[i], sig, scale, first_sample,
			 last_sample, window_function, centre_sample);
	    (*pm_indices)[i] = centre_sample - first_sample;
	    
	    prev_pm = current_pm;
	  }

      //last frame window size is set according to pm.t(end) and the number
      //of samples in the waveform (it is presumed the waveform begins at the
      //preceeding pitchmark and ends at the pitchmark following the current 
      //unit...)

	  current_pm = pm.t(j);
	  centre_sample = (int)rint( current_pm*(float)sample_rate );
	  first_pos = prev_pm - (period * (window_factor-1.0));
	  first_sample = (int)rint( first_pos*(float)sample_rate );
	  last_sample = sig.num_samples()-1;
	  window_frame(frames[i], sig, scale, first_sample,
		       last_sample, window_function);
	  (*pm_indices)[i] = centre_sample - first_sample;
	  
#if defined(EST_DEBUGGING)
	  cerr << "changed: " << i << " " << pm_indices->n() << endl; 
#endif
	  
	  ++i;
	}
    }
}

void window_units( EST_Relation &unit_stream, 
		   EST_TVector<EST_Wave> &frames, 
		   float window_factor, 
		   EST_String window_name,
		   bool window_symmetric,
		   EST_IVector *pm_indices )
{
    int i;
    EST_Wave *sig;
    EST_Item *u;
    EST_Track *coefs;
    int num = 0;
    float scale;
    EST_WindowFunc *window_function;
    
    for (u = unit_stream.head(); u; u = inext(u))
	num += track(u->f("coefs"))->num_frames();
    frames.resize(num);

    if( pm_indices != 0 )
      pm_indices->resize(num);

    if (window_name == "")
      window_name = "hanning";

    window_function =  EST_Window::creator(window_name);
    
    for (i = 0, u = unit_stream.head(); u; u = inext(u))
    {
	sig = wave(u->f("sig"));
	coefs = track(u->f("coefs"));
	scale = (u->f_present("scale") ? u->F("scale") : 1.0);

	window_signal(*sig, *coefs, frames, i, scale, window_factor,
		      window_function, window_symmetric, pm_indices);
    }
}


void us_unit_concat(EST_Utterance &utt, float window_factor, 
		    const EST_String &window_name, 
		    bool no_waveform=false,
		    bool window_symmetric=true)

{
  EST_Relation *unit_stream;
  EST_Track *source_coef = new EST_Track;
  EST_WaveVector *frames = new EST_WaveVector;
  EST_IVector *pm_indices = 0;
  
  unit_stream = utt.relation("Unit", 1);
  
  concatenate_unit_coefs(*unit_stream, *source_coef);
  
  utt.create_relation("SourceCoef");
  EST_Item *item = utt.relation("SourceCoef")->append();
  item->set("name", "coef");
  item->set_val("coefs", est_val(source_coef));
  
  if (!no_waveform){
    if( !window_symmetric )
      pm_indices = new EST_IVector;
    
    window_units(*unit_stream, *frames, 
		 window_factor, window_name, window_symmetric, pm_indices);
    
    item->set_val("frame", est_val(frames));
    
    if( !window_symmetric )
      item->set_val("pm_indices", est_val(pm_indices));
  }
}


void us_get_copy_wave(EST_Utterance &utt, EST_Wave &source_sig, 
		       EST_Track &source_coefs, EST_Relation &source_seg)
{
    EST_Item *s, *n;

    if (!utt.relation_present("Segment"))
	EST_error("utterance must have \"Segment\" relation\n"); 

    utt.create_relation("TmpSegment");

    for (s = source_seg.head(); s; s = inext(s))
    {
	n = utt.relation("TmpSegment")->append();
	merge_features(n, s, 0);
    }

    utt.relation("Segment")->remove_item_feature("source_end");

    dp_time_align(utt, "TmpSegment", "Segment", "source_", 0);

    utt.create_relation("Unit");
    EST_Item *d = utt.relation("Unit")->append();


    EST_Wave *ss = new EST_Wave;
    *ss = source_sig;

    EST_Track *c = new EST_Track;
    *c = source_coefs;

    d->set_val("sig", est_val(ss));
    d->set_val("coefs", est_val(c));

    utt.remove_relation("TmpSegment");
}


void us_energy_normalise(EST_Relation &unit)
{
    EST_Wave *sig;

    for (EST_Item *s = unit.head(); s; s = inext(s))
    {
	sig = wave(s->f("sig"));
	if (s->f_present("energy_factor"))
	    sig->rescale(s->F("energy_factor"));
    }
}

void us_unit_raw_concat(EST_Utterance &utt)
{
    EST_Wave *sig, *unit_sig;
    EST_Track *unit_coefs=0;
    float window_factor;
    int i, j, k;
    int first_pm, last_pm, last_length;
    float first_pos, last_pos;

    window_factor = get_c_float(siod_get_lval("window_factor",
					      "UniSyn: no window_factor"));
    sig = new EST_Wave;

    sig->resize(1000000);
    sig->fill(0);
    j = 0;

    for (EST_Item *s = utt.relation("Unit", 1)->head(); s; s = inext(s))
    {
	unit_sig = wave(s->f("sig"));
	unit_coefs = track(s->f("coefs"));

	first_pos = unit_coefs->t(1);
	first_pm = (int)(first_pos * (float)unit_sig->sample_rate());

	last_pos = unit_coefs->t(unit_coefs->num_frames()-2);
	last_pm = (int)(last_pos * (float)unit_sig->sample_rate());
	last_length = unit_sig->num_samples() - last_pm;

//	cout << "first pm: " << first_pm << endl;
//	cout << "last pm: " << last_pm << endl;
//	cout << "last length: " << last_length << endl;

	j -= first_pm;

	for (i = 0; i < first_pm; ++i, ++j)
	    sig->a_safe(j) += (short)((((float) i)/ (float)first_pm) *(float)unit_sig->a_safe(i)+0.5);

	for (; i < last_pm; ++i, ++j)
	    sig->a(j) = unit_sig->a(i);

	for (k = 0; i < unit_sig->num_samples(); ++i, ++j, ++k)
	    sig->a_safe(j) += (short)((1.0 - (((float) k) / (float) last_length)) 
	      * (float)unit_sig->a_safe(i) + 0.5);

//	j -= last_length;
//	j += 2000;
    }

    sig->resize(j);
    sig->set_sample_rate(16000);

    add_wave_to_utterance(utt, *sig, "Wave");
}


void concatenate_unit_coefs(EST_Relation &unit_stream, EST_Track &source_lpc)
{
    int num_source_frames   = 0;
    int num_source_channels = 0;;
    float prev_time, abs_offset, rel_offset, period, offset;
    int i, j, k, l;
    EST_Track *coefs;

    EST_Item *u = unit_stream.head();
    if( u == 0 ){
      //sometimes we are just asked to synthesise empty utterances, and
      //code elsewhere wants us to continue...
      source_lpc.resize(0,0); 
    }
    else{
      EST_Track *t = 0;
      for ( ; u; u = inext(u))
	{
	  t = track(u->f("coefs"));
	  num_source_frames += t->num_frames();
	}
      
      num_source_channels = t->num_channels();
      
      source_lpc.resize(num_source_frames, num_source_channels);
      source_lpc.copy_setup(*t);
      
      prev_time = 0.0;
      // copy basic information
      for (i = 0, l = 0, u = unit_stream.head(); u; u = inext(u))
	{
	  coefs = track(u->f("coefs"));
	  
	  for (j = 0; j < coefs->num_frames(); ++j, ++i)
	    {
	      for (k = 0; k < coefs->num_channels(); ++k)
		source_lpc.a_no_check(i, k) = coefs->a_no_check(j, k);
	      source_lpc.t(i) = coefs->t(j) + prev_time;
	    }
	  
	  prev_time = source_lpc.t(i - 1);
	  u->set("end", prev_time);
	  u->set("num_frames", coefs->num_frames());
	}
    }

    // adjust pitchmarks
    abs_offset = 0.0;
    rel_offset = 0.0;
    // absolute offset in seconds
    abs_offset = get_c_float(siod_get_lval("us_abs_offset", "zz"));
    // relative offset as a function of local pitch period
    rel_offset = get_c_float(siod_get_lval("us_rel_offset", "zz"));

    if( abs_offset!=0.0 || rel_offset!=0.0 ){
      cerr << "Adjusting pitchmarks" << endl;
      for (i = 0; i < source_lpc.num_frames(); ++i){
	period = get_time_frame_size(source_lpc, (i));
	offset = abs_offset + (rel_offset * period);
	source_lpc.t(i) = source_lpc.t(i) + offset;
      }
    }
}

// jointimes specifies centre of last pitch period in each 
// concatenated unit
// void us_linear_smooth_amplitude( EST_Wave *w, 
// 				 const EST_Track &pm,
// 				 const EST_FVector &jointimes)
// {
//   int num_joins = jointimes.length();
  
//   EST_Track *factor_contour = new EST_Track( num_joins );
  
//   for( int i=0; i<num_joins; ++i ){
//     float join_t = jointimes(i);
//     int join_indx = pm.index_below( join_t );
    
//     // estimate local short-time energy function either side of join
//     int left_start = rount(pm.t(join_indx-2)*(float)16000);
//     int left_end   = rount(pm.t(join_indx)*(float)16000);
//     float left_power = 0.0 ; 
//     for( int j=left_start; j<left_end; ++j )
//       left_power += pow( w[j], 2 );
    
//     left_power /= (left_end - left_start); //normalise for frame length
    
//     int right_start = rount(pm.t(join_indx+1)*(float)16000);
//     int right_end   = rount(pm.t(join_indx+3)*(float)16000);
//     float right_power = 0.0;
//     for( int j=right_start; j<right_end; ++j )
//       right_power += pow( w[j], 2 );

//     right_power /= (right_end - right_start); //normalise for frame length

//     float mean_power = (left_power+right_power)/2.0;

//     float left_factor  = left_power/mean_power;
//     float right_factor = right_power/mean_power;

//     (*factor_contour)[i]   =  left_factor;
//     (*factor_contour)[i+1] = right_factor;
//   }
 
// }

static EST_Track* us_pitch_period_energy_contour( const EST_WaveVector &pp,
						  const EST_Track &pm )
{
  const int pp_length = pp.length();

  EST_Track *contour = new EST_Track;
  contour->resize( pp_length, 1 );

  for( int i=0; i<pp_length; ++i ){
    const EST_Wave &frame = pp(i);
    const int frame_length = frame.length();

    // RMSE for EST_Wave window
    int j;
    for( contour->a_no_check(i,0) = 0.0, j=0; j<frame_length; ++j )
      contour->a_no_check( i, 0 ) += pow( float(frame.a_no_check( j )), float(2.0) );

    contour->a_no_check(i,0) = sqrt( contour->a_no_check(i,0) / (float)j ); 
    contour->t(i) = pm.t(i);
  }
  
  return contour;
} 

EST_Val ffeature(EST_Item *item,const EST_String &fname);

void us_linear_smooth_amplitude( EST_Utterance *utt )
{
  EST_WaveVector *pp = wavevector(utt->relation("SourceCoef")->first()->f("frame"));
  EST_Track *pm = track(utt->relation("SourceCoef")->first()->f("coefs"));

  EST_Track *energy = us_pitch_period_energy_contour( *pp, *pm );
  energy->save( "./energy_track.est", "est" );

  FILE *ofile = fopen( "./join_times.est", "w" );
  EST_Relation *units = utt->relation("Unit");
  for( EST_Item *u=units->head(); u; u=inext(u) ){

    EST_Item *diphone_left = u;
    //    EST_Item *diphone_right = inext(u);

    fprintf( ofile, "%s\t%f\n", diphone_left->S("name").str(), diphone_left->F("end"));

    EST_Item *join_phone_left = inext(item(diphone_left->f("ph1")));
    EST_String phone_name = join_phone_left->S("name");
    if( ph_is_sonorant( phone_name ) && !ph_is_silence( phone_name )){

      //if( (ffeature(join_phone_left, "ph_vc")).S() == "+"){ // ideally for sonorants

      cerr << "smoothing phone " << join_phone_left->S("name") << "\n";
      
      //      EST_Item *join_phone_right = item(diphone_right->f("ph1"));

      int left_end_index = energy->index(diphone_left->F("end"));
      int right_start_index = left_end_index + 1; 
      float left_power  = energy->a(left_end_index,0);
      float right_power = energy->a(right_start_index,0);

      float mean_power = (left_power+right_power)/2.0;
      float left_factor  = left_power/mean_power;
      float right_factor = right_power/mean_power;
   
      int smooth_start_index = left_end_index-5;
      int smooth_end_index   = right_start_index+5;


      // rescale left pitch periods
      float factor = 1.0;
      float factor_incr = (left_factor-1.0)/(float)(left_end_index - smooth_start_index);
      for( int i=smooth_start_index; i<=left_end_index; ++i, factor+=factor_incr ){
	(*pp)[i].rescale( factor, 0 );
	cerr << "rescaled frame " << i << "(factor " << factor << ")\n";
      }

      // rescale right pitch periods
      factor = right_factor;
      factor_incr = (1.0-right_factor)/(float)(smooth_end_index-right_start_index);
      for( int i=right_start_index; i<=smooth_end_index; ++i, factor+=factor_incr){
	(*pp)[i].rescale( factor, 0 );
	cerr << "rescaled frame " << i << "(factor " << factor << ")\n";
      }
    }
    else
      cerr << "no smoothing for " << join_phone_left->S("name") << "\n";

    cerr <<endl;
  }  

  fclose( ofile );
  delete energy;
}

