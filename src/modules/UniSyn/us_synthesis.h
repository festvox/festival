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
/*                          Author: Paul Taylor                          */
/*                           Date: March 1998                            */
/* --------------------------------------------------------------------- */
/*                                                                       */
/*************************************************************************/


#ifndef __US_SYNTHESIS_H__
#define __US_SYNTHESIS_H__

#include "EST.h"

typedef EST_TVector <EST_Wave> EST_WaveVector;

VAL_REGISTER_TYPE_DCLS(wavevector,EST_WaveVector)
VAL_REGISTER_TYPE_DCLS(ivector,EST_IVector)

SIOD_REGISTER_TYPE_DCLS( wavevector, EST_WaveVector)

void add_wave_to_utterance(EST_Utterance &u, EST_Wave &sig, 
			const EST_String &name);


/*void us_lpc_synthesis(EST_Utterance &utt);

void us_mapping(EST_Utterance &utt, const EST_String &method);
void us_td_synthesis(EST_Utterance &utt, const EST_String &filter_method,
		     const EST_String &ola_method);

void lpc_synthesis(EST_Track &source_coef, EST_Track &target_coef, 
		   EST_TVector <EST_Wave> &frames, EST_Wave &sig, 
		   EST_IVector &map);

void window_signal(EST_Wave &sig, EST_Track &pm, 
		   EST_TVector<EST_Wave> &frames, int &i, float scale, 
		   float window_factor, 
		   const EST_String &window_name);

void window_signal(EST_Relation &unit_stream, 
		   EST_TVector<EST_Wave> &frames, 
		   float window_factor, 
		   EST_String window_name);

void concatenate_coefs(EST_Relation &unit_stream, 
			      EST_Track &source_lpc);

void us_unit_copy_wave(EST_Utterance &utt, EST_Wave &source_sig, 
		  EST_Track *source_pm);

void us_unit_concat(EST_Utterance &utt);

void us_full_cut(EST_Relation &unit);

void us_energy_normalise(EST_Relation &unit);

void us_unit_raw_concat(EST_Utterance &utt);


void pitchmarks_to_f0(EST_Track &pm, EST_Track &fz, float shift);

void f0_to_pitchmarks(EST_Track &fz, EST_Track &pm, float target_end = -1.0);

void targets_to_pitchmarks(EST_Relation &targ, EST_Track &pitchmarks, 
			   int num_channels,float end);

void targets_to_f0(EST_Relation &targ, EST_Track &f0, const float shift);

void stretch_f0_time(EST_Track &f0, float stretch, 
		     float s_last_time, float t_last_time);

void warp_f0(EST_Track &source_f0, EST_Relation &source_seg,
	     EST_Track &target_f0, EST_Relation &target_seg);

void warp_pitchmarks(EST_Utterance &utt, EST_Track *source_pm, 
		    EST_Relation &source_seg, EST_Relation &target_seg);

void us_F0targets_to_pitchmarks(EST_Utterance &utt, 
				const EST_String &seg_relation);

void add_wave_to_utterance(EST_Utterance &u, EST_Wave &sig, 
			const EST_String &name);

void make_linear_mapping(EST_Track &pm, EST_IVector &map);

void make_dp_mapping(EST_Relation &source_lab, EST_Track &source_pm, 
		     EST_Relation &target_lab, EST_Track &target_pm, 
		     const EST_String &match_name, EST_IVector &map);

void make_segment_double_mapping(EST_Relation &source_lab, EST_Track &source_pm, 
		  EST_Relation &target_lab,
		  EST_Track &target_pm, EST_IVector &map);

void make_segment_single_mapping(EST_Relation &source_lab, EST_Track &source_pm, 
			   EST_Track &target_pm, EST_IVector &map);
*/



#endif // __US_SYNTHESIS_H__
