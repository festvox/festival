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


#ifndef __UNISYN_H__
#define __UNISYN_H__

#include "festival.h"


typedef EST_TVector <EST_Wave> EST_WaveVector;

#ifdef HAVE_US_TDPSOLA_TM
void us_init_tdpsola();
void us_tdpsola_synthesis(EST_Utterance &utt, 
			  const EST_String &ola_method);
#endif


void us_linear_smooth_amplitude( EST_Utterance *utt );

/**@name Functions for Concatenating Units

*/ 

//@{

/** Iterate through the Unit relation and create the<parameter>
    SourceCeof</parameter> relation, which contains a series of
    windowed frames of speech and a track of pitch-synchronous
    coefficients.</para> 

    <para> <parameter>SourceCoef</parameter> contains a single item
    with two features, <parameter>coefs</parameter> and
    <parameter>frame</parameter>

    <parameter>coefs'</parameter>value is a track with all the
    concatenated pitchmarks and coefficients from the units.

    <function>us_unit_concat</function> is where the pitch synchronous
    windowing of the frames in each Unit is performed and the result
    of this is stored as the value of <parameter>frame</parameter>
    </para>
    <formalpara><title>Require:</title><para>Unit</para>
	</formalpara>
    <formalpara><title>Provide:</title><para>SourceCoef</para>
	</formalpara>


    @param utt: utterance

    @param window_factor: This specifies
    how large the analysis window is in relation to the local pitch
    period. A value of 1.0 is often used as this means each frame
    approximately extends from the previous pitch mark to the next.

    @param window_name: This specifies
    the type of window used. "hanning" is standard but any window type
    available from the signal processing library can be used.

    @param window_symmetric: if this is set to true, then symmetric
    analysis windows are used centred at each pitch mark, with size
    determined by the time difference between current and previous 
    pitchmarks.
    
    @param no_waveform: if this is set to true, only the coefficients
    are copied into SourceCoef - no waveform analysis is performed.
*/

void us_unit_concat(EST_Utterance &utt, float window_factor, 
		    const EST_String &window_name, 
		    bool no_waveform=false,
		    bool window_symmetric=true);


/** This function provides the setup for copy resynthesis. In copy
resynthesis, a natural waveform is used as the source speech for
synthesis rather than diphones or other concatenated units. This is
often useful for testing a prosody module or for altering the pitch or
duration of a natural waveform for an experiment. (As such, this
function should really be thought of as a very simple unit selection
module)</para>

<para> In addition to the speech waveform itself, the function
requires a set of pitchmarks in the standard form, and a set of labels
which mark segment boundaries. The <parameter>Segment</parameter>
relation must already exist in the utterance prior to calling this
function. </para>

First, the function creates a<parameter>Unit</parameter> relation with
a single item containing the waveform and the pitchmarks. Next it adds
a set of <parameter>source_end</parameter> features to each item in
the <parameter>Segment</parameter> relation. It does this by
calculating a mapping between the <parameter>Segment</parameter>
relation and the input labels. This mapping is performed by dynamic
programming, as often the two sets of labels don't match exactly.
</para>
<para>
The final result, therefore is a Unit relation and Segment relation
with source_end features. As this is exactly the same output of the
standard concantenative synthesis modules, from here on the utterance
can be processed as if the units were from a genuine synthesizer.
</para>
<para>
Copy synthesis itself can be performed by ....
</para>

    <formalpara><title>Require:</title><para>Segment</para>
	</formalpara>
    <formalpara><title>Provide:</title><para>Unit</para>
	</formalpara>


    @param utt: utterance

    @param source_sig: waveform
    @param source_pm: pitchmarks belonging to waveform
    @param source_seg: set of items with end times referring to points 
    in the waveform

*/

void us_get_copy_wave(EST_Utterance &utt, EST_Wave &source_sig, 
		       EST_Track &source_pm, EST_Relation &source_seg);

/** This function produces a waveform from the Unit relation without
prosodic modification. In effect, this function simply concatenates
the waveform parts of the units in the unit relation. An overlap add
operation is performed at unit boundaries so that waveform
discontinuities don't occur.


*/
void us_unit_raw_concat(EST_Utterance &utt);

/** Items in the Unit relation can take an optional
flag<parameter>energy_factor</parameter>, which scales the amplitude
of the unit waveform. This is useful because units often have
different energy levels due to different recording circumstances.  An
<parameter>energy_factor</parameter> of 1.0 leaves the waveform
unchanged.

*/

void us_energy_normalise(EST_Relation &unit);

//@}


/**@name Functions for Producing Mappings

*/ 

//@{

/** This function produces the mapping between the SourceCoef track
and TargetCoef track. The mapping is controlled by two types of
modification, <emph>duration</emph> and <emph>pitch</emph>. </para>

<para>Duration is specified by the <parameter>Segment</parameter>
relation. Each item in this relation has two features
<parameter>source_end</parameter> and
<parameter>target_end</parameter>.<parameter>source_end</parameter> is
marks the end point of that segment in the concatenated set of source
coefficients, while <parameter>target_end</parameter> marks the
desired end of that segment.</para>

<para> Pitch modification is specified by the patterns of pitchmarks
in the <parameter>SourceCoef</parameter> track and
<parameter>TargetCoef</parameter> track. While these tracks actually
represent periods, their reciprocal represents the source and target
F0 contours.
</para><para>

The mapping is an integer array with one element for every pitchmark in
the TargetCoef track. Therefore, every target pitchmark has a mapping
element, and the value of that element is the frame number in the
SourceCoef track which should be used to generate the frame of speech
for that target pitchmark. Depending on the mapping, source frames can
be duplicated or skipped.

</para><para> If the duration is constant, a higher target pitch will
mean source frames are duplicated. If the pitch is constant, a longer
target duration will also mean source frames are duplicated.  The
duration and pitch modifications are calculated at the same time,
leading to a single mapping.  

<formalpara><title>Require:</title><para>SourceCoef, TargetCoef, Segment</para>
</formalpara>

	<formalpara><title>Provide:</title><para>US_Map</para>
	</formalpara>
*/


void us_mapping(EST_Utterance &utt, const EST_String &method);


// for graphical display only:
void map_to_relation(EST_IVector &map, EST_Relation &r, 
		     const EST_Track &source_pm, 
		     const EST_Track &target_pm);
//@}

/**@name Functions for Generating Waveforms

*/ 

//@{

/** Standard waveform generation function. This function genrates the
actual synthetic speech waveform, using information in the SourceCoef,
TargetCoef and US_map relations.
</para><para>

The first stage involves time domain processing, whereby a speech
waveform or residual waveform is generated. The second (optional)
stage passes this waveform through the set of filter coefficients
specified in the TargetCoef track. The output synthetic waveform is
put in the Wave relation.
</para>
<para>
LPC resynthesis  is performed by the <link linkend="lpc-filter-1">lpc_filter_1</link> function.

</para>

    <formalpara><title>Require:</title><para>SourceCoef, TargetCoef,
    US_map</para> </formalpara>
    <formalpara><title>Provide:</title><para>Wave</para>
    </formalpara>

    @param utt: utterance
    @param filter_method: type of filter used - normally "lpc" or none ("")
    @param td_method: type of time domain synthesis.
*/

void us_generate_wave(EST_Utterance &utt, 
		      const EST_String &filter_method,
		      const EST_String &ola_method);

/** This copies coefficients from <parameter>source_coef</parameter>
into <parameter>target_coef</parameter> according to the frame mapping
specified by
<parameter>map</parameter>. <parameter>target_coef</parameter> should
already have been allocated, and the pitchmarks in the time array set
to appropriate values. (this can be done by the <link
linkend="f0-to-pitchmarks">f0_to_pitchmarks</link> function).

*/

void map_coefs(EST_Track &source_coef, EST_Track &target_coef, 
	       EST_IVector &map);

/** Time domain resynthesis.

Generate a speech waveform by copying frames into a set of time
positions given by target_pm. The frame used for each time position is
given by map, and the frames themselves are stored individually as
waveforms in frames.
</para>

@param target_sig: output waveform
@param target_pm: new pitchmark positions
@param frames: array containing waveforms, each representing a single analysis
     frame
@param map: mapping between target_pm and frames.

*/

void td_synthesis(EST_WaveVector &frames,
		  EST_Track &target_pm, EST_Wave &target_sig,
		  EST_IVector &map);


/** Variant of td_synthesis, where each frame is re-windowed according to the 
size of the local synthesis pitch period.
</para>
@param target_sig: output waveform
@param target_pm: new pitchmark positions
@param frames: array containing waveforms, each representing a single analysis
     frame
@param map: mapping between target_pm and frames.

*/

void td_synthesis2(EST_WaveVector &frames,
		   EST_Track &target_pm, EST_Wave &target_sig,
		   EST_IVector &map);

//@}


void asymmetric_window_td_synthesis(EST_WaveVector &frames,
				    EST_Track &target_pm, 
				    EST_Wave &target_sig,
				    EST_IVector &map,
				    EST_IVector &frame_pm_indices);


/**@name Pitchmark Functions

*/
//@{

/** This function generates the target pitchmarks from the target F0
contour. The pitchmarks are generated by reading a value, \(f_{0}\)
off the f0 contour at time \(t\), calculating the local pitch period
\(\tau = 1/f_{0}\), and placing a pitchmark at time \(T + t\). The
process is then repeated by reading the F0 value at this new point and
so on. </para> 

<para> The F0 contour must be continuous in all regions, that is
unvoiced regions must have pseudo f0 values also. Although artificial
contours are best generated in this way to begin with, the function
\ref{**} can be used to interpolate through unvoiced regions for
non-continuous contours.
</para> 

<para> As the last F0 value in the contour may not be the end of the
utterance (for example if the last phone is unvoiced), the pitchmarks may be extended past the end of the contour.

</para> <para>

After processing, the generated track only contains the target
pitchmarks, but later functions may fill the amplitude array of the
track with target coefficients, and hence the space for these can be
allocated at this stage.

</para>
@param fz: input F0 contour.

@param pm: set of pitchmarks to be generated. These are set to the
correct size in the function.

@param num_channels: (optional) number of coefficients used in further
processing.

@param default_f0: (optional) f0 value for interpolated end values

@param target_end: (optional) fill from the end of the contour to this
point with default f0 values.

*/
void f0_to_pitchmarks(EST_Track &fz, EST_Track &pm, int num_channels=0,
		      float default_f0=100.0, float target_end=-1);



/** This is a utility function for converting a set of pitchmarks back
to an F0 contour and is usually used in system development etc.  The
generated F0 is evenly spaced.

@param pm: input set of pitchmarks to be generated

@param fz: otuput F0 contour.

@param shift: frame shift of generated contour in seconds.
*/

void pitchmarks_to_f0(EST_Track &pm, EST_Track &fz, float shift);

//@}

void register_unisyn_features(void);
    
#endif // __UNISYN_H__

