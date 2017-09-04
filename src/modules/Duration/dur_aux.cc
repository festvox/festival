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
/* Basic duration utilities common between different methods             */
/*                                                                       */
/*=======================================================================*/
#include <cstdio>
#include "festival.h"
#include "durationP.h"

float dur_get_stretch(void)
{
    LISP lstretch = ft_get_param("Duration_Stretch");
    float stretch;

    if (lstretch == NIL)
	stretch = 1.0;
    else
	stretch = get_c_float(lstretch);
    if (stretch < 0.1)
    {
	cerr << "Duration_Stretch: is too small (" << stretch <<
	    ") ingnoring it\n";
	stretch = 1.0;
    }
    
    return stretch;
}

float dur_get_stretch_at_seg(EST_Item *s)
{
    float global_stretch = dur_get_stretch();
    EST_Item *nn = parent(parent(parent(s,"SylStructure")),"Token");
    EST_Item *syl = parent(s,"SylStructure");  
    float local_stretch = 0.0;
    float syl_stretch = 0.0;  
    float seg_stretch = 0.0;  
    float stretch = 1.0;      

    if (nn)
      local_stretch = ffeature(nn,"dur_stretch").Float();
    if (syl)
      syl_stretch = ffeature(syl,"dur_stretch").Float(); 
    seg_stretch = ffeature(s,"dur_stretch").Float();    
    if (local_stretch != 0.0)
      stretch *= local_stretch; 
    if (syl_stretch != 0.0)
      stretch *= syl_stretch;
    if (seg_stretch != 0.0)
      stretch *= seg_stretch;

    return stretch*global_stretch;

}

void festival_Duration_init(void)
{
    festival_def_utt_module("Duration_Averages",FT_Duration_Ave_Utt,
    "(Duration_Averages UTT)\n\
  Label all segments with their average duration found from the assoc\n\
  list of phone names to averages in phoneme_durations.  This module is\n\
  called through the module Duration when the Parameter Duration_Method\n\
  is set to Averages. [see Average durations]");
    festival_def_utt_module("Duration_Default",FT_Duration_Def_Utt,
    "(Duration_Default UTT)\n\
  Label all segments with a fixed duration of 100ms.  This module is\n\
  called through the module Duration when the Parameter Duration_Method\n\
  is unset or set to Default. [see Default durations]");
    festival_def_utt_module("Duration_Tree_ZScores",
			    FT_Duration_Tree_ZScores_Utt,
    "(Duration_Tree_ZScores UTT)\n\
  Uses the CART tree in duration_cart_tree to predict z scores duration\n\
  values for each segment in UTT.  The z scores are converted back to\n\
  absolute values by the assoc list of phones to means and standard\n\
  deviations in the variable duration_ph_info.  This module is called\n\
  through the module Duration when the Parameter Duration_Method is set\n\
  to Tree_ZScores.  This method modifies its predicted durations by the\n\
  factor set in the Parameter Duration_Stretch (if set).\n\
  [see CART durations]");
    festival_def_utt_module("Duration_Tree",FT_Duration_Tree_Utt,
    "(Duration_Tree UTT)\n\
  Uses the CART tree in duration_cart_tree to predict absolute durations\n\
  for each segment in UTT. This module is called through the module\n\
  Duration when the Parameter Duration_Method is set to Tree.  This\n\
  method modifies its predicted durations by the factor set in the\n\
  Parameter Duration_Stretch (if set). [see CART durations]");
    festival_def_utt_module("Duration_Klatt",FT_Duration_Klatt_Utt,
    "(Duration_Klatt UTT)\n\
  This uses an implementation of the Klatt Duration rules to predict\n\
  durations for each segment in UTT.  It uses the information in\n\
  duration_klatt_params for mean and lower bound for each phone. This\n\
  module is called through the module Duration when the Parameter\n\
  Duration_Method is set to Klatt.  This method modifies its predicted \n\
  durations by the factor set in the Parameter Duration_Stretch (if set).\n\
  [see Klatt durations]");

}
