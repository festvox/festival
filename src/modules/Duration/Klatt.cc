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
/*                      Author :  Paul Taylor                            */
/*                      Date   :  July 1995                              */
/*-----------------------------------------------------------------------*/
/*                                                                       */
/*              Klatt Duration Rules                                     */
/*                                                                       */
/*=======================================================================*/

/*
This is an implementation of the Klatt rule system as described in
chapter 9 of "From text to speech: The MITalk system", Allen,
Hunnicutt and Klatt.

The function klatt_seg_dur() calculates a duration for each
segment in the input. It does this by calling a number
of rules (named 1 to 11) as defined in the MITalk book. Most
rules return a number which modifies the inherenent duration of
each segment. The original rules are set up so as to return
a percentage, here the system retursn a floating point value
which I think is neater.
*/

#include <cstdio>
#include "festival.h"
#include "durationP.h"

static void klatt_dur_debug(EST_Item *s);

static float rule2(EST_Item *seg);
static float rule3(EST_Item *seg);
static float rule4(EST_Item *seg);
static float rule5(EST_Item *seg);
static float rule6(EST_Item *seg);
static float rule7(EST_Item *seg);
static float rule8(EST_Item *seg);
static float rule9(EST_Item *seg);
static float rule10(EST_Item *seg);
static float rule9a(EST_Item *seg);
static float sub_rule9a(const EST_String &ph);

static int klatt_seg_dur(EST_Item *seg);
static float min_dur(EST_Item *s_seg);
static float inher_dur(EST_Item *s_seg);

int onset(EST_Item *seg);

static LISP klatt_params = NIL;
static int debug = 0;

LISP FT_Duration_Klatt_Utt(LISP utt)
{
    // Predict fixed duration on segments
    EST_Utterance *u = get_c_utt(utt);
    EST_Item *s;

    *cdebug << "Duration Klatt module\n";

    klatt_params = siod_get_lval("duration_klatt_params",
				 "no klatt duration params");

    for (s=u->relation("Segment")->first(); s != 0; s = inext(s))
	klatt_seg_dur(s);

    return utt;
}

static int klatt_seg_dur(EST_Item *seg)
{
    float min;
    float fact = 1.0;
    float start, dur;
    float duration_speed = dur_get_stretch_at_seg(seg);

    start = ffeature(seg,"segment_start");

    if (ph_is_silence(seg->name()))
	dur = 0.250 * duration_speed;
    else
    {
	if (debug) klatt_dur_debug(seg);
	fact *= rule2(seg) * rule3(seg) * rule4(seg) * rule5(seg) 
	    * rule6(seg) * rule7(seg) * rule8(seg) * 
		rule9(seg) * rule10(seg);

	min = (rule7(seg) != 1.0) ? min_dur(seg)/2: min_dur(seg);

	dur = ((((inher_dur(seg) - min) * fact) + min) / 1000.0) 
	    * duration_speed;
    }

    seg->set("end",start + dur);

    return 0;
}

static float min_dur(EST_Item *seg)
{
    LISP p = siod_assoc_str(seg->name(),klatt_params);
    
    if (p == NIL)
    {
	cerr << "Klatt_Duration: no minimum duration for \"" << seg->name()
	    << "\"\n";
	festival_error();
    }

    return get_c_float(car(cdr(cdr(p))));
}

static float inher_dur(EST_Item *seg)
{
    LISP p = siod_assoc_str(seg->name(),klatt_params);
    
    if (p == NIL)
    {
	cerr << "Klatt_Duration: no minimum duration for \"" << seg->name()
	    << "\"\n";
	festival_error();
    }

    return get_c_float(car(cdr(p)));
}

static int word_final(EST_Item *seg)
{
    // True if this segment is the last in a word
    EST_Item *nn = seg->as_relation("SylStructure");

    if (inext(nn) || (inext(parent(nn))))
	return FALSE;
    else
	return TRUE;
}

static int syl_final(EST_Item *seg)
{
    // True if this segment is the last in a syllable
    EST_Item *nn = seg->as_relation("SylStructure");

    if (inext(nn))
	return FALSE;
    else
	return TRUE;
}

static int word_initial(EST_Item *seg)
{
    // True if this segment is the first in a word
    EST_Item *nn = seg->as_relation("SylStructure");

    if (iprev(nn) || iprev(parent(nn)))
	return FALSE;
    else
	return TRUE;
}

static int phrase_initial(EST_Item *seg)
{
    // True if this segment is the first in a phrase

    if (word_initial(seg))
    {
	EST_Item *nn = parent(parent(seg,"SylStructure"));
	if (iprev(as(nn,"Phrase")))
	    return FALSE;
	else
	    return TRUE;
    }
    return
	FALSE;
}

int onset(EST_Item *seg)
{
    if (ffeature(seg,"onsetcoda") == "onset")
	return 1;
    else 
	return 0;
}

int coda(EST_Item *seg)
{
    if (ffeature(seg,"onsetcoda") == "coda")
	return 1;
    else 
	return 0;
}

static float rule2(EST_Item *seg)
{   // clause final lengthening

    if (coda(seg))
    {
	int b = ffeature(seg,"R:SylStructure.parent.syl_break");
	if ((b > 1) && (b < 4))
	    return 1.4;
    }
    return 1.0;

}

static float rule3(EST_Item *seg)
{   // Non-phrase-final shortening 
    // syllabic segments are shortened by 60 if not in a phrase-final syllable
    int b = ffeature(seg,"R:SylStructure.parent.syl_break");

    if ((b < 2) && ph_is_syllabic(seg->name()))
	return 0.6;

    // A phrase-final postvocalic liquid or nasal is lengthened by 140
    if ((b == 4) && (ph_is_liquid(seg->name()) || ph_is_nasal(seg->name())))
	return(1.4);

    return 1.0;
}

static float rule4(EST_Item *seg) 
{   // Non-word-final shortening
    int b = ffeature(seg,"R:SylStructure.parent.syl_break");

    // Syllabic segments are shortened by 85 if not in a word-final syllable
    if ((b == 0) && ph_is_syllabic(seg->name()))
	return(0.85);

    return 1.0;
}

static float rule5(EST_Item *seg)
{   // Polysyllabic Shortening
    int num_syls = ffeature(seg,"R:SylStructure.parent.parent.num_syls");

    // Syllabic segments in a polysyllabic word are shortened by 80.
    if ((num_syls > 1) && ph_is_syllabic(seg->name()))
	return 0.8;

    return 1.0;
}

static float rule6(EST_Item *seg)
{   // Non-initial-consonant shortening

    if (!word_initial(seg) && (ph_is_consonant(seg->name())))
	return 0.85;

    return 1.0;
}

static float rule7(EST_Item *seg)
{   // Unstressed shortening

    if (ffeature(seg,"R:SylStructure.parent.stress") == 1)
	return 1.0;

    if (ph_is_syllabic(seg->name()))
    {
	if (word_initial(seg) || word_final(seg))
	    return 0.7;
	else
	    return 0.5;
    }

    if (onset(seg) && ph_is_liquid(seg->name())) // or glide...
	return 0.1;
    
    return 0.7;
}

// Lengthening for emphasis
static float rule8(EST_Item *seg) 
{ 

    if (!ph_is_vowel(seg->name()))
	return  1.0;

    if (ffeature(seg,"R:SylStructure.parent.accented") == 1)
	return 1.4;

    return 1.0;
}

// this is really rule 9b, but its eaiser to make it call rule 9a

static float rule9(EST_Item *seg) 
{   // Postvocalic context of vowels */
    int b = ffeature(seg,"R:SylStructure.parent.syl_break");
    
    if (b > 1)
	return (0.7 + (0.3 * rule9a(seg)));
    else
	return rule9a(seg);
}


static float rule9a(EST_Item *seg)
{   // Postvocalic context of vowels 
    EST_Item *s_next,*s_next_next;

    if (ph_is_vowel(seg->name()))
    {
	if (syl_final(seg))
	    return 1.2;
	s_next = inext(seg);
	if ((s_next) && (syl_final(s_next)))
	    return sub_rule9a(s_next->name());
	s_next_next = inext(s_next);
	if ((ph_is_sonorant(s_next->name())) &&
	    (s_next_next) &&
	    (ph_is_obstruent(s_next_next->name())))
	    return sub_rule9a(s_next_next->name());
    }
    else if (onset(seg))
	return 1.0;
    else if (ph_is_sonorant(seg->name()))
    {
	if (syl_final(seg))
	    return 1.2;
	s_next = inext(seg);
	if (ph_is_obstruent(s_next->name()))
	    return sub_rule9a(s_next->name());
    }

    return 1.0;
}

// sub rule, independent of seg position
static float sub_rule9a(const EST_String &ph)
{
    if (ph_is_voiced(ph))
    {
	if (ph_is_fricative(ph))
	    return 1.6;
	else if (ph_is_stop(ph))
	    return 1.2;
	else if (ph_is_nasal(ph))
	    return  0.85;
	else 
	    return 1.0;
    }
    else if (ph_is_stop(ph))
	return 0.7;
    else
	return 1.0;
}

// Shortening in clusters

static float rule10(EST_Item *seg) 
{
    int b = ffeature(seg,"R:SylStructure.parent.syl_break");

    if (syl_final(seg) && (b > 1))
	return 1.0;
    else
    {
	if (ph_is_vowel(seg->name()))
	{
	    if (ph_is_vowel(inext(seg)->name()))
		return 1.20;
	    else if ((!phrase_initial(seg)) &&
		     (ph_is_vowel(iprev(seg)->name())))
		return 0.70;
	    else
		return 1.0;
	}
	else if (ph_is_consonant(inext(seg)->name()))
	    if (!phrase_initial(seg) &&
		(ph_is_consonant(iprev(seg)->name())))
		return 0.5;
	    else
		return 0.7;
	else if (!phrase_initial(seg) &&
                 (ph_is_consonant(iprev(seg)->name())))
	    return 0.7;
    }

    return 1.0;
}


static void klatt_dur_debug(EST_Item *seg)
{
    float f;
    if ((f = rule2(seg))!= 1.0) cout << "Fired rule  2 " << f << endl; 
    if ((f = rule3(seg))!= 1.0) cout << "Fired rule  3 " << f << endl;
    if ((f = rule4(seg))!= 1.0) cout << "Fired rule  4 " << f << endl;
    if ((f = rule5(seg))!= 1.0) cout << "Fired rule  5 " << f << endl;
    if ((f = rule6(seg))!= 1.0) cout << "Fired rule  6 " << f << endl;
    if ((f = rule7(seg))!= 1.0) cout << "Fired rule  7 " << f << endl;
    if ((f = rule8(seg))!= 1.0) cout << "Fired rule  8 " << f << endl;
    if ((f = rule9(seg))!= 1.0) cout << "Fired rule  9 " << f << endl;
    if ((f = rule10(seg))!= 1.0) cout << "Fired rule 10" << f << endl;

    return;
}


