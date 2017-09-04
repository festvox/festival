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
/*                      Date   :  May 1998                               */
/*-----------------------------------------------------------------------*/
/*                                                                       */
/* Basic builtin features                                                */
/*                                                                       */
/*=======================================================================*/
#include <cstdio>
#include "festival.h"
#include "modules.h"

static EST_String stressname("stress");
static EST_Val val_string0("0");
static EST_Val val_string1("1");
static EST_Val val_int0(0);
static EST_Val val_int1(1);
static EST_Val default_val_float(0.0);

static EST_Val ff_addr(EST_Item *i)
{
    char a[1024];

    // The address of the contents so that the same item from different views
    // have the same address
    sprintf(a,"%p",i->contents());  
    return EST_Val(a);
}

static EST_Val ff_segment_duration(EST_Item *s)
{
    EST_Item *n = as(s,"Segment");
    if (n == 0)
    {
	cerr << "Asked for segment duration of item not in Segment relation."
	    << endl;
	festival_error();
    }
    if (iprev(n) == 0)
	return EST_Val(s->F("end", 0));
    else
	return EST_Val(s->F("end", 0)-(iprev(n)->F("end",0)));
}

static EST_Val ff_syllable_duration(EST_Item *s)
{
    EST_Item *n = as(s,"SylStructure");
    if (n == 0)
    {
	cerr << "Asked for syllable duration of item not in SylStructure relation."
	     << endl;
	festival_error();
    }
    else
    {
	EST_Item *fd = daughter1(n);
	EST_Item *ld = last(fd);

	if (ld == 0)
	    return val_int0;
	EST_Item *ps = iprev(as(fd,"Segment"));
	if (ps == 0)
	    return ld->F("end",0);
	else
	    return EST_Val(ld->F("end",0)-ps->F("end",0));
    }

    // dummy for stupid VC++ compiler
    {
	EST_Val junk;
	return junk;
    }
}

static EST_Val ff_word_duration(EST_Item *s)
{
    EST_Item *n = as(s,"SylStructure");
    if (n == 0)
    {
	cerr << "Asked for word duration of item not in SylStructure relation."
	    << endl;
	festival_error();
    }
    else
    {
	EST_Item *fd = daughter1(daughter1(n));
	EST_Item *ld = daughtern(daughtern(n));

	if (ld == 0)
	    return val_int0;
	EST_Item *ps = iprev(as(fd,"Segment"));
	if (ps == 0)
	    return ld->F("end",0);
	else
	    return EST_Val(ld->F("end",0)-ps->F("end",0));
    }
    // dummy for stupid VC++ compiler
    {
      EST_Val junk;
      return junk;
    }
}

static EST_Val ff_seg_start(EST_Item *s)
{
    EST_Item *n = as(s,"Segment");
    if (iprev(n) == 0)
	return default_val_float;
    else
	return iprev(n)->F("end",0);
}

static EST_Val ff_syl_start(EST_Item *s)
{
    EST_Item *n = as(s,"SylStructure");
    if (daughter1(n) == 0)
	return default_val_float;
    else
	return ff_seg_start(daughter1(n));
}

static EST_Val ff_word_start(EST_Item *s)
{
    EST_Item *n = as(s,"SylStructure");
    if (daughter1(daughter1(n)) == 0)
	return default_val_float;
    else
	return ff_seg_start(daughter1(daughter1(n)));
}

static EST_Val ff_seg_end(EST_Item *s)
{
    return s->F("end",0);
}

static EST_Val ff_seg_mid(EST_Item *s)
{
    return EST_Val(((float)ff_seg_start(s)+(float)ff_seg_end(s))/2.0);
}

static EST_Val ff_syl_end(EST_Item *s)
{
    EST_Item *n = as(s,"SylStructure");
    if (daughtern(n) == 0)
	return default_val_float;
    else
	return ff_seg_end(daughtern(n));
}

static EST_Val ff_word_end(EST_Item *s)
{
    EST_Item *n = as(s,"SylStructure");
    if (daughtern(n) == 0)
	return default_val_float;
    else
	return ff_syl_end(daughtern(n));
}

static EST_Val ff_position_type(EST_Item *s)
{
    /* Position of syllable in this word: initial, mod, final */
    EST_Item *nn = as(s,"SylStructure");

    if (nn == 0)  // its not really a syllable
	return EST_Val("single");
    else if (inext(nn) == 0)
    {
	if (iprev(nn) == 0)
	    return EST_Val("single");
	else
	    return EST_Val("final");
    }
    else if (iprev(nn) == 0)
	return EST_Val("initial");
    else
	return EST_Val("mid");
}

static EST_Val ff_word_break(EST_Item *w)
{
    /* Break index of word */
    EST_Item *nn = as(w,"Phrase");
    static EST_Val val4 = EST_Val(4);
    static EST_Val val3 = EST_Val(3);
    static EST_Val val2 = EST_Val(2);

    if ((nn == 0) || (inext(nn) != 0))
	return val_int1;
    else 
    {
	EST_Item *p = parent(nn);
	if (p)
	{
	    if (p->name() == "BB")
		return val4;
	    else if (p->name() == "B")
		return val3;
	    else if (p->name() == "mB")
		return val2;
	    else
		return EST_Val(p->name());

	}
	else
	    return val_int1;
    }
}

static EST_Val ff_syl_break(EST_Item *s)
{
    // 0 internal syl end, 1 word end, 4 phrase end (ToBI 3 and 4)
    EST_Item *nn = as(s,"SylStructure");

    if (nn == 0)
	return val_int1;  // no sylstructure so maybe its standalone
    else if (inext(nn) != 0)  // word internal
	return val_int0;
    else if (parent(nn) == 0)  // not in a word -- strange
	return val_int1;
    else
	return ff_word_break(parent(nn));  // take it from the word
}

static EST_Val ff_old_syl_break(EST_Item *s)
{
    // 0 internal syl end, 1 word end, 4 phrase end (ToBI 3 and 4)
    // 2's and threes are promoted to 4s
    EST_Item *nn = as(s,"SylStructure");
    static EST_Val val4 = EST_Val(4);

    if (nn == 0)
	return val_int1;  // no sylstructure so maybe its standalone
    else if (inext(nn) != 0)  // word internal
	return val_int0;
    else if (parent(nn) == 0)  // not in a word -- strange
	return val_int1;
    else
    {
	EST_Val v = ff_word_break(parent(nn));
	if ((v == 3) || (v == 2))
	    return val4;
	else 
	    return v;
    }
}

static EST_Val ff_syl_accented(EST_Item *s)
{
    // t if syllable is accented or not 
    EST_Item *nn = as(s,"Intonation");

    if ((nn == 0) || (daughter1(nn) == 0))
	return val_int0;
    else
	return val_int1;
}

static EST_Val ff_tobi_accent(EST_Item *s)
{
    // First tobi accent related to syllable
    EST_Item *nn = as(s,"Intonation");
    EST_Item *p;

    for (p=daughter1(nn); p; p=inext(p))
	if (p->name().contains("*"))
	    return EST_Val(p->name());
    return EST_Val("NONE");
}

static EST_Val ff_tobi_endtone(EST_Item *s)
{
    // First tobi endtone (phrase accent or boundary tone)
    EST_Item *nn = as(s,"Intonation");
    EST_Item *p;

    for (p=daughter1(nn); p; p=inext(p))
    {
	EST_String l = p->name();
	if ((l.contains("%")) || (l.contains("-")))
	    return EST_Val(p->name());
    }

    return EST_Val("NONE");
}

static EST_Val ff_syl_accent(EST_Item *s)
{
    // (first) accent or NONE on given syllable
    EST_Item *nn = as(s,"Intonation");

    if (daughter2(nn))
	return EST_Val("multi");
    else if (daughter1(nn))
	return EST_Val(daughter1(nn)->name());
    else
	return EST_Val("NONE");
}

static EST_Val ff_syl_numphones(EST_Item *s)
{
    // Number of phones in syllable
    EST_Item *nn = as(s,"SylStructure");

    return EST_Val(daughter1(nn)->length());
}

static EST_Val ff_word_numsyls(EST_Item *s)
{
    // Number of syllable in word
    EST_Item *nn = as(s,"SylStructure");

    return EST_Val(daughter1(nn)->length());
}

static EST_Val ff_syl_onsetsize(EST_Item *s)
{
    // number of segments in the onset
    EST_Item *nn = as(s,"SylStructure");
    EST_Item *p;
    int size;

    for (p=daughter1(nn),size=0; p; p=inext(p),size++)
	if (ph_is_vowel(p->name()))
	    return EST_Val(size);
    
    return EST_Val(size);

}

static EST_Val ff_syl_vowel(EST_Item *s)
{
    // the vowel in the syllable
    EST_Item *nn = as(s,"SylStructure");
    EST_Item *p;
    int size;

    for (p=daughter1(nn),size=0; p; p=inext(p),size++)
	if (ph_is_vowel(p->name()))
	    return EST_Val(p->name());

    // no vowel 
    return EST_Val("novowel");
}

static EST_Val ff_syl_codasize(EST_Item *s)
{
    // number of segments in the coda
    EST_Item *nn = as(s,"SylStructure");
    EST_Item *p;
    int size;

    for (p=daughtern(nn),size=1; p; p=iprev(p),size++)
	if (ph_is_vowel(p->name()))
	    return EST_Val(size);
    
    return EST_Val(size);
}

static EST_Val ff_syl_pc_unvox(EST_Item *s)
{
    // Returns percentage of syllable from start to first voiced phone
    EST_Item *nn = as(s,"SylStructure");
    EST_Item *p,*ps;
    float unvox,start = 0;

    if (daughter1(nn) == 0)
	return val_int0;  // no segments in syllable
    else if ((ps = iprev(as(daughter1(nn),"Segment"))) != 0)
	start = ps->F("end",0);
    unvox = start;

    for (p=daughter1(nn); p != 0; p=inext(p))
    {
	if ((ph_is_vowel(p->name())) ||
	    (ph_is_voiced(p->name())))
	    break;
	unvox = p->F("end",0);
    }

    return EST_Val((int)(((unvox-start)*100)/
			 (daughtern(nn)->F("end",0)-start)));
}

static EST_Val ff_syl_vowel_start(EST_Item *s)
{
    // Returns start time of vowel in syllable (or start of syllable)
    EST_Item *nn = as(s,"SylStructure");
    EST_Item *p;

    for (p=daughter1(nn); p != 0; p=inext(p))
    {
	if (ph_is_vowel(p->name()))
	    return EST_Val(ff_seg_start(p));
    }
    // There isn't a vowel, so just take start of syl
    return EST_Val(ff_syl_start(p));
}

static EST_Val ff_seg_onsetcoda(EST_Item *s)
{
    // onset if seg in onset, coda otherwise (vowel is in coda)
    EST_Item *nn = as(s,"SylStructure");
    EST_Item *p;

    for (p=inext(nn); p; p=inext(p))
	if (ph_is_vowel(p->name()))
	    return EST_Val("onset");
    return EST_Val("coda");
}

static EST_Val ff_seg_onset_stop(EST_Item *s)
{
    // 1 if onset of the syllable attached to this segment has a stop
    EST_Item *nn = first(as(s,"SylStructure"));

    for ( ; nn ; nn=inext(nn))
    {
	if (ph_is_vowel(nn->name()))
	    return val_string0;
	if (ph_is_stop(nn->name()))
	    return val_string1;
    }
    return val_string0;
}

static EST_Val ff_seg_coda_fric(EST_Item *s)
{
    // 1 if coda of the syllable attached to this segment has a fricative
    EST_Item *nn = last(as(s,"SylStructure"));

    for ( ; nn ; nn=iprev(nn))
    {
	if (ph_is_vowel(nn->name()))
	    return val_string0;
	if (ph_is_fricative(nn->name()))
	    return val_string1;
    }
    return val_string0;
}

static EST_Val ff_seg_pos_in_syl(EST_Item *s)
{
    // position of segment in syllable
    EST_Item *nn = as(s,"SylStructure");
    EST_Item *p;
    int pos=0;

    for (p=first(nn); p; p=inext(p),pos++)
	if (p == nn)
	    return EST_Val(pos);
    // don't think you can get here
    return EST_Val(pos);
}

static EST_Val ff_seg_syl_initial(EST_Item *s)
{
    // 1 if seg is syllable initial, 0 otherwise.
    EST_Item *nn = as(s,"SylStructure");

    if (iprev(nn) == 0)
	return val_string1;
    else
	return val_string0;
}

static EST_Val ff_seg_syl_final(EST_Item *s)
{
    // 1 if seg is syllable initial, 0 otherwise.
    EST_Item *nn = as(s,"SylStructure");

    if (inext(nn) == 0)
	return val_string1;
    else
	return val_string0;
}

static EST_Val ff_syl_pos_in_word(EST_Item *s)
{
    // position of syllable in word
    EST_Item *nn = as(s,"SylStructure");
    EST_Item *p;
    int pos=0;

    for (p=first(nn); p; p=inext(p),pos++)
	if (p == nn)
	    return EST_Val(pos);
    // don't think you can get here
    return EST_Val(pos);
}

static EST_Val ff_pos_in_phrase(EST_Item *s)
{
    // position of word in phrase
    EST_Item *nn = as(s,"Phrase");
    EST_Item *p;
    int pos=0;

    for (p=first(nn); p; p=inext(p),pos++)
	if (p == nn)
	    return EST_Val(pos);
    // don't think you can get here
    return EST_Val(pos);
}

static EST_Val ff_num_break(EST_Item *s)
{
    // 1 if this word is at the end of a number group and followed by
    // a new number group
    EST_Item *nn = as(s,"Token");

    if ((inext(nn) == 0) &&
	(parent(nn)->name().matches(RXdouble)) &&
	(inext(parent(nn))->name().matches(RXdouble)))
	return val_string1;
    else
	return val_string0;
}

static EST_Val ff_words_out(EST_Item *s)
{
    return EST_Val(as(s,"Phrase")->length());
}

static EST_Val ff_syl_midpitch(EST_Item *s)
{
    // pitch of mid vowel in syllable
    EST_Item *nn = as(s,"SylStructure");
    EST_Item *p;

    for (p=daughter1(nn); p; p = inext(p))
    {
	if (ph_is_vowel(p->name()))
	    return ffeature(p,"R:Target.daughter1.f0");
    }
    // must be a silence or a syllabic consonant
    return default_val_float;
}

static EST_Val ff_syl_startpitch(EST_Item *s)
{
    // pitch at start of syllable 
    // average of first segment and previous segment target (if exists)

    float pt = ffeature(s,"R:SylStructure.daughter1.R:Segment.p.R:Target.daughter1.f0");
    float tt = ffeature(s,"R:SylStructure.daughter1.R:Segment.R:Target.daughter1.f0");
	
    if (pt < 0.1)
 	return EST_Val(tt);
    else if (tt < 0.1)
	return EST_Val(pt);
    else
	return EST_Val((tt+pt)/2.0);
}

static EST_Val ff_syl_endpitch(EST_Item *s)
{
    // pitch at start of syllable 
    // average of first segment and previous segment target (if exists)

    float nt = ffeature(s,"R:SylStructure.daughtern.R:Segment.n.R:Target.daughter1.f0");
    float tt = ffeature(s,"R:SylStructure.daughtern.R:Segment.R:Target.daughter1.f0");
	
    if (nt < 0.1)
 	return EST_Val(tt);
    else if (tt < 0.1)
	return EST_Val(nt);
    else
	return EST_Val((tt+nt)/2.0);
}

static EST_Val ff_seg_pitch(EST_Item *s)
{
    // Return interpolated pitch at mid-point of s
    EST_Item *t,*lastt;
    float spoint,deltaf0,deltatime;
    float smid = ff_seg_mid(s);
    EST_Utterance *u = get_utt(s);

    for (lastt=t=first_leaf(u->relation("Target")->first()); 
	 next_leaf(t) != 0; t=next_leaf(t))
    {
	if (smid <= t->F("pos",0))
	    break;
	lastt=t;
    }

    if (lastt == 0)
	return EST_Val((float)0.0);

    deltaf0 = t->F("f0",0)-lastt->F("f0",0);
    deltatime = t->F("pos",0) - lastt->F("pos",0);
    if (deltatime <= 0)
	spoint = lastt->F("f0",0);
    else
	spoint = lastt->F("f0",0) + 
	    (deltaf0*((smid-lastt->F("pos",0))/deltatime));

    if (spoint > 35)
	return EST_Val(spoint);
    else
	return EST_Val((float)0.0);
}

static EST_Val ff_syl_in(EST_Item *s)
{
    // Number of syllables to since last phrase break 
    EST_Item *nn = as(s,"Syllable");
    // The first syllable in the phrase
    EST_Item *fsyl = 
	as(daughter1(first(as(parent(s,"SylStructure"),"Phrase")),"SylStructure"),
	   "Syllable");
    EST_Item *p;
    int count;

    for (count=0,p=nn; p != 0; p=iprev(p),count++)
	if (p == fsyl)
	    return EST_Val(count);
    return EST_Val(count);
}

static EST_Val ff_syl_out(EST_Item *s)
{
    // Number of syllables since last phrase break 
    EST_Item *nn = as(s,"Syllable");
    // The last syllable in the phrase
    EST_Item *lsyl = 
	as(daughtern(last(as(parent(s,"SylStructure"),"Phrase")),"SylStructure"),
	   "Syllable");
    EST_Item *p;
    int count;

    for (count=0,p=nn; p != 0; p=inext(p),count++)
	if (p == lsyl)
	    return EST_Val(count);
    return EST_Val(count);
}

static EST_Val ff_ssyl_in(EST_Item *s)
{
    // Number of stressed syllables since last phrase break 
    EST_Item *nn = as(s,"Syllable");
    EST_Item *fsyl = 
	as(daughter1(first(as(parent(s,"SylStructure"),"Phrase")),"SylStructure"),
	   "Syllable");
    EST_Item *p;
    int count;

    if (nn == fsyl) return val_int0;
    for (count=0,p=iprev(nn); (p != 0) && (p != fsyl); p = iprev(p))
	if (p->F(stressname,0) == 1)
	    count ++;
    return EST_Val(count);
}

// This function is modified version of ff_ssyl_in().
// If first syllable in the current phrase is stressed syllable,
// ff_ssyl_in() does not count it as stressed syllable.
// In the following function, this problem is fixed.
static EST_Val ff_ssyl_in_modified_version(EST_Item *s)
{
    // Number of stressed syllables since last phrase break 
    EST_Item *nn = as(s,"Syllable");
    EST_Item *fsyl = 
	as(daughter1(first(as(parent(s,"SylStructure"),"Phrase")),"SylStructure"),
	   "Syllable");
    EST_Item *p;
    int count;

    if (nn == fsyl) return val_int0;
    for (count=0,p=iprev(nn); (p != 0); p = iprev(p))
    {
	if (p->F(stressname,0) == 1)
	    count ++;
	if (p == fsyl) break;
    }
    return EST_Val(count);
}

static EST_Val ff_ssyl_out(EST_Item *s)
{
    // Number of stressed syllables to next phrase break 
    EST_Item *nn = as(s,"Syllable");
    // The last syllable in the phrase
    EST_Item *lsyl = 
	as(daughtern(last(as(parent(s,"SylStructure"),"Phrase")),"SylStructure"),
	   "Syllable");
    EST_Item *p;
    int count;

    if (nn == lsyl) return val_int0;
    for (count=0,p=inext(nn); (p != 0); p=inext(p))
    {
	if (p->F(stressname,0) == 1)
	    count ++;
	if (p == lsyl) break;
    }
    return EST_Val(count);
}

static EST_Val ff_asyl_in(EST_Item *s)
{
    // Number of accented syllables since last phrase break 
    EST_Item *nn = as(s,"Syllable");
    // The first syllable in the phrase
    EST_Item *fsyl = 
	as(daughter1(first(as(parent(s,"SylStructure"),"Phrase")),"SylStructure"),
	   "Syllable");
    EST_Item *p;
    int count;

    if (nn == fsyl) return val_int0;
    for (count=0,p=iprev(nn); (p != 0) && (p != fsyl); p = iprev(p))
	if (ff_syl_accented(p) == 1)
	    count ++;
    return EST_Val(count);
}

// This function is modified version of ff_asyl_in().
// If first syllable in the current phrase is accented syllable,
// ff_asyl_in() does not count it as accented syllable.
// In the following function, this problem is fixed.
static EST_Val ff_asyl_in_modified_version(EST_Item *s)
{
    // Number of accented syllables since last phrase break 
    EST_Item *nn = as(s,"Syllable");
    // The first syllable in the phrase
    EST_Item *fsyl = 
	as(daughter1(first(as(parent(s,"SylStructure"),"Phrase")),"SylStructure"),
	   "Syllable");
    EST_Item *p;
    int count;

    if (nn == fsyl) return val_int0;
    for (count=0,p=iprev(nn); (p != 0); p = iprev(p))
    {
	if (ff_syl_accented(p) == 1)
	    count ++;
	if (p == fsyl) break;
    }
    return EST_Val(count);
}

static EST_Val ff_asyl_out(EST_Item *s)
{
    // Number of accented syllables to next phrase break 
    EST_Item *nn = as(s,"Syllable");
    // The last syllable in the phrase
    EST_Item *lsyl = 
	as(daughtern(last(as(parent(s,"SylStructure"),"Phrase")),"SylStructure"),
	   "Syllable");
    EST_Item *p;
    int count;

    if (nn == lsyl) return val_int0;
    for (count=0,p=inext(nn); (p != 0); p=inext(p))
    {
	if (ff_syl_accented(p) == 1)
	    count ++;
	if (p == lsyl) break;
    }
    return EST_Val(count);
}
	    
static EST_Val ff_last_accent(EST_Item *s)
{
    // Number of syllables since last accented syllable
    EST_Item *nn = as(s,"Syllable");
    EST_Item *p;
    int count;

    for (count=0,p=iprev(nn); p != 0; p=iprev(p),count++)
	if (ff_syl_accented(p) == 1)
	    return EST_Val(count);

    return EST_Val(count);
}

static EST_Val ff_next_accent(EST_Item *s)
{
    // Number of syllables to next accented syllable
    EST_Item *nn = as(s,"Syllable");
    EST_Item *p;
    int count;

    for (count=0,p=inext(nn); p != 0; p=inext(p),count++)
	if (ff_syl_accented(p) == 1)
	    return EST_Val(count);

    return EST_Val(count);
}

static EST_Val ff_sub_phrases(EST_Item *s)
{
    // Number of non-major phrase breaks since last major phrase break
    EST_Item *nn = parent(parent(s,"SylStructure"),"Phrase");
    EST_Item *p;
    int count;

    for (count=0,p=iprev(nn); p != 0; p=iprev(p))
    {
	if (p->name() == "BB")
	    return EST_Val(count);
	count ++;
    }

    return EST_Val(count);
}

void festival_ff_init(void)
{

    festival_def_nff("segment_duration","Segment",ff_segment_duration,
    "Segment.segment_duration\n\
  The duration of the given stream item calculated as the end of this\n\
  item minus the end of the previous item in the Segment relation.");
    festival_def_nff("syllable_duration","Syllable",ff_syllable_duration,
    "Syllable.syllable_duration\n\
  The duration of the given stream item calculated as the end of last\n\
  daughter minus the end of previous item in the Segment relation of the\n\
  first duaghter.");
    festival_def_nff("word_duration","Word",ff_word_duration,
    "Word.word_duration\n\
  The duration of the given stream item.  This is defined as the end of\n\
  last segment in the last syllable (via the SylStructure relation) minus\n\
  the segment immediate preceding the first segment in the first syllable.");
    festival_def_nff("segment_start","Segment",ff_seg_start,
    "Segement.segment_start\n\
  The start time of the given segment.");
    festival_def_nff("segment_mid","Segment",ff_seg_mid,
    "Segement.segment_mid\n\
  The middle time of the given segment.");
    festival_def_nff("syllable_start","Syllable",ff_syl_start,
    "Syllable.syllable_start\n\
  The start time of the given syllable.");
    festival_def_nff("word_start","Word",ff_word_start,
    "Word.word_start\n\
  The start time of the given word.");
    festival_def_nff("segment_end","Segment",ff_seg_end,
    "Segment.segment_end\n\
  The end time of the given segment.");
    festival_def_nff("syllable_end","Syllable",ff_syl_end,
    "Syllable.syllable_end\n\
  The end time of the given syllable.");
    festival_def_nff("word_end","Word",ff_word_end,
    "Word.word_end\n\
  The end time of the given word.");
    festival_def_nff("addr","ANY",ff_addr,
    "ANY.addr\n\
  Returned by popular demand, returns the address of given item that\n\
  is guaranteed unique for this session.");

    festival_def_nff("accented","Syllable",ff_syl_accented,
    "Syllable.accented\n\
  Returns 1 if syllable is accented, 0 otherwise.  A syllable is\n\
  accented if there is at least one IntEvent related to it.");
    festival_def_nff("syl_accent","Syllable",ff_syl_accent,
    "Syllable.syl_accent\n\
  Returns the name of the accent related to the syllable.  NONE is returned\n\
  if there are no accents, and multi is returned if there is more than one.");
    festival_def_nff("tobi_accent","Syllable",ff_tobi_accent,
    "Syllable.tobi_accent\n\
  Returns the ToBI accent related to syllable.  ToBI accents are\n\
  those which contain a *.  NONE is returned if there are none.  If\n\
  there is more than one ToBI accent related to this syllable the\n\
  first one is returned.");
    festival_def_nff("tobi_endtone","Syllable",ff_tobi_endtone,
    "Syllable.tobi_endtone\n\
  Returns the ToBI endtone related to syllable.  ToBI end tones are\n\
  those IntEvent labels which contain a % or a - (i.e. end tones or\n\
  phrase accents).  NONE is returned if there are none.  If\n\
  there is more than one ToBI end tone related to this syllable the\n\
  first one is returned.");
    festival_def_nff("syl_onsetsize","Syllable",ff_syl_onsetsize,
    "Syllable.syl_onsetsize\n\
  Returns the number of segments before the vowel in this syllable.  If\n\
  there is no vowel in the syllable this will return the total number\n\
  of segments in the syllable.");
    festival_def_nff("syl_vowel","Syllable",ff_syl_vowel,
    "Syllable.syl_vowel\n\
  Returns the name of the vowel within this syllable.  Note this is not\n\
  the general form you probably want.  You can't refer to ph_* features \n\
  of this.  Returns \"novowel\" is no vowel can be found.");
    festival_def_nff("syl_codasize","Syllable",ff_syl_codasize,
    "Syllable.syl_codasize\n\
  Returns the number of segments after the vowel in this syllable.  If\n\
  there is no vowel in the syllable this will return the total number\n\
  of segments in the syllable." );
    festival_def_nff("seg_onsetcoda","Segment",ff_seg_onsetcoda,
    "Segment.seg_onsetcoda\n\
  Returns onset if this segment is before the vowel in the syllable it\n\
  is contained within.  Returns coda if it is the vowel or after.  If\n\
  the segment is not in a syllable it returns onset.");
    festival_def_nff("seg_onset_stop","Segment",ff_seg_onset_stop,
    "Segment.seg_onset_stop\n\
  Returns 1 if onset of the syllable this segment is in contains a stop.\n\
  0 otherwise.");
    festival_def_nff("seg_coda_fric","Segment",ff_seg_coda_fric,
    "Segment.seg_coda_fric\n\
  Returns 1 if coda of the syllable this segment is in contains a fricative.\n\
  0 otherwise.");
    festival_def_nff("syl_numphones","Syllable",ff_syl_numphones,
    "Syllable.syl_numphones\n\
  Returns number of phones in syllable.");
    festival_def_nff("syl_pc_unvox","Syllable",ff_syl_pc_unvox,
    "Syllable.syl_pc_unvox\n\
  Percentage of total duration of unvoiced segments from\n\
  start of syllable. (i.e. percentage to start of first voiced segment)");
    festival_def_nff("syl_vowel_start","Syllable",ff_syl_vowel_start,
    "Syllable.syl_vowel_start\n\
  Start position of vowel in syllable.  If there is no vowel the start\n\
  position of the syllable is returned.");
    festival_def_nff("syl_midpitch","Syllable",ff_syl_midpitch,
    "Syllable.syl_midpitch\n\
  Pitch at the mid vowel of this syllable.");
    festival_def_nff("syl_startpitch","Syllable",ff_syl_startpitch,
    "Syllable.syl_startpitch\n\
  Pitch at the start of this syllable.");
    festival_def_nff("syl_endpitch","Syllable",ff_syl_endpitch,
    "Syllable.syl_endpitch\n\
  Pitch at the end of this syllable.");
    festival_def_nff("seg_pitch","Segment",ff_seg_pitch,
    "Segment.seg_pitch\n\
  Pitch at the middle of this segment.");
    
    festival_def_nff("syl_in","Syllable",ff_syl_in,
    "Syllable.syl_in\n\
  Returns number of syllables since last phrase break.  This is 0 if\n\
  this syllable is phrase initial.");
    festival_def_nff("syl_out","Syllable",ff_syl_out,
    "Syllable.syl_out\n\
  Returns number of syllables to next phrase break.  This is 0 if\n\
  this syllable is phrase final.");
    festival_def_nff("ssyl_in","Syllable",ff_ssyl_in,
    "Syllable.ssyl_in\n\
  Returns number of stressed syllables since last phrase break, not\n\
  including this one.");
    festival_def_nff("ssyl_in_modified_version","Syllable",ff_ssyl_in_modified_version,
    "Syllable.ssyl_in_modified_version\n\
  Returns number of stressed syllables since last phrase break, not\n\
  including this one.");
    festival_def_nff("ssyl_out","Syllable",ff_ssyl_out,
    "Syllable.ssyl_out\n\
  Returns number of stressed syllables to next phrase break, not including\n\
  this one.");
    festival_def_nff("asyl_in","Syllable",ff_asyl_in,
    "Syllable.asyl_in\n\
  Returns number of accented syllables since last phrase break, not\n\
  including this one.  Accentedness is as defined by the syl_accented\n\
  feature.");
    festival_def_nff("asyl_in_modified_version","Syllable",ff_asyl_in_modified_version,
    "Syllable.asyl_in_modified_version\n\
  Returns number of accented syllables since last phrase break, not\n\
  including this one.  Accentedness is as defined by the syl_accented\n\
  feature.");
    festival_def_nff("asyl_out","Syllable",ff_asyl_out,
    "Syllable.asyl_out\n\
  Returns number of accented syllables to the next phrase break, not\n\
  including this one.  Accentedness is as defined by the syl_accented\n\
  feature.");
    festival_def_nff("last_accent","Syllable",ff_last_accent,
    "Syllable.last_accent\n\
  Returns the number of syllables since last accented syllable.");
    festival_def_nff("next_accent","Syllable",ff_next_accent,
    "Syllable.next_accent\n\
  Returns the number of syllables to the next accented syllable.");
    festival_def_nff("sub_phrases","Syllable",ff_sub_phrases,
    "Syllable.sub_phrases\n\
  Returns the number of non-major phrase breaks since last major\n\
  phrase break.  Major phrase breaks are 4, as returned by syl_break,\n\
  minor phrase breaks are 2 and 3.");
    festival_def_nff("syl_break","Syllable",ff_syl_break,
    "Syllable.syl_break\n\
  The break level after this syllable.  Word internal is syllables\n\
  return 0, non phrase final words return 1.  Final syllables in \n\
  phrase final words return the name of the phrase they are related to.\n\
  Note the occasional \"-\" that may appear of phrase names is removed\n\
  so that this feature function returns a number in the range 0,1,2,3,4.");
    festival_def_nff("old_syl_break","Syllable",ff_old_syl_break,
    "Syllable.old_syl_break\n\
  Like syl_break but 2 and 3 are promoted to 4 (to be compatible with\n\
  some older models.");
    festival_def_nff("pos_in_syl","Segment",ff_seg_pos_in_syl,
    "Segment.pos_in_syl\n\
  The position of this segment in the syllable it is related to.  The index\n\
  counts from 0.  If this segment is not related to a syllable this \n\
  returns 0.");
    festival_def_nff("syl_initial","Segment",ff_seg_syl_initial,
    "Segment.syl_initial\n\
  Returns 1 if this segment is the first segment in the syllable it\n\
  is related to, or if it is not related to any syllable.");
    festival_def_nff("syl_final","Segment",ff_seg_syl_final,
    "Segment.syl_final\n\
  Returns 1 if this segment is the last segment in the syllable it\n\
  is related to, or if it is not related to any syllable.");
    festival_def_nff("pos_in_word","Syllable",ff_syl_pos_in_word,
    "Syllable.pos_in_word\n\
  The position of this syllable in the word it is related to.  The index\n\
  counts from 0.  If this syllable is not related to a word then 0 is\n\
  returned.");
    festival_def_nff("word_numsyls","Word",ff_word_numsyls,
    "Word.word_numsyls\n\
  Returns number of syllables in a word.");
    festival_def_nff("word_break","Word",ff_word_break,
    "Word.word_break\n\
  The break level after this word.  Non-phrase final words return 1\n\
  Phrase final words return the name of the phrase they are in.");
    festival_def_nff("pos_in_phrase","Word",ff_pos_in_phrase,
    "Word.pos_in_phrase\n\
  The position of this word in the phrase this word is in.");
    festival_def_nff("num_break","Word",ff_num_break,
    "Word.num_break\n\
  1 if this is the last word in a numeric token and it is followed by\n\
  a numeric token.");
    festival_def_nff("words_out","Word",ff_words_out,
    "Word.words_out\n\
  Number of words to end of this phrase.");
    festival_def_nff("position_type","Syllable",ff_position_type,
    "Syllable.position_type\n\
  The type of syllable with respect to the word it it related to.  This\n\
  may be any of: single for single syllable words, initial for word\n\
  initial syllables in a poly-syllabic word, final for word final\n\
  syllables in poly-syllabic words, and mid for syllables within \n\
  poly-syllabic words.");

}
