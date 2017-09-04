/*************************************************************************/
/*                                                                       */
/*                Centre for Speech Technology Research                  */
/*                     University of Edinburgh, UK                       */
/*                         Copyright (c) 1998                            */
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
/*             Author :  Alan W Black                                    */
/*             Date   :  May 1998                                        */
/*-----------------------------------------------------------------------*/
/*                                                                       */
/* Word based ffeature functions                                        */
/*                                                                       */
/*=======================================================================*/
#include <cstdio>
#include "festival.h"
#include "lexiconP.h"

static EST_String Phrase("Phrase");
static EST_Val f_content("content");
static EST_Val f_string0("0");
static EST_Val f_string1("1");

static EST_Val ff_word_gpos(EST_Item *s)
{
    /* Part of speech by guessing, returns, prep, det, aux, content */
    /* from simple lookup list                                      */
    EST_String word;
    LISP l;
    LISP guess_pos;

    word = downcase(s->name());

    guess_pos = siod_get_lval("guess_pos","no guess_pos set");
    
    for (l=guess_pos; l != NIL; l=cdr(l))
	if (siod_member_str(word,cdr(car(l))))
	    return EST_Val(get_c_string(car(car(l))));

    return f_content;
}

EST_Val ff_word_contentp(EST_Item *s)
{
    /* 1 if this is a content word, 0 otherwise */
    
    if (ff_word_gpos(s) == "content")
	return f_string1;
    else
	return f_string0;
}

static EST_Val ff_word_n_content(EST_Item *s)
{
    // returns the next content word after s
    EST_Item *p;

    for (p=inext(s->as_relation("Word")); p != 0; p = inext(p))
    {
	if (ff_word_gpos(p) == "content")
	    return EST_Val(p->name());
    }

    return f_string0;
}

static EST_Val ff_word_nn_content(EST_Item *s)
{
    // returns the next next content word after s
    int count = 0;
    EST_Item *p;

    for (p=inext(s->as_relation("Word")); p != 0; p = inext(p))
    {
	if (ff_word_gpos(p) == "content")
	{
	    count ++;
	    if (count == 2)
		return EST_Val(p->name());
	}
    }

    return f_string0;
}

static EST_Val ff_word_p_content(EST_Item *s)
{
    // returns the previous content word after s
    EST_Item *p;

    for (p=iprev(s->as_relation("Word")); p != 0; p = iprev(p))
	if (ff_word_gpos(p) == "content")
	    return EST_Val(p->name());

    return f_string0;
}

static EST_Val ff_word_pp_content(EST_Item *s)
{
    // returns the previous previous content word after s
    int count = 0;
    EST_Item *p;

    for (p=iprev(s->as_relation("Word")); p != 0; p = iprev(p))
    {
	if (ff_word_gpos(p) == "content")
	{
	    count ++;
	    if (count == 2)
		return EST_Val(p->name());
	}
    }

    return f_string0;
}

static EST_Val ff_content_words_out(EST_Item *s)
{
    EST_Item *nn = s->as_relation(Phrase);
    EST_Item *p;
    int pos=0;

    for (p=inext(nn); p; p=inext(p))
    {
	if (ff_word_gpos(p) == "content")
	    pos++;
    }
    // don't think you can get here
    return EST_Val(pos);
}

static EST_Val ff_content_words_in(EST_Item *s)
{
    EST_Item *nn = s->as_relation(Phrase);
    EST_Item *p;
    int pos=0;

    for (p=iprev(nn); p; p=iprev(p))
    {
	if (ff_word_gpos(p) == "content")
	    pos++;
    }
    // don't think you can get here
    return EST_Val(pos);
}

static EST_Val ff_word_cap(EST_Item *s)
{
    //  "1" is the word starts with a capital letter
    const char *word = s->name();

    if ((word[0] >= 'A') && (word[0] <='Z'))
	return f_string1;
    else
	return f_string0;
}

static EST_Val ff_syl_onset_type(EST_Item *s)
{
    // Return van Santen's classification of onset type in to one
    // of three forms:
    //   -V    contains only voiceless consonants
    //   +V-S  contains voiced obstruents but no sonorants
    //   +S    contains just sonorants
    EST_Item *nn = s->as_relation("SylStructure");
    EST_Item *p;
    int vox=FALSE;
    int sonorant=FALSE;

    for (p=daughter1(nn); inext(p) != 0; p=inext(p))
    {
	if (ph_is_vowel(p->name()))
	    break;
	if (ph_is_voiced(p->name()))
	    vox = TRUE;
	if (ph_is_sonorant(p->name()))
	    sonorant = TRUE;
    }

    if (p==daughter1(nn)) // null-onset case
	return EST_Val("+V-S");
    else if (sonorant)
	return EST_Val("+S");
    else if (vox)
	return EST_Val("+V-S");
    else
	return EST_Val("-V");
}

static EST_Val ff_syl_coda_type(EST_Item *s)
{
    // Return van Santen's classification of onset type in to one
    // of three forms:
    //   -V    contains only voiceless consonants
    //   +V-S  contains voiced obstruents but no sonorants
    //   +S    contains just sonorants
    EST_Item *nn = s->as_relation("SylStructure");
    EST_Item *p;
    int vox=FALSE;
    int sonorant=FALSE;

    for (p=daughter1(nn); inext(p) != 0; p=inext(p))
    {
	if (ph_is_vowel(p->name()))
	    break;
    }

    if (inext(p) == 0)         // empty coda
	return EST_Val("+S");

    for (p=inext(p); p != 0; p=inext(p))
    {
	if (ph_is_voiced(p->name()))
	    vox = TRUE;
	if (ph_is_sonorant(p->name()))
	    sonorant = TRUE;
    }

    if (sonorant)
	return EST_Val("+S");
    else if (vox)
	return EST_Val("+V-S");
    else
	return EST_Val("-V");
}

void festival_lex_ff_init(void)
{

    festival_def_nff("gpos","Word",ff_word_gpos,
    "Word.gpos\n\
  Returns a guess at the part of speech of this word.  The lisp a-list\n\
  guess_pos is used to load up this word.  If no part of speech is\n\
  found in there \"content\" is returned.  This allows a quick efficient\n\
  method for part of speech tagging into closed class and content words.");
    festival_def_nff("contentp","Word",ff_word_contentp,
    "Word.contentp\n\
  Returns 1 if this word is a content word as defined by gpos, 0 otherwise.");
    festival_def_nff("cap","Word",ff_word_cap,
    "Word.cap\n\
  Returns 1 if this word starts with a capital letter, 0 otherwise.");
    festival_def_nff("n_content","Word",ff_word_n_content,
    "Word.n_content\n\
  Next content word.  Note this doesn't use the standard n. notation as\n\
  it may have to search a number of words forward before finding a\n\
  non-function word.  Uses gpos to define content/function word distinction.\n\
  This also works for Tokens.");
    festival_def_nff("nn_content","Word",ff_word_nn_content,
    "Word.nn_content\n\
  Next next content word.  Note this doesn't use the standard n.n. notation\n\
  as it may have to search a number of words forward before finding the \n\
  second non-function word.  Uses gpos to define content/function word\n\
  distinction.  This also works for Tokens.");
    festival_def_nff("p_content","Word",ff_word_p_content,
    "Word.p_content\n\
  Previous content word.  Note this doesn't use the standard p. notation\n\
  as it may have to search a number of words backward before finding the \n\
  first non-function word.  Uses gpos to define content/function word\n\
  distinction.  This also works for Tokens.");
    festival_def_nff("pp_content","Word",ff_word_pp_content,
    "Word.pp_content\n\
  Previous previous content word.  Note this doesn't use the standard p.p.\n\
  notation as it may have to search a number of words backward before\n\
  finding the first non-function word.  Uses gpos to define \n\
  content/function word distinction.  This also works for Tokens.");
    festival_def_nff("content_words_out","Word",ff_content_words_out,
    "Word.content_words_out\n\
  Number of content words to end of this phrase.");
    festival_def_nff("content_words_in","Word",ff_content_words_in,
    "Word.content_words_in\n\
  Number of content words from start this phrase.");
    festival_def_nff("syl_onset_type","Syllable",ff_syl_onset_type,
    "Syllable.syl_onset_type\n\
  Return the van Santen and Hirschberg classification. -V for unvoiced,\n\
  +V-S for voiced but no sonorants, and +S for sonorants.");
    festival_def_nff("syl_coda_type","Syllable",ff_syl_coda_type,
    "Syllable.syl_coda_type\n\
  Return the van Santen and Hirschberg classification. -V for unvoiced,\n\
  +V-S for voiced but no sonorants, and +S for sonorants.");

}
