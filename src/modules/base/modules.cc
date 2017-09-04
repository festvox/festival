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
/* Some basic initialization functions for modules                       */
/*                                                                       */
/*=======================================================================*/
#include <cstdio>
#include "festival.h"
#include "lexicon.h"
#include "modules.h"
#include "intonation.h"

static void create_words(EST_Utterance *u);
static void create_segments(EST_Utterance *u);
static void create_wave(EST_Utterance *u);
static void create_phones(EST_Utterance *u);

LISP FT_Initialize_Utt(LISP utt)
{
    // Main utterance intialization routine
    // creates appropriate streams and loads them from the input
    EST_Utterance *u = get_c_utt(utt);
    EST_String type;

    *cdebug << "Initialize module\n";

    type = utt_type(*u);

    utt_cleanup(*u);  // delete all relations

    if (type == "Words")
	create_words(u);
    else if (type == "Text")
	;
    else if (type == "Segments")
	create_segments(u);
    else if (type == "Phones")
	create_phones(u);
    else if (type == "Phrase")
	create_phraseinput(u);
    else if (type == "Wave")
	create_wave(u);
    else
    {
	// error
	cerr << "Unknown utterance type \"" << type << "\" for initialization "
	    << endl;
	festival_error();
    }

    return utt;
}

void create_words(EST_Utterance *u)
{
    // Add words from IForm
    LISP lwords,w;
    EST_Item *word;

    u->create_relation("Word");
    lwords = utt_iform(*u);

    for (w=lwords; w != NIL; w=cdr(w))
    {
	if (consp(car(w)))  // word has features too
	{
	    word = add_word(u,get_c_string(car(car(w))));
	    add_item_features(word,car(cdr(car(w))));
	}
	else
	    add_word(u,get_c_string(car(w)));
    }

}

void create_wave(EST_Utterance *u)
{
    // Get the fname for the wave and load it
    EST_Item *item = 0;
    LISP lwave;
    EST_Wave *wave = new EST_Wave;

    lwave = utt_iform(*u);

    if (wave->load(get_c_string(lwave)) != format_ok)
    {
	cerr << "Cannot load wavefile: " << get_c_string(lwave) << endl;
	festival_error();
    }

    item = u->create_relation("Wave")->append();
    item->set_val("wave",est_val(wave));

}

void create_segments(EST_Utterance *u)
{
    // Add segments from IForm
    LISP lsegs,s,targs,t;
    EST_String seg;
    EST_Item *Seg;;
    float start,end,dur,tpos,tval;
    u->create_relation("Segment");
    u->create_relation("Target");

    lsegs = utt_iform(*u);

    end = 0.0;
    for (s=lsegs; s != NIL; s=cdr(s))
    {
	seg = get_c_string(car(car(s)));
	dur = get_c_float(car(cdr(car(s))));
	targs = cdr(cdr(car(s)));
	Seg = add_segment(u,seg);
	start = end;
	end += dur;
	Seg->set("end",end);
	for (t=targs; t != NIL; t=cdr(t))
	{
	    tpos = start + (get_c_float(car(car(t))));
	    tval = get_c_float(car(cdr(car(t))));
	    add_target(u,Seg,tpos,tval);
	}
    }

}

static void create_phones(EST_Utterance *u)
{
    // Add phones from IForm
    LISP lsegs,s;
    EST_String seg;

    u->create_relation("Segment");
    lsegs = utt_iform(*u);

    for (s=lsegs; s != NIL; s=cdr(s))
    {
	seg = get_c_string(car(s));
	add_segment(u,seg);
    }
}

LISP FT_Initialize_Utt(LISP args);
LISP FT_Classic_Phrasify_Utt(LISP args);
LISP FT_Classic_Word_Utt(LISP args);
LISP FT_Unilex_Word_Utt(LISP args);
LISP FT_Classic_POS_Utt(LISP args);
LISP FT_PostLex_Utt(LISP utt);
void festival_ff_init(void);

void festival_base_init(void)
{
    // Thing I haven't put anywhere else yet

    festival_ff_init();  // basic feature functions
    // Basic EST_Utterance modules 
    festival_def_utt_module("Initialize",FT_Initialize_Utt,
    "(Initialize UTT)\n\
  This module should be called first on all utterances it does some\n\
  necessary initialization of the utterance and loads the base\n\
  streams with the information from the input form.");
    festival_def_utt_module("Classic_Phrasify",FT_Classic_Phrasify_Utt,
    "(Classic_Phrasify UTT)\n\
  Creates phrases from words, if pos_supported is non-nil, a more elaborate\n\
  system of prediction is used.  Here probability models based on part of\n\
  speech and B/NB distribution are used to predict breaks.  This system\n\
  uses standard Viterbi decoding techniques. If pos_supported is nil,\n\
  a simple CART-based prediction model is used. [see Phrase breaks]");
    festival_def_utt_module("Classic_Word",FT_Classic_Word_Utt,
    "(Classic_Word UTT)\n\
  Build the syllable/segment/SylStructure from the given words using the\n\
  Lexicon.  Uses part of speech information in the lexicon look up if\n\
  present.");
    festival_def_utt_module("Unilex_Word",FT_Unilex_Word_Utt,
    "(Unilex_Word UTT)\n\
  Build the syllable/segment/SylStructure from the given words using the\n\
  Lexicon.  Uses part of speech information in the lexicon look up if\n\
  present.");
    festival_def_utt_module("Classic_POS",FT_Classic_POS_Utt,
    "(Classic_POS UTT)\n\
  Predict part of speech tags for the existing word stream.  If the variable\n\
  pos_lex_name is nil nothing happens, otherwise it is assumed to point to\n\
  a lexicon file giving part of speech distribution for words. An ngram\n\
  model file should be in pos_ngram_name.  The system uses standard\n\
  Viterbi decoding techniques. [see POS tagging]");
    festival_def_utt_module("Builtin_PostLex",FT_PostLex_Utt,
    "(Builtin_PostLex UTT)\n\
  Post-lexical rules.  Currently only vowel reduction applied to each\n\
  syllable using postlex_vowel_reduce_cart_tree, and the table of \n\
  vowel reduction pairs in postlex_vowel_reduce_table.");

}
