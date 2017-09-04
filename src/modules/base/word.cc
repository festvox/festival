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
/* From words to syllables and segments using the lexicon                */
/*                                                                       */
/*=======================================================================*/

#include <cstdio>
#include "festival.h"
#include "lexicon.h"
#include "modules.h"

static EST_Item *add_syllable(EST_Utterance *u, int stress);
static LISP specified_word_pronunciation(EST_Item *w, LISP lpos);

LISP FT_Classic_Word_Utt(LISP utt)
{
    // Look up words in lexicon and create syllable and segment streams
    EST_Utterance *u = get_c_utt(utt);
    EST_Item *w;
    LISP entry,s,p,lpos;
    EST_String pos;
    EST_Item *syl,*seg;
    EST_Relation *SylStructure;

    *cdebug << "Word module\n";

    u->create_relation("Syllable");
    u->create_relation("Segment");
    SylStructure = u->create_relation("SylStructure");

    for (w=u->relation("Word")->first(); w != 0; w = inext(w))
    {
	lpos = NIL;
	pos = (EST_String)ffeature(w,"hg_pos");
                        // explicit homograph pos disambiguation
	if (pos == "0")
	    pos = (EST_String)ffeature(w,"pos");
	if (pos != "0")
	    lpos = rintern(pos);

	//  Check if there is an explicitly given pronunciation before
	//  going to the lexicon
	if ((entry = specified_word_pronunciation(w,lpos)) == NIL)
	    entry = lex_lookup_word(w->name(),lpos);
	if (lpos == NIL)
	    w->set("pos",get_c_string(car(cdr(entry))));
	SylStructure->append(w);
	for (s=car(cdr(cdr(entry))); s != NIL; s=cdr(s))
	{
	    syl = add_syllable(u,get_c_int(car(cdr(car(s)))));
	    append_daughter(w,"SylStructure",syl);
	    for (p=car(car(s)); p != NIL; p=cdr(p))
	    {
		seg = add_segment(u,get_c_string(car(p)));
		append_daughter(syl,"SylStructure",seg);
	    }
	}
    }

    return utt;
}

LISP FT_Unilex_Word_Utt(LISP utt)
{
  // This tries to be a bit cleverer than Classic_Word in dealing with full and reduced forms of words.
  // Look up words in lexicon and create syllable and segment streams
    EST_Utterance *u = get_c_utt(utt);
    EST_Item *w;
    LISP entry,entry2,s,p,s2,p2,lpos,lexpos;
    EST_String pos, vowel_form,sname,s2name;
    EST_Item *syl,*seg;
    EST_Relation *SylStructure;

    *cdebug << "Word module\n";

    u->create_relation("Syllable");
    u->create_relation("Segment");
    SylStructure = u->create_relation("SylStructure");
    
    for (w=u->relation("Word")->first(); w != 0; w = inext(w))
    {
	lpos = NIL;
	pos = EST_String(ffeature(w,"hg_pos"));
                        // explicit homograph pos disambiguation
	if (pos == "0")
	    pos = EST_String(ffeature(w,"pos"));
	if (pos != "0")
	    lpos = rintern(pos);

	//  Check if there is an explicitly given pronunciation before
	//  going to the lexicon
	if ((entry = specified_word_pronunciation(w,lpos)) == NIL)
	    entry = lex_lookup_word(w->name(),lpos);
	lexpos = car(cdr(entry));
	// deal with full/reduced specification in pos as a list.
	entry2 = NIL;
	if (! atomp(lexpos))
	  {
	    if ( (vowel_form = get_c_string(car(cdr(lexpos)))) == "full")
	      {
		entry2 = lex_lookup_word(w->name(),cons(rintern("reduced"),NIL));
		if (lpos == NIL)
		  w->set("pos",get_c_string(car(lexpos)));
	      }
	  }
	else if (lpos == NIL)
	  w->set("pos",get_c_string(lexpos));
	SylStructure->append(w);
	if (entry2) // compare full and reduced form entries
	  for (s=car(cdr(cdr(entry))),s2=car(cdr(cdr(entry2))) ; s != NIL; s=cdr(s))
	    {
	      syl = add_syllable(u,get_c_int(car(cdr(car(s)))));
	      append_daughter(w,"SylStructure",syl);
	      for (p=car(car(s)),p2=car(car(s2)); p != NIL; p=cdr(p))
		{
		  seg = add_segment(u,get_c_string(car(p)));
		  append_daughter(syl,"SylStructure",seg);

		  if(p2 != NIL)
		    {
		      sname = get_c_string(car(p));
		      s2name = get_c_string(car(p2));
		      if (sname != s2name)
			{
			  seg->set("reducable",1);
			  seg->set("fullform",sname);
			  seg->set("reducedform",s2name);
			}
		      p2=cdr(p2);
		    }
		}
	      if(s2 != NIL)
		s2 = cdr(s2);
	  }
	else
	  for (s=car(cdr(cdr(entry))); s != NIL; s=cdr(s))
	    {
	      syl = add_syllable(u,get_c_int(car(cdr(car(s)))));
	      append_daughter(w,"SylStructure",syl);
	      for (p=car(car(s)); p != NIL; p=cdr(p))
		{
		  seg = add_segment(u,get_c_string(car(p)));
		  append_daughter(syl,"SylStructure",seg);
		}
	    }	
    }

    return utt;
}


static LISP specified_word_pronunciation(EST_Item *w, LISP lpos)
{
    //  If there is a phoneme feature on w or the Token related to 
    //  w use that as the pronunciation.  Note the value will be a string
    //  from which a list can be read.
    EST_String p;

    if (((p = (EST_String)ffeature(w,"phonemes")) != "0") ||
	((p = (EST_String)ffeature(w,"R:Token.parent.phonemes")) != "0"))
    {
	LISP phones = read_from_lstring(strintern(p));

	return cons(strintern(w->name()),
		    cons(lpos,
			 cons(lex_syllabify(phones),NIL)));
    }
    else
	return NIL;

}

EST_Item *add_word(EST_Utterance *u, const EST_String &name)
{
    EST_Item *item = u->relation("Word")->append();

    item->set_name(name);
    
    return item;
}

EST_Item *add_word(EST_Utterance *u, LISP word)
{
    // Build a Word Ling_Item from the Lisp description, which may
    // contain other features
    LISP f;
    EST_Item *si_word;
    int has_name = FALSE;

    if (consp(word))
    {
	// feature form
	si_word = add_word(u,"");
	for (f=word; f != NIL; f=cdr(f))
	{
	    if (streq("name",get_c_string(car(car(f)))))
	    {
		has_name = TRUE;
		si_word->set_name(get_c_string(car(cdr(car(f)))));
	    }
	    else
		si_word->set(get_c_string(car(car(f))),
			      get_c_string(car(cdr(car(f)))));
	}
	if (!has_name)
	{
	    cerr << "add_word: word has description but no name" << endl;
	    cerr << "  " << siod_sprint(word) << endl;
	    festival_error();
	}
    }
    else // just the name
	si_word = add_word(u,get_c_string(word));

    return si_word;
}

static EST_Item *add_syllable(EST_Utterance *u, int stress)
{
    EST_Item *item = u->relation("Syllable")->append();
    
    item->set_name("syl");
    item->set("stress",stress);

    return item;
}

EST_Item *add_segment(EST_Utterance *u, const EST_String &s)
{
    EST_Item *item = u->relation("Segment")->append();
    
    item->set_name(s);

    return item;
}

