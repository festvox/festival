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
/*             Author :  Alan W Black                                    */
/*             Date   :  April 1996                                      */
/*-----------------------------------------------------------------------*/
/*                                                                       */
/* A front end to the letter to sound rule system(s)                     */
/*                                                                       */
/*=======================================================================*/
#include <cstdio>
#include "festival.h"
#include "lexicon.h"
#include "lts.h"

static LISP lts_create_entry(const EST_String &word,
			     LISP features,LISP syllables);
static LISP map_phones(LISP phones);

LISP lts(const EST_String &word,LISP features,const EST_String &rulesetname)
{
    /* Return lexical entry for given word in best possible way */
    LISP phones;
    EST_String dword = downcase(word);
    LISP lword = strintern(dword);
    LISP lrulesetname = rintern(rulesetname);

    if (lts_in_alphabet(lword,lrulesetname) != NIL)
    {   // this check doesn't guarantee success
	phones = lts_apply_ruleset(lword,lrulesetname);
    }
    else
	phones = NIL; // otherwise can't do anything

    return lts_create_entry(word, features, 
			    lex_syllabify(map_phones(phones)));

}

static LISP lts_create_entry(const EST_String &word,LISP features,LISP syllables)
{
    /* build an entry from information */

    return cons(strcons(strlen(word),word),
		cons(features,cons(syllables,NIL)));
}

static LISP map_phones(LISP phones)
{
    // map list of phones to lexical list of phones 

    // Users responsibility to get right phone set
    return phones;

    // If lts rulesets get their own phonesets then the following should
    // be used (with appropriate changes)
#if 0
    LISP mapped = NIL,p;
    EST_String lexset,mappedph;
    
    lexset = lex_current_phoneset();

    if (lexset != "nrl")
    {
	for (p=phones; p != NIL; p=cdr(p))
	{
	    mappedph = map_phone(get_c_string(car(p)),"nrl",lexset);
	    mapped = cons(rintern(mappedph),mapped);
	}
	return reverse(mapped);
    }
    else
	return phones;
#endif
}

