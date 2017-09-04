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
/* Basic lexicon utilities                                               */
/*                                                                       */
/*=======================================================================*/
#include <cstdio>
#include "festival.h"
#include "lexicon.h"
#include "lexiconP.h"

static void split_stress(LISP phones, LISP &phs, LISP &stresses);
static char *v_stress(const char *ph,int &stress);
static int syl_contains_vowel(LISP phones);
static int syl_breakable(LISP syl, LISP rest);

LISP lex_syllabify(LISP phones)
{
    /* Given a simple list of phones, syllabify them and add stress */
    LISP syl,syls,p;
    int stress = 1;

    for (syl=NIL,syls=NIL,p=phones; p != NIL; p=cdr(p))
    {
	syl = cons(car(p),syl);
	if (syl_breakable(syl,cdr(p)))
	{
	    syls = cons(cons(reverse(syl),cons(flocons(stress),NIL)),syls);
	    stress = 0;
	    syl = NIL;
	}
    }

    return reverse(syls);
}

LISP lex_syllabify_phstress(LISP phones)
{
    /* Given a list of phones where vowels may have stress numeral,  */
    /* as found in BEEP and CMU syllabify them */
    LISP syl,syls,p,phs,stresses,s;
    int stress = 0;
    const char *ph;

    split_stress(phones,phs,stresses);

    for (syl=NIL,syls=NIL,p=phs,s=stresses; 
	 p != NIL; 
	 p=cdr(p),s=cdr(s))
    {
	ph = get_c_string(car(p));
	if (!streq(ph,ph_silence()))
	    syl = cons(car(p),syl);
	if (car(s) && (!streq(get_c_string(car(s)),"0")))
	    stress = 1; // should worry about 2 stress too
	if (streq(ph,ph_silence()) || syl_breakable(syl,cdr(p)))
	{
	    syls = cons(cons(reverse(syl),cons(flocons(stress),NIL)),syls);
	    stress = 0;
	    syl = NIL;
	}
    }

    return reverse(syls);
}

static void split_stress(LISP phones, LISP &phs, LISP &stresses)
{
    // unpack the list of phones. When they come from certain types
    // of lexical entries (CMU, BEEP) vowels may have a 1 or 2 at their
    // end to denote stress.
    // This returns two list of equal length, one with the phones and
    // one with nils (for each phone) except when there is an explicit
    // stress number
    LISP p,np,ns;
    char *nph;
    int stress;

    for (p=phones,np=ns=NIL; p != NIL; p=cdr(p))
    {
	stress = 0;
	nph = v_stress(get_c_string(car(p)),stress);
	if (streq(nph,"-"))  // a break of some sort
	    np = cons(rintern(ph_silence()),np);
	else
	    np = cons(rintern(nph),np);
	wfree(nph);
	if (stress != 0)
	    ns = cons(flocons(stress),ns);
	else
	    ns = cons(NIL,ns);
    }

    phs = reverse(np);
    stresses = reverse(ns);
}

static char *v_stress(const char *ph,int &stress)
{
    //  Checks to see if final character is a numeral, if so treats
    //  is as stress value.
    char *nph;

    if ((strlen(ph) > 1) &&
        ((ph[strlen(ph)-1] == '1') || 
         (ph[strlen(ph)-1] == '2') ||
         (ph[strlen(ph)-1] == '0')))
    {
	stress = ph[strlen(ph)-1]-'0';
	nph = wstrdup(ph);
	nph[strlen(ph)-1] = '\0';
	return nph;
    }
    else
	return wstrdup(ph);

}

static int syl_breakable(LISP syl, LISP rest)
{
    if (rest == NIL)
	return TRUE;
    else if (!syl_contains_vowel(rest))
	return FALSE;  // must be a vowel remaining in rest 
    else if (syl_contains_vowel(syl))
    {
	if (ph_is_vowel(get_c_string(car(rest))))
	    return TRUE;
	else if (cdr(rest) == NIL)
	    return FALSE;
	int p = ph_sonority(get_c_string(car(syl)));
	int n = ph_sonority(get_c_string(car(rest)));
	int nn = ph_sonority(get_c_string(car(cdr(rest))));

	if ((p <= n) && (n <= nn))
	    return TRUE;
	else
	    return FALSE;
    }
    else
	return FALSE;
}

static int syl_contains_vowel(LISP phones)
{
    // So we can support "vowels" like ah2, oy2 (i.e. vowels with 
    // stress markings) we need to make this a hack.  Vowels are
    // assumed to start with one of aiueo
    LISP p;

    for (p=phones; p !=NIL; p=cdr(p))
	if (strchr("aiueoAIUEO",get_c_string(car(p))[0]) != NULL)
	    return TRUE;
	else if (ph_is_vowel(get_c_string(car(p))))
	    return TRUE;
	else if (ph_is_silence(get_c_string(car(p))))
	    return FALSE;

    return FALSE;
}

