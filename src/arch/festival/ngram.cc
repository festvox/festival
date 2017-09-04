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
/*                 Authors:  Alan W Black                                */
/*                 Date   :  December 1997                               */
/*-----------------------------------------------------------------------*/
/*  Access to the Ngrammar                                               */
/*                                                                       */
/*=======================================================================*/
#include <cstdio>
#include "festival.h"
#include "festivalP.h"

static LISP ngram_loaded_list = NIL;
static EST_Ngrammar *load_ngram(const EST_String &filename);
static LISP add_ngram(const EST_String &name,EST_Ngrammar *n);

SIOD_REGISTER_CLASS(ngrammar,EST_Ngrammar)

static LISP lisp_load_ngram(LISP name, LISP filename)
{
    EST_Ngrammar *n;

    n = load_ngram(get_c_string(filename));
    add_ngram(get_c_string(name),n);

    return name;
}

static EST_Ngrammar *load_ngram(const EST_String &filename)
{
    EST_Ngrammar *n = new EST_Ngrammar();
    if (n->load(filename) != 0)
    {
	fprintf(stderr,"Ngrammar: failed to read ngrammar from \"%s\"",
		(const char *)filename);
	festival_error();
    }

    return n;
}

static LISP add_ngram(const EST_String &name,EST_Ngrammar *n)
{
    LISP lpair; 

    lpair = siod_assoc_str(name,ngram_loaded_list);

    if (ngram_loaded_list == NIL)
    {   // First time round so do a little initialization 
	gc_protect(&ngram_loaded_list);
    }

    LISP ng = siod(n);
    
    if (lpair == NIL)
	ngram_loaded_list = 
	    cons(cons(strintern(name),cons(ng,NIL)),ngram_loaded_list);
    else
    {
	cwarn << "Ngrammar: " << name << " recreated" << endl;
	setcar(cdr(lpair),ng);
    }

    return ng;
}

EST_Ngrammar *get_ngram(const EST_String &name,const EST_String &filename)
{
    //  Find ngram named name, returns NULL if none;
    LISP lpair;
    
    lpair = siod_assoc_str(name,ngram_loaded_list);

    if (lpair == NIL)
    {
	if (filename != EST_String::Empty)
	{
	    EST_Ngrammar *n = load_ngram(filename);
	    add_ngram(name,n);
	    return n;
	}
	else
	{
	    cwarn << "Ngrammar: no ngram named \"" << name << "\"" << endl;
	    return 0;
	}
    }
    else
	return ngrammar(car(cdr(lpair)));
}

void festival_ngram_init()
{
    init_subr_2("ngram.load",lisp_load_ngram,
 "(ngram.load NAME FILENAME)\n\
  Load an ngram from FILENAME and store it named NAME for later access.");

}
