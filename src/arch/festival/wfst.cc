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
/*  Access to WFST classes                                               */
/*                                                                       */
/*=======================================================================*/
#include <cstdio>
#include "festival.h"
#include "festivalP.h"

static LISP wfst_loaded_list = NIL;
static EST_WFST *load_wfst(const EST_String &filename);
static LISP add_wfst(const EST_String &name,EST_WFST *n);

SIOD_REGISTER_CLASS(wfst,EST_WFST)

static LISP lisp_load_wfst(LISP name, LISP filename)
{
    EST_WFST *n;

    n = load_wfst(get_c_string(filename));
    add_wfst(get_c_string(name),n);

    return name;
}

static EST_WFST *load_wfst(const EST_String &filename)
{
    EST_WFST *n = new EST_WFST();
    if (n->load(filename) != 0)
    {
	fprintf(stderr,"WFST: failed to read wfst from \"%s\"\n",
		(const char *)filename);
	festival_error();
    }

    return n;
}

static LISP add_wfst(const EST_String &name,EST_WFST *n)
{
    LISP lpair; 

    lpair = siod_assoc_str(name,wfst_loaded_list);

    if (wfst_loaded_list == NIL)
	gc_protect(&wfst_loaded_list);

    LISP lwfst = siod(n);
    
    if (lpair == NIL)
	wfst_loaded_list = 
	    cons(cons(strintern(name),cons(lwfst,NIL)),wfst_loaded_list);
    else
    {
	cwarn << "WFST: " << name << " recreated" << endl;
	setcar(cdr(lpair),lwfst);
    }
    return lwfst;
}

EST_WFST *get_wfst(const EST_String &name,const EST_String &filename)
{
    //  Find ngram named name, returns NULL if none;
    LISP lpair;
    
    lpair = siod_assoc_str(name,wfst_loaded_list);

    if (lpair == NIL)
    {
	if (filename != EST_String::Empty)
	{
	    EST_WFST *n = load_wfst(filename);
	    add_wfst(name,n);
	    return n;
	}
	else
	{
	    cwarn << "WFST: no wfst named \"" << name << "\" loaded" << endl;
	    return 0;
	}
    }
    else
	return wfst(car(cdr(lpair)));
}

LISP lisp_wfst_transduce(LISP wfstname, LISP input)
{
    EST_WFST *wfst = get_wfst(get_c_string(wfstname));
    EST_StrList in,out;
    int r;

    if (consp(input))
	siod_list_to_strlist(input,in);
    else
	siod_list_to_strlist(stringexplode(get_c_string(input)),in);

    r = transduce(*wfst,in,out);

    if (r == FALSE)
	return rintern("FAILED");
    else
	return siod_strlist_to_list(out);
}

void festival_wfst_init()
{

    init_subr_2("wfst.load",lisp_load_wfst,
 "(wfst.load NAME FILENAME)\n\
  Load a WFST from FILENAME and store it named NAME for later access.");
    init_subr_2("wfst.transduce",lisp_wfst_transduce,
 "(wfst.trasduce WFSTNAME INPUT)\n\
  Transduce list INPUT (or exploded INPUT if its an atom) to a list of \n\
  outputs.  The atom FAILED is return if the transduction fails.");
    

}
