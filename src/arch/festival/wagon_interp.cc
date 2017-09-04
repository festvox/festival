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
/*                     Author :  Alan W Black                            */
/*                     Date   :  May 1996                                */
/*-----------------------------------------------------------------------*/
/*                                                                       */
/* A simple interpreter for CART trees as produced by Wagon              */
/*                                                                       */
/*=======================================================================*/
#include <cstdio>
#include "EST_unix.h"
#include <cstdlib>
#include "festival.h"

#define ques_oper_str(X) (get_c_string(car(cdr(X))))
#define ques_operand(X) (car(cdr(cdr(X))))
static int wagon_ask(EST_Item *s, LISP tree,
		     EST_TKVL <EST_String,EST_Val> *fcache);
static LISP l_wagon_predict(EST_Item *s, LISP tree,
			    EST_TKVL <EST_String,EST_Val> *fcache);

/* It seems to be worth building a feature cache */

EST_Val wagon_predict(EST_Item *s, LISP tree)
{
    LISP answer,val;
    EST_TKVL <EST_String,EST_Val> *fcache;

    fcache = new EST_TKVL<EST_String,EST_Val>;  // feature cache saves some calls
    answer = l_wagon_predict(s,tree,fcache);
    delete fcache;
    
    // Decide if this is a number of a string
    val = car(siod_last(answer));
    if (!FLONUMP(val))  // just in case 
	return EST_Val(get_c_string(val));
    else if (!CONSP(car(answer)))
	return EST_Val(get_c_float(val));
    else
	return EST_Val(get_c_string(val));

}

LISP wagon_pd(EST_Item *s, LISP tree)
{
    // return probability distribution 
    LISP answer;
    EST_TKVL <EST_String,EST_Val> *fcache;

    fcache = new EST_TKVL<EST_String,EST_Val>;  // feature cache saves some calls
    answer = l_wagon_predict(s,tree,fcache);
    delete fcache;

    return answer;

}

static LISP l_wagon_predict(EST_Item *s, LISP tree,
			    EST_TKVL <EST_String,EST_Val> *fcache)
{
    // Use the tree to predict

    if (cdr(tree) == NIL)
	return car(tree);
    else if (wagon_ask(s,car(tree),fcache) == TRUE)
	return l_wagon_predict(s,car(cdr(tree)),fcache);
    else
	return l_wagon_predict(s,car(cdr(cdr(tree))),fcache);
}

static int wagon_ask(EST_Item *s, LISP question,
		     EST_TKVL <EST_String,EST_Val> *fcache)
{
    // Ask a question of this stream item
    EST_Val answer;
    const char *str_oper;
    const EST_String fname = get_c_string(car(question));
    LISP operand;

    /* printf("wagon_ask %s\n",(const char *)siod_sprint(question)); */
    if (!fcache->present(fname))
    {
	answer = ffeature(s,fname);
	fcache->add_item(fname,answer,0);
    }
    else
	answer = fcache->val(fname);

    str_oper = ques_oper_str(question);
    operand = ques_operand(question);
    // So that you can use LISP variables in the operand (or any LISP)
    // check if we've got a , if so eval it.
    if ((consp(operand)) && (!consp(car(operand))) &&
	(streq("+internal-comma",get_c_string(car(operand)))))
	operand = leval(cdr(operand),NIL);
    
    if (streq("is",str_oper))
	if (answer.string() == get_c_string(operand))
        {
            /*            printf("wagon_ask %s is %s\n",(const char *)answer.string(),(const char *)get_c_string(operand)); */
	    return TRUE;
        }
	else
        {
            /* printf("wagon_ask %s isnot %s\n",(const char *)answer.string(),(const char *)get_c_string(operand)); */
	    return FALSE;
        }
    else if (streq("=",str_oper))
	if (answer == get_c_float(operand))
	    return TRUE;
	else
	    return FALSE;
    else if (streq("<",str_oper))
	if ((float)answer < get_c_float(operand))
	    return TRUE;
	else
	    return FALSE;
    else if (streq(">",str_oper))
	if ((float)answer > get_c_float(operand))
	    return TRUE;
	else
	    return FALSE;
    else if (streq("matches",str_oper))
	if (answer.string().matches(make_regex(get_c_string(operand))))
	    return TRUE;
	else
	    return FALSE;
    else if (streq("in",str_oper))
	if (siod_member_str(answer.string(),operand) != NIL)
	    return TRUE;
	else
	    return FALSE;
    else 
    {
	cerr << "Decision tree: unknown question operator: \"" << 
	    str_oper << "\"\n";
	festival_error();
    }
    return 0;
}

LISP l_wagon(LISP si, LISP tree)
{
    // Lisp level binding for tree prediction 
    EST_Item *s = item(si);
    LISP answer;
    EST_TKVL <EST_String,EST_Val> *fcache;
    
    fcache = new EST_TKVL<EST_String,EST_Val>;  // feature cache saves some calls
    answer = l_wagon_predict(s,tree,fcache);
    delete fcache;
    return answer;
}
    
