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
/*                      Date   :  June 1997                              */
/*-----------------------------------------------------------------------*/
/*                                                                       */
/* An input method for much more general info on tokens                 */
/*                                                                       */
/*=======================================================================*/

#include <cstdio>
#include "festival.h"
#include "modules.h"
#include "text.h"

static EST_Item *make_phrase_node(EST_Utterance *u,
					 const EST_String &name,
					 LISP feats);
static EST_Item *make_token_node(EST_Utterance *u,
					const EST_String &name,
					LISP feats);

void create_phraseinput(EST_Utterance *u)
{
    // Build from phrase input form (phrase, tokens, segs)
    LISP l,ptree,t;
    EST_Item *phrase,*token;

    ptree = utt_iform(*u);
    
    u->create_relation("Phrase");
    u->create_relation("Token");

    for (l=ptree; l != NIL; l=cdr(l))
    {
	if (streq("Phrase",get_c_string(car(car(l)))))
	{
	    phrase = make_phrase_node(u,"Phrase",car(cdr(car(l))));
	    for (t=cdr(cdr(car(l))); t != NIL; t=cdr(t))
	    {
		if (consp(car(t)))
		    token = make_token_node(u,get_c_string(car(car(t))),
					    car(cdr(car(t))));
		else
		    token = make_token_node(u,get_c_string(car(t)),NIL);
		append_daughter(phrase,token);
	    }
	}
	else  // no explicit phrase marker
	{
	    cerr << "PhrInfo: malformed input form." << endl;
	    festival_error();
	}
    }
}

static EST_Item *make_phrase_node(EST_Utterance *u,
				       const EST_String &name,
				       LISP feats)
{
    // Create a phrase node with name and features
    EST_Item *p;

    p = add_phrase(u);
    p->set_name(name);
    add_item_features(p,feats);
    return p;
}

static EST_Item *make_token_node(EST_Utterance *u, 
				      const EST_String &name,
				      LISP feats)
{
    // Create a token node with name and features
    EST_Token t = name;
    EST_Item *li = add_token(u,t);
    LISP f;

    for (f=feats; f != NIL; f=cdr(f))
    {
	const char *nname = get_c_string(car(car(f)));
	if (streq(nname,"punctuation"))
	    li->set("punc",get_c_string(car(cdr(car(f)))));
	else
	    li->set(nname,get_c_string(car(cdr(car(f)))));
    }

    return li;
}

