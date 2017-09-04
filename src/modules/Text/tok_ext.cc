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
/*             Date   :  February 1997                                   */
/*-----------------------------------------------------------------------*/
/*                                                                       */
/* EST_Token extract                                                         */
/*                                                                       */
/* Extract tokens and related features from text files.                  */
/*                                                                       */
/* This is designed to find examples of particular tokens from large     */
/* text corpora (10s millions of words).  This initial use is for        */
/* Resolving homographs using David Yarowsky's techniques.               */
/*                                                                       */
/*=======================================================================*/
#include <cstdio>
#include "festival.h"
#include "lexicon.h"
#include "text.h"

static int rhc = 10;
static int lhc = 10;

static void search_file(const EST_String &filename, LISP tokens, LISP ofile);
static EST_Item *next_token(EST_TokenStream &ts, EST_Relation &ps,
				   EST_Item *s);
static void append_token(EST_Relation &ps, const EST_Token &s);
static void output_find(const EST_String &filename,
			EST_Item *s,LISP v,LISP tfeats, FILE* fd);

LISP extract_tokens(LISP file, LISP tokens, LISP ofile)
{
    // Extract occurrences of tokens in file 

    search_file(get_c_string(file),tokens, ofile);
    
    return NIL;
}

static void search_file(const EST_String &filename, LISP tokens, LISP ofile)
{
    // Step through the file token by token with a window of tokens
    // output info when current token matchs on of those required
    EST_TokenStream ts;
    EST_Relation ps;
    EST_Item *s = 0;
    LISP l,v;
    FILE *ofd;

    if (ts.open(filename) == -1)
    {
	cerr << "Extract_tokens: can't open file \"" <<
	    filename << "\" for reading\n";
	festival_error();
    }
    ts.set_PunctuationSymbols(EST_Token_Default_PunctuationSymbols);
    ts.set_PrePunctuationSymbols(EST_Token_Default_PrePunctuationSymbols);

    if (ofile == NIL)
	ofd = stdout;
    else if ((ofd = fopen(get_c_string(ofile),"a")) == NULL)
    {
	cerr << "extract_tokens: cannot open \"" << get_c_string(ofile)
	    << "\" for appending" << endl;
	festival_error();
    }

    for (s = next_token(ts,ps,s); s != 0; s=next_token(ts,ps,s))
    {
	for (l=tokens; l != NIL; l=cdr(l))
//	    if (s->name().matches(make_regex(get_c_string(car(car(l))))))
	{
	    v = leval(cons(car(car(l)),cons(siod(s),NIL)),NIL);
	    if (v != NIL)
		output_find(filename,s,v,car(l),ofd);
	}
    }

    ts.close();
    if (ofd != stdout)
	fclose(ofd);

}

static void output_find(const EST_String &filename,
			EST_Item *s,LISP v,LISP tfeats, FILE* fd)
{
    // Found a match so output info
    LISP t;
    
    fprintf(fd,"%s %s ",get_c_string(v),
	    (const char *)filename);
    for (t=cdr(tfeats); t != NIL; t=cdr(t))
	fprintf(fd,"%s ",(const char *)
		ffeature(s,get_c_string(car(t))).string());
    fprintf(fd,"\n");
}

static EST_Item *next_token(EST_TokenStream &ts, 
				   EST_Relation &ps,
				   EST_Item *s)
{
    // return next EST_Token as stream item extending right hand context
    // and deleting left hand context as required.
    EST_Item *ns;
    int i;

    if (s == 0)
    {
	// at start so fill rhc
	for (i=0; i < lhc; i++)
	    append_token(ps,EST_Token("*lhc*"));
	append_token(ps,ts.get());
	ns = ps.rlast();
	for (i=0; i < rhc; i++)
	    append_token(ps,ts.get());
	return ns;
    }

    // As token can be "" if there is trailing whitespace before eof
    // I ignore it.
    if ((!ts.eof()) && (ts.peek() != ""))
	append_token(ps,ts.get());
    remove_item(ps.first(),"Token");

    return inext(s);
}

static void append_token(EST_Relation &ps, const EST_Token &t)
{
    // Append s as stream cell on end of ps
    EST_Item *item = ps.append();

    item->set_name(t.string());
    item->set("filepos",t.filepos());
}
