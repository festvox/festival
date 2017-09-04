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
/*             Date   :  August 1997                                     */
/*-----------------------------------------------------------------------*/
/*                                                                       */
/*  There are just too many different versions of sgml-based mark up     */
/*  and none of them are stable so this is allows arbitrary              */
/*  of tags to Lisp functions so any of them can be implemented in Lisp  */
/*  That is people can worry about the actual content later and do not   */
/*  need to change the C++                                               */
/*                                                                       */
/*  Of course once I give you this functionality you'll just want more   */
/*                                                                       */
/*=======================================================================*/
#include "EST_unix.h"
#include "festival.h"
#include "text.h"
#include "lexicon.h"

static LISP xxml_get_attribute(const EST_String &remainder);
static char *xxml_process_line(const char *line);
static void tts_xxml_token(EST_Item *t);
static void tts_xxml_utt(LISP lutt);

static LISP xxml_word_features = NIL;
static LISP xxml_token_hooks = NIL;

void tts_file_xxml(LISP filename)
{
    // For stml ssml rsml and jsml etc
    // filename contains *output* from something like nsgml
    EST_String inname = get_c_string(filename);
    EST_String line, type, remainder;
    EST_TokenStream ts;
    LISP atts, element_defs;
    LISP utt = NIL;   // for cummulation of tokens

    if (ts.open(inname) == -1)
    {
	cerr << "xxml: unable to open output from SGML parser" << endl;
	festival_error();
    }
    ts.set_WhiteSpaceChars(" \t\r\n");
    ts.set_SingleCharSymbols("");
    ts.set_PunctuationSymbols("");
    ts.set_PrePunctuationSymbols("");

    element_defs = siod_get_lval("xxml_elements",NULL);
    atts = NIL;

    if (ts.peek() != get_c_string(car(car(element_defs))))
    {
	cerr << "xxml parse error: " << get_c_string(filename) <<
	    " Expected " << get_c_string(car(car(element_defs)))
		<< " but found " << ts.peek() << endl;
	festival_error();
    }
    while (ts.peek() != get_c_string(car(car(cdr(element_defs)))))
    {
	if (ts.eof())
	{
	    cerr << "xxml parse error: unexpected end of file \n";
	    festival_error();
	}
	line = (EST_String)ts.get_upto_eoln();
	type = line.at(0,1);
	remainder = line.after(0);
	if (type == "-")
	{   //  Segments into utterances as it goes along
	    utt = xxml_get_tokens(remainder,
				  siod_get_lval("xxml_word_features",NULL),
				  utt);
	}
	else if (type == "A")        // general attribute
	{
	    atts = cons(xxml_get_attribute(remainder),atts);
	}
	else if ((type == "(") || (type == ")"))
	{   
	    utt = xxml_call_element_function(type+remainder,atts,
					     element_defs,utt);
	    atts = NIL;
	}
	else
	{
	    cerr << "xxml parse error: unexpected token found " 
		<< line << endl;
	    festival_error();
	}
    }
    // Last call (should synthesize trailing tokens
    utt = xxml_call_element_function(ts.get().string(),atts,element_defs,utt);

    ts.close();
}

LISP xxml_call_element_function(const EST_String &element,
				LISP atts, LISP elements, LISP utt)
{
    // Form the call to the defined element function, with the attributes
    // and the utterance, returns the utterance
    LISP def,l;

    def = siod_assoc_str(element,elements);

    if (def != NIL)
    {
	// You get two arguments, ATTLIST and UTTERANCE
	l = cons(
	    make_param_lisp("ATTLIST",
			    cons(rintern("quote"),cons(atts,NIL))),
	    cons(
	    make_param_lisp("UTT",
			    cons(rintern("quote"),cons(utt,NIL))),
		 NIL));
	return leval(cons(rintern("let"),
			  cons(l,cdr(cdr(def)))),NIL);
    }
    else   // no definition to do nothing
	return utt;
}

static LISP xxml_get_attribute(const EST_String &remainder)
{
    EST_TokenStream ts;
    LISP tokens=NIL,att=NIL;
    EST_String name;
    EST_Token t;

    ts.open_string(remainder);
    name = (EST_String)ts.get();
    if ((t=ts.get()) == "IMPLIED")
	att = cons(rintern(name),cons(NIL,NIL));
    else if (t == "TOKEN")
    {
	EST_Token v = ts.get();
	att = cons(rintern(name),cons(cons(rintern(v.string()),NIL),NIL));
    }
    else if (t == "CDATA")
    {
	while (!ts.eof())
	    tokens = cons(rintern(ts.get().string()),tokens);
	att = cons(rintern(name),cons(reverse(tokens),NIL));
    }
    else
    {
	cerr << "XXML: unknow attribute type " << remainder << endl;
	festival_error();
    }

    ts.close();
    return att;
}

static char *xxml_process_line(const char *line)
{
    // STML (sgml) data line have a number of special escape characters
    // this undoes them, namely "\\n" to "\n"
    char *procline = walloc(char,strlen(line)+1);
    int i,j;

    for (i=j=0; line[i] != '\0'; j++,i++)
    {
	if (line[i] == '\\')
	{
	    i++;
	    if (line[i] == 'n')
		procline[j] = '\n';
	    else if (line[i] == '\\')
		procline[j] = '\\';
	    else if ((line[i] == '0') || // its an octal number 
		     (line[i] == '1'))
	    {
		int k,oct = 0;
		for (k=0; k < 3; k++,i++)
		    oct = (oct*8)+(line[i]-'0');
		procline[j] = oct;
		i--;
	    }
	    else
	    {
		procline[j] = line[i]; // no change
		i--;
	    }
	}
	else
	    procline[j] = line[i]; // no change
    }
    procline[j] = '\0';
    return procline;
}

static void tts_xxml_token(EST_Item *t)
{
    // Add xxml_word features to t
    LISP a;

    for (a=xxml_word_features; a != NIL; a=cdr(a))
	if ((car(cdr(car(a))) != NIL) &&
	    (!streq("NAME",get_c_string(car(car(a))))))
	{
	    if (cdr(cdr(car(a))) == NIL)
		t->set(get_c_string(car(car(a))),
		       get_c_string(car(cdr(car(a)))));
	    else
	    {
		// Its more complex than a single atom so save the list
		t->set(get_c_string(car(car(a))),
		       siod_sprint(car(cdr(car(a)))));
	    }
	}

    apply_hooks(xxml_token_hooks,siod(t));
}

LISP xxml_get_tokens(const EST_String &line,LISP feats,LISP utt)
{
    // Read from here until end of line collects all the tokens 
    // Note tokens are in reverse order until they are made into an
    // utterance
    EST_TokenStream ls;
    EST_Token t;
    LISP eou_tree;
    char *processed_line;
    processed_line = xxml_process_line(line);
    ls.open_string(processed_line);
    ls.set_SingleCharSymbols(
        get_c_string(siod_get_lval("token.singlecharsymbols",
				   "token.singlecharsymbols unset")));
    ls.set_PunctuationSymbols(
        get_c_string(siod_get_lval("token.punctuation",
				   "token.punctuation unset")));
    ls.set_PrePunctuationSymbols(
        get_c_string(siod_get_lval("token.prepunctuation",
				   "token.prepunctuation unset")));
    ls.set_WhiteSpaceChars(
        get_c_string(siod_get_lval("token.whitespace",
				   "token.whitespace unset")));

    eou_tree = siod_get_lval("eou_tree","No end of utterance tree set");

    xxml_word_features = feats;
    xxml_token_hooks = siod_get_lval("xxml_token_hooks",NULL);
    
    // Segment and synth as much as appropriate
    utt = tts_chunk_stream(ls,tts_xxml_token,tts_xxml_utt,eou_tree,utt);

    return utt;
}

static void tts_xxml_utt(LISP lutt)
{
    // Build and utterance with these tokens and apply xxml synth function

    if ((lutt == NIL) ||
	(get_c_utt(lutt)->relation("Token")->length() == 0))
	return;   // in this case do nothing.

    leval(cons(rintern("xxml_synth"),
	       cons(quote(lutt),NIL)),NIL);
}    

