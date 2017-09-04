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
/* Basic text utilities                                                  */
/*                                                                       */
/* This seems to be the only language specific part that cannot be       */
/* reasonably parameterized.  I'd like to change this but I'm not sure   */
/* of the best way.  Language-specific token processing module           */
/* generating Words (lexical items) from Tokens are current written as   */
/* FT_*_Token_Utt functions.  A language-independent one is available    */
/* FT_Any_Token_Utt which depends heavily on the lexicon can be used     */
/* when you don't have the language specific version.                    */
/*                                                                       */
/*=======================================================================*/
#include <cstdio>
#include "festival.h"
#include "text.h"

static void tts_raw_token(EST_Item *t);
static void tts_raw_utt(LISP utt);

LISP FT_Text_Utt(LISP utt)
{
    // Parse text into words 
    EST_Utterance *u = get_c_utt(utt);
    EST_String text;
    EST_TokenStream ts;
    LISP ws,punc,scs;
    EST_Token tok;

    *cdebug << "Text module\n";

    text = get_c_string(utt_iform(*u));

    u->create_relation("Token");

    ts.open_string(text);
    ts.set_SingleCharSymbols(EST_Token_Default_SingleCharSymbols);
    ts.set_PunctuationSymbols(EST_Token_Default_PunctuationSymbols);
    ts.set_PrePunctuationSymbols(EST_Token_Default_PrePunctuationSymbols);
    if ((ws = siod_get_lval("token.whitespace",NULL)) == NIL)
	ts.set_WhiteSpaceChars(EST_Token_Default_WhiteSpaceChars);
    else
	ts.set_WhiteSpaceChars(get_c_string(ws));
    if ((punc = siod_get_lval("token.punctuation",NULL)) == NIL)
	ts.set_PunctuationSymbols(EST_Token_Default_PunctuationSymbols);
    else
	ts.set_PunctuationSymbols(get_c_string(punc));
    if ((punc = siod_get_lval("token.prepunctuation",NULL)) == NIL)
	ts.set_PrePunctuationSymbols(EST_Token_Default_PrePunctuationSymbols);
    else
	ts.set_PrePunctuationSymbols(get_c_string(punc));
    if ((scs = siod_get_lval("token.singlecharsymbols",NULL)) == NIL)
	ts.set_SingleCharSymbols(EST_Token_Default_SingleCharSymbols);
    else
	ts.set_SingleCharSymbols(get_c_string(scs));

    for (ts >> tok; tok.string() != ""; ts >> tok)
	add_token(u,tok);

    return utt;
}

LISP tts_file(LISP filename,LISP mode)
{
    LISP user_text_modes,t_mode;

    user_text_modes = siod_get_lval("tts_text_modes",NULL);
    
    if ((mode == NIL) ||
	(streq(get_c_string(mode),"text")) ||
	(streq(get_c_string(mode),"fundamental")))
	tts_file_raw(filename);  // Simple text file 
    else 
    {
	t_mode = siod_assoc_str(get_c_string(mode),user_text_modes);
	if (t_mode == NIL)
	{
	    // Attempt to load it
	    leval(cons(rintern("request"),
		       cons(strintern(EST_String(get_c_string(mode))+
				      "-mode"),NIL)),NIL);
	    // get it again, and see if its defined
	    user_text_modes = siod_get_lval("tts_text_modes",NULL);
	}
	t_mode = siod_assoc_str(get_c_string(mode),user_text_modes);
	if (t_mode == NIL)
	{
	    cerr << "tts_file: can't find mode description \"" 
		<< get_c_string(mode) << "\" using raw mode instead" << endl;
	    tts_file_raw(filename);  // so read it as simple text file 
	}
	else
	    tts_file_user_mode(filename,car(cdr(t_mode)));
    }

    return NIL;
}

void tts_file_raw(LISP filename)
{
    // Say the contents of a named file 
    EST_TokenStream ts;
    LISP ws,prepunc,punc,scs;
    LISP lutt,eou_tree;
    LISP stream = NULL;


    stream = fopen_c(get_c_string(filename), "rb");
    if (ts.open(stream->storage_as.c_file.f, FALSE) == -1)
      {
	cerr << "tts_file: can't open file \"" << filename << "\"\n";
	festival_error();
      }
    ts.set_SingleCharSymbols(EST_Token_Default_SingleCharSymbols);
    ts.set_PunctuationSymbols(EST_Token_Default_PunctuationSymbols);
    ts.set_PrePunctuationSymbols(EST_Token_Default_PrePunctuationSymbols);
    if ((ws = siod_get_lval("token.whitespace",NULL)) == NIL)
	ts.set_WhiteSpaceChars(EST_Token_Default_WhiteSpaceChars);
    else
	ts.set_WhiteSpaceChars(get_c_string(ws));
    if ((punc = siod_get_lval("token.punctuation",NULL)) == NIL)
	ts.set_PunctuationSymbols(EST_Token_Default_PunctuationSymbols);
    else
	ts.set_PunctuationSymbols(get_c_string(punc));
    if ((prepunc = siod_get_lval("token.prepunctuation",NULL)) == NIL)
	ts.set_PrePunctuationSymbols(EST_Token_Default_PrePunctuationSymbols);
    else
	ts.set_PrePunctuationSymbols(get_c_string(prepunc));
    if ((scs = siod_get_lval("token.singlecharsymbols",NULL)) == NIL)
	ts.set_SingleCharSymbols(EST_Token_Default_SingleCharSymbols);
    else
	ts.set_SingleCharSymbols(get_c_string(scs));
    eou_tree = siod_get_lval("eou_tree","No end of utterance tree set");

    lutt = tts_chunk_stream(ts,tts_raw_token,tts_raw_utt,eou_tree,0);
    
    // The last one is returned because the chunker doesn't know if this
    // is truly the end of an utterance or not, but here we do know.
    tts_raw_utt(lutt);

    ts.close();
    if (stream)
      fclose_l(stream);
}

static void tts_raw_token(EST_Item *t)
{
    // Do something to token, in this case nothing
    (void)t;
}

static void tts_raw_utt(LISP utt)
{
    // Do (simple) tts on this utt
    LISP lutt;

    // There are some pessimal cases when the utterance is empty
    if ((utt == NIL) ||
	(get_c_utt(utt)->relation("Token")->length() == 0))
	return;   // in this case do nothing.

    lutt = quote(utt);
    lutt = cons(rintern("apply_hooks"),
		cons(rintern("tts_hooks"),
		     cons(lutt,NIL)));



    lutt = cons(rintern("set!"),
		cons(rintern("utt_tts"),
		     cons(lutt,NIL)));

    // Synth and Play it
    lutt = leval(lutt,NIL);
    user_gc(NIL);
}

LISP new_token_utt(void)
{
    // An empty utterance ready to take Tokens
    EST_Utterance *u = new EST_Utterance;
    u->f.set("type","Tokens");
    u->create_relation("Token");
    return siod(u);
}

LISP tts_chunk_stream(EST_TokenStream &ts,
		      TTS_app_tok app_tok, 
		      TTS_app_utt app_utt,
		      LISP eou_tree,    
		      LISP utt)
{
    // Get tokens from ts and cummulate them in u.  
    // Apply app_tok to each token
    // Apply app_utt to each utt signalled
    // Return untermitated utterance potentially for next call
    // Uses the wagon tree eou_tree to predict utterance termination on
    // penultimate token.
    EST_Item *tok, *ebo;
    EST_Token t;
    if (utt == NIL)
	utt = new_token_utt();
    EST_Utterance *u = get_c_utt(utt);

    while (!ts.eof())
    {
	t = ts.get();
	tok = add_token(u,t);
	app_tok(tok);     // do what you do with the token
	ebo = iprev(as(tok,"Token"));  // end but one token
	if ((ebo != 0) &&
	    (wagon_predict(ebo,eou_tree) == 1))
	{
	    // Remove that extra token
	    remove_item(tok,"Token");
	    app_utt(utt);  // do what you do with the utt
	    utt = new_token_utt();
	    u = get_c_utt(utt);
	    add_token(u,t);  // add that last token to the new utt.
	}
    }

    return utt;
}

#if 0
LISP memon(void)
{
    printf("memon\n");
    putenv("MALLOC_TRACE=mallfile");
    mtrace();
    return NIL;
}

LISP memoff(void)
{
    muntrace();
    printf("memoff\n");
    return NIL;
}
#endif

void festival_Text_init(void)
{
    festival_token_init();
    festival_def_utt_module("Text",FT_Text_Utt,
    "(Text UTT)\n\
  From string in input form tokenize and create a token stream.");
    init_subr_2("tts_file",tts_file,
    "(tts_file FILE MODE)\n\
  Low level access to tts function, you probably want to use the function\n\
  tts rather than this one.  Render data in FILE as speech.  Respect\n\
  MODE.  Currently modes are defined through the variable tts_text_modes.");
#if 0
    init_subr_0("memon",memon,
		"(tts_file FILE MODE)");
    init_subr_0("memoff",memoff,
		"(tts_file FILE MODE)");
#endif
    init_subr_3("extract_tokens",extract_tokens,
    "(extract_tokens FILE TOKENS OUTFILE)\n\
  Find all occurrences of TOKENS in FILE and output specified context around\n\
  the token.  Results are appended to OUTFILE, if OUTFILE is nil, output\n\
  goes to stdout.");
}

