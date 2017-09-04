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
/*             Date   :  November 1996                                   */
/*-----------------------------------------------------------------------*/
/*                                                                       */
/* Tokenizing                                                            */
/*                                                                       */
/* This provides tokenizing methods for tokens into words.  All that     */
/* special rules stuff for analysizing numbers, dates, acronyms etc.     */
/* Much of this is still too specific and although easy to add to it     */
/* be better if the rules could be specified externally                  */
/*                                                                       */
/* Note only English tokenization has any substance at present           */
/*                                                                       */
/*=======================================================================*/
#include <cstdio>

using namespace std;

#include "festival.h"
#include "lexicon.h"
#include "modules.h"
#include "text.h"
#include "tokenP.h"

static EST_Regex numpointnum("[0-9]*\\.[0-9]+");
static EST_Regex RXintcommaed("[0-9][0-9]?[0-9]?,\\([0-9][0-9][0-9],\\)*[0-9][0-9][0-9]\\(\\.[0-9]+\\)?");
static EST_Regex RXintord("[0-9]*\\(1st\\|2nd\\|3rd\\|[0-9]th\\)");
static EST_Regex RXdottedabbrev("\\([A-Za-z]\\.\\)+[A-Za-z]\\.?");
static EST_Regex RXapostropheS(".*'[sS]$");
static EST_String PunctuationChars("'`.,:;!?{}[]()-\"");
static EST_Regex RXpunctuation("\\(\\]\\|[-[.,!?]\\)+");
static EST_String remove_punct(const EST_String &tok);
static int only_punc(const EST_String &tok);
static LISP num_2_words(int iword);
static LISP say_num_as_words(const EST_String &num);
static LISP word_it(EST_Item *t,const EST_String tok);
static LISP builtin_word_it(EST_Item *token, EST_String tok);
static LISP say_as_letters(const EST_String &word);
static LISP say_num_as_ordinal(const EST_String &num);
static LISP say_num_as_year(const EST_String &num);
static LISP say_as_digits(const EST_String &word);

LISP FT_English_Token_Utt(LISP utt);
LISP FT_Welsh_Token_Utt(LISP utt);
LISP FT_Spanish_Token_Utt(LISP utt);
LISP FT_Any_Token_Utt(LISP utt);

static LISP user_token_to_word_func = NIL;

LISP FT_Welsh_Token_Utt(LISP utt)
{
    return FT_Any_Token_Utt(utt);
}

LISP FT_Spanish_Token_Utt(LISP utt)
{
    (void)utt;
    cerr << "TOKEN: Spanish tokenization not yet supported\n";
    festival_error();

    // never happens
    return NULL;
}

LISP FT_Any_Token_Utt(LISP utt)
{
    // Language independent EST_Token to Word module.  Uses user specified
    // token to word function of simply creates a word for each token.
    EST_Utterance *u = get_c_utt(utt);
    LISP words,w;
    EST_Item *t;
    EST_Item *new_word;

    user_token_to_word_func = siod_get_lval("token_to_words",NULL);
    u->create_relation("Word");

    for (t=u->relation("Token")->first(); t != 0; t = inext(t))
    {
	if (user_token_to_word_func != NIL)
	{
	    words = word_it(t,t->name());
	    for (w=words; w != NIL; w=cdr(w))
	    {
		new_word = add_word(u,car(w));
		append_daughter(t,"Token",new_word);
	    }
	}
	else
	{   // No user token_to_word function so just do it directly
	    new_word = add_word(u,t->name());
	    append_daughter(t,"Token",new_word);
	}
    }
    user_token_to_word_func = NIL;  // reset this

    return utt;
}

LISP FT_English_Token_Utt(LISP utt)
{
    // This module generates a word stream from a token stream
    // Tokens may go to zero or more words, tokens retain information 
    // about their punctuation and spacing on the page.
    //  Preceeding and succeeding punctuation become words
    EST_Utterance *u = get_c_utt(utt);
    EST_Item *t;
    LISP words,w,eou_tree,l;
    EST_Item *new_word;

    *cdebug << "Token module (English)" << endl;

    eou_tree = siod_get_lval("eou_tree","No end of utterance tree");
    user_token_to_word_func = siod_get_lval("token_to_words",NULL);
    u->create_relation("Word");

    for (t=u->relation("Token")->first(); t != 0; t = inext(t))
    {
	words = word_it(t,t->name());
	// Initial punctuation becomes words
	new_word = 0;
	if ((t->f("prepunctuation") != "0") &&
	    (t->f("prepunctuation") != ""))
	{
	    l = symbolexplode(strintern(t->f("prepunctuation").string()));
	    for (w=l; w != NIL; w=cdr(w))
	    {
		new_word = add_word(u,car(w));
		append_daughter(t,"Token",new_word);
	    }
	}

	// Words become words
	for (w=words; w != NIL; w=cdr(w))
	{
	    new_word = add_word(u,car(w));
	    append_daughter(t,"Token",new_word);
	}

	// final word gets punctuation marking
	if ((new_word != 0) && (ffeature(t,"punc") != "0"))
	{
	    if ((ffeature(t,"punc") == ".") &&
		(wagon_predict(t,eou_tree) == 0))
	    {   // It wasn't a really punctuation mark
		t->set("punc","0");
	    }
	    else
	    {
		l = symbolexplode(strintern(ffeature(t,"punc").string()));
		for (w=l; w != NIL; w=cdr(w))
		{
		    new_word = add_word(u,car(w));
		    append_daughter(t,"Token",new_word);
		}
	    }
	}
    }

    user_token_to_word_func = NIL;

    return utt;

}

static LISP word_it(EST_Item *token, const EST_String tok)
{
    // The user may specify their own addition;a token to word rules
    // through the variable user_token_to_word_func if so we must
    // call that, which may or may not call the builtin version
    // The builtin version if bound in LISP so that recursion works
    // properly
    // This takes a LISP utt as an argument as creating a new wraparound
    // will cause gc to fail.
    LISP tok_string = strcons(tok.length(),tok);

    if (user_token_to_word_func != NIL)            // check user's rules
	return leval(cons(user_token_to_word_func,
			  cons(siod(token),
			       cons(tok_string,NIL))),NIL);
    else
	return builtin_word_it(token,tok);
}

static LISP builtin_word_it(EST_Item *token, EST_String tok)
{
    // Return a list of words for this token 
    EST_String token_pos;

    if (tok == "")
	return NIL;
    else if (in_current_lexicon(downcase(tok),NIL))  // if in lexicon use as is
    {
	if ((tok != token->name()) && // mainly to catch internal "a"
	    (tok.length() == 1))
	{
	    LISP let_pos = siod_get_lval("token.letter_pos",NULL);
	    return cons(cons(make_param_str("name",tok),
			     cons(make_param_lisp("pos",let_pos),NIL)),
			NIL);
	}
	else
	    return cons(strintern(tok),NIL);
    }
    else if ((token_pos = (EST_String)ffeature(token,"token_pos")) == "ordinal")
	return say_num_as_ordinal(tok);
    else if (token_pos == "year")
	return say_num_as_year(tok);
    else if ((token_pos == "digits") ||
	     (tok.matches(make_regex("0[0-9]+"))))
	return say_as_digits(tok);
    else if (tok.matches(RXint))
	return say_num_as_words(tok);
    else if (tok.matches(RXintord))
	return say_num_as_ordinal(tok.at(0,tok.length()-2));
    else if (tok.matches(RXintcommaed))  // containing commas at thousands
    {
	if (tok.contains("."))
	    return word_it(token,remove_punct(tok.before("."))+
			   "."+tok.after("."));
	else
	    return say_num_as_words(remove_punct(tok));
    }
    else if (tok.matches(RXapostropheS))
    {
	return append(word_it(token,tok.at(0,tok.length()-2)),
			cons(strintern("'s"),NIL));
    }
    else if (tok.matches(numpointnum))
    {
	EST_String afterpoint = tok.after(".");
	LISP ap = NIL;
	int i;
	for (i=0; i < afterpoint.length(); i++)
	     ap = append(say_num_as_words(afterpoint.at(i,1)),ap);
	return append(say_num_as_words(tok.before(".")),
			cons(strintern("point"),reverse(ap)));
    }
    else if ((tok.matches(make_regex("[A-Z][A-Z]+"))) &&
	     ((!tok.contains(make_regex("[AEIOUY]"))) ||
	      ((!tok.contains(make_regex("[^AEIOU][AEIOU][^AEIOU]"))) &&
	       (tok.length() < 5))))       // an acronym
	return say_as_letters(tok);
    else if (tok.matches(RXdottedabbrev))
	return say_as_letters(remove_punct(tok));
    else if ((tok.matches(RXalpha)) &&
	     !(tok.matches(make_regex(".*[AEIOUYaeiouy].*"))))
	return say_as_letters(tok);   // no vowels so spell it
    else if (tok.matches(RXalpha))   // as is, some sort of word
	return cons(strintern(tok),NIL);
    else if (only_punc(tok))
	return stringexplode(tok);
    else if (tok.contains("-"))
	return append(word_it(token,tok.before("-")),
		      word_it(token,tok.after("-")));
    else if (tok.contains(".")) // internet address
    {
	LISP a=NIL;
	EST_String tok2 = tok;
	for ( ; tok2.contains("."); tok2 = tok2.after("."))
	    a = append(a,
			 append(word_it(token,tok2.before(".")),
				  cons(strintern("dot"),NIL)));
	a = append(a,word_it(token,tok2));
	return a;
    }
    else if (tok.contains("/")) 
	return append(word_it(token,tok.before("/")),
		      cons(strintern("slash"),
			   word_it(token,tok.after("/"))));
    else if (tok.contains("&")) 
	return append(word_it(token,tok.before("&")),
		      cons(strintern("ampersand"),
			   word_it(token,tok.after("&"))));
    else if (tok.contains("_")) 
	return append(word_it(token,tok.before("_")),
		      cons(strintern("underscore"),
			   word_it(token,tok.after("_"))));
    else if (tok.contains("'"))
	return word_it(token,tok.before("'")+tok.after("'"));
    else if (tok.contains("`"))
	return append(word_it(token,tok.before("`")),
			word_it(token,tok.after("`")));
    else if (tok.contains("\""))
	return append(word_it(token,tok.before("\"")),
			word_it(token,tok.after("\"")));
    else if (tok.contains(","))
	return append(word_it(token,tok.before(",")),
			word_it(token,tok.after(",")));
    else if (tok.contains("("))
	return append(word_it(token,tok.before("(")),
			word_it(token,tok.after("(")));
    else if (tok.contains(")"))
	return append(word_it(token,tok.before(")")),
			word_it(token,tok.after(")")));
    else if (tok.matches(make_regex("^[^a-zA-Z].+"))) // incrementally remove 
	return append(say_as_letters(tok.at(0,1)),// num/symbol from front
			word_it(token,tok.at(1,tok.length()-1)));
    else if (tok.matches(make_regex(".+[^a-zA-Z]$"))) // incrementally remove rear
	return append(word_it(token,tok.at(0,tok.length()-1)),
			say_as_letters(tok.at((int)tok.length()-1,1)));
    else  // could try harder
	return say_as_letters(remove_punct(tok));
}

static LISP say_as_digits(const EST_String &word)
{
    // Should be string of digits, but I wont require it
    // This isn't really correct for telephone numbers (oh/zero/double)
    LISP l;
    LISP lets = stringexplode(word);
    LISP let_pos = siod_get_lval("token.letter_pos",NULL);

    for (l=lets; l != NIL; l=cdr(l))
    {
	if (streq(get_c_string(car(l)),"0"))
	    CAR(l) = strintern("zero");
	else if (streq(get_c_string(car(l)),"1"))
	    CAR(l) = strintern("one");
	else if (streq(get_c_string(car(l)),"2"))
	    CAR(l) = strintern("two");
	else if (streq(get_c_string(car(l)),"3"))
	    CAR(l) = strintern("three");
	else if (streq(get_c_string(car(l)),"4"))
	    CAR(l) = strintern("four");
	else if (streq(get_c_string(car(l)),"5"))
	    CAR(l) = strintern("five");
	else if (streq(get_c_string(car(l)),"6"))
	    CAR(l) = strintern("six");
	else if (streq(get_c_string(car(l)),"7"))
	    CAR(l) = strintern("seven");
	else if (streq(get_c_string(car(l)),"8"))
	    CAR(l) = strintern("eight");
	else if (streq(get_c_string(car(l)),"9"))
	    CAR(l) = strintern("nine");
	else
	    CAR(l) = cons(make_param_lisp("name",car(l)),
			  cons(make_param_lisp("pos",let_pos),NIL));
    }
    
    return lets;
}

static LISP say_as_letters(const EST_String &word)
{
    // Explode letters in word and say them, marking them as nouns
    // This is particularly designed so that A/a doesn't come out as 
    // the determiner a which in typically schwa'd
    LISP l;
    LISP lets = stringexplode(word);
    LISP let_pos = siod_get_lval("token.letter_pos",NULL);

    for (l=lets; l != NIL; l=cdr(l))
    {
	EST_String name = EST_String(get_c_string(car(l)));
	if (name.matches(make_regex("[0-9]")))
	    CAR(l) = car(say_as_digits(get_c_string(car(l))));
//	else if (name.matches(make_regex("[^a-zA-Z]")))
//	    // Not sure, probably a bug to get here
//	    CAR(l) = cons(make_param_str("name","symbol"),
//			  cons(make_param_lisp("pos",let_pos),NIL));
	else
	    CAR(l) = cons(make_param_lisp("name",car(l)),
			  cons(make_param_lisp("pos",let_pos),NIL));
    }

    return lets;
}

static int only_punc(const EST_String &tok)
{
    // If this token consists solely of punctuation chars
    // If this is true I'm probably suppose to say some of them
    int i;
    EST_String np;
    const char *tokch = tok;

    for (i=0; i<tok.length(); i++)
      if (strchr((const char *)PunctuationChars,tokch[i]) == NULL)
	    return FALSE;

    return TRUE;
}

static EST_String remove_punct(const EST_String &tok)
{
    EST_String np(tok);

    np.make_updatable();

    np.gsub(RXpunctuation, "");

    return np;
}
    

static LISP say_num_as_ordinal(const EST_String &num)
{
    LISP numwords = num_2_words(atoi(num));
    LISP last;
    
    // Now change the last word to the appropriate ordinal
    for (last=numwords; cdr(last) != NIL; last=cdr(last));
    const char *lastword = get_c_string(car(last));
    
    if (streq(lastword,"zero"))  
	CAR(last) = strintern("zeroth");
    else if (streq(lastword,"one"))  
	CAR(last) = strintern("first");
    else if (streq(lastword,"two"))  
	CAR(last) = strintern("second");
    else if (streq(lastword,"three"))  
	CAR(last) = strintern("third");
    else if (streq(lastword,"four"))  
	CAR(last) = strintern("fourth");
    else if (streq(lastword,"five"))  
	CAR(last) = strintern("fifth");
    else if (streq(lastword,"six"))  
	CAR(last) = strintern("sixth");
    else if (streq(lastword,"seven"))  
	CAR(last) = strintern("seventh");
    else if (streq(lastword,"eight"))  
	CAR(last) = strintern("eighth");
    else if (streq(lastword,"nine"))  
	CAR(last) = strintern("ninth");
    else if (streq(lastword,"ten"))  
	CAR(last) = strintern("tenth");
    else if (streq(lastword,"eleven"))  
	CAR(last) = strintern("eleventh");
    else if (streq(lastword,"twelve"))  
	CAR(last) = strintern("twelfth");
    else if (streq(&lastword[strlen(lastword)-4],"teen"))
	CAR(last) = strintern(EST_String(lastword)+"th");
    else if (streq(&lastword[strlen(lastword)-2],"ty"))
	CAR(last) = strintern(EST_String(lastword).before("ty")+"tieth");
    else if (streq(lastword,"hundred"))  
	CAR(last) = strintern("hundredth");
    else if (streq(lastword,"thousand"))  
	CAR(last) = strintern("thousandth");
    else if (streq(&lastword[strlen(lastword)-6],"illion"))
	CAR(last) = strintern(EST_String(lastword)+"th");
    else
    {
	// I don't think I've forgotten anything 
	*cdebug << "Token: can't make ordinal from \"" << lastword 
	    << "\"" << endl;
	CAR(last) = strintern(EST_String(lastword)+"th");
    }

    return numwords;
}

static LISP say_num_as_words(const EST_String &num)
{
    if (num.length() > 9)
    {
	if (num(0) == '-')
	    return cons(strintern("minus"),say_as_digits(num.after("-")));
	else
	    return say_as_digits(num);
    }
    else	    
	return num_2_words(atoi(num));
}

static LISP say_num_as_year(const EST_String &num)
{
    int iword = atoi(num);

    if (num.length() > 4) 
	return say_num_as_words(num);
    else if (num.matches(make_regex("00")))
    {
	return cons(strintern("o"),
		    cons(strintern("o"),NIL));
    }
    else if (num.matches(make_regex("0[0-9]")))
    {
	return cons(strintern("o"),
		    num_2_words(iword));
    }
    else if (iword < 100)
	return num_2_words(iword);
    else if ((iword % 1000) < 10)
    {
	if ((iword % 1000) == 0)
	    return append(num_2_words(iword/1000),
			    cons(strintern("thousand"),NIL));
	else
	    return append(num_2_words(iword/1000),
			    cons(strintern("thousand"),
				 cons(strintern("and"),
				      num_2_words(iword%1000))));
    }
    else if ((iword % 100) == 0)
	return append(num_2_words(iword/100),
			cons(strintern("hundred"),NIL));
    else if ((iword % 100) < 10)
	return append(num_2_words(iword/100),
			cons(strintern("o"),
			     num_2_words(iword%100)));
    else 
	return append(num_2_words(iword/100),
			num_2_words(iword%100));
}
    
static LISP num_2_words(int iword)
{
    // Convert number of list of words
    int tens, units;
    LISP s_tens, lang_stype=NIL;
    
    if (iword < 0)
	return cons(strintern("minus"),num_2_words(-iword));
    else if (iword < 20)
	switch (iword) 	{
	  case 0: return cons(strintern("zero"),NIL);
          case 1: return cons(strintern("one"),NIL);
	  case 2: return cons(strintern("two"),NIL);
	  case 3: return cons(strintern("three"),NIL);
	  case 4: return cons(strintern("four"),NIL);
	  case 5: return cons(strintern("five"),NIL);
	  case 6: return cons(strintern("six"),NIL);
	  case 7: return cons(strintern("seven"),NIL);
	  case 8: return cons(strintern("eight"),NIL);
	  case 9: return cons(strintern("nine"),NIL);
	  case 10: return cons(strintern("ten"),NIL);
	  case 11: return cons(strintern("eleven"),NIL);
	  case 12: return cons(strintern("twelve"),NIL);
	  case 13: return cons(strintern("thirteen"),NIL);
	  case 14: return cons(strintern("fourteen"),NIL);
	  case 15: return cons(strintern("fifteen"),NIL);
	  case 16: return cons(strintern("sixteen"),NIL);
	  case 17: return cons(strintern("seventeen"),NIL);
	  case 18: return cons(strintern("eighteen"),NIL);
	  case 19: return cons(strintern("nineteen"),NIL);
	  default: return cons(siod_get_lval("token.unknown_word_name",NULL),
			       NIL);
      }
    else if (iword < 100)
    {
	tens = iword / 10;
	units = iword % 10;
	switch (tens)
	{
	  case 2: s_tens = strintern("twenty"); break;
	  case 3: s_tens = strintern("thirty"); break;
	  case 4: s_tens = strintern("forty"); break;
	  case 5: s_tens = strintern("fifty"); break;
	  case 6: s_tens = strintern("sixty"); break;
	  case 7: s_tens = strintern("seventy"); break;
	  case 8: s_tens = strintern("eighty"); break;
	  case 9: s_tens = strintern("ninety"); break;
	  default: return cons(siod_get_lval("token.unknown_word_name",NULL),
			       NIL);
	}
        if (units != 0)
	    return cons(s_tens,num_2_words(units));
	else
	    return cons(s_tens,NIL);
    }
    else if (iword < 1000)
    {
	lang_stype = ft_get_param("Language");
	if (streq("americanenglish",get_c_string(lang_stype)))
	    return append(num_2_words(iword/100),
			    cons(strintern("hundred"),
				 (((iword % 100) != 0) ?
				  num_2_words(iword % 100) :
				  NIL)));
	else
	    return append(num_2_words(iword/100),
			    cons(strintern("hundred"),
				 (((iword % 100) != 0) ?
				  cons(strintern("and"),
				       num_2_words(iword % 100)) :
				  NIL)));
    }
#if 0
    // We don't depend on this hack now.
    else if ((iword > 1910) &&
	     (iword < 2000))   // hacky date condition
	return append(num_2_words(iword/100),
			num_2_words(iword%100));
#endif
    else if (iword < 1000000)
	return append(num_2_words(iword/1000),
			cons(strintern("thousand"),
			     (((iword % 1000) != 0) ?
			      ((((iword % 1000)/100) == 0) ?
			       cons(strintern("and"),num_2_words(iword % 1000)):
			       num_2_words(iword % 1000)) :
			      NIL)));
    else if (iword >= 1000000)
	return append(num_2_words(iword/1000000),
			cons(strintern("million"),
			     (((iword % 1000000) != 0) ?
			      num_2_words(iword % 1000000) :
			      NIL)));
    else 
	return cons(strintern("bignum"),NIL);
}

static LISP l_word_it(LISP token, LISP tok)
{
    // Lisp wrap around for word_it
    EST_Item *t = get_c_item(token);
    EST_String tok_name = get_c_string(tok);

    return builtin_word_it(t,tok_name);
}

void festival_token_init(void)
{
    festival_def_utt_module("Token_English",FT_English_Token_Utt,
    "(Token_English UTT)\n\
  Build a Word stream from the Token stream, for English (American and\n\
  British English), analyzing compound words, numbers, etc. as tokens\n\
  into words.");
    festival_def_utt_module("Token_Welsh",FT_Welsh_Token_Utt,
    "(Token_Welsh UTT)\n\
  Build a Word stream from the Token stream, for Welsh, analyzing\n\
  compound words, numbers etc as tokens into words.");
    festival_def_utt_module("Token_Spanish",FT_Spanish_Token_Utt,
    "(Token_Spanish UTT)\n\
  Build a Word stream from the Token stream, for Castillian Spanish,\n\
  analyzing compound words, numbers etc as tokens into words.");
    festival_def_utt_module("Token_Any",FT_Any_Token_Utt,
    "(Token_Any UTT)\n\
  Build a Word stream from the Token stream, in a language independent way,\n\
  which means that all simple tokens should be in the lexicon, or analysed\n\
  by letter to sound rules.");
    festival_def_utt_module("Token_POS",FT_Token_POS_Utt,
    "(Token_POS UTT)\n\
  Assign feature token_pos to tokens thats match CART trees in the\n\
  variable token_pos_cart_trees.  These are used for gross level pos\n\
  such as identifying how numbers should be pronunced.");
    init_subr_2("builtin_english_token_to_words",l_word_it,
    "(english_token_to_words TOKENSTREAM TOKENNAME)\n\
  Returns a list of words expanded from TOKENNAME.  Note that as this\n\
  function may be called recursively TOKENNAME may not be the name of\n\
  TOKENSTREAM.");
}
