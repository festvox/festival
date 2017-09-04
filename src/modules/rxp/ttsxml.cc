 /*************************************************************************/
 /*                                                                       */
 /*                Centre for Speech Technology Research                  */
 /*                     University of Edinburgh, UK                       */
 /*                         Copyright (c) 1998                            */
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
 /*                                                                       */
 /*             Author :  Alan W Black                                    */
 /*             Date   :  May 1998                                        */
 /* -------------------------------------------------------------------   */
 /*  Interface between festival and the Richard's XML parser              */
 /*                                                                       */
 /*  Provides a LISP function to do the analysis so that this directory   */
 /*  reasonable be optional                                               */
 /*                                                                       */
 /*************************************************************************/

#include "EST_Pathname.h"
#include "festival.h"
#include "text.h"
#include "rxp.h"

// So we can share the known_ids table.
#include "ling_class/EST_utterance_xml.h"


static InputSource entity_open(Entity ent, void *arg);

static LISP tts_file_xml(LISP filename)
{
    // Parse the xml file using the LTG's xml parser
    EST_String inname = get_c_string(filename);
    EST_String line, type, remainder;
    Parser p;
    Entity ent = 0;
    InputSource source = 0;
    LISP element_defs;
    LISP utt = NIL;   // for cummulation of tokens

    if (inname == "-")
	source = SourceFromStream("<stdin>",stdin);
    else
    {
	ent = NewExternalEntity(0,0,strdup8(inname),0,0);
	if (ent)
	    source = EntityOpen(ent);
    }

    if (!source)
    {
	cerr << "xml: unable to open input file \"" << inname << "\"" << endl;
	festival_error();
    }
    element_defs = siod_get_lval("xxml_elements",NULL);
    p = NewParser();
    ParserSetEntityOpener(p, entity_open);
    ParserSetFlag(p, ReturnDefaultedAttributes, 1);
    if (ParserPush(p, source) == -1)
    {
	cerr << "xml: parser error\n" << endl;
	festival_error();
    }

    while (1)
    {
	XBit bit = ReadXBit(p);
	if (bit->type == XBIT_eof)
	    break;
	else if (bit->type == XBIT_start)
	{
	    Attribute b;
	    LISP att=NIL;
	    for (b=bit->attributes; b; b=b->next)
		att = cons(cons(rintern(b->definition->name),
				cons(cons(rintern(b->value),NIL),NIL)),att);
	    utt = xxml_call_element_function(
		   EST_String("(")+bit->element_definition->name,att,
					     element_defs,utt);
	}
	else if (bit->type == XBIT_end)
	{
	    utt = xxml_call_element_function(
  		   EST_String(")")+bit->element_definition->name,NIL,
					     element_defs,utt);
	}
	else if (bit->type == XBIT_empty)
	{
	    Attribute b;
	    LISP att=NIL;
	    for (b=bit->attributes; b; b=b->next)
		att = cons(cons(rintern(b->definition->name),
				cons(cons(rintern(b->value),NIL),NIL)),att);
	    utt = xxml_call_element_function(
		   EST_String(bit->element_definition->name),att,
					     element_defs,utt);
	}
	else if (bit->type == XBIT_pcdata)
	{
	    utt = xxml_get_tokens(bit->pcdata_chars,
				  siod_get_lval("xxml_word_features",NULL),
				  utt);
	}
	else if (bit->type == XBIT_cdsect)
	{
	    utt = xxml_get_tokens(bit->cdsect_chars,
				  siod_get_lval("xxml_word_features",NULL),
				  utt);
	}
	else if (bit->type == XBIT_pi)
	{
	    cerr << "xml: ignoring pi " << bit->pi_chars << endl;
	}
	else if (bit->type == XBIT_error)
	{
	    ParserPerror(p,bit);
	    festival_error();
	}
	else
	{
	    // ignore it
	}
	FreeXBit(bit);
    }
    // Last call (should synthesize trailing tokens)
    utt = xxml_call_element_function(" ",NIL,element_defs,utt);

    FreeDtd(p->dtd);
    FreeParser(p);
    if (ent) FreeEntity(ent);
    return NIL;
}

static LISP xml_register_id(LISP pattern_l, LISP result_l)
{
  EST_String pattern = get_c_string(pattern_l);
  EST_String result = get_c_string(result_l);

  utterance_xml_register_id(pattern, result);
  return NIL;
}

static LISP xml_registered_ids()
{
  EST_StrList ids;
  utterance_xml_registered_ids(ids);
  LISP result= NIL;

  EST_Litem *p;

  for(p=ids.head(); p != NULL; p=p->next())
    {
      EST_String pat = ids(p);
      p=p->next();
      EST_String res = ids(p);
      result = cons(
		 cons(strcons(pat.length(), pat), 
		      strcons(res.length(), res)), 
		 result);
    }

  return result;
}

void festival_rxp_init()
{
    proclaim_module("rxp");

    init_subr_1("tts_file_xml",tts_file_xml,
    "(tts_file_xml FILE)\n\
  Low level tts processor for XML files.  This assumes that element\n\
  instructions are set up in the variable xxml_elements.");

    init_subr_2("xml_register_id", xml_register_id,
    "(xml_register_id PATTERN RESULT) \n\
  Add a rule for where to find XML entities such as DTDs.\n\
  The pattern is a regular expression, the result is a string\n\
  with substitutions. If the PATTERN matches the a PUBLIC\n\
  or SYSTEM identifier of an XML entity, the RESULT is expanded\n\
  and then used as a filename.");

    init_subr_0("xml_registered_ids", xml_registered_ids,
    "(xml_registered_ids) \n\
  Return the current list of places to look for XML entities.");
}

static InputSource entity_open(Entity ent, void *arg)
{
    (void)arg;
    return utterance_xml_try_and_open(ent);
}
