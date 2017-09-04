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
/*             Date   :  June 1997                                       */
/*-----------------------------------------------------------------------*/
/*                                                                       */
/*  Probabilistic parser for (S)CFG                                      */
/*                                                                       */
/*=======================================================================*/
#include <cmath>
#include "festival.h"
#include "parser.h"
#include "EST_SCFG_Chart.h"

LISP FT_PParse_Generalized_Utt(LISP args, LISP env) {
  LISP utt;    // Utterance to parse
  LISP gram;   // Grammar Name
  LISP prel;   // Relation Name to parse over
  LISP pfeat;  // Feature Name to parse over
  LISP crel;   // Relation to create parse in

  args = leval(car(args), env);
  utt = car(args);
  gram = car(cdr(args));
  prel = car(cdr(cdr(args)));
  pfeat = car(cdr(cdr(cdr(args))));
  crel = car(cdr(cdr(cdr(cdr(args)))));

  // Parse utt using items in prel, using features in pfeat, and store
  // the parser output into crel

  EST_Utterance *u;
  LISP rules;
  const char *gram_name, *prel_name, *pfeat_name, *crel_name;

  u = get_c_utt(utt);
  gram_name = get_c_string(gram);
  prel_name = get_c_string(prel);
  pfeat_name = get_c_string(pfeat);
  crel_name = get_c_string(crel);

  rules = siod_get_lval(gram_name, NULL);
  if (rules == NULL)
    return utt;

  EST_SCFG grammar(rules);

  scfg_parse(u->relation(prel_name), pfeat_name,
             u->create_relation(crel_name), grammar);

  return utt;
}

LISP FT_PParse_Utt(LISP utt)
{
    // Parse Words (using part of speech tags) using given
    // probabilistic grammar
    EST_Utterance *u = get_c_utt(utt);
    LISP rules;

    rules = siod_get_lval("scfg_grammar", NULL);
    if (rules == NULL)
	return utt;

    EST_SCFG grammar(rules);

    scfg_parse(u->relation("Word"),"phr_pos",
	       u->create_relation("Syntax"),grammar);

    return utt;
}

LISP FT_MultiParse_Utt(LISP utt)
{
    // You give them a parser and they just want more ...
    // Because in some modes utterance may contain multiple sentences
    // and the grammars we have only have only deal in more
    // traditional sentences this tries to split the utterance into
    // sentences and parse them individualls and add them to
    // a single Syntax relation as a list of trees.
    EST_Utterance *u = get_c_utt(utt);
    LISP rules, eos_tree;
    EST_Item *s,*e,*st,*et;

    rules = siod_get_lval("scfg_grammar", NULL);
    if (rules == NULL)
	return utt;
    eos_tree = siod_get_lval("scfg_eos_tree",NULL);
    u->create_relation("Syntax");
    EST_SCFG_Chart chart;
    chart.set_grammar_rules(rules);

    for (st=u->relation("Token")->head(); st; st = inext(st))
    {
	for (et=inext(st); et; et=inext(et))
	    if (wagon_predict(et,eos_tree) != 0)
		break;
	// Now find related words
	s = first_leaf(st)->as_relation("Word");
	e = first_leaf(inext(et))->as_relation("Word");
	chart.setup_wfst(s,e,"phr_pos");
	chart.parse();
	chart.extract_parse(u->relation("Syntax"),s,e,TRUE);
	st = et;
    }

    return utt;
}

void MultiParse(EST_Utterance &u)
{
    // You give them a parser and they just want more ...
    // Because in some modes utterance may contain multiple sentences
    // and the grammars we have only have only deal in more
    // traditional sentences this tries to split the utterance into
    // sentences and parse them individualls and add them to
    // a single Syntax release as a list of trees.
    LISP rules, eos_tree;
    EST_Item *s, *w;

    rules = siod_get_lval("scfg_grammar", NULL);
    if (rules == NULL)
	EST_error("Couldn't find grammar rules\n");
    eos_tree = siod_get_lval("scfg_eos_tree",NULL);
    u.create_relation("Syntax");
    EST_SCFG_Chart chart;
    chart.set_grammar_rules(rules);

    // produce a parse wherever there is a sentence end marker or
    // the end of utterance.

    for (w = s = u.relation("Word")->head(); w; w = inext(w))
	if (w->f_present("sentence_end") || (inext(w) == 0))
	{
	    chart.setup_wfst(s, inext(w), "phr_pos");
	    chart.parse();
	    chart.extract_parse(u.relation("Syntax"), s, inext(w), TRUE);
	    s = inext(w);
	}
}

void festival_parser_init(void)
{
    proclaim_module("parser");

    festival_def_utt_module("ProbParse",FT_PParse_Utt,
    "(ProbParse UTT)\n\
  Parse part of speech tags in Word relation.  Loads the grammar \n\
  from scfg_grammar_filename and saves the best parse\n\
  in the Syntax Relation.");

    init_fsubr("ProbParseGeneralized", FT_PParse_Generalized_Utt,
                "(ProbParseGeneralized (list utt gram prel pfeat crel))\n"
                "Parse utt over the prel relation using its pfeat feature\n"
                "Load grammar from gram, and save parse in relation crel");

    festival_def_utt_module("MultiProbParse",FT_MultiParse_Utt,
    "(MultiProbParse UTT)\n\
  Parse part of speech tags in Word relation.  Unlike ProbParse this \n\
  allows multiple sentences to appear in the one utterance.  The CART \n\
  tree in eos_tree is used to define end of sentence.  Loads the \n\
  grammar from scfg_grammar_filename and saves the best parse\n\
  in the Syntax Relation.");
}
