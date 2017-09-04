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
/*             Date   :  September 1996                                  */
/*-----------------------------------------------------------------------*/
/*                                                                       */
/* A letter to sound rule system that allows rules to be specified       */
/* externally.  This is specified desined to use the existing Welsh      */
/* letter to rules developed by Briony Williams for the Welsh            */
/* synthesizer.  This form came from a program by Greg Lee (Univ of  */
/* Hawaii), but this is not using his code.                              */
/*                                                                       */
/* Multiple rule sets are supported alloing multiple applications of     */
/* varying rule sets                                                     */
/*                                                                       */
/* A set of rules consists of set definitions, and rules                 */
/* Each rule consists of a left hand side and a write hand side          */
/* The LHS consist of a left context [ change chars ] right context      */
/* The RHS consist of the new symbols                                    */
/*                                                                       */
/* The rules are interpreted in order, the first match rule gives        */
/* the replacement, and the search start again with the pointer          */
/* incremented                                                           */
/*                                                                       */
/*=======================================================================*/
#include <cstdio>
#include <cstdlib>
#include "festival.h"
#include "lts.h"

class LTS_Ruleset{
  private:
    EST_String p_name;
    int num_rules;
    LISP p_rules;
    LISP p_alphabet;
    LISP p_sets;      // short hand for sets
    LISP normalize(LISP rules);
    int item_match(LISP actual_item, LISP rule_item);
    int context_match(LISP actual_context, LISP rule_context);
    int match_rule(LISP lc, LISP remainder, LISP rule, LISP *rest);
    LISP rewrite(LISP lc, LISP remainder, LISP rules, LISP *rest);
    LISP this_match(LISP remainder, LISP rule_this); 
    void update_alphabet(LISP newitems);
 public:
    LTS_Ruleset(LISP name, LISP rules, LISP sets);
    ~LTS_Ruleset(void);
    const EST_String &name(void) const {return p_name;}
    LISP apply(LISP word);
    LISP check_alpha(LISP word);
};

static LISP fix_postfix_ops(LISP l);

static LISP lts_rules_list = NIL;

#define LTS_LC(R) (car(R))
#define LTS_THIS(R) (car(cdr(R)))
#define LTS_RC(R) (car(cdr(cdr(R))))
#define LTS_RHS(R) (car(cdr(cdr(cdr(R)))))

VAL_REGISTER_CLASS(ltsruleset,LTS_Ruleset)
SIOD_REGISTER_CLASS(ltsruleset,LTS_Ruleset)

LTS_Ruleset::LTS_Ruleset(LISP name, LISP rules, LISP sets)
{
    p_alphabet = NIL;
    gc_protect(&p_alphabet);
    p_name = get_c_string(name);
    p_sets = sets;
    gc_protect(&p_sets);
    p_rules = normalize(rules);
    gc_protect(&p_rules);
}
    
LTS_Ruleset::~LTS_Ruleset(void)
{
    gc_unprotect(&p_sets);
    gc_unprotect(&p_rules);
    gc_unprotect(&p_alphabet);
}

LISP LTS_Ruleset::normalize(LISP rules)
{
    // Change the rule format to I can access it faster
    LISP r, rc, t, lc, rhs, c;
    LISP nrs = NIL;
    int state;

    for (r=rules; r != NIL; r=cdr(r))
    {
	lc = t = rc = rhs = NIL;
	state = 0;
	for (c=car(r); c != NIL; c = cdr(c))
	{
	    if (state == 0)
	    {
		if (streq("[",get_c_string(car(c))))
		    state = 1;
		else
		    lc = cons(car(c),lc);
	    }
	    else if (state == 1)
	    {
		if (streq("]",get_c_string(car(c))))
		    state = 2;
		else
		    t = cons(car(c),t);
	    }
	    else if (state == 2)
	    {
		if (streq("=",get_c_string(car(c))))
		{
		    state = 3;
		    rhs = cdr(c);
		    break;
		}
		else
		    rc = cons(car(c),rc);
	    }
	    else
	    {
		cerr << "LTS_Rules:: misparsed a rule\n";
		cerr << "LTS_Rules:: ";
		pprint(car(r));
		festival_error();
	    }
	}
	update_alphabet(t);
	if ((state != 3) ||
	    (t == NIL))
	{
	    cerr << "LTS_Rules:: misparsed a rule\n";
	    cerr << "LTS_Rules:: ";
	    pprint(car(r));
	    festival_error();
	}
	nrs = cons(cons(fix_postfix_ops(lc),cons(reverse(t),cons(reverse(rc),
						cons(rhs,NIL)))),nrs);
    }
    
    return reverse(nrs);
}    

void LTS_Ruleset::update_alphabet(LISP newitems)
{
    // Add new items to alphabet is not already there
    LISP p;

    for (p=newitems; p != NIL; p=cdr(p))
	if (siod_member_str(get_c_string(car(p)),p_alphabet) == NIL)
	    p_alphabet = cons(car(p),p_alphabet);
}    

static LISP fix_postfix_ops(LISP l)
{
    // This list have been built in reverse so the postfix operators * and +
    // are wrong.  Destrictively fix them
    LISP p,q;

    for (p=l; p != NIL; p=cdr(p))
	if ((streq("*",get_c_string(car(p)))) ||
	    (streq("+",get_c_string(car(p)))))
	{
	    if (cdr(p) == NIL)
	    {
		cerr << "LTS_Rules:: malformed left context\n";
		pprint(reverse(l));
	    }
	    q = car(p);
	    CAR(p) = car(cdr(p));
	    CAR(cdr(p)) = q;
	    p = cdr(p);
	}
    
    return l;
}	    

LISP LTS_Ruleset::apply(LISP word)
{
    // Apply rules to word
    LISP lc,remainder,result,newremainder,r,l;
    int i;

    lc = cons(rintern("#"),NIL);
    remainder = append(word,lc); // add # at end of right context
    result = NIL;

    for (;
	 !streq("#",get_c_string(car(remainder)));
	 )
    {
	r = rewrite(lc,remainder,p_rules,&newremainder);
	result = append(reverse(r),result);
	for (i=0,l=remainder; 
	     i < siod_llength(remainder)-siod_llength(newremainder);
	     i++,l=cdr(l))
	     lc=cons(car(l),lc);
	remainder = newremainder;
    }
    return reverse(result);
}

LISP LTS_Ruleset::check_alpha(LISP word)
{
    // Check all characters in word can (possibly) be mapped by this ruleset
    LISP word_chars,p;

    if (consp(word))
	word_chars = word;
    else
	word_chars = symbolexplode(word);

    for (p=word_chars; p != NIL; p=cdr(p))
	if (siod_member_str(get_c_string(car(p)),p_alphabet) == NIL)
	    return NIL;

    return rintern("t");
}

LISP LTS_Ruleset::rewrite(LISP lc, LISP remainder, LISP rules, LISP *rest)
{
    // Find a rule to match this context 
    LISP r,t;

    for (r=rules; r != NIL; r=cdr(r))
	if (match_rule(lc,remainder,car(r),rest) == TRUE)
	    return LTS_RHS(car(r));

    cerr << "LTS_Ruleset " << p_name << ": no rule matches: \n";
    cerr << "LTS_Ruleset: "; 
    for (t=reverse(lc); t != NIL; t = cdr(t))
	cerr << get_c_string(car(t)) << " ";
    cerr << "*here* ";
    for (t=remainder; t != NIL; t = cdr(t))
	cerr << get_c_string(car(t)) << " ";
    cerr << endl;
    festival_error();
    return NIL;
}    

int LTS_Ruleset::match_rule(LISP lc, LISP remainder, LISP rule, LISP *rest)
{
    //  Match this rule

    *rest = this_match(remainder,LTS_THIS(rule));
    
    return ((*rest != NIL) &&
	    (context_match(*rest,LTS_RC(rule))) &&
	    (context_match(lc,LTS_LC(rule))));
}

int LTS_Ruleset::context_match(LISP acontext, LISP rcontext)
{
    // TRUE if rule context is initial sub list of actual context

    if (rcontext == NIL)
	return TRUE;
    else if ((cdr(rcontext) != NIL) &&
	     (streq("*",get_c_string(car(cdr(rcontext))))))
	return ((context_match(acontext,cdr(cdr(rcontext)))) ||
		(context_match(acontext,cons(car(rcontext),
					     cdr(cdr(rcontext))))) ||
		((item_match(car(acontext),car(rcontext))) &&
		 context_match(cdr(acontext),rcontext)));
    else if ((cdr(rcontext) != NIL) &&
	     (streq("+",get_c_string(car(cdr(rcontext))))))
	return ((item_match(car(acontext),car(rcontext))) &&
		(context_match(cdr(acontext),
			       cons(car(rcontext),
				    cons(rintern("*"),
					 cdr(cdr(rcontext)))))));
    else if (item_match(car(acontext),car(rcontext)))
	return context_match(cdr(acontext),cdr(rcontext));
    else
	return FALSE;

#if 0
    for (a=actual_context,r=rule_context; r != NIL; a=cdr(a),r=cdr(r))
    {
	if (item_match(car(a),car(r)))
	    return FALSE;
    }

    return TRUE;
#endif
}

LISP LTS_Ruleset::this_match(LISP remainder, LISP rule_this)
{
    // Match the centre of the rule to the remainder.  Returning
    // the new remainder if the match is successful
    LISP a,r;
    
    for (a=remainder,r=rule_this; r != NIL; a=cdr(a),r=cdr(r))
	if (!item_match(car(a),car(r)))
	    return NIL;

    return a;
}

int LTS_Ruleset::item_match(LISP actual_item, LISP rule_item)
{
    // Checks for a match, possible rule_item is a set name
    // returns remainder if match, NIL otherwise.

    if (streq(get_c_string(actual_item),get_c_string(rule_item)))
	return TRUE;
    else
    {
	LISP lpair = assq(rule_item,p_sets);
	if (lpair == NIL)
	    return FALSE;
	else if (siod_member_str(get_c_string(actual_item),cdr(lpair)) != NIL)
	    return TRUE;
	else
	    return FALSE;
    }
}

LISP lts_def_ruleset(LISP args, LISP penv)
{
    // Define a new rule set
    (void)penv;
    LTS_Ruleset *rs = new LTS_Ruleset(car(args),
				      car(cdr(cdr(args))),
				      car(cdr(args)));
    LISP name = car(args);
    LISP lpair;
    
    if (lts_rules_list == NIL)
	gc_protect(&lts_rules_list);

    lpair = siod_assoc_str(get_c_string(name),lts_rules_list);
    
    if (lpair == NIL)
    {
	lts_rules_list = cons(cons(name,
				   cons(siod(rs),NIL)),
			      lts_rules_list);
    }
    else
    {
	cwarn << "LTS_Rules: " << get_c_string(name) << " recreated" << endl;
	setcar(cdr(lpair),siod(rs));
    }

    return name;
}

LISP lts_in_alphabet(LISP word, LISP rulesetname)
{
    // check if this word is in the input alphabet for ruleset
    LTS_Ruleset *rs;
    LISP lpair;

    lpair = siod_assoc_str(get_c_string(rulesetname),lts_rules_list);
    if (lpair == NIL)
    {
	cerr << "LTS_Rules: no rule set named \"" << 
	    get_c_string(rulesetname) << "\"\n";
	festival_error();
    }
    else
    {
	rs = ltsruleset(car(cdr(lpair)));
	return rs->check_alpha(word);
    }

    return NIL;
}

LISP lts_list()
{
    // List all currently defined rule sets
    LISP r,rulesets = NIL;

    for (r=lts_rules_list; r != NIL; r=cdr(r))
	rulesets = cons(car(car(r)),rulesets);

    return rulesets;
}

LISP lts_apply_ruleset(LISP word, LISP rulesetname)
{
    // Apply the rule set to word (an atom or list of atoms)
    LTS_Ruleset *rs;
    LISP lpair;

    lpair = siod_assoc_str(get_c_string(rulesetname),lts_rules_list);
    if (lpair == NIL)
    {
	cerr << "LTS_Rule: no rule set named \"" << 
	    get_c_string(rulesetname) << "\"\n";
	festival_error();
    }
    else
    {
	rs = ltsruleset(car(cdr(lpair)));
	if (consp(word))
	    return rs->apply(word);
	else
	    return rs->apply(symbolexplode(word));
    }
    return NIL;
}


