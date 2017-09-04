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
/*             Date   :  June 1996                                       */
/*-----------------------------------------------------------------------*/
/*                                                                       */
/* Compile a lexicon from set of exntries                                */
/*                                                                       */
/*=======================================================================*/
#include <cstdio>
#include <cstdlib>
#include "festival.h"
#include "lexicon.h"
#include "lexiconP.h"
#include "lts.h"

static LISP check_and_fix(LISP entry);
static void check_sylphones(const char *name,LISP syls);

struct LIST_ent_struct {
    EST_String word;
    char *pos;
    char *entry;
    LIST_ent_struct *next;
};
typedef LIST_ent_struct *LIST_ent;

static LISP lex_lts_set = NIL;
static LISP lex_syllabification = NIL;

int entry_compare(const void *e1, const void *e2)
{
    LIST_ent le1 = *((LIST_ent *)e1);
    LIST_ent le2 = *((LIST_ent *)e2);
    int rcode;

    if ((rcode = fcompare(le1->word,le2->word,0)) != 0)
	return rcode;
    else if ((rcode = strcmp(le1->pos,le2->pos)) != 0)
	return rcode;
    else if ((rcode = strcmp(le1->word,le2->word)) != 0)
	return rcode;
    else
    {   // its a homograph but to ensure its in the same order
	// not matter which machine we're on
	return strcmp(le1->entry,le2->entry);
    }
}

LISP lexicon_compile(LISP finname, LISP foutname)
{
    // Take a file of entries and process them checking phones
    // syllabifying if necessary.  Sorts them and writes them to
    // fout
    FILE *fin, *fout;
    LISP entry;
    LIST_ent entries = NULL,e;
    LIST_ent *ent_list;
    int num_entries=0,i;
    EST_String tmpname;

    if ((fin=fopen(get_c_string(finname),"rb")) == NULL)
    {
	cerr << "Lexicon compile: unable to open " << get_c_string(finname) 
	    << " for reading\n";
	festival_error();
    }

    lex_lts_set = siod_get_lval("lex_lts_set",NULL);
    lex_syllabification = siod_get_lval("lex_syllabification",NULL);

    while (!siod_eof((entry = lreadf(fin))))
    {
	e = new LIST_ent_struct;
	*cdebug << "Processing entry " << get_c_string(car(entry)) <<
	    endl;
	entry = check_and_fix(entry);
	e->word = get_c_string(car(entry));
	e->pos = wstrdup(siod_sprint(car(cdr(entry))));
	e->entry = wstrdup(siod_sprint(entry));
	e->next = entries;
	entries = e;
	num_entries++;
    }
    fclose(fin);

    // Make it into an array for sorting
    ent_list = new LIST_ent[num_entries];
    for (i=0,e=entries; i < num_entries; i++,e=e->next)
	ent_list[i] = e;
    qsort(ent_list,num_entries,sizeof(LIST_ent),entry_compare);

    if ((fout=fopen(get_c_string(foutname),"wb")) == NULL)
    {
	cerr << "Lexicon compile: unable to open " << get_c_string(foutname) 
	    << " for writing\n";
	fclose(fin);
	festival_error();
    }
    fprintf(fout,"MNCL\n");
    for (i=0; i < num_entries; i++)
    {
	fprintf(fout,"%s\n",ent_list[i]->entry);
	wfree(ent_list[i]->pos);
	wfree(ent_list[i]->entry);
	delete ent_list[i];
    }
    delete ent_list;
    fclose(fout);

    cwarn << "Compiled lexicon \"" << get_c_string(finname) <<
	"\" into \"" << get_c_string(foutname) << "\" " <<
	    num_entries << " entries\n";
    
    return NIL;
}

static LISP check_and_fix(LISP entry)
{
    // Check shape and that phones are in current phone set
    // Syllabify entry if required
    LISP syls;

    if (siod_llength(entry) < 2)
    {
	cerr << "Lexicon compile: entry: ";
	lprint(entry);
	cerr << "has too few fields\n";
	festival_error();
    }
    else if (CONSP(car(entry)))
    {
	cerr << "Lexicon compile: entry: ";
	lprint(entry);
	cerr << "has non-atomic head word\n";
	festival_error();
    }
    // else if (CONSP(car(cdr(entry))))    // The lookup code allows for this so why not allow it here.
    // {
    //  cerr << "Lexicon compile: entry: ";
    //  lprint(entry);
    //  cerr << "has non-atomic pos field\n";
    //  festival_error();
    // }

    if ((lex_syllabification == NIL) &&
	(siod_atomic_list(car(cdr(cdr(entry))))))
    {
	// syllabify them (in an old  special way)
	LISP phones = car(cdr(cdr(entry)));
	if (lex_lts_set != NIL)
	    phones = lts_apply_ruleset(phones,lex_lts_set);
	syls = lex_syllabify_phstress(phones);
	check_sylphones(get_c_string(car(entry)),syls);
    }
    else if ((lex_syllabification != NIL) &&
	     (atomp(lex_syllabification)) &&
	     (streq(get_c_string(lex_syllabification),"NONE")))
	syls = car(cdr(cdr(entry)));
    else
	syls = apply_hooks(lex_syllabification,car(cdr(cdr(entry))));

    return cons(car(entry),cons(car(cdr(entry)),
				cons(syls,cdr(cdr(cdr(entry))))));
}

static void check_sylphones(const char *name,LISP syls)
{
    // check shape of syllables, and that phones are valid 
    LISP s,p;

    for (s=syls; s != NIL; s=cdr(s))
    {
	if (siod_llength(car(s)) != 2)
	{
	    cerr << "Malformed lexical entry: \"" << name << 
		"\" syllable malformed\n";
	    festival_error();
	}
	if (!siod_atomic_list(car(car(s))))
	{
	    cerr << "Malformed lexical entry: \"" << name << 
		"\" syllable phone list malformed\n";
	    festival_error();
	}
	for (p=car(car(s)); p != NIL; p=cdr(p))
	 ;   
    }
}
	    
	
