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
/* lexicons: lookup                                                      */
/*                                                                       */
/*=======================================================================*/
#include <cstdio>
#include "festival.h"
#include "lexicon.h"
#include "lexiconP.h"
#include "lts.h"

static int bl_match_entry(LISP entry,const EST_String &word);
static int match_features(LISP req_feats, LISP act_feats);

// doesn't need to be exact -- used it jumping back chunks when 
// searching for entries with same head word
#define DEFAULT_LEX_ENTRY_SIZE 40

// Depth to build the index cache to
#define CACHE_DEPTH 8

static LISP lexicon_list = NIL;
static Lexicon *current_lex = NULL;

VAL_REGISTER_CLASS(lexicon,Lexicon)
SIOD_REGISTER_CLASS(lexicon,Lexicon)

Lexicon::Lexicon()
{
    type = lex_external; 
    name = ""; 
    binlexfp = NULL; 
    posmap = NIL; 
    gc_protect(&posmap);
    addenda = NIL; 
    gc_protect(&addenda);
    index_cache = NIL; 
    gc_protect(&index_cache);
    matched_lexical_entries = NIL; 
    gc_protect(&matched_lexical_entries);
    pre_hooks = NIL;
    gc_protect(&pre_hooks);
    post_hooks = NIL;
    gc_protect(&post_hooks);
    bl_filename = ""; 
    lts_method="";
}


Lexicon::~Lexicon()
{
    if (binlexfp != NULL)
	fclose(binlexfp);
    gc_unprotect(&addenda);
    gc_unprotect(&index_cache);
    gc_unprotect(&posmap);
    gc_unprotect(&matched_lexical_entries);
    gc_unprotect(&pre_hooks);
    gc_unprotect(&post_hooks);

}

LISP Lexicon::lookup(const EST_String &word, const LISP features)
{
    LISP entry,mapped,hooked, entry2;
    EST_String sword;

    if (pre_hooks != NIL)
    {   // We could just call it and it wont do anything if NIL
	// but this check may save some extra consing
	hooked = apply_hooks_right(pre_hooks,
				   cons(strintern(word),cons(features,NIL)));
	sword = get_c_string(car(hooked));
	mapped = map_pos(posmap,car(cdr(hooked)));
    }
    else
    {
	sword = word;
	mapped = map_pos(posmap,features);
    }

    // Check addenda, them complex, then lts
    if ((entry = lookup_addenda(sword,mapped)) == NIL)
    {   if ((entry = lookup_complex(sword,mapped)) == NIL)
	    entry = lookup_lts(sword,mapped);
    } // After adding the city Nice to the addendum, FT said "nice" wrong, there-
    else if(mapped != NIL)  // fore we try to find an entry with matching pos.
         if(car(cdr(entry)) != NIL &&  // addendum pos is not NIL
            mapped != car(cdr(entry))) // and does not match
              if((entry2 = lookup_complex(sword,mapped)) != NIL)
                    if(mapped == car(cdr(entry2))) // comp. lex. pos does match
                       entry = entry2;
    

    if (post_hooks != NIL)
    {
	return apply_hooks_right(post_hooks,cons(entry,NIL));
    }
    else
	return entry;
}

LISP Lexicon::lookup_all(const EST_String &word)
{
    // Find all entries that match word.
    LISP entries = NIL;
    LISP l;
    
    for (l=addenda; l != 0; l=cdr(l))
	if (bl_match_entry(car(l),word) == 0)
	    entries = cons(car(l),entries);

    lookup_complex(word,flocons(-1));

    return reverse(append(matched_lexical_entries,entries));
}

int Lexicon::in_lexicon(const EST_String &word,LISP features)
{
    // Checks to see if this word is in the lexicons (addenda or 
    // compiled lexicon.  Ignores any letter to sound method.
    // This is used to determine if further analysis on this word might
    // be worthwhile
    
    if ((lookup_addenda(word,features) != NIL) ||
	(lookup_complex(word,features) != NIL))
	return TRUE;
    else
	return FALSE;
}

EST_String Lexicon::str_lookup(const EST_String &word, const LISP features)
{
    LISP entry = lookup(word,features);
    return siod_sprint(entry);
}

LISP Lexicon::lookup_addenda(const EST_String &word,LISP features)
{
    // Lookup addenda (for first match)
    LISP l,potential=NIL;

    for (l=addenda; l != 0; l=cdr(l))
	if (bl_match_entry(car(l),word) == 0)
	{
	    if (potential == NIL) // if nothing matches features this'll do
		potential = car(l);
	    if (match_features(features,(car(cdr(car(l))))))
		return car(l); 
	}

    // If there isn't a complete match, a match of name is sufficient
    return potential;
}

LISP Lexicon::lookup_complex(const EST_String &word,LISP features)
{
    //  Lookup the word in a compile lexicon file 
    int start,end;

    if (bl_filename == "")
	return NIL;   // there isn't a compiled lexicon

    binlex_init();
    int depth = 0;
    matched_lexical_entries = NIL;
    lex_entry_match = 0;

    bl_lookup_cache(index_cache,word,start,end,depth);

    return bl_bsearch(word,features,start,end,depth);
}

void Lexicon::bl_lookup_cache(LISP cache, const EST_String &word, 
			      int &start, int &end, int &depth)
{
    // Look up word in the index cache to get a better idea where 
    // to look in the real file
    int fc;
    
    if (cdr(cache) == NIL)  // hit bottom
    {
	start = get_c_int(car(car(cache)));
	end = get_c_int(cdr(car(cache)));
    }
    else if ((fc=fcompare(word,get_c_string(car(cdr(cache))),NULL)) < 0)
	bl_lookup_cache(siod_nth(2,cache),word,start,end,++depth);
    else if (fc == 0)
    {
	start = get_c_int(car(car(cache)));
	end = get_c_int(cdr(car(cache)));
    }
    else
	bl_lookup_cache(siod_nth(3,cache),word,start,end,++depth);
}

void Lexicon::add_to_cache(LISP cache,
			   const EST_String &word,
			   int start,int mid, int end)
{
    int fc;

    if (cdr(cache) == NIL)  // hit bottom
    {
	LISP a,b;

	a = cons(cons(flocons(start),flocons(mid)),NIL);
	b = cons(cons(flocons(mid),flocons(end)),NIL);
	
	setcdr(cache,cons(strintern(word),cons(a,cons(b,NIL))));
    }
    else if ((fc = fcompare(word,get_c_string(car(cdr(cache))),NULL)) < 0)
	add_to_cache(siod_nth(2,cache),word,start,mid,end);
    else if (fc == 0)
	return; /* already in cache */
    else
	add_to_cache(siod_nth(3,cache),word,start,mid,end);
}

void Lexicon::binlex_init(void)
{
    // Open the compiled lexicon file if not already open 
    char magic_number[20];
    int end;

    if (binlexfp == NULL)
    {
	if (bl_filename == "")
	{
	    cerr << "Lexicon: no compile file given" << endl;
	    festival_error();
	}
	binlexfp = fopen(bl_filename,"rb");
	if (binlexfp == NULL)
	{
	    cerr << "Lexicon: compile file \"" << bl_filename << 
		"\" not found or unreadble " << endl;
	    festival_error();
	}
	fread(magic_number,sizeof(char),4,binlexfp);
	magic_number[4] = '\0';
	if ((EST_String)"MNCM" == (EST_String)magic_number)
	{   // A compiled lexicon plus features
            // Also different entry format (pos distributions)
	    LISP features = lreadf(binlexfp);
	    comp_num_entries = get_param_int("num_entries",features,-1);
	}     
	else if ((EST_String)"MNCL" == (EST_String)magic_number)
	{
	    comp_num_entries = -1;
	}
	else
	{
	    cerr << "Lexicon: compile file \"" << bl_filename << 
		"\" not a compiled lexicon " << endl;
	    festival_error();
	}
	blstart = ftell(binlexfp);
	fseek(binlexfp,0L, SEEK_END);
	end = ftell(binlexfp);
	index_cache = cons(cons(flocons(blstart),flocons(end)),NIL);
    }
}

LISP Lexicon::lookup_lts(const EST_String &word,LISP features)
{
    // Look up using letter to sound rules 

    if ((lts_method == "") ||
	(lts_method == "Error"))
    {
	cerr << "LEXICON: Word " << word << " (plus features) not found in lexicon "
	    << endl;
	festival_error();
    }
    else if (lts_method == "lts_rules")
	return lts(word,features,lts_ruleset);
    else if (lts_method == "none")
	return cons(strintern(word),cons(NIL,cons(NIL,NIL)));
    else if (lts_method == "function")
	return leval(cons(rintern("lex_user_unknown_word"),
			  cons(quote(strintern(word)),
			       cons(quote(features),NIL))),
		     NIL);
    else
	return leval(cons(rintern(lts_method),
			  cons(quote(strintern(word)),
			       cons(quote(features),NIL))),
		     NIL);

    return NIL;
}

LISP Lexicon::bl_bsearch(const EST_String &word,LISP features,
			 int start,int end, int depth)
{
    // Do a binary search for word in file 
    LISP closest_entry;
    int mid, compare;

    if (start==end)  // only happens if first item has been tested (and failed)
	return NIL;
    else if ((end-start) < 10)
    {
	if (start == blstart)   
	{
	    mid = start; 
	    end = start;
	}
	else
	    return NIL;  // failed
    }
    else
	mid = start + (end-start)/2;
    
    closest_entry = bl_find_next_entry(mid);
    if ((depth < CACHE_DEPTH) &&
	(end-start > 256))
    {
	add_to_cache(index_cache,get_c_string(car(closest_entry)),
		     start,mid,end);
    }

    compare = bl_match_entry(closest_entry,word);

/*    printf("%s %s %d %d %d\n",
	   (const char *)word,
	   get_c_string(car(closest_entry)),
	   start,mid,end); */

    if (compare == 0)
	return bl_find_actual_entry(mid,word,features);
    else if (compare < 0)  // too far
	return bl_bsearch(word,features,start,mid,++depth);
    else  // not far enough
	return bl_bsearch(word,features,mid,end,++depth);
}

LISP Lexicon::bl_find_next_entry(int pos)
{
    // Read the next full entry after this point 
    int c;

    fseek(binlexfp,(long)pos,SEEK_SET);

    while ((c = getc(binlexfp)) != '\n')
	if (c == EOF) return NIL;

    return lreadf(binlexfp);
}

static int bl_match_entry(LISP entry,const EST_String &word)
{
    return fcompare(word,get_c_string(car(entry)),NULL);
}    

LISP Lexicon::bl_find_actual_entry(int pos,const EST_String &word,LISP features)
{
    //  Well there may be a number of entries with the same head word
    //  we want to find the one that matches (with features) or the
    //  the first one.  To do this we must go back until we find one that
    //  doesn't match and then search through each for a real match
    //  If no features have a match the first headword match is returned
    //  as that pronunciation is probably still better than letter to 
    //  sound rules
    LISP n_entry;
    LISP f_entry = NIL;
    
    do 
    {
	pos -= DEFAULT_LEX_ENTRY_SIZE;
	if (pos < blstart)
	{            
	    pos = blstart; fseek(binlexfp,pos,SEEK_SET);
	    break;
	}
	n_entry = bl_find_next_entry(pos);
    }
    while (bl_match_entry(n_entry,word) == 0);

    // now pos is definitely before the first entry whose head is word 
    n_entry = lreadf(binlexfp);
    lex_entry_match = 0;
    matched_lexical_entries = NIL;
    while (bl_match_entry(n_entry,word) >= 0)
    {
	if (bl_match_entry(n_entry,word) == 0)
	{
	    if (f_entry == NIL) f_entry = n_entry;
	    matched_lexical_entries = cons(n_entry,matched_lexical_entries);
	    lex_entry_match++;
	    if (match_features(features,(car(cdr(n_entry)))))
		return n_entry;
	}
	n_entry = lreadf(binlexfp);
	if (siod_eof(n_entry))
	    return NIL;
    }
    
    return f_entry;
}

static int match_features(LISP req_feats, LISP act_feats)
{
    // Match required features with actualy features
    // required should be less specific than actual
    LISP l,m;

    if ((req_feats == NIL) ||
	(eql(req_feats,act_feats)))
	return TRUE;
    else if (consp(req_feats) && consp(act_feats))
    {
	for (l=req_feats; l != 0; l=cdr(l))
	{
	    for (m=act_feats; m != 0; m=cdr(m))
		if (eql(car(l),car(m)))
		    break;
	    if (m == NIL)
		return FALSE;
	}
	return TRUE;
    }
    else
	return FALSE;
}

static void check_current_lex(void)
{
    // check there is a current lexicon 

    if (current_lex == NULL)
    {
	cerr << "No lexicon" << endl;
	festival_error();
    }
}

static void lex_add_lexicon(const EST_String &name, Lexicon *l)
{
    //  Add lexicon to list of lexicons
    LISP lpair;

    lpair = siod_assoc_str(name,lexicon_list);

    if (lexicon_list == NIL)
	gc_protect(&lexicon_list);
    
    if (lpair == NIL)
    {
	lexicon_list = cons(cons(strintern(name),
				   cons(siod(l),NIL)),
			    lexicon_list);
    }
    else
    {
	cwarn << "lexicon " << name << " recreated" << endl;
	setcar(cdr(lpair),siod(l));
    }

    return;
}

//
//  This functions provide the LISP access to the lexicons including
//  access and selection
//
static LISP lex_set_compile_file(LISP fname)
{
    EST_String filename = get_c_string(fname);
    
    check_current_lex();

    current_lex->set_bl_filename(filename);

    return fname;
}

static LISP lex_add_entry(LISP entry)
{

    check_current_lex();

    current_lex->add_addenda(entry);

    return NIL;
}

static LISP lex_set_lts_method(LISP method)
{
    EST_String smethod;

    check_current_lex();

    if (method == NIL)
	smethod = "none";
    else
	smethod = get_c_string(method);

    current_lex->set_lts_method(smethod);

    return method;
}

static LISP lex_set_lts_ruleset(LISP rulesetname)
{
    EST_String sruleset;

    check_current_lex();

    if (rulesetname == NIL)
    {
	cerr << "LEXICON: no ruleset name given\n";
	festival_error();
    }
    else
	sruleset = get_c_string(rulesetname);

    current_lex->set_lts_ruleset(sruleset);

    return rulesetname;
}

static LISP lex_set_pos_map(LISP posmap)
{
    check_current_lex();

    current_lex->set_pos_map(posmap);

    return posmap;
}

static LISP lex_set_phoneset(LISP psname)
{
    EST_String phoneset = get_c_string(psname);

    check_current_lex();
    current_lex->set_phoneset_name(phoneset);

    return psname;
}

static LISP lex_set_pre_hooks(LISP hooks)
{
    LISP last_hooks;
    check_current_lex();

    last_hooks = current_lex->pre_hooks;
    current_lex->pre_hooks = hooks;

    return last_hooks;
}

static LISP lex_set_post_hooks(LISP hooks)
{
    LISP last_hooks;
    check_current_lex();

    last_hooks = current_lex->post_hooks;
    current_lex->post_hooks = hooks;

    return last_hooks;
}

EST_String lex_current_phoneset(void)
{
    check_current_lex();
    return current_lex->phoneset_name();
}

static LISP lex_create_lex(LISP lexname)
{
    //  Make a new lexicon current (and select it)
    Lexicon *l = new Lexicon;
    EST_String name = get_c_string(lexname);

    l->set_lex_name(name);

    lex_add_lexicon(name,l);

    current_lex = l;

    return lexname;
}

LISP lex_select_lex(LISP lexname)
{
    // Select named lexicon and make it current, return name of previous
    EST_String name = get_c_string(lexname);
    LISP lpair, lastname;

    lpair = siod_assoc_str(name,lexicon_list);
    if (current_lex == NULL)
    {
	cerr << "lexicon: no current lexicon -- shouldn't happen\n";
	festival_error();
    }
    else
	lastname = rintern((const char *)current_lex->get_lex_name());

    if (lpair == NIL)
    {
	cerr << "lexicon " << name << " not defined" << endl;
	festival_error();
    }
    else
	current_lex = lexicon(car(cdr(lpair)));

    return lastname;
}

LISP lex_list(void)
{
    // List names of all current defined lexicons
    LISP lexica = NIL;
    LISP l;

    for (l=lexicon_list; l != NIL; l=cdr(l))
	lexica = cons(car(car(l)),lexica);

    return lexica;
}

static LISP lex_lookup_lisp(LISP lword, LISP features)
{
    return lex_lookup_word(get_c_string(lword),features);
}

static LISP lex_lookup_all(LISP lword)
{
    check_current_lex();

    return current_lex->lookup_all(get_c_string(lword));
}

static LISP lex_entrycount(LISP lword)
{
    check_current_lex();

    // assumes -1 is never in the pos field
    current_lex->lookup(get_c_string(lword),flocons(-1));

    return flocons(current_lex->num_matches());
}

LISP lex_lookup_word(const EST_String &word, LISP features)
{
    check_current_lex();

    return current_lex->lookup(word,features);
}

int in_current_lexicon(const EST_String &word, LISP features)
{
    check_current_lex();
    return current_lex->in_lexicon(word,features);
}

void festival_lex_ff_init(void);

void festival_Lexicon_init(void)
{
    // define lexicon related functions

    festival_lex_ff_init();

    init_subr_1("lex.set.compile.file",lex_set_compile_file,
    "(lex.set.compile.file COMPFILENAME)\n\
  Set the current lexicon's compile file to COMPFILENAME.  COMPFILENAME\n\
  is a compiled lexicon file created by lex.compile.\n\
  [see Defining lexicons]");
    init_subr_0("lex.list",lex_list,
    "(lex.list)\n\
  List names of all currently defined lexicons.");
    init_subr_1("lex.set.lts.method",lex_set_lts_method,
    "(lex.set.lts.method METHOD)\n\
  Set the current lexicon's letter-to-sound method to METHOD.  METHOD\n\
  can take any of the following values: Error (the default) signal a\n\
  festival error if a word is not found in the lexicon; lts_rules use the\n\
  letter to sound rule set named by lts_ruleset; none return\n\
  simply nil in the pronunciation field; function use call the two argument\n\
  function lex_user_unknown_word (as set by the user) with the word and\n\
  features to provide an entry. [see Letter to sound rules]");
    init_subr_1("lex.set.lts.ruleset",lex_set_lts_ruleset,
    "(lex.set.lts.ruleset RULESETNAME)\n\
  Set the current lexicon's letter-to-sound ruleset to RULESETNAME.\n\
  A ruleset of that name must already be defined.  This is used if\n\
  lts.method is set to lts_rules. [see Letter to sound rules]");
    init_subr_1("lex.set.pos.map",lex_set_pos_map,
    "(lex.set.pos.map POSMAP)\n\
  A reverse assoc-list mapping part of speech tags to the lexical\n\
  part of speech tag set. [see Lexical entries]");
    init_subr_1("lex.set.pre_hooks",lex_set_pre_hooks,
    "(lex.set.pre_hooks HOOKS)\n\
  Set a function or list of functions that are to be applied to the entry\n\
  before lookup.  Returns previous value [see Lexical entries]");
    init_subr_1("lex.set.post_hooks",lex_set_post_hooks,
    "(lex.set.post_hooks HOOKS)\n\
  Set a function or list of functions that are to be applied to the entry\n\
  after lookup.  Returns previous value [see Lexical entries]");
    init_subr_1("lex.set.phoneset",lex_set_phoneset,
    "(lex.set.phoneset PHONESETNAME)\n\
  Set current lexicon's phone set to PHONESETNAME.  PHONESETNAME must be\n\
  a currently defined (and, of course, loaded) phone set.\n\
  [see Defining lexicons]");
    init_subr_1("lex.add.entry",lex_add_entry,
    "(lex.add.entry ENTRY)\n\
  Add ENTRY to the addenda of the current lexicon.  As the addenda is\n\
  checked before the compiled lexicon or letter to sound rules, this will\n\
  cause ENTRY to be found before all others. If a word already in the\n\
  addenda is added again the most recent addition will be found (part of\n\
  speech tags are respected in the look up).  [see Lexical entries]");
    init_subr_1("lex.select",lex_select_lex,
    "(lex.select LEXNAME)\n\
  Select LEXNAME as current lexicon.  The name of the previously selected\n\
  lexicon is returned.");
    init_subr_1("lex.create",lex_create_lex,
    "(lex.create LEXNAME)\n\
  Create a new lexicon of name LEXNAME.  If it already exists, the old one\n\
  is deleted first.  [see Defining lexicons]");
    init_subr_2("lex.lookup",lex_lookup_lisp,
    "(lex.lookup WORD FEATURES)\n\
  Lookup word in current lexicon.  The addenda is checked first, if WORD\n\
  with matching FEATURES (so far this is only the part of speech tag) is\n\
  not found the compiled lexicon is checked.  Only if the word is still not\n\
  found the letter to sound rules (or whatever method specified by the\n\
  current lexicon's lts.method is used). [see Lookup process]");
    init_subr_1("lex.lookup_all",lex_lookup_all,
    "(lex.lookup_all WORD)\n\
  Return list of all entries in the addenda and compiled lexicon that\n\
  match this word.  The letter to sound rules and user defined unknown\n\
  word function is ignored.");
    init_subr_1("lex.entrycount",lex_entrycount,
    "(lex.entrycount WORD)\n\
  Return the number of entries in the compiled lexicon that match this\n\
  word.  This is used in detecting homographs.");
    init_subr_1("lex.syllabify.phstress",lex_syllabify_phstress,
    "(lex.syllabify.phstress PHONELIST)\n\
  Syllabify the given phone list (if current phone set).  Vowels may have\n\
  the numerals 0, 1, or 2 as suffixes, if so these are taken to be stress\n\
  for the syllable they are in.  This format is similar to the entry format\n\
  in the CMU and BEEP lexicons. [see Defining lexicons]");
    init_subr_2("lex.compile",lexicon_compile,
    "(lex.compile ENTRYFILE COMPILEFILE)\n\
  Compile the list of lexical entries in ENTRYFILE into a compiled file in\n\
  COMPILEFILE.  [see Defining lexicons]");
    init_fsubr("lts.ruleset",lts_def_ruleset,
    "(lts.ruleset NAME RULES SETS)\n\
  Define a new set of letter to sound rules. [see Letter to sound rules]");
    init_subr_2("lts.apply",lts_apply_ruleset,
    "(lts.apply WORD RULESETNAME)\n\
  Apply lts ruleset RULESETNAME to word returning result. \n\
  [see Letter to sound rules]");
    init_subr_2("lts.in.alphabet",lts_in_alphabet,
    "(lts.in.alphabet WORD RULESETNAME)\n\
  Returns t is all characters in symbol word (or items in list WORD)\n\
  are in the alphabet of letter to sound ruleset name RULESETNAME.  nil\n\
  otherwise. [see Letter to sound rules]");
    init_subr_0("lts.list",lts_list,
    "(lts.list)\n\
  Return list of all current defined LTS rulesets.");

}
