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
/*                     Author :  Alan W Black                            */
/*                     Date   :  April 1996                              */
/*-----------------------------------------------------------------------*/
/*                                                                       */
/* Functions for accessing parts of the utterance through "features"     */
/* features are a cononical addressing method for relatively accessing   */
/* information in an utterance from a given item in it.                  */
/*                                                                       */
/* A feature name is is a dotted separated string of names ended by a    */
/* a final feature.  The names before the final feature are navigational */
/* consisting of a few builtin forms (i.e. next and prev) or names of    */
/* relations that are to be followed,                                    */
/*                                                                       */
/* For example:                                                          */
/*   "name"          name of the current item                            */
/*   "n.name"        name of the next item (in the current relation      */
/*   "n.n.name"      is the name of the next next item                   */
/*   "R:SylStructure.parent.name"   name of the parent in the            */
/*                   SylStructure relation                               */
/* for example from an item in the Segment relation                      */
/*   "p.R:SylStructure.parent.syl_break"                                 */
/*                   is the syllable break level of the syllable related */
/*                   the previous segment.                               */
/*   "R:SylStucture.parent.R:Syllable.p.syl_break"                       */
/*                   is the syllable break level of the syllable related */
/*                   this item's previous Syllable item.                 */
/*                                                                       */
/* The following features are defined for all stream items               */
/*     name                                                              */
/* Other feature are defined through the C++ function festival_def_ff    */
/* Note duration, is no longer predefined                                */
/*                                                                       */
/* To extre features are defined here ph_* (i.. any feature prefixed by  */
/* ph_) and lisp_*.  ph_ function check the phoneme set and use the      */
/* remainder of the name as a phone feature return its value for the     */
/* current item (which will normally be a Segment stream item).  e.g.    */
/* ph_vc or ph_vheight                                                   */
/* The lisp_* feature will call a lisp function (the name following      */
/* the lisp_) with two arguments the utterance and the stream item.      */
/* this allows arbitrary new features without recompilation              */
/*                                                                       */
/* Another feature defined here is utt_* (any feature prefixed by utt_)  */
/* It looks up the remainder of the name as a feature defined at the     */
/* Utterance level.                                                      */
/*                                                                       */
/*=======================================================================*/
#include <cstdio>
#include <cstdlib>
#include "EST_unix.h"
#include <cstring>
#include "festival.h"
#include "festivalP.h"

static LISP ff_pref_assoc(const char *name, LISP alist);

static LISP ff_docstrings = NULL;
static LISP ff_pref_list = NULL;

void festival_def_nff(const EST_String &name,const EST_String &sname, 
		     EST_Item_featfunc func,const char *doc)
{
    //  define the given feature function with documentation

    register_featfunc(name,func);
    if (ff_docstrings == NIL)
	gc_protect(&ff_docstrings);
    EST_String id = sname + "." + name;
    ff_docstrings = cons(cons(rintern(id),cstrcons(doc)),ff_docstrings);
    siod_set_lval("ff_docstrings",ff_docstrings);

    return;
}

VAL_REGISTER_FUNCPTR(pref_ffunc,FT_ff_pref_func)
SIOD_REGISTER_FUNCPTR(pref_ffunc,FT_ff_pref_func)

void festival_def_ff_pref(const EST_String &pref,const EST_String &sname, 
			  FT_ff_pref_func func, const char *doc)
{
    // define the given class of feature functions
    // All feature functions names with this prefix will go to this func
    LISP lpair;

    lpair = siod_assoc_str(pref,ff_pref_list);
    
    if (lpair == NIL)
    {
	if (ff_pref_list == NIL)
	    gc_protect(&ff_pref_list);
	ff_pref_list = cons(cons(rintern(pref),
				 cons(siod(func),NIL)),
		       ff_pref_list);
	EST_String id = sname + "." + pref;
	ff_docstrings = cons(cons(rintern(id),cstrcons(doc)),ff_docstrings);
	siod_set_lval("ff_docstrings",ff_docstrings);
    }
    else
    {
	fprintf(stderr,"ffeature (prefix) %s duplicate definition\n",
		(const char *)pref);
	festival_error();
    }

    return;
}

static LISP ff_pref_assoc(const char *name, LISP alist)
{
    // Search list of ff_pref_funcs to see if name has an appropriate
    // prefix 
    LISP l;
    const char *prefix;
    
    for (l=alist; CONSP(l); l=CDR(l))
    {
	prefix = get_c_string(CAR(CAR(l)));
	if (strstr(name,prefix) == name)
	    return CAR(l);
    }

    // not found
    return NIL;
}

static EST_Item *parent_to(EST_Item *s,const EST_String &relname)
{
    // Follow parent relation of s until Item in relname
    // *includes* testing this s
    if (s == 0)
	return 0;
    if (s->relations().present(relname))
	return s->as_relation(relname);
    else
	return parent_to(parent(s),relname);
}

static EST_Item *daughter1_to(EST_Item *s,const EST_String &relname)
{
    // Follow daughter1 relation of s until Item in relname
    // *includes* testing this s
    if (s == 0)
	return 0;
    if (s->relations().present(relname))
	return s->as_relation(relname);
    else
	return daughter1_to(daughter1(s),relname);
}

static EST_Item *daughtern_to(EST_Item *s,const EST_String &relname)
{
    // Follow parent relation of s until Item in relname
    // *includes* testing this s
    if (s == 0)
	return 0;
    if (s->relations().present(relname))
	return s->as_relation(relname);
    else
	return daughtern_to(daughtern(s),relname);
}

static EST_String Feature_Separator = ".";
static EST_String Feature_PunctuationSymbols = EST_String::Empty;
static EST_String Feature_PrePunctuationSymbols = EST_String::Empty;
static EST_Val default_feature_value(0);

// Moving this to be static gives me an extra second but of course
// makes it thread unsafe, and makes reentrancy dubious
static EST_TokenStream ts;

EST_Val ffeature(EST_Item *item,const EST_String &fname)
{
    // Select and apply feature function name to s and return result 
    FT_ff_pref_func pfunc;
    EST_Item_featfunc func = 0;
    LISP lpair;
    EST_Item *s = item;

    if (item == 0)
	return default_feature_value;
    if (strchr(fname,'.') == 0)
    {   // if its a simple name do it quickly, without tokenizing
	if ((func = get_featfunc(fname)) != 0)
	    return (func)(item);
	else if ((lpair = ff_pref_assoc(fname,ff_pref_list)) != NIL)
	{
	    pfunc = pref_ffunc(CAR(CDR(lpair)));
	    return (pfunc)(item,fname);
	}
	else // it must be a feature name for this item
	    return item->f(fname,default_feature_value);
    }
    ts.open_string(fname);
    ts.set_WhiteSpaceChars(Feature_Separator);
    ts.set_PunctuationSymbols(Feature_PunctuationSymbols);
    ts.set_PrePunctuationSymbols(Feature_PrePunctuationSymbols);

    while (!ts.eof())
    {
	const EST_String &Sname = ts.get().string();
	const char *name = Sname;
	if (streq(name,"n"))
	    s=inext(s);
	else if (streq(name,"p"))
	    s=iprev(s);
	else if (streq(name,"nn"))
	    s=inext(inext(s));
	else if (streq(name,"pp"))
	    s=iprev(iprev(s));
	else if (streq(name,"up"))  // up down should really be private
	    s=iup(s);
	else if (streq(name,"down"))
	    s=idown(s);
	else if (streq(name,"parent"))
	    s=parent(s);
	else if (streq(name,"parent_to"))
	    s=parent_to(s,ts.get().string());
	else if (streq(name,"daughter1_to"))
	    s=daughter1_to(s,ts.get().string());
	else if (streq(name,"daughtern_to"))
	    s=daughtern_to(s,ts.get().string());
	else if (streq(name,"root"))
	    s=root(s);
	else if (streq(name,"daughter1"))
	    s=daughter1(s);
	else if (streq(name,"daughter2"))
	    s=daughter2(s);
	else if (streq(name,"daughtern"))
	    s=daughtern(s);
	else if (streq(name,"last"))
	    s=last(s);
	else if (streq(name,"first"))
	    s=first(s);
	else if (strncmp(name,"R:",2) == 0)  // new relation structure
	    s = s->as_relation(&name[2]);
	else if (s->f_present(Sname))
	{
	    EST_String p = Sname;
	    while (!ts.eof())
		p = EST_String::cat(p,Feature_Separator,ts.get().string());
	    return s->f(p, default_feature_value);
	}
	else if ((func = get_featfunc(Sname)) != 0)
	    return (func)(s);
	else if ((lpair = ff_pref_assoc(name,ff_pref_list)) != NIL)
	{
	    pfunc = pref_ffunc(CAR(CDR(lpair)));
	    return (pfunc)(s,Sname);
	}
	else   // unrecognized
	    s = 0;

	if (s==0)
	    return default_feature_value;
    }

    cerr << "Invalid ffeature name: \"" << fname << "\"" << endl;
    festival_error();

    return default_feature_value;
}

static LISP lisp_item_feature(LISP litem, LISP name)
{
    // return the ffeature name for this stream 
    EST_Item *s = item(litem);
    EST_String fname = get_c_string(name);

    return lisp_val(ffeature(s,fname));
}

static LISP lisp_item_raw_feature(LISP litem, LISP name)
{
    // return the ffeature name for this stream 
    EST_Item *s = item(litem);
    EST_String fname = get_c_string(name);

    EST_Val v = ffeature(s,fname);

    if (v.type() == val_type_feats)
	return siod(feats(v));
    else
	return lisp_val(ffeature(s,fname));
}

static LISP lisp_item_set_feat(LISP litem, LISP name, LISP value)
{
    // set the feature (locally) on this sitem
    EST_Item *s = item(litem);
    EST_String fname = get_c_string(name);

    if (fname.contains("R:"))
    {
	cerr << "item.set_feat: cannot set feat name containing " <<
	    "\"R:\"" << endl;
	festival_error();
    }
    s->set_val(fname,val_lisp(value));

    return value;
}

static LISP lisp_item_set_function(LISP litem, LISP name, LISP funcname)
{
    EST_Item *s = item(litem);

    s->set_function(get_c_string(name),get_c_string(funcname));

    return funcname;
}

static LISP lisp_relation_feature(LISP utt, LISP relname, LISP name)
{
    // return the ffeature name for this stream 
    EST_Utterance *u = utterance(utt);
    EST_String fname = get_c_string(name);

    return lisp_val(u->relation(get_c_string(relname))->f.val(fname));
}

static LISP lisp_relation_set_feat(LISP utt, LISP relname, LISP name, LISP val)
{
    // return the ffeature name for this stream 
    EST_Utterance *u = utterance(utt);
    EST_String fname = get_c_string(name);

    u->relation(get_c_string(relname))->f.set_path(fname,val_lisp(val));
    return val;
}

static LISP lisp_relation_remove_item_feat(LISP utt, LISP relname, LISP name)
{
    // return the ffeature name for this stream 
    EST_Utterance *u = utterance(utt);
    EST_String fname = get_c_string(name);

    u->relation(get_c_string(relname))->remove_item_feature(fname);

    return NIL;
}

static LISP lisp_relation_remove_feat(LISP utt, LISP relname, LISP name)
{
    // return the ffeature name for this stream 
    EST_Utterance *u = utterance(utt);
    EST_String fname = get_c_string(name);

    u->relation(get_c_string(relname))->f.remove(fname);

    return NIL;
}

void value_sort(EST_Features &f, const EST_String &field);

static LISP lisp_feature_value_sort(LISP f, LISP name)
{
    value_sort(*(feats(f)), get_c_string(name));
    return NIL;
}


static EST_Val ff_lisp_func(EST_Item *i,const EST_String &name)
{
    // This function is called for features functions starting lisp_
    // It calls the lisp function following that with u and i
    // as arguments, the return value (which must be atomic) is
    // then passed back as a Val.  I'm not sure if this will be 
    // particularly efficient, but it will make development of
    // new features quicker as they can be done in Lisp without
    // changing the C++ code.
    EST_String lfunc_name = name.after("lisp_");
    LISP r,l;

    l = cons(rintern(lfunc_name),
	     cons(siod(i),NIL));
    r = leval(l,NIL);
    if ((consp(r)) || (r == NIL))
    {
	cerr << "FFeature Lisp function: " << lfunc_name << 
	    " returned non-atomic value" << endl;
	festival_error();
    }
    else if (numberp(r))
	return EST_Val(get_c_float(r));
    
    return EST_Val(get_c_string(r));
}

static EST_Val ff_utt_func(EST_Item *i, const EST_String &name) {
  // This function is called to retrieve utterance level features for
  // a given item. It uses the name after utt_ to be the feature
  // name. The value of this feature is assumed to be atomic.

  EST_String lfeat_name = name.after("utt_");
  EST_Utterance *u = get_utt(i);

  return EST_Val(get_c_string(lisp_val(u->f(lfeat_name, 0))));
}

void festival_features_init(void)
{
    // declare feature specific Lisp functions 

    festival_def_ff_pref("lisp_","any",ff_lisp_func,
    "ANY.lisp_*\n\
  Apply Lisp function named after lisp_.  The function is called with\n\
  an stream item.  It must return an atomic value.\n\
  This method may be inefficient and is primarily desgined to allow\n\
  quick prototyping of new feature functions.");

    festival_def_ff_pref(
        "utt_", "any", ff_utt_func,
        "ANY.utt_*\n"
        "Retrieve utterance level feature, given an item.\n"
        "It must be an atomic value.");

    init_subr_2("item.feat",lisp_item_feature,
    "(item.feat ITEM FEATNAME)\n\
   Return value of FEATNAME (which may be a simple feature name or a\n\
   pathname) of ITEM.");

    init_subr_2("item.raw_feat",lisp_item_raw_feature,
    "(item.raw_feat ITEM FEATNAME)\n\
   Return value of FEATNAME as native features structure \n\
   (which may be a simple feature name or a\n\
   pathname) of ITEM.");

    init_subr_2("feats.value_sort",lisp_feature_value_sort,
    "(feats.value_sort FEATURES NAME)\n");

    init_subr_3("item.set_feat",lisp_item_set_feat,
    "(item.set_feat ITEM FEATNAME VALUE)\n\
   Set FEATNAME to VALUE in ITEM.");

    init_subr_3("item.set_function",lisp_item_set_function,
    "(item.set_function ITEM FEATNAME FEATFUNCNAME)\n\
   Set FEATNAME to feature function name FEATFUNCNAME in ITEM.");

    init_subr_3("utt.relation.feat",lisp_relation_feature,
    "(utt.relation.feat UTT RELNAME FEATNAME)\n\
   Return value of FEATNAME on relation RELNAME in UTT.");

    init_subr_3("utt.relation.remove_feat",lisp_relation_remove_feat,
    "(utt.relation.remove_feat UTT RELNAME FEATNAME)\n\
   Remove FEATNAME on relation RELNAME in UTT.");

    init_subr_3("utt.relation.remove_item_feat",lisp_relation_remove_item_feat,
    "(utt.relation.remove_item_feat UTT RELNAME FEATNAME)\n\
   Remove FEATNAME on every item in relation RELNAME in UTT.");

    init_subr_4("utt.relation.set_feat",lisp_relation_set_feat,
    "(utt.relation.set_feat UTT RELNAME FEATNAME VALUE)\n\
   Set FEATNAME to VALUE on relation RELNAME in UTT.");
}



