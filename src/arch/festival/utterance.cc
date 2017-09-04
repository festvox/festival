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
/*               EST_Utterance access functions (from Lisp)              */
/*                                                                       */
/*=======================================================================*/
#include <cstdio>
#include "EST_unix.h"
#include "festival.h"
#include "festivalP.h"

static LISP item_features(LISP sitem, LISP leval = NIL);
static LISP item_features(EST_Item *s, bool evaluate_ff=false);
static LISP stream_tree_to_lisp(EST_Item *s);

LISP utt_iform(EST_Utterance &utt)
{
    return read_from_lstring(strintern(utt_iform_string(utt)));
}

const EST_String utt_iform_string(EST_Utterance &utt)
{
    return utt.f("iform").string();
}

const EST_String utt_type(EST_Utterance &utt)
{
    return utt.f("type").string();
}


static LISP utt_flat_repr( LISP l_utt )
{
  EST_String flat_repr;
  EST_Utterance *utt = get_c_utt( l_utt );
  
  utt_2_flat_repr( *utt, flat_repr ); 
 
  return strcons(flat_repr.length(), flat_repr.str());
}


static LISP utt_feat(LISP lutt, LISP feat)
{
    EST_Utterance *u = utterance(lutt);
    EST_String f = get_c_string(feat);
    return lisp_val(u->f(f));
}

static LISP item_utt(LISP i)
{
    return siod(get_utt(item(i)));
}

static LISP item_sub_utt(LISP i)
{
    EST_Utterance *u = new EST_Utterance;

    sub_utterance(*u,item(i));
    
    return siod(u);
}

static LISP utt_set_feat(LISP u, LISP name, LISP value)
{
    EST_String n = get_c_string(name);
    
    if (TYPEP(value,tc_flonum))
	utterance(u)->f.set(n,get_c_float(value));
    else if (val_p(value))
	utterance(u)->f.set_val(n,val(value));
    else
	utterance(u)->f.set(n,get_c_string(value));

    return value;
}

static LISP utt_save(LISP utt, LISP fname, LISP ltype)
{
    EST_Utterance *u = utterance(utt);
    EST_String filename = get_c_string(fname);
    if (fname == NIL)
	filename = "save.utt";
    EST_String type = get_c_string(ltype);
    if (ltype == NIL) type = "est_ascii";

    if (type == "est_ascii")
    {
	if (u->save(filename,type) != write_ok)
	{
	    cerr << "utt.save: saving to \"" << filename << "\" failed" <<
		endl;
	    festival_error();
	}
    }
    else
    {
	cerr << "utt.save: unknown save format" << endl;
	festival_error();
    }

    return utt;
}

static LISP utt_save_relation(LISP utt, LISP rname, LISP fname, 
			      LISP evaluate_ff)
{
    // Save relation in named file
    EST_Utterance *u = utterance(utt);
    EST_String relname = get_c_string(rname);
    EST_String filename = get_c_string(fname);
    bool a;

    if ((evaluate_ff == NIL) || (get_c_int(evaluate_ff) == 0))
	a = false;
    else
	a = true;

    if (fname == NIL)
	filename = "save.utt";
    EST_Relation *r = u->relation(relname);

    if (r->save(filename, a) != write_ok)
    {
	cerr << "utt.save.relation: saving to \"" << filename << "\" failed" <<
	    endl;
	festival_error();
    }
    return utt;
}

static LISP utt_load(LISP utt, LISP fname)
{
    EST_Utterance *u;
    if (utt == NIL)
	u = new EST_Utterance;
    else
	u = utterance(utt);
    EST_String filename = get_c_string(fname);

    if (u->load(filename) != 0)
    {
	cerr << "utt.load: loading from \"" << filename << "\" failed" <<
	    endl;
	festival_error();
    }

    if (utt == NIL)
	return siod(u);
    else
	return utt;
}

static LISP utt_relation_load(LISP utt, LISP lrelname, LISP lfilename)
{
    EST_Utterance *u;
    if (utt == NIL)
	u = new EST_Utterance;
    else
	u = utterance(utt);
    EST_String filename = get_c_string(lfilename);
    EST_String relname = get_c_string(lrelname);
    EST_Relation *rel = u->create_relation(relname);
    
    if (rel->load(filename,"esps") != 0)
    {
	cerr << "utt.load.relation: loading from \"" << filename << 
	    "\" failed" << endl;
	festival_error();
    }

    if (utt == NIL)
	return siod(u);
    else
	return utt;
}

static LISP utt_evaluate_features(LISP utt)
{
    EST_Utterance *u = utterance(utt);
    u->evaluate_all_features();
    return NIL;
}

static LISP utt_evaluate_relation(LISP utt, LISP rname)
{
    EST_Utterance *u = utterance(utt);
    EST_String relname = get_c_string(rname);
    EST_Relation *r = u->relation(relname);
    
    r->evaluate_item_features();
    return NIL;
}

static LISP utt_copy_relation(LISP utt, LISP l_old_name, LISP l_new_name)
{
    EST_Utterance *u = utterance(utt);
    EST_String old_name = get_c_string(l_old_name);
    EST_String new_name = get_c_string(l_new_name);

    u->create_relation(new_name);

    u->relation(new_name)->f = u->relation(old_name)->f;

    copy_relation(*u->relation(old_name), *u->relation(new_name));
	
    return utt;
}

static LISP utt_copy_relation_and_items(LISP utt, LISP l_old_name, 
					LISP l_new_name)
{
    EST_Utterance *u = utterance(utt);
    EST_String old_name = get_c_string(l_old_name);
    EST_String new_name = get_c_string(l_new_name);

    u->create_relation(new_name);

    u->relation(new_name)->f = u->relation(old_name)->f;

    *u->relation(new_name) = *u->relation(old_name);
	
    return utt;
}

static LISP utt_relation_print(LISP utt, LISP l_name)
{
    EST_Utterance *u = utterance(utt);
    EST_String name = get_c_string(l_name);

    cout << *u->relation(name);
    return NIL;
}

static LISP utt_relation_items(LISP utt, LISP rname)
{
    EST_Utterance *u = utterance(utt);
    EST_String relationname = get_c_string(rname);
    EST_Item *i;
    LISP l = NIL;
    
    for (i=u->relation(relationname)->head(); i != 0; i=next_item(i))
	l = cons(siod(i),l);
	
    return reverse(l);
}

// could be merged with above

LISP utt_relation_tree(LISP utt, LISP sname)
{
    EST_Utterance *u = utterance(utt);
    EST_String relname = get_c_string(sname);

    return stream_tree_to_lisp(u->relation(relname)->head());
}

static LISP stream_tree_to_lisp(EST_Item *s)
{
    if (s == 0)
	return NIL;
    else
    {
	LISP desc = cons(strintern(s->name()),
			 cons(item_features(s, false),NIL));
	return cons(cons(desc,stream_tree_to_lisp(idown(s))),
		    stream_tree_to_lisp(inext(s)));
    }
}

static LISP set_item_name(LISP litem, LISP newname)
{
    // Set a stream's name to newname
    EST_Item *s = item(litem);
    
    if (s != 0)
	s->set_name(get_c_string(newname));
    return litem;
}

static LISP utt_relation(LISP utt, LISP relname)
{
    EST_Utterance *u = utterance(utt);
    EST_String rn = get_c_string(relname);
    EST_Item *r;

    r = u->relation(rn)->head();
    
    return siod(r);
}

static LISP utt_relation_create(LISP utt, LISP relname)
{
    EST_Utterance *u = utterance(utt);
    EST_String rn = get_c_string(relname);

    u->create_relation(rn);
    
    return utt;
}

static LISP utt_relation_delete(LISP utt, LISP relname)
{
    EST_Utterance *u = utterance(utt);
    EST_String rn = get_c_string(relname);

    u->remove_relation(rn);
    
    return utt;
}

static LISP utt_relationnames(LISP utt)
{
    // Return list of relation names
    EST_Utterance *u = utterance(utt);
    LISP relnames = NIL;
    EST_Features::Entries p;

    for (p.begin(u->relations); p; p++)
	relnames = cons(rintern(p->k),relnames);

    return reverse(relnames);
}

static LISP item_relations(LISP si)
{
    // Return list of relation names
    EST_Item *s = item(si);
    LISP relnames = NIL;
    EST_Litem *p;

    for (p = s->relations().list.head(); p; p=p->next())
	relnames = cons(rintern(s->relations().list(p).k),relnames);

    return reverse(relnames);
}

static LISP item_relation_name(LISP si)
{
    // Return list of relation names
    EST_Item *s = item(si);

    return rintern(s->relation_name());
}

static LISP item_relation_remove(LISP li, LISP relname)
{
    EST_String rn = get_c_string(relname);
    EST_Item *si = item(li);
    remove_item(si,rn);
    // Just in case someone tries to access this again 
    // we set its contents to be 0 which will picked up by item
    delete (EST_Val *)USERVAL(li);
    EST_Val vv = est_val((EST_Item *)0);
    USERVAL(li) = new EST_Val(vv);
    return NIL;
}

static LISP utt_relation_append(LISP utt, LISP relname, LISP li)
{
    EST_Utterance *u = utterance(utt);
    EST_String rn = get_c_string(relname);
    EST_Relation *r = u->relation(rn);
    EST_Item *s=0;

    if (!r)
	return NIL;
    if (item_p(li))
	s = item(li);

    s = r->append(s);

    if (consp(li))
    {
	s->set_name(get_c_string(car(li)));
	add_item_features(s,car(cdr(li)));
    }
    
    return siod(s);
}

static LISP item_next(LISP li)
{
    return (li == NIL) ? NIL : siod(inext(item(li)));
}

static LISP item_prev(LISP li)
{
    return (li == NIL) ? NIL : siod(iprev(item(li)));
}

static LISP item_up(LISP li)
{
    return (li == NIL) ? NIL : siod(iup(item(li)));
}

static LISP item_down(LISP li)
{
    return (li == NIL) ? NIL : siod(idown(item(li)));
}

static LISP item_parent(LISP li)
{
    if (li == NIL) 
	return NIL;
    else 
	return siod(parent(item(li)));
}

static LISP item_daughter1(LISP li)
{
    if (li == NIL) 
	return NIL;
    else 
	return siod(daughter1(item(li)));
}

static LISP item_daughter2(LISP li)
{
    if (li == NIL) 
	return NIL;
    else 
	return siod(daughter2(item(li)));
}

static LISP item_daughtern(LISP li)
{
    if (li == NIL) 
	return NIL;
    else 
	return siod(daughtern(item(li)));
}

static LISP item_link1(LISP li)
{
    if (li == NIL) 
	return NIL;
    else 
	return siod(link1(item(li)));
}

static LISP item_link2(LISP li)
{
    if (li == NIL) 
	return NIL;
    else 
	return siod(link2(item(li)));
}

static LISP item_linkn(LISP li)
{
    if (li == NIL) 
	return NIL;
    else 
	return siod(linkn(item(li)));
}

static LISP item_next_link(LISP li)
{
    if (li == NIL) 
	return NIL;
    else 
	return siod(next_link(item(li)));
}

static LISP item_linkedfrom(LISP li)
{
    if (li == NIL) 
	return NIL;
    else 
	return siod(linkedfrom(item(li)));
}

static LISP item_next_leaf(LISP li)
{
    return (li == NIL) ? NIL : siod(next_leaf(item(li)));
}

static LISP item_first_leaf(LISP li)
{
    return (li == NIL) ? NIL : siod(first_leaf(item(li)));
}

static LISP item_last_leaf(LISP li)
{
    return (li == NIL) ? NIL : siod(last_leaf(item(li)));
}

static LISP item_add_link(LISP lfrom, LISP lto)
{
    add_link(item(lfrom),item(lto));
    return NIL;
}

static LISP utt_id(LISP lutt, LISP l_id)
{
    EST_Utterance *u = utterance(lutt);
    
    return siod(u->id(get_c_string(l_id)));
}

static LISP item_next_item(LISP li)
{
    return (li == NIL) ? NIL : siod(next_item(item(li)));
}

static LISP item_append_daughter(LISP li,LISP nli)
{
    EST_Item *l = item(li);
    EST_Item *s = 0;

    if (item_p(nli))
	s = item(nli);

    s = l->append_daughter(s);

    if (consp(nli))
    {
	s->set_name(get_c_string(car(nli)));
	add_item_features(s,car(cdr(nli)));
    }
    
    return siod(s);
}

static LISP item_prepend_daughter(LISP li,LISP nli)
{
    EST_Item *l = item(li);
    EST_Item *s = 0;

    if (item_p(nli))
	s = item(nli);

    s = l->prepend_daughter(s);

    if (consp(nli))
    {
	s->set_name(get_c_string(car(nli)));
	add_item_features(s,car(cdr(nli)));
    }
    
    return siod(s);
}

static LISP item_insert_parent(LISP li,LISP nparent)
{
    EST_Item *l = item(li);
    EST_Item *s = 0;

    if (item_p(nparent))
	s = item(nparent);

    s = l->insert_parent(s);

    if (consp(nparent))
    {
	s->set_name(get_c_string(car(nparent)));
	add_item_features(s,car(cdr(nparent)));
    }
    
    return siod(s);
}

static LISP item_insert(LISP li,LISP nli,LISP direction)
{
    EST_Item *n = item(li);
    EST_String dir;
    EST_Item *s;

    if (item_p(nli))
	s = item(nli);
    else
	s = 0;

    if (direction)
	dir = get_c_string(direction);
    else
	dir = "after";

    if (dir == "after")
	s = n->insert_after(s);
    else if (dir == "before")
	s = n->insert_before(s);
    else if (dir == "above")
	s = n->insert_above(s);
    else if (dir == "below")
	s = n->insert_below(s);
    else
    {
	cerr << "item.insert: unknown direction \"" << dir << "\"" << endl;
	festival_error();
    }

    if (consp(nli))  // specified information
    {
	s->set_name(get_c_string(car(nli)));
	add_item_features(s,car(cdr(nli)));
    }

    return siod(s);
}

static LISP item_move_tree(LISP from,LISP to)
{
    EST_Item *f = item(from);
    EST_Item *t = item(to);

    if (move_sub_tree(f,t) == TRUE)
	return truth;
    else
	return NIL;  
}

static LISP item_merge_item(LISP from,LISP to)
{
    EST_Item *f = item(from);
    EST_Item *t = item(to);

    merge_item(f,t);
    return truth;
}

static LISP item_exchange_tree(LISP from,LISP to)
{
    EST_Item *f = item(from);
    EST_Item *t = item(to);

    if (exchange_sub_trees(f,t) == TRUE)
	return truth;
    else
	return NIL;  
}

static LISP item_relation(LISP lingitem,LISP relname)
{
    EST_Item *li = item(lingitem);
    EST_String rn = get_c_string(relname);
    return siod(li->as_relation(rn));
}

void utt_cleanup(EST_Utterance &u)
{
    // Remove all relations
    // This is called in the Initialization to ensure we can 
    // continue with a nice clean utterance

    u.relations.clear();
}

LISP make_utterance(LISP args,LISP env)
{
    /* Make an utterance structure from given input */
    (void)env;
    EST_Utterance *u = new EST_Utterance;
    EST_String t;
    LISP lform;

    u->f.set("type",get_c_string(car(args)));
    lform = car(cdr(args));
    u->f.set("iform",siod_sprint(lform));

    return siod(u);
}

static LISP item_delete(LISP litem)
{
    EST_Item *s = item(litem);

    s->unref_all();
    delete (EST_Val *)USERVAL(litem);
    EST_Val vv = est_val((EST_Item *)0);
    USERVAL(litem) = new EST_Val(vv);

    return NIL;
}

static LISP item_remove_feature(LISP litem,LISP fname)
{
    EST_Item *s = item(litem);
    EST_String f = get_c_string(fname);

    s->f_remove(f);

    return rintern("t");
}

static LISP item_features(LISP litem, LISP leval)
{
    // Return assoc list of features on this stream
    return item_features(item(litem), ((leval != NIL) ? true : false));
}

static LISP item_features(EST_Item *s, bool evaluate_ff)
{
    // Can't simply use features_to_lisp as evaluation requires access
    // to item.
    LISP features = NIL;
    EST_Val tv;

    EST_Features::Entries p;
    for (p.begin(s->features()); p != 0; ++p)
    {
	const EST_Val &v = p->v;
	LISP fpair;
	
	if (v.type() == val_int)
	    fpair = make_param_int(p->k, v.Int());
	else if (v.type() == val_float)
	    fpair = make_param_float(p->k, v.Float());
	else if (v.type() == val_type_feats)
	    fpair = make_param_lisp(p->k, 
				    features_to_lisp(*feats(v)));
	else if ((v.type() == val_type_featfunc) && evaluate_ff)
	{
	    tv = (featfunc(v))(s);
	    if (tv.type() == val_int)
		fpair = make_param_int(p->k, tv.Int());
	    else if (tv.type() == val_float)
	    {
		fpair = make_param_float(p->k, 
					 tv.Float());
	    }
	    else
		fpair = make_param_lisp(p->k,
					strintern(tv.string()));
	}
	else
	    fpair = make_param_lisp(p->k,
				    strintern(v.string()));
	features = cons(fpair,features);
    }

    return reverse(features);
}

void add_item_features(EST_Item *s,LISP features)
{
    // Add LISP specified features to s;
    LISP f;

    for (f=features; f != NIL; f=cdr(f))
	s->set_val(get_c_string(car(car(f))),
		   val_lisp(car(cdr(car(f)))));
}

void festival_utterance_init(void)
{
    // declare utterance specific Lisp functions 

    // Standard functions
    init_fsubr("Utterance",make_utterance,
 "(Utterance TYPE DATA)\n\
  Build an utterance of type TYPE from DATA.  Different TYPEs require\n\
  different types of data.  New types may be defined by defUttType.\n\
  [see Utterance types]");
    init_subr_2("utt.load",utt_load,
 "(utt.load UTT FILENAME)\n\
  Loads UTT with the streams and stream items described in FILENAME.\n\
  The format is Xlabel-like as saved by utt.save.  If UTT is nil a new\n\
  utterance is created, loaded and returned.  If FILENAME is \"-\"\n\
  the data is read from stdin.");
    init_subr_2("utt.feat",utt_feat,
 "(utt.feat UTT FEATNAME)\n\
  Return value of feature name in UTT.");
    init_subr_3("utt.set_feat",utt_set_feat,
 "(utt.set_feat UTT FEATNAME VALUE)\n\
  Set feature FEATNAME with VALUE in UTT.");
    init_subr_1("utt.flat_repr", utt_flat_repr,
  "(utt.flat_repr UTT)\n\
   Returns a flat, string representation of the linguistic information\n\
   contained in fully formed utterance structure UTT." );
    init_subr_3("utt.relation.load",utt_relation_load,
 "(utt.relation.load UTT RELATIONNAME FILENAME)\n\
  Loads (and creates) RELATIONNAME from FILENAME into UTT.  FILENAME\n\
  should contain simple Xlabel format information.  The label part\n\
  may contain the label proper followed by semi-colon separated\n\
  pairs of feature and value.");
    init_subr_3("utt.save",utt_save,
 "(utt.save UTT FILENAME TYPE)\n\
  Save UTT in FILENAME in an Xlabel-like format.  If FILENAME is \"-\"\n\
  then print output to stdout.  TYPE may be nil or est_ascii");
    init_subr_4("utt.save.relation",utt_save_relation,
 "(utt.save UTT RELATIONNAME FILENAME EVALUATE_FEATURES)\n\
  Save relation RELATIONNAME in FILENAME in an Xlabel-like format. \n\
  If FILENAME is \"-\" then print output to stdout.");

    init_subr_3("utt.copy_relation", utt_copy_relation,
    "(utt.copy_relation UTT FROM TO)\n\
    copy relation \"from\" to a new relation \"to\". Note that items\n\
    are NOT copied, simply linked into the new relation");
 
     init_subr_3("utt.copy_relation_and_items", utt_copy_relation_and_items,
   "(utt.copy_relation_and_items UTT FROM TO)\n\
    copy relation and contents of items \"from\" to a new relation \"to\"");
 
    init_subr_2("utt.relation.print", utt_relation_print,
  "(utt.relation.print UTT NAME)\n\
   print contents of relation NAME");

    init_subr_1("utt.evaluate", utt_evaluate_features,
  "(utt.evaluate UTT)\n\
   evaluate all the features in UTT, replacing feature functions\n\
   with their evaluation.");

    init_subr_2("utt.evaluate.relation", utt_evaluate_relation,
  "(utt.evaluate.relation UTT)\n\
   evaluate all the features in RELATION in UTT, replacing feature functions\n\
   with their evaluation.");

    init_subr_2("utt.relation.items",utt_relation_items,
 "(utt.relation.items UTT RELATIONNAME)\n\
  Return a list of stream items in RELATIONNAME in UTT. \n\
  If this relation is a tree, the parent streamitem is listed before its \n\
  daughters.");
    init_subr_2("utt.relation_tree",utt_relation_tree,
 "(utt.relation_tree UTT RELATIONNAME)\n\
  Return a tree of stream items in RELATIONNAME in UTT.  This will give a\n\
  simple list if the relation has no ups and downs. \n\
  [see Accessing an utterance]");
    init_subr_1("item.delete",item_delete,
 "(item.delete ITEM)\n\
  Remove this item from all relations it is in and delete it.");
    init_subr_2("item.set_name",set_item_name,
 "(item.set_name ITEM NAME)\n\
  Sets ITEM's name to NAME. [see Accessing an utterance]");
    init_subr_2("item.features",item_features,
 "(item.features ITEM EVALUATE_FEATURES))\n\
  Returns all features in ITEM as an assoc list.");
    init_subr_2("item.remove_feature",item_remove_feature,
 "(item.remove_feature ITEM FNAME)\n\
  Remove feature named FNAME from ITEM.  Returns t is successfully\n\
  remove, nil if not found.");

    // New Utterance architecture (Relations and items)
    init_subr_2("utt.relation",utt_relation,
 "(utt.relation UTT RELATIONNAME)\n\
  Return root item of relation RELATIONNAME in UTT.");
    init_subr_2("utt.relation.create",utt_relation_create,
 "(utt.relation.create UTT RELATIONNAME)\n\
  Create new relation called RELATIONNAME in UTT.");
    init_subr_2("utt.relation.delete",utt_relation_delete,
 "(utt.relation.delete UTT RELATIONNAME)\n\
  Delete relation from utt, it the stream items are not linked elsewhere\n\
  in the utterance they will be deleted too.");
    init_subr_2("item.relation.remove",item_relation_remove,
 "(item.relation.remove ITEM RELATIONNAME)\n\
  Remove this item from Relation, if it apears in no other relation it\n\
  will be deleted too, in contrast item.delete will remove an item\n\
  from all other relations, while this just removes it from this relation.\n\
  Note this will also remove all daughters of this item in this \n\
  relation from this relation.");
    init_subr_1("utt.relationnames",utt_relationnames,
 "(utt.relationnames UTT)\n\
  List of all relations in this utterance.");
    init_subr_3("utt.relation.append",utt_relation_append,
 "(utt.relation.append UTT RELATIONNAME ITEM)\n\
  Append ITEM to top of RELATIONNAM in UTT.  ITEM may be\n\
  a LISP description of an item or an item itself.");

    init_subr_1("item.next",item_next,
 "(item.next ITEM)\n\
  Return the next ITEM in the current relation, or nil if there is\n\
  no next.");
    init_subr_1("item.prev",item_prev,
 "(item.prev ITEM)\n\
  Return the previous ITEM in the current relation, or nil if there\n\
  is no previous.");
    init_subr_1("item.up",item_up,
 "(item.up ITEM)\n\
  Return the item above ITEM, or nil if there is none.");
    init_subr_1("item.down",item_down,
 "(item.down ITEM)\n\
  Return the item below ITEM, or nil if there is none.");
    init_subr_3("item.insert",item_insert,
 "(item.insert ITEM1 ITEM2 DIRECTION)\n\
  Insert ITEM2 in ITEM1's relation with respect to DIRECTION.  If DIRECTION\n\
  is unspecified, after, is assumed.  Valid DIRECTIONS as before, after,\n\
  above and below.  Use the functions item.insert_parent and\n\
  item.append_daughter for specific tree adjoining.  If ITEM2 is of\n\
  type item then it is added directly, otherwise it is treated as a\n\
  description of an item and new one is created.");

    // Relation tree access/creation functions
    init_subr_1("item.parent",item_parent,
 "(item.parent ITEM)\n\
  Return the item of ITEM, or nil if there is none.");
    init_subr_1("item.daughter1",item_daughter1,
 "(item.daughter1 ITEM)\n\
  Return the first daughter of ITEM, or nil if there is none.");
    init_subr_1("item.daughter2",item_daughter2,
 "(item.daughter2 ITEM)\n\
  Return the second daughter of ITEM, or nil if there is none.");
    init_subr_1("item.daughtern",item_daughtern,
 "(item.daughtern ITEM)\n\
  Return the last daughter of ITEM, or nil if there is none.");
    init_subr_1("item.next_leaf",item_next_leaf,
 "(item.next_leaf ITEM)\n\
  Return the next leaf item (i.e. one with no daughters) in this \n\
  relation.  Note this may traverse up and down the relation tree \n\
  significantly to find it.");
    init_subr_1("item.first_leaf",item_first_leaf,
 "(item.first_leaf ITEM)\n\
  Returns he left most leaf in the tree dominated by ITEM.  This \n\
  is like calling item.daughter1 recursively until an item with no \n\
  daughters is found.");
    init_subr_1("item.last_leaf",item_last_leaf,
 "(item.last_leaf ITEM)\n\
  Returns he right most leaf in the tree dominated by ITEM.  This \n\
  is like calling item.daughtern recursively until an item with no \n\
  daughters is found.");
    init_subr_2("item.append_daughter",item_append_daughter,
 "(item.append_daughter ITEM1 ITEM2)\n\
  Add a ITEM2 a new daughter (right-most) to ITEM1 in the relation of \n\
  ITEM1. If ITEM2 is of type item then it is added directly otherwise\n\
  ITEM2 is treated as a description of an item and a one is created\n\
  with that description (name features).");
    init_subr_2("item.prepend_daughter",item_prepend_daughter,
 "(item.prepend_daughter ITEM1 ITEM2)\n\
  Add a ITEM2 a new daughter (left-most) to ITEM1 in the relation of ITEM1.\n\
  If ITEM2 is of type item then it is added directly otherwise\n\
  ITEM2 is treated as a description of an item and a one is created\n\
  with that description (name features).");
    init_subr_2("item.insert_parent",item_insert_parent,
 "(item.insert_parent ITEM1 ITEM2)\n\
  Insert a new parent between this ITEM1 and its parentm in ITEM1's \n\
  relation.  If ITEM2 is of type item then it is added directly, \n\
  otherwise it is treated as a description of an item and  one is created\n\
  with that description (name features).");


    // MLS access/creation functions
    init_subr_1("item.link1",item_link1,
 "(item.link1 ITEM)\n\
  Return first item linked to ITEM in current relation.");
    init_subr_1("item.link2",item_link2,
 "(item.link2 ITEM)\n\
  Return second item linked to ITEM in current relation.");
    init_subr_1("item.linkn",item_linkn,
 "(item.linkn ITEM)\n\
  Return last item linked to ITEM in current relation.");
    init_subr_1("item.next_link",item_next_link,
 "(item.next_link ITEM)\n\
  Return next item licked to the same item ITEM is linked to.");
    init_subr_1("item.linkedfrom",item_linkedfrom,
 "(item.linkedfrom ITEM)\n\
  Return the item tht is linked to ITEM.");
    init_subr_2("item.add_link",item_add_link,
 "(item.add_link ITEMFROM ITEMTO)\n\
  Add a link from ITEMFROM to ITEMTO is the relation ITEMFROM is in.");

    init_subr_1("item.next_item",item_next_item,
 "(item.next_item ITEM)\n\
  Will give next item in this relation visiting every item in the \n\
  relation until the end.  Traverses in pre-order, root followed by \n\
  daughters (then siblings).");

    init_subr_2("utt.id", utt_id,
 "(utt.id UTT id_number)\n\
  Return the item in UTT whose id matches id_number.");

    init_subr_2("item.relation",item_relation,
 "(item.relation ITEM RELATIONNAME)\n\
  Return the item such whose relation is RELATIONNAME.  If ITEM\n\
  is not in RELATIONNAME then nil is return.");

    init_subr_1("item.relations",item_relations,
 "(item.relations ITEM)\n\
  Return a list of names of the relations this item is in.");
    init_subr_1("item.relation.name",item_relation_name,
 "(item.relation.name ITEM)\n\
  Return the name of the relation this ITEM is currently being viewed\n\
  through.");
    init_subr_2("item.move_tree",item_move_tree,
 "(item.move_tree FROM TO)\n\
  Move contents, and descendants of FROM to TO. Old daughters of TO are\n\
  deleted.  FROM will be deleted too if it is being viewed as the same\n\
  same relation as TO.  FROM will be deleted from its current place in\n\
  TO's relation. Returns t if successful, returns nil if TO is within FROM.");
    init_subr_2("item.exchange_trees",item_exchange_tree,
 "(item.exchange_tree FROM TO)\n\
  Exchanged contents of FROM and TO, and descendants of FROM and TO.\n\
  Returns t if successful, or nil if FROM or TO contain each other.");
    init_subr_2("item.merge",item_merge_item,
 "(item.merge FROM TO)\n\
  Merge FROM into TO making them the same items.  All features in FROM\n\
  are merged into TO and all references to FROM are made to point to TO.");
    init_subr_1("item.get_utt",item_utt,
  "(item.get_utt ITEM)\n\
  Get utterance from given ITEM (if possible).");
    init_subr_1("sub_utt",item_sub_utt,
  "(sub_utt ITEM)\n\
  Return a new utterance that contains a copy of this item and all its\n\
  descendants and related descendants.");
    
    init_subr_1("audio_mode",l_audio_mode,
 "(audio_mode MODE)\n\
 Control audio specific modes.  Five subcommands are supported. If\n\
 MODE is async, start the audio spooler so that Festival need not wait\n\
 for a waveform to complete playing before continuing.  If MODE is\n\
 sync wait for the audio spooler to empty, if running, and they cause\n\
 future plays to wait for the playing to complete before continuing.\n\
 Other MODEs are, close which waits for the audio spooler to finish\n\
 any waveforms in the queue and then closes the spooler (it will restart\n\
 on the next play), shutup, stops the current waveform playing and empties\n\
 the queue, and query which lists the files in the queue.  The queue may\n\
 be up to five waveforms long. [see Audio output]");

}
