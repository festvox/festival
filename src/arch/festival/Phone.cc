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
/* Phone Name sybsystem, allowing definitions of PhoneSets               */
/* by arbitrary features (and mapping)                                   */
/*                                                                       */
/*=======================================================================*/
#include <cstdio>
#include "EST_unix.h"
#include "festival.h"
#include "festivalP.h"

static void check_phoneset(void);
static void ps_add_def(const EST_String &name, PhoneSet *ps);
static LISP lisp_select_phoneset(LISP phoneset);

static LISP phone_set_list = NULL;
static PhoneSet *current_phoneset = NULL;

static const EST_String f_cvox("cvox");
static const EST_String f_vc("vc");
static const EST_String f_ctype("ctype");

VAL_REGISTER_CLASS(phone,Phone)
SIOD_REGISTER_CLASS(phone,Phone)
VAL_REGISTER_CLASS(phoneset,PhoneSet)
SIOD_REGISTER_CLASS(phoneset,PhoneSet)

int Phone::match_features(Phone *foreign)
{
    // Try to match the features of the foreign phone with this one
    EST_Litem *f;

    for (f=features.list.head(); f != 0; f=f->next())
    {	
	if ( features.list(f).v != foreign->val(features.list(f).k))
	    return FALSE;
    }

    return TRUE;

}

PhoneSet::~PhoneSet()
{
    gc_unprotect(&silences);
    gc_unprotect(&map);
    gc_unprotect(&feature_defs);
    gc_unprotect(&phones);
}

Phone * PhoneSet::member(const EST_String &ph) const
{
    LISP p = siod_assoc_str(ph,phones);
    if (p != 0)
	return phone(car(cdr(p)));
    else 
    {
	cerr << "Phone \"" << ph << "\" not member of PhoneSet \"" << 
	    psetname << "\"" << endl;
	return 0;
    }
}

const char *PhoneSet::phnum(const int n) const
{
    // return the nth phone name
    int i;
    LISP p;

    // Should use nth for this, but I want to controll the error case
    for (i=0,p=phones; p != NIL; p=cdr(p),i++)
    {
	if (i == n)
	    return get_c_string(car(car(p)));
    }

    cerr << "Phone (phnum) " << n << 
	" too large, not that many members in PhoneSet \"" << 
	psetname << "\"" << endl;
    festival_error();
    return NULL;
}

int PhoneSet::add_phone(Phone *phone)
{
    // Add phone (deleting existing one with warning)
    LISP lpair;

    lpair = siod_assoc_str(phone->phone_name(),phones);

    if (lpair == NIL)
    {
	phones = cons(make_param_lisp(phone->phone_name(),
				      siod(phone)),
		      phones);
	return TRUE;
    }
    else
	return FALSE;
}

void PhoneSet::set_feature(const EST_String &name, LISP vals)
{
    LISP lpair;

    lpair = siod_assoc_str(name, feature_defs);

    if (lpair == NIL)
	feature_defs = cons(make_param_lisp(name,vals),feature_defs);
    else
    {
	cerr << "PhoneSet: replacing feature definition of " <<
	    name << " PhoneSet " << psetname << endl;
	CAR(cdr(lpair)) = vals;
    }
}

int PhoneSet::phnum(const char *phone) const
{
    // Return a unique number for this phone, i.e. position in list
    int i;
    LISP p;

    for (i=0,p=phones; p != NIL; p=cdr(p),i++)
    {
	if (streq(phone,get_c_string(car(car(p)))))
	    return i;
    }

    cerr << "Phone \"" << phone << "\" not member of PhoneSet \"" << 
	psetname << "\"" << endl;
    festival_error();

    return -1;
}

Phone *PhoneSet::find_matched_phone(Phone *foreign)
{
    // find a phone in the current set that matches the features
    // in foreign (from a different phoneset)
    LISP p;

    for (p=phones; p != NIL; p=cdr(p))
    {
	if (phone(car(cdr(car(p))))->match_features(foreign))
	    return phone(car(cdr(car(p))));
    }

    // could try harder 

    cerr << "Cannot map phoneme " << *foreign << endl;
    festival_error();

    return 0;

}

int PhoneSet::is_silence(const EST_String &ph) const
{
    // TRUE is ph is silence
    
    return (siod_member_str(ph,silences) != NIL);

}

void PhoneSet::set_silences(LISP sils)
{
    silences=sils;
}

void PhoneSet::set_map(LISP m)
{
    map=m;
}

PhoneSet &PhoneSet::operator = (const PhoneSet &a)
{
    psetname = a.psetname;
    silences = a.silences;
    map = a.map;
    feature_defs = a.feature_defs;
    phones = a.phones;
    return *this;
}

LISP make_phoneset(LISP args,LISP env)
{
    // define a new phoneme set 
    (void)env;
    PhoneSet *ps = new PhoneSet;
    Phone *phone;
    LISP f,p,pv;
    LISP name, features, phones;
    EST_String feat,val;
    int num_feats;

    name = car(args);
    features = car(cdr(args));
    phones = car(cdr(cdr(args)));

    ps->set_phone_set_name(get_c_string(name));
    // Define the phonetic features
    num_feats = siod_llength(features);
    for (f=features; f != NIL; f=cdr(f))
	ps->set_feature(get_c_string(car(car(f))),cdr(car(f)));

    // Define the phones
    for (p=phones; p != NIL; p=cdr(p))
    {
	if (siod_llength(cdr(car(p))) != num_feats)
	{
	    cerr << "Wrong number of phone features for "
		<< get_c_string(car(car(p))) << " in " <<
		    get_c_string(name) << endl;
	    festival_error();
	}
	phone = new Phone;
	phone->set_phone_name(get_c_string(car(car(p))));
	for (pv=cdr(car(p)),f=features; f != NIL; pv=cdr(pv),f=cdr(f))
	{
	    feat = get_c_string(car(car(f)));
	    val = get_c_string(car(pv));
	    if (ps->feat_val(feat,val))
		phone->add_feat(feat,val);
	    else
	    {
		cerr << "Phone " << phone->phone_name() << 
		    " has invalid value "
		    << get_c_string(car(pv)) << " for feature "
			<< feat << endl;
		festival_error();
	    }
	}
	if (ps->add_phone(phone) == FALSE)
	{
	    cerr << "Phone " << phone->phone_name() << 
		" multiply defined " << endl;
	    festival_error();
	}
    }

    ps_add_def(ps->phone_set_name(),ps);
    current_phoneset = ps;  // selects this one as current 

    return NIL;
}

static LISP lisp_set_silence(LISP silences)
{
    // Set list of names as silences for current phoneset

    check_phoneset();
    current_phoneset->set_silences(silences);
    return silences;
}

PhoneSet *phoneset_name_to_set(const EST_String &name)
{
    LISP lpair = siod_assoc_str(name,phone_set_list);

    if (lpair == NIL)
    {
	cerr << "Phoneset " << name << " not defined" << endl;
	festival_error();
    }
    
    return phoneset(car(cdr(lpair)));

}

static LISP lisp_select_phoneset(LISP pset)
{
    // Select named phoneset and make it current
    EST_String name = get_c_string(pset);
    LISP lpair;

    lpair = siod_assoc_str(name,phone_set_list);

    if (lpair == NIL)
    {
	cerr << "Phoneset " << name << " not defined" << endl;
	festival_error();
    }
    else
	current_phoneset = phoneset(car(cdr(lpair)));

    return pset;
}

static void ps_add_def(const EST_String &name, PhoneSet *ps)
{
    //  Add phoneset to list of phonesets
    LISP lpair;

    if (phone_set_list == NIL)
	gc_protect(&phone_set_list);

    lpair = siod_assoc_str(name,phone_set_list);
    
    if (lpair == NIL)
    {
	phone_set_list = cons(cons(rintern(name),
				   cons(siod(ps),NIL)),
			      phone_set_list);
    }
    else
    {
	cwarn << "Phoneset \"" << name << "\" redefined" << endl;
	setcar(cdr(lpair),siod(ps));
    }

    return;
}

static void check_phoneset(void)
{
    // check if there is a phoneset defined 
    
    if (current_phoneset == NULL)
    {
	cerr << "No phoneset currently selected";
	festival_error();
    }
}

static EST_Val ff_ph_feature(EST_Item *s,const EST_String &name)
{
    // This function is called for all phone features.
    // It looks at the name to find out the
    // the actual name used to call this feature and removed the
    // ph_ prefix and uses the remainder as the phone feature name
    Phone *phone_def;

    if (!name.contains("ph_",0))
    {
	cerr << "Not a phone feature function " << name << endl;
	festival_error();
    }

    check_phoneset();

    const EST_String &fname = name.after("ph_");
    phone_def = current_phoneset->member(s->name());
    if (phone_def == 0)
    {
	cerr << "Phone " << s->name() << " not in PhoneSet \"" <<
	    current_phoneset->phone_set_name() << "\"" << endl;
	festival_error();
    }

    const EST_String &rrr = phone_def->val(fname,EST_String::Empty);
    if (rrr == EST_String::Empty) 
    {
	cerr << "Phone " << s->name() << " does not have feature " <<
	    fname << endl;
	festival_error();
    }

    return EST_Val(rrr);
}

static PhoneSet *find_phoneset(EST_String name)
{
    // get the phone set from the phone set list
    LISP lpair;

    lpair = siod_assoc_str(name,phone_set_list);

    if (lpair == NIL)
    {
	cerr << "Phoneset \"" << name << "\" not defined" << endl;
	festival_error();
    }
    return phoneset(car(cdr(lpair)));

}

const EST_String &map_phone(const EST_String &fromphonename, const EST_String &fromsetname,
			const EST_String &tosetname)
{
    PhoneSet *fromset, *toset;
    Phone *fromphone, *tophone;

    fromset = find_phoneset(fromsetname);
    toset = find_phoneset(tosetname);

    // should check specific matches in fromset first
    fromphone = fromset->member(fromphonename);
    if (fromphone == 0)
	festival_error();
    tophone = toset->find_matched_phone(fromphone);

    return tophone->phone_name();
}

int ph_is_silence(const EST_String &ph)
{
    // TRUE if this phone is silence

    check_phoneset();
    return current_phoneset->is_silence(ph);

}

EST_String ph_silence(void)
{
    // return the first silence in the current_phoneset
    EST_String s;

    check_phoneset();
    
    if (current_phoneset->get_silences() == NIL)
    {
	cerr << "No silences set for PhoneSet\"" << 
	    current_phoneset->phone_set_name() << "\"" << endl;
	return "sil";
    }
    else
	return get_c_string(car(current_phoneset->get_silences()));

}

int ph_is_vowel(const EST_String &ph)
{
    // TRUE if this phone is a vowel, assumes the feature vc is used

    return (ph_feat(ph,f_vc) == "+");
}

int ph_is_consonant(const EST_String &ph)
{
    // TRUE if this phone is a consonant, assumes the feature vc is used

    return ((ph_feat(ph,f_vc) == "-") &&
	    !(ph_is_silence(ph)));
}

int ph_is_liquid(const EST_String &ph)
{
    // TRUE if this phone is a liquid

    return (ph_feat(ph,f_ctype) == "l");
}

int ph_is_approximant(const EST_String &ph)
{
    // TRUE if this phone is an approximant

    return (ph_feat(ph,f_ctype) == "r");
}

int ph_is_stop(const EST_String &ph)
{
    // TRUE if this phone is a stop

    return (ph_feat(ph,f_ctype) == "s");
}

int ph_is_fricative(const EST_String &ph)
{
    // TRUE if this phone is a stop

    return (ph_feat(ph,f_ctype) == "f");
}

int ph_is_nasal(const EST_String &ph)
{
    // TRUE if this phone is a nasal

    return (ph_feat(ph,f_ctype) == "n");
}

int ph_is_obstruent(const EST_String &ph)
{
    // TRUE if this phone is a obstruent
    EST_String v = ph_feat(ph,f_ctype);

    return ((v == "s") || (v == "f") || (v == "a"));
}

int ph_is_sonorant(const EST_String &ph)
{
    // TRUE if this phone is a sonorant

    return !ph_is_obstruent(ph);
}

int ph_is_voiced(const EST_String &ph)
{
    // TRUE if this phone is a sonorant

    return (ph_feat(ph,f_cvox) == "+");
}

int ph_is_syllabic(const EST_String &ph)
{
    // TRUE if this phone is a syllabic consonant (or vowel)
    // Yes I know we just don't have this ...

    return (ph_feat(ph,f_vc) == "+");
}

const EST_String &ph_feat(const EST_String &ph,const EST_String &feat)
{
    // Values for this phone -- error is phone of feat doesn't exist
    Phone *phone_def;
    EST_String rrr;

    check_phoneset();
    phone_def = current_phoneset->member(ph);
    if (phone_def == 0)
    {
	cerr << "Phone " << ph << " not in phone set " <<
	    current_phoneset->phone_set_name() << endl;
	festival_error();
    }

    return phone_def->val(feat,EST_String::Empty);

}

int ph_sonority(const EST_String &ph)
{
    // assumes standard phone features
    Phone *p;

    check_phoneset();
    p = current_phoneset->member(ph);
    if (p == 0)
	return 1;
    
    if (p->val(f_vc) == "+")
	return 5;
    else if (p->val(f_ctype) == "l") // || glide 
        return 4;
    else if (p->val(f_ctype) == "n")
        return 3;
    else if (p->val(f_cvox) == "+") // voiced obstruents (stop fric affric)
        return 2;
    else
        return 1;

}

LISP l_phoneset(LISP options)
{
    //  Return Lisp form of current phone set 
    LISP description=NIL;

    check_phoneset();

    if ((options == NIL) ||
	(siod_member_str("silences",options)))
    {
	description = cons(make_param_lisp("silences",
					   current_phoneset->get_silences()),
			   description);
    }
    if ((options == NIL) ||
	(siod_member_str("phones",options)))
    {
	LISP phones = current_phoneset->get_phones();
	LISP features = current_phoneset->get_feature_defs();
	LISP p,f,p_desc=NIL,f_desc=NIL;
	
	for (p=phones; p != NIL; p=cdr(p))
	{
	    f_desc = NIL;
	    for (f=reverse(features); f != NIL; f=cdr(f))
	    {
		f_desc = cons(rintern(ph_feat(get_c_string(car(car(p))),
						get_c_string(car(car(f))))),
			      f_desc);
	    }
	    p_desc = cons(cons(car(car(p)),f_desc),p_desc);
	}
	description = cons(make_param_lisp("phones",p_desc),description);
    }
    if ((options == NIL) ||
	(siod_member_str("features",options)))
    {
	description = cons(make_param_lisp("features",
					   current_phoneset->get_feature_defs()),
			   description);
    }
    if ((options == NIL) ||
	(siod_member_str("name",options)))
    {
	description = cons(make_param_str("name",
					  current_phoneset->phone_set_name()),
			   description);
    }

    return description;
}

LISP l_phoneset_list()
{
    LISP phonesets = NIL;
    LISP l;

    for (l=phone_set_list; l != NIL; l=cdr(l))
	phonesets = cons(car(car(l)),phonesets);

    return phonesets;
}

void festival_Phone_init(void)
{
    // define Lisp accessor functions 

    init_fsubr("defPhoneSet",make_phoneset,
 "(defPhoneSet PHONESETNAME FEATURES PHONEDEFS)\n\
  Define a new phoneset named PHONESETNAME.  Each phone is described with a\n\
  set of features as described in FEATURES.  Some of these FEATURES may\n\
  be significant in various parts of the system.  Copying an existing\n\
  description is a good start. [see Phonesets]");
    init_subr_1("PhoneSet.select",lisp_select_phoneset,
 "(PhoneSet.select PHONESETNAME)\n\
  Select PHONESETNAME as current phoneset. [see Phonesets]");
    init_subr_1("PhoneSet.silences",lisp_set_silence,
 "(PhoneSet.silences LIST)\n\
  Declare LIST of phones as silences.  The first in the list should be\n\
  the \"most\" silent. [see Phonesets]");
    init_subr_1("PhoneSet.description",l_phoneset,
 "(Phoneset.description OPTIONS)\n\
  Returns a lisp for of the current phoneme set.  Options is a list of\n\
  parts of the definition you require.  OPTIONS may include, silences,\n\
  phones, features and/or name.  If nil all are returned.");
    init_subr_0("PhoneSet.list",l_phoneset_list,
 "(Phoneset.list)\n\
  List the names of all currently defined Phonesets.");
    // All feature functions starting with "ph_"
    festival_def_ff_pref("ph_","Segment",ff_ph_feature,
    "Segment.ph_*\n\
  Access phoneset features for a segment.  This definition covers multiple\n\
  feature functions where ph_ may be extended with any features that\n\
  are defined in the phoneset (e.g. vc, vlng, cplace etc.).");

}

