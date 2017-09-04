/*************************************************************************/
/*                                                                       */
/*                   Carnegie Mellon University and                      */
/*                Centre for Speech Technology Research                  */
/*                     University of Edinburgh, UK                       */
/*                       Copyright (c) 1998-2001                         */
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
/*  THE UNIVERSITY OF EDINBURGH, CARNEGIE MELLON UNIVERSITY AND THE      */
/*  CONTRIBUTORS TO THIS WORK DISCLAIM ALL WARRANTIES WITH REGARD TO     */
/*  THIS SOFTWARE, INCLUDING ALL IMPLIED WARRANTIES OF MERCHANTABILITY   */
/*  AND FITNESS, IN NO EVENT SHALL THE UNIVERSITY OF EDINBURGH, CARNEGIE */
/*  MELLON UNIVERSITY NOR THE CONTRIBUTORS BE LIABLE FOR ANY SPECIAL,    */
/*  INDIRECT OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES WHATSOEVER          */
/*  RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN  AN ACTION   */
/*  OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF     */
/*  OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.       */
/*                                                                       */
/*************************************************************************/
/*             Author :  Alan W Black                                    */
/*             Date   :  April 1998                                      */
/*-----------------------------------------------------------------------*/
/*                                                                       */
/*  A quick database structure                                           */
/*                                                                       */
/*=======================================================================*/
#include <cstdlib>
#include <cmath>
#include "festival.h"
#include "EST_FileType.h"
#include "clunits.h"

VAL_REGISTER_CLASS(clunitsdb,CLDB)
SIOD_REGISTER_CLASS(clunitsdb,CLDB)
static void cl_load_catalogue(CLDB *cldb,EST_String &indexfile);

static LISP CLDB_list = NIL;
static CLDB *current_cldb = 0;

static void cldb_add(const EST_String &name, CLDB *cldb)
{
    //  Add lexicon to list of lexicons
    LISP lpair;

    lpair = siod_assoc_str(name,CLDB_list);

    if (CLDB_list == NIL)
	gc_protect(&CLDB_list);
    
    if (lpair == NIL)
    {
	CLDB_list = cons(cons(strintern(name),
				   cons(siod(cldb),NIL)),
			    CLDB_list);
    }
    else
    {
	cwarn << "CLDB " << name << " recreated" << endl;
	// old one will be garbage collected
	setcar(cdr(lpair),siod(cldb));
    }

    return;
}


LISP cl_load_db(LISP params)
{
    EST_String indexfile;
    int i;
    LISP w;
    CLDB *cldb = new CLDB;

    cldb->params = params;
    
    indexfile = EST_String("") +
	get_param_str("db_dir",params,"./")+
	get_param_str("catalogue_dir",params,"./")+
	get_param_str("index_name",params,"catalogue")+
	    ".catalogue";
    
    cl_load_catalogue(cldb,indexfile);

    cldb->cweights.resize(siod_llength(get_param_lisp("join_weights",params,NIL)));
    for (i=0,w=get_param_lisp("join_weights",params,NIL); w; w=cdr(w),i++)
	cldb->cweights[i] = get_c_float(car(w));

    cldb_add(get_param_str("index_name",params,"catalogue"),cldb);

    current_cldb = cldb;

    return NIL;
}

static void cl_load_catalogue(CLDB *cldb,EST_String &indexfile)
{
    EST_TokenStream ts;
    EST_EstFileType t;
    EST_Option hinfo;
    EST_String v;
    bool ascii;
    EST_read_status r;
    
    if (((indexfile == "-") ? ts.open(cin) : ts.open(indexfile)) != 0)
    {
	cerr << "CLUNITS: Can't open catalogue file " << indexfile << endl;
	festival_error();
    }

    if (((r = read_est_header(ts, hinfo, ascii, t)) != format_ok) ||
	(t != est_file_index))
    {
	cerr << "CLUNITS: " << indexfile << " is not an indexfile" << endl;
	festival_error();
    }

   CLunit *ls = 0;
    while(!ts.eof())
    {
	CLunit *s = new CLunit;
	s->name = ts.get().string();
	s->base_name = s->name.before("_");
	s->fileid = ts.get().string();
	s->start = atof(ts.get().string());
	s->mid = atof(ts.get().string());
	s->end = atof(ts.get().string());

	if ((ls != 0) &&
	    (ls->fileid == s->fileid) &&
	    (ls->end == s->start))
	{
	    s->prev_unit = ls;
	    ls->next_unit = s;
	}
	cldb->index.add(s->name,s);
	ls = s;
    }
}

CLDB *check_cldb()
{
    if (current_cldb == 0)
    {
	cerr << "CLDB: no database loaded\n";
	festival_error();
    }
    return current_cldb;
}

void cl_maybe_fix_pitch_c0(EST_Track *c)
{
    // If its pitch synchronous, trash the first coefficient with
    // the pitch value, there should be a cleaner way to do this
    int i;
    float ltime = 0;
    
    if (!c->equal_space())
    {
	for (i=0; i < c->num_frames(); i++)
	{
	    c->a_no_check(i,0) = 1/(c->t(i)-ltime);
	    ltime = c->t(i);
	}
    }
}

void CLDB::load_join_coefs(CLunit *unit)
{
    // Load in the coefficients and signal for this unit.
    CLfile *fileitem;
    EST_Track *join_coeffs;

    if (unit->join_coeffs != 0)
	return;

    fileitem = get_file_join_coefs(unit->fileid);

    EST_Track *unit_join_coeffs = new EST_Track;
    join_coeffs = fileitem->join_coeffs;

    int pm_start = join_coeffs->index(unit->start);
    int pm_end = join_coeffs->index(unit->end);

    join_coeffs->sub_track(*unit_join_coeffs, pm_start, pm_end-pm_start+1,0);
    unit->join_coeffs = unit_join_coeffs;
}

CLfile *CLDB::get_file_join_coefs(const EST_String &fileid)
{
    CLfile *fileitem;

    fileitem = get_fileitem(fileid);

    if (fileitem == 0)
    {   // even the file isn't here
	fileitem = new CLfile;
	fileindex.add(fileid,fileitem);
    }
    if (fileitem->join_coeffs == 0)
    {
	EST_Track *join_coeffs = new EST_Track;
	EST_String jc_filename = 
	    EST_String("") +
		get_param_str("db_dir",params,"./") +
		    get_param_str("coeffs_dir",params,"wav/") +
			fileid+
  		          get_param_str("coeffs_ext",params,".dcoeffs");
	if (join_coeffs->load(jc_filename) != format_ok)
	{
	    delete join_coeffs;
	    cerr << "CLUNITS: failed to load join coeffs file " << 
		jc_filename << endl;
	    festival_error();
	} 
//	cl_maybe_fix_pitch_c0(join_coeffs);
	fileitem->join_coeffs = join_coeffs;
    }

    return fileitem;
}

CLfile *CLDB::get_file_coefs_sig(const EST_String &fileid)
{
    CLfile *fileitem = get_fileitem(fileid);
    
    if (fileitem == 0)
    {   // even the file isn't here
	fileitem = new CLfile;
	fileindex.add(fileid,fileitem);
    }
    if (fileitem->sig == 0)
    {
	EST_Track *track = new EST_Track;
	EST_String coef_filename = 
	    EST_String("") +
		get_param_str("db_dir",params,"./") +
		    get_param_str("pm_coeffs_dir",params,"pm/") +
			fileid+
			    get_param_str("pm_coeffs_ext",params,".pm");
	if (track->load(coef_filename) != format_ok)
	{
	    delete track;
	    cerr << "CLUNITS: failed to load coeffs file " << 
		coef_filename << endl;
	    festival_error();
	}
	fileitem->coefs = track;
	
	EST_Wave *sig = new EST_Wave;
	EST_String sig_filename = 
	    EST_String("") +
		get_param_str("db_dir",params,"./") +
		    get_param_str("sig_dir",params,"wav/") +
			fileid+
			    get_param_str("sig_ext",params,".wav");
	if (sig->load(sig_filename) != format_ok)
	{
	    delete sig;
	    cerr << "CLUNITS: failed to load signal file " << 
		sig_filename << endl;
	    festival_error();
	} 
	fileitem->sig = sig;
    }
    return fileitem;
}

void CLDB::load_coefs_sig(EST_Item *unit)
{
    // Load in the coefficients and signal for this unit.
    EST_String fileid = unit->f("fileid");
    CLfile *fileitem;

    fileitem = get_file_coefs_sig(fileid);

    EST_Track *coeffs = fileitem->coefs;
    EST_Wave *sig = fileitem->sig;
    EST_Track u1;
    EST_Wave *unit_sig = new EST_Wave;

    int pm_start = coeffs->index(unit->F("start"));
    int pm_middle = coeffs->index(unit->F("middle"));
    int pm_end = coeffs->index(unit->F("end"));

//    coeffs->sub_track(u1,Gof((pm_start-1),0), pm_end - pm_start + 1);
    coeffs->sub_track(u1,pm_start, pm_end - pm_start + 1,0);
    EST_Track *unit_coeffs = new EST_Track(u1);
    for (int j = 0; j < u1.num_frames(); ++j)
	unit_coeffs->t(j) = u1.t(j) - coeffs->t(Gof((pm_start - 1), 0));

/*    printf("coefs %s: pm_start %d pm_end %d pm_length %d\n",
	   (const char *)fileid,
	   pm_start, pm_end, pm_end - pm_start + 1); */
    unit->set_val("coefs",est_val(unit_coeffs));

    if ((pm_middle-pm_start-1) < 1)
	unit->set("middle_frame", 1);
    else
	unit->set("middle_frame", pm_middle - pm_start -1);
    int samp_start = (int)(coeffs->t(Gof((pm_start - 1), 0))
 		  * (float)sig->sample_rate());
    int samp_end;
    if ((pm_end + 1) < coeffs->num_frames())
 	samp_end = (int)(coeffs->t(pm_end + 1) * (float)sig->sample_rate());
    else
 	samp_end = (int)(coeffs->t(pm_end) * (float)sig->sample_rate());
    int real_samp_start = (int)(unit->F("start") * (float)sig->sample_rate());
    int real_samp_end = (int)(unit->F("end") * (float)sig->sample_rate());
    if (samp_end-samp_start < 1)
	sig->sub_wave(*unit_sig,samp_start, 1);
    else
	sig->sub_wave(*unit_sig,samp_start, samp_end-samp_start);
    if (real_samp_start-samp_start<0)
        unit->set("samp_start",0);
    else
        unit->set("samp_start",real_samp_start-samp_start);
    unit->set("samp_end",real_samp_end-samp_start);
    /* Need to preserve where the phone boundary is  (which may actually */
    /* be past the end of this unit                                      */
    unit->set("samp_seg_start",
	      (int)(unit->F("seg_start") * 
		    (float)sig->sample_rate())-samp_start);
    unit->set_val("sig",est_val(unit_sig));
}

CLunit::CLunit()
{
    start=0;
    mid=0;
    end=0;
    prev_unit = 0;
    next_unit = 0;
    samp_start = 0;
    samp_end = 0;
    join_coeffs = 0;
    coefs = 0;
    sig = 0;
}

CLunit::~CLunit()
{
    delete join_coeffs;
    delete coefs;
    delete sig;
}

CLfile::CLfile()
{
    join_coeffs = 0;
    coefs = 0;
    sig = 0;
}

CLfile::~CLfile()
{
    delete join_coeffs;
    delete coefs;
    delete sig;
}

CLDB::CLDB()
{
    gc_protect(&params);
}

static void del_clunit(void *s) { delete (CLunit *)s; }
static void del_clfile(void *s) { delete (CLfile *)s; }
CLDB::~CLDB()
{
    index.clear(del_clunit);
    fileindex.clear(del_clfile);
    gc_unprotect(&params);
}

LISP cldb_list(void)
{
    // List names of all current defined cluster dbs
    LISP d = NIL;
    LISP l;

    for (l=CLDB_list; l != NIL; l=cdr(l))
	d = cons(car(car(l)),d);

    return d;
}

LISP cldb_select(LISP dbname)
{
    // Select named cldb and make it current
    EST_String name = get_c_string(dbname);
    LISP lpair;

    lpair = siod_assoc_str(name,CLDB_list);

    if (lpair == NIL)
    {
	cerr << "CLDB " << name << " not defined" << endl;
	festival_error();
    }
    else
	current_cldb = clunitsdb(car(cdr(lpair)));

    return dbname;
}



