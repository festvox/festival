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
/*                                                                       */
/*                 Author: Paul Taylor                                   */
/*                   Date: 1998, 1999                                    */
/* --------------------------------------------------------------------- */
/*            LPC residual synthesis alternative version                 */
/*                                                                       */
/*************************************************************************/

#include "siod.h"
#include "EST.h"
#include "us_diphone.h"
#include "Phone.h"

static bool US_full_coefs = false;
USDiphIndex *diph_index = 0;
extern LISP us_dbs;

static void us_get_all_diphones(EST_Relation &diphone);
static void load_grouped_diphone(int unit);
void get_diphone(EST_Item &d);
static int find_diphone_index_simple(const EST_String &d,USDiphIndex &di);
void us_check_db();
void us_add_diphonedb(USDiphIndex *db);
void load_full_diphone(int unit);

VAL_REGISTER_CLASS(us_db,USDiphIndex)
SIOD_REGISTER_CLASS(us_db,USDiphIndex)

USDiphIndex::USDiphIndex() : dihash(1500)
{ 
    gc_protect(&params); 
}

USDiphIndex::~USDiphIndex() 
{ 
    gc_unprotect(&params);
    
}

void us_check_db()
{
    if (diph_index == 0)
	EST_error("US DB: no diphone database loaded\n");
    diph_index->ts.restart();
}

void awb_free_diph_index()
{
    /* chaising memory leaks */
    if (diph_index != 0)
    {
        delete diph_index;
        diph_index = 0;
    }
}

/*static EST_String get_diphone_name(EST_Item *item,const EST_String dir)
{
    // Get diphone name which may differ from phone name
    // Looks for us_diphone_<dir>, us_diphone, or name in that order
    EST_String d1;
    static EST_String dname = "us_diphone";

    if (!item)
	return "";
    else if ((d1 = item->S(dname+"_"+dir)) != "0")
	return d1;
    else if ((d1 = item->S(dname)) != "0")
	return d1;
    else
	return item->S("name");
}
*/

static EST_String get_diphone_name(EST_Item *item,const EST_String dir)
{
    // Get diphone name which may differ from phone name
    // Looks for us_diphone_<dir>, us_diphone, or name in that order
    EST_String d1;
    static EST_String dname = "us_diphone";
    static EST_String def = "0";

    if (!item)
	return "";
    else if ((d1 = (EST_String)item->f(dname+"_"+dir,def)) != "0")
	return d1;
    else if ((d1 = (EST_String)item->f(dname,def)) != "0")
	return d1;
    else
	return item->f("name","0").string();
}


void us_get_diphones(EST_Utterance &utt)
{
    // Create unit stream with coefficients and signals for each
    // diphone
    EST_Item *p, *d;
    EST_String name1, name2, file;

    us_check_db();

    if (!utt.relation_present("Unit"))
	utt.create_relation("Unit");

    US_full_coefs = (siod_get_lval("us_full_coefs", NULL) == NIL) 
	? false : true;

    p = utt.relation("Segment")->head();
    name1 = get_diphone_name(p,"left");  // left part of first diphone

    utt.relation("Unit")->f.set("grouped", ((diph_index->grouped) ? 1 : 0));

    if (!diph_index->grouped)
    {
	utt.relation("Unit")->f.set("coef_dir", diph_index->coef_dir);
	utt.relation("Unit")->f.set("sig_dir", diph_index->sig_dir);
	utt.relation("Unit")->f.set("coef_ext", diph_index->coef_ext);
	utt.relation("Unit")->f.set("sig_ext", diph_index->sig_ext);
    }

    for (p = inext(p); p; p = inext(p))
    {
	d = utt.relation("Unit")->append();
	name2 = get_diphone_name(p,"right");
	d->set("name", (name1 + "-" + name2));
	get_diphone(*d);
	name1 = get_diphone_name(p,"left");
    }

//    utt.create_relation("SourceSegments");
//    for (p = utt.relation("Segment", 1)->head(); p; p = inext(p))
//    {
//	d = utt.relation("SourceSegments")->append();
//	d->set_name(p->name());
//    }

    if (!US_full_coefs)
	parse_diphone_times(*(utt.relation("Unit", 1)), 
			    *(utt.relation("Segment", 1)));
}

LISP us_check_diphone_presence(LISP name)
{
    /* requested by "Nicholas Volk" <nvolk@bitlips.fi> */
    int x = find_diphone_index_simple(get_c_string(name),*diph_index);
    if ( x < 0 ) 
	return NIL;
    else
	return name;
}

LISP us_make_group_file(LISP lname, LISP params)
{
    EST_String group_file, index_file;
    EST_String track_file_format, sig_file_format, sig_sample_format;
    EST_Relation diphone;
    EST_TokenStream ts;
    EST_Item *d;
    EST_Wave *sig;
    EST_Track *tr;
    FILE *fp, *fp_group;
    const int block_size = 1024;
    int pos;

    us_check_db();

    track_file_format = get_param_str("track_file_format",params,"est_binary");
    sig_file_format = get_param_str("sig_file_format",params,"snd");
    sig_sample_format = get_param_str("sig_sample_format",params,"mulaw");

    group_file = make_tmp_filename();
    group_file += ".group";
    index_file = get_c_string(lname);
    us_get_all_diphones(diphone);
    
    if ((fp = fopen(group_file, "wb")) == NULL)
	EST_error("US DB: failed to open group file as temporary file\n");

    for (d = diphone.head(); d; d = inext(d))
    {
	sig = wave(d->f("sig"));
	tr = track(d->f("coefs"));

	pos = ftell(fp);
	d->set("track_start", pos);
	tr->save(fp, track_file_format);

	pos = ftell(fp);
	d->set("wave_start", pos);
	sig->save_file(fp, sig_file_format, sig_sample_format, EST_NATIVE_BO);
    }
    fclose(fp);

    if ((fp = fopen(index_file, "wb")) == NULL)
	EST_error("US DB: failed to open group file \"%s\" for writing\n",
		  (const char *) index_file);

    fprintf(fp, "EST_File index\n");
    fprintf(fp, "DataType ascii\n");
    fprintf(fp, "NumEntries %d\n", diphone.length());
    fprintf(fp, "IndexName %s\n", (const char *)diph_index->name);
    fprintf(fp, "DataFormat grouped\n");
    fprintf(fp, "Version 2\n");
    fprintf(fp, "track_file_format %s\n",(const char *)track_file_format);
    fprintf(fp, "sig_file_format %s\n",(const char *)sig_file_format);
    fprintf(fp, "EST_Header_End\n");

    for (d = diphone.head(); d; d = inext(d))
	fprintf(fp, "%s %d %d %d\n", (const char *)d->S("name"), 
		d->I("track_start"), d->I("wave_start"),
		d->I("middle_frame"));

    // Copy binary data from temporary group file to end of
    // real group file
    char buf[block_size];
    int r;

    if ((fp_group = fopen(group_file, "rb")) == NULL)
    {
	fprintf(stderr,"Unexpected lost temporary group file from \"%s\"\n",
		(const char *)group_file);
	return NIL;
    }

    while ((r = fread(buf, sizeof(char), block_size, fp_group)) != 0)
	fwrite(buf, sizeof(char), r, fp);

    fclose(fp);
    fclose(fp_group);
    unlink(group_file);

    return NIL;
}

static void us_get_all_diphones(EST_Relation &diphone)
{
    EST_Item *d;
    EST_String name1;

    for (int i = 0; i < diph_index->diphone.n(); ++i)
    {
	d = diphone.append();
	d->set("name", diph_index->diphone[i].S("name"));
	get_diphone(*d);
    }
}

int read_diphone_index(const EST_String &filename, 
			  USDiphIndex &di)
{
    EST_TokenStream ts;
    int i, ref;
    int num_entries;
    EST_Option hinfo;
    EST_EstFileType t;
    EST_String v, pointer, n;
    bool ascii;
    EST_read_status r;
    EST_Item d;

    di.index_offset = 0;
    
    if (ts.open(filename) != 0)
    {
	cerr << "US DB: can't open index file " << filename << endl;
	return misc_read_error;
    }
    // set up the character constant values for this stream
    ts.set_SingleCharSymbols(";");
    
    if ((r = read_est_header(ts, hinfo, ascii, t)) != format_ok)
	return r;
    if (t != est_file_index)
	return misc_read_error;
    
    num_entries = hinfo.ival("NumEntries");
    di.grouped = (hinfo.val("DataFormat") == "grouped") ? true : false;
    di.diphone.resize(num_entries);
    
    if (di.grouped)
    {
	di.track_file_format = hinfo.val_def("track_file_format","est");
	di.sig_file_format = hinfo.val_def("sig_file_format","est");
	for (i = 0; i < num_entries; ++i)
	{
	    di.diphone[i].set("name", ts.get().string());
	    di.diphone[i].set("count", 0);
	    di.diphone[i].set("track_start", atoi(ts.get().string()));
	    di.diphone[i].set("wave_start", atoi(ts.get().string()));
	    di.diphone[i].set("middle_frame", atoi(ts.get().string()));
	    di.dihash.add_item(di.diphone[i].f("name"),i);
	}
	di.index_offset = ts.tell();
	// cout << "index offset = " << di.index_offset << endl;
    }
    else
    {
	int n_num_entries = num_entries;
	for (i = 0; i < n_num_entries; ++i)
	{
	    if (ts.eof())
		EST_error("US DB: unexpected EOF in \"%s\": %d entries "
			  "instead of %s\n", (const char *)filename, 
			  i, num_entries);

	    n = ts.get().string();
	    di.diphone[i].set("name", n);
	    di.diphone[i].set("filename", ts.get().string());
	    di.diphone[i].set("count", 0);
	    if (!di.diphone[i].S("filename").contains("&", 0))
	    {
		di.diphone[i].set("start", atof(ts.get().string()));
		di.diphone[i].set("middle", atof(ts.get().string()));
		di.diphone[i].set("end", ts.get().string());
		if ((di.diphone[i].F("start")>=di.diphone[i].F("middle"))||
		    (di.diphone[i].F("middle") >= di.diphone[i].F("end")))
		{
		    cerr << "US DB: diphone index for " << n << 
			" start middle end not in order, ignored " << endl;
		    i--;
		    n_num_entries--;
		}
	    }
	    di.dihash.add_item(n,i);
	}
    
	// now copy reference entries
	for (i = 0; i < n_num_entries; ++i)
	    if (di.diphone[i].S("filename").contains("&", 0))
	    {
		pointer = di.diphone[i].S("filename").after("&", 0);
//		cout << "pointer: = " << pointer << endl;
		if ((ref = find_diphone_index_simple(pointer,di)) == -1)
		{
		    cerr << "US DB: Illegal diphone pointer in index file: " 
			<< i << " " 
			<< di.diphone[i].S("name") << " -> " << 
			    di.diphone[i].S("filename") << endl;
		    EST_error("");
		}
		di.diphone[i].set("filename",
				    di.diphone[ref].S("filename"));
		di.diphone[i].set("start", di.diphone[ref].S("start"));
		di.diphone[i].set("middle", di.diphone[ref].S("middle"));
		di.diphone[i].set("end", di.diphone[ref].S("end"));
		// accurate values of these depend on where the
		// pitchmarks are placed.
		//		di.diphone[i].fset("first_dur",di.diphone[ref].fS("first_dur"));
		//		di.diphone[i].fset("second_dur",
		//di.diphone[ref].fS("second_dur"));
	    }
    }

    return format_ok;
}

void get_diphone(EST_Item &d)
{
    int unit;
    
    unit = find_diphone_index(d);
    
    if (diph_index->diphone[unit].f("count") == 0)
    {
	if (diph_index->grouped)
	    load_grouped_diphone(unit);
	else
	{
	  if (US_full_coefs)
	    load_full_diphone(unit);
	  else
	    load_separate_diphone(unit, false, "all");
	}
	diph_index->diphone[unit].set("count", d.I("count", 0) + 1);
    }
    
    if (US_full_coefs)
    {
	d.set_val("full_sig", diph_index->diphone[unit].f("full_sig"));
	d.set_val("full_coefs", diph_index->diphone[unit].f("full_coefs"));
    }
    else
    {
	d.set_val("sig", diph_index->diphone[unit].f("sig"));
	d.set_val("coefs", diph_index->diphone[unit].f("coefs"));
	d.set_val("middle_frame", 
		   diph_index->diphone[unit].f("middle_frame"));
    }

    if (!diph_index->grouped)
    {
	d.set_val("filename", diph_index->diphone[unit].f("filename"));
	d.set_val("diphone_start", diph_index->diphone[unit].F("start"));
	d.set_val("diphone_middle", diph_index->diphone[unit].F("middle"));
	d.set_val("diphone_end", diph_index->diphone[unit].F("end"));
    }
}

static void load_grouped_diphone(int unit)
{
    int middle_frame;
    EST_Track *coefs;
    EST_Wave *sig;
    int wave_start, track_start;
    
    coefs = new EST_Track;
    sig = new EST_Wave;
    
    track_start = diph_index->diphone[unit].I("track_start");
    wave_start = diph_index->diphone[unit].I("wave_start");
    middle_frame = diph_index->diphone[unit].I("middle_frame");
    
    diph_index->ts.seek(track_start + diph_index->index_offset);
    coefs->load(diph_index->ts); // type is self determined at present
    
    diph_index->ts.seek(wave_start + diph_index->index_offset);
    sig->load(diph_index->ts,diph_index->sig_file_format);

    diph_index->diphone[unit].set_val("coefs", est_val(coefs));
    diph_index->diphone[unit].set("middle_frame", middle_frame);
    
    diph_index->diphone[unit].set_val("sig", est_val(sig));
}


static int find_diphone_index_simple(const EST_String &d,USDiphIndex &di)
{
    int found,r;
    
    r = di.dihash.val(d,found);
    if (found)
	return r;
    else
	return -1;
}

int find_diphone_index(const EST_Item &d)
{
    // Find the approrpiate entry in the diphone index table
    // mapping to alternates if required.
    int index;
    EST_String diname = d.f("name");

    // If all goes well the diphone will be found directly in the index
    index=find_diphone_index_simple(diname,*diph_index);
    if ((index=find_diphone_index_simple(diname,*diph_index)) != -1)
	return index;

    // But for various reasons it might not be there so allow
    // a run time specification of alternates.  This isn't optimal
    // but is better than falling immediately back on just a default
    // diphone
    LISP alt_left = get_param_lisp("alternates_left",diph_index->params,NIL);
    LISP alt_right = get_param_lisp("alternates_right",diph_index->params,NIL);
    EST_String di_left = diname.before("-");
    EST_String di_right = diname.after("-");
    EST_String di_left_alt = get_param_str(di_left,alt_left,di_left);
    EST_String di_right_alt = get_param_str(di_right,alt_right,di_right);
    EST_String di_alt = di_left_alt+"-"+di_right_alt;

    if ((index=find_diphone_index_simple(di_alt,*diph_index)) != -1)
    {
//	cout << "UniSyn: using alternate diphone " << di_alt << " for " <<
//	    diname << endl;
	return index;
    }
    
    // It really isn't there so return the default one and print and error
    // now
    EST_String default_diphone = 
	get_param_str("default_diphone",diph_index->params,"");
    
    if (default_diphone != "")
    {
	index = find_diphone_index_simple(default_diphone,*diph_index);
	if (index == -1)
	{
	    cerr << "US DB: can't find diphone " << d.f("name") 
		<< " and even default diphone (" << default_diphone 
		    << ") doesn't exist" << endl;
	    EST_error("");
	}
	else
	    cerr << "UniSyn: using default diphone " << default_diphone << 
		" for " << diname << endl;
	return index;
    }
    else
    {
	cerr << "US DB: can't find diphone " << d.f("name") << 
	    " nor alternatives" << endl;
	EST_error("");
    }
    return -1;
}

void us_full_cut(EST_Relation &unit)
{
    EST_Track *full_coefs, *sub_coefs;
    EST_Wave *full_sig, sub_sig;
    EST_Item *s;
    int pm_start, pm_end, pm_middle;
    int samp_start, samp_end;
    float start_time;

    for (s = unit.head(); s; s = inext(s))
    {
	sub_coefs = new EST_Track;

	full_coefs = track(s->f("full_coefs"));
	full_sig = wave(s->f("full_sig"));

	pm_start = full_coefs->index(s->F("diphone_start"));
	pm_middle = full_coefs->index(s->F("diphone_middle"));
	pm_end = full_coefs->index(s->F("diphone_end"));

	full_coefs->copy_sub_track(*sub_coefs, pm_start, 
				   pm_end - pm_start + 1);

	start_time = full_coefs->t(Gof((pm_start - 1), 0));

	for (int j = 0; j < sub_coefs->num_frames(); ++j)
	    sub_coefs->t(j) = sub_coefs->t(j) - start_time;

	
	s->set("middle_frame", pm_middle - pm_start -1);
	s->set_val("coefs", est_val(sub_coefs));

	// go to the periods before and after
	samp_start = (int)(full_coefs->t(Gof((pm_start - 1), 0))
			   * (float)full_sig->sample_rate());
	if (pm_end+1 < full_coefs->num_frames())
	    pm_end++;
	samp_end = (int)(full_coefs->t(pm_end) 
			 * (float)full_sig->sample_rate());


	full_sig->sub_wave(sub_sig, samp_start, samp_end - samp_start + 1);
	EST_Wave *sig = new EST_Wave(sub_sig);

	s->set_val("sig", est_val(sig));

    }
}


void us_add_diphonedb(USDiphIndex *db)
{
    // Add this to list of loaded diphone dbs and select it
    LISP lpair;

    if (us_dbs == NIL)
	gc_protect(&us_dbs);

    lpair = siod_assoc_str(db->name,us_dbs);

    if (lpair == NIL)
    {   // new diphone db of this name
	us_dbs = cons(cons(rintern(db->name),
			   cons(siod(db),NIL)),
			   us_dbs);
    }
    else
    {	// already one of this name
	cerr << "US_db: warning redefining diphone database "
	    << db->name << endl;
	setcar(cdr(lpair),siod(db));
    }
    
    diph_index = db;
}

