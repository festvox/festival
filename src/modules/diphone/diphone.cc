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
/*             Author :  Alistair Conkie                                 */
/*             Date   :  August 1996                                     */
/*-----------------------------------------------------------------------*/
/*                                                                       */
/* A free version of diphone selection and concatenation supported the   */
/* standard CSTR diphone dbs                                             */
/*                                                                       */
/*=======================================================================*/
#include <iostream>
#include <fstream>
#include "festival.h"
#include "ModuleDescription.h"
#include "diphone.h"

static ModuleDescription di_description =
{
  "diphone", 1.0,
  "CSTR",
  "Alistair Conkie, Alan Black <awb@cstr.ed.ac.uk>",
  {
    "A free version of diphone selection and concatenation supported the",
    "standard CSTR diphone dbs ",
    NULL
  },
  { { "Segment", "Segments fo the utterance." },
    { "Target", "Intonation targets" }, 
    { NULL, NULL } },
  { { NULL,NULL} }, 
  { { "Wave", "Synthesized waveform" },
    { NULL,NULL} }, 
  { {NULL,NULL,NULL,NULL} }
};

static int lookupd(DIPHONE_DATABASE *database,char *p1, char *p2);
static void di_diphones(DIPHONE_SPN *ps, DIPHONE_DATABASE *database);
static void make_silence(DIPHONE_DATABASE *db,DIPHONE_SPN *ps,DIPHONE_OUTPUT *o);
static void merge_silences(EST_Utterance &u);

static void delete_diphone_db(DIPHONE_DATABASE *config);
static DIPHONE_SPN *make_spn(EST_Utterance &u);
static void delete_spn(DIPHONE_SPN *ps);
static DIPHONE_ACOUSTIC *make_as(DIPHONE_SPN *ps);
static void delete_as(DIPHONE_ACOUSTIC *ss);
static DIPHONE_OUTPUT *make_output(DIPHONE_DATABASE *database,int size);
static void delete_output(DIPHONE_OUTPUT *o);

static LISP diphone_dbs = NIL;
static DIPHONE_DATABASE *di_db=0;
static EST_Regex RXdi_ud("[$_]");

VAL_REGISTER_TYPE(diphone_db,DD_STRUCT)
SIOD_REGISTER_CLASS(diphone_db,DD_STRUCT)

LISP FT_Diphone_Load_Diphones(LISP params)
{
    DIPHONE_DATABASE *db;
    EST_Pathname groupfile;

    db = make_diphone_db();

    groupfile = get_param_str("group_file",params,"");
    if (!streq(groupfile,""))
    {
	db->gtype = di_grouped;
	di_load_grouped_db(groupfile,db,params);
    }
    else
    {
	db->gtype = di_ungrouped;
	di_general_parameters(db,params);
	di_fixed_parameters(db,params);
	di_load_database(db);    // load in the diphones/index
    }

    di_add_diphonedb(db);    // add to db list, selects it too 

    return NIL;
}

void di_general_parameters(DIPHONE_DATABASE *db,LISP params)
{
    // These may be reset from those set in group file

    db->name = wstrdup(get_param_str("name",params,"none"));
    db->default_diphone = wstrdup(get_param_str("default_diphone",params,""));
    db->alternates_before = get_param_lisp("alternates_before",params,NIL);
    gc_protect(&db->alternates_before);
    db->alternates_after = get_param_lisp("alternates_after",params,NIL);
    gc_protect(&db->alternates_after);
    db->sig_access_type_str = 
	wstrdup(get_param_str("access_type",params,"direct"));
    if (streq(db->sig_access_type_str,"direct"))
	db->sig_access_type = di_direct;
    else if (streq(db->sig_access_type_str,"dynamic"))
	db->sig_access_type = di_dynamic;
    else if (streq(db->sig_access_type_str,"ondemand"))
	db->sig_access_type = di_ondemand;
    else
    {
	cerr << "Diphone: unknown access method " << 
	    db->sig_access_type_str << endl;
	festival_error();
    }

}

void di_fixed_parameters(DIPHONE_DATABASE *db,LISP params)
{
    // These cannot be changed in a compiled group file
    int nphones;

    db->index_file = wstrdup(get_param_str("index_file",params,"index"));
    db->type_str = wstrdup(get_param_str("type",params,"pcm"));
    if (streq(db->type_str,"pcm"))
	db->type = di_pcm;
    else if (streq(db->type_str,"lpc"))
	db->type = di_lpc;
    else
    {
	cerr << "Diphone: unknown database type \"" <<
	    db->type_str << "\"" << endl;
	festival_error();
    }
    db->signal_dir = wstrdup(get_param_str("signal_dir",params,"signal/"));
    db->signal_ext = wstrdup(get_param_str("signal_ext",params,".vox"));
    db->signal_type = wstrdup(get_param_str("signal_type",params,"vox"));
    db->lpc_dir = wstrdup(get_param_str("lpc_dir",params,"lpc/"));
    db->lpc_ext = wstrdup(get_param_str("lpc_ext",params,".lpc"));
    db->lpc_res_ext = wstrdup(get_param_str("lpc_res_ext",params,".res"));
    db->lpc_res_type = wstrdup(get_param_str("lpc_res_type",params,"esps"));
    db->lpc_type = wstrdup(get_param_str("lpc_type",params,"htk"));
    db->lpc_order = get_param_int("lpc_order",params,18);
    db->lpc_frame_shift = get_param_float("lpc_frame_shift",params,0.010);
    db->lpc_res_offset = get_param_float("lpc_res_offset",params,
					 db->lpc_frame_shift);
    db->lpc_frame_offset = get_param_int("lpc_frame_offset",params,1);
    if (streq("t",get_param_str("lpc_pitch_synch",params,"f")))
	db->lpc_pitch_synch = TRUE;
    db->pitch_dir = wstrdup(get_param_str("pitch_dir",params,"pitch/"));
    db->pitch_ext = wstrdup(get_param_str("pitch_ext",params,".pm"));
    db->samp_freq = get_param_int("samp_freq",params,16000);
    // old name was "group_type", new name "group_encoding"
    db->group_encoding_str=wstrdup(get_param_str("group_type",params,"raw")); 
    db->group_encoding_str = wstrdup(get_param_str("group_encoding",params,
					   db->group_encoding_str));
    db->def_f0 = get_param_int("def_f0",params,125);
    if (streq(db->group_encoding_str,"raw"))
	db->group_encoding = di_raw;
    else if (streq(db->group_encoding_str,"ulaw"))
	db->group_encoding = di_ulaw;
    else
    {
	cerr << "Diphone: unknown group encoding" << endl;
	festival_error();
    }
    db->phoneset = wstrdup(get_param_str("phoneset",params,"none"));
    nphones = phoneset_name_to_set(db->phoneset)->num_phones();
    db->ndiphs = nphones*nphones;  // max limit of diphones
    db->ndiphs = get_param_int("num_diphones",params,db->ndiphs);
    db->nindex = db->ndiphs;
    db->sig_band = get_param_int("sig_band",params,400);
}

static void delete_diphone_db(DIPHONE_DATABASE *db)
{
    int i,j;

    wfree(db->type_str);
    wfree(db->name);
    wfree(db->index_file);
    wfree(db->signal_dir);
    wfree(db->signal_ext);
    wfree(db->signal_type);
    wfree(db->lpc_dir);
    wfree(db->lpc_ext);
    wfree(db->lpc_res_ext);
    wfree(db->pitch_dir);
    wfree(db->pitch_ext);
    wfree(db->phoneset);
    wfree(db->group_encoding_str);
    wfree(db->sig_access_type_str);
    gc_unprotect(&db->alternates_before);
    gc_unprotect(&db->alternates_after);

    if (db->gtype == di_grouped)
    {
	wfree(db->indx[0]->diph);  // ptr to the whole diphname table
	wfree(db->allsignal);
	wfree(db->allulawsignal);
	wfree(db->allframes);
    }
    for (i=0; i < db->nindex; i++)
    {
	if (db->gtype == di_ungrouped)
	{
	    wfree(db->indx[i]->diph);
	    wfree(db->indx[i]->file);
	}
	wfree(db->indx[i]);
	if ((db->gtype == di_ungrouped) ||
	    (db->group_encoding != di_raw))
	    wfree(db->vox[i]->signal);
	wfree(db->vox[i]);
	wfree(db->pm[i]->mark);
	wfree(db->pm[i]);
	if (db->lpc[i]->f != 0)
	{
	    if ((db->gtype == di_ungrouped) ||    // when they are not ptrs 
		(db->group_encoding != di_raw))   // into db->allframes
		for (j=0; j < db->lpc[i]->nframes; j++)
		    wfree(db->lpc[i]->f[j]);
	    wfree(db->lpc[i]->f);
	}
	wfree(db->lpc[i]);
    }
    wfree(db->indx);
    wfree(db->vox);
    wfree(db->pm);
    wfree(db->lpc);
    wfree(db->offsets);
    if (db->gfd != NULL)
	fclose(db->gfd);
    
}

DIPHONE_DATABASE *make_diphone_db(void)
{
    // Allocate a new database structure 
    DIPHONE_DATABASE *db = walloc(DIPHONE_DATABASE,1);
    memset(db,0,sizeof(DIPHONE_DATABASE));   // all zeroed 

    db->type = di_pcm;
    db->gtype = di_ungrouped;
    db->swap = FALSE;

    db->index_file = 0;
    db->signal_dir = 0;
    db->pitch_dir = 0;
    db->xfd = 0;
    db->samp_freq = 0;
    db->sig_band = 0;
    db->phoneset = 0;
    db->alternates_before = NIL;
    db->alternates_after = NIL;
    db->allsignal = 0;
    db->allulawsignal = 0;
    db->offsets = 0;
    db->gfd = 0;
    db->default_diphone = 0;
    db->lpc_pitch_synch = FALSE;

    db->nindex = 0;

    return db;
}

LISP FT_Diphone_Synthesize_Utt(LISP utt)
{
    EST_Utterance *u = get_c_utt(utt);
    DIPHONE_SPN *ps;
    DIPHONE_ACOUSTIC *as;
    DIPHONE_OUTPUT *output;
    EST_Item *item=0;

    *cdebug << "Diphone module" << endl;

    if (di_db == 0)
    {
	cerr << "Diphone: no diphone database loaded" << endl;
	festival_error();
    }

    // Find diphone names (_ and $ etc) if specified for this db
    apply_hooks(siod_get_lval("diphone_module_hooks",NULL),utt);

    /* Build structure */
    merge_silences(*u);
    ps = make_spn(*u);   // get info from utterance structure

    output = make_output(di_db,(ps->p_sz != 0 ? ps->cum_dur[ps->p_sz-1] : 0));

    if (ps->p_sz < 2)
    {   //  just build a silence of the specified time
	make_silence(di_db,ps,output);
    }
    else
    {
	as = make_as(ps);
	di_diphones(ps,di_db);
	di_calc_pitch(di_db,ps,as);
	di_frame_select(di_db,ps,as);
	if (di_db->type == di_lpc)
	    di_reslpc(di_db,as,output);       // residual excited LPC
	else if (di_db->type == di_pcm)
	    di_psola_tm(di_db,as,output);     // can't be distributed
	else
	{
	    cerr << "Diphone: unsupported database form\n";
	    festival_error();
	}
	delete_as(as);
    }

    delete_spn(ps);

    // Add wave into utterance
    EST_Wave *w = new EST_Wave;
    w->resize(output->o_sz,1,0);
    w->set_sample_rate(di_db->samp_freq);
    for (int i=0; i<w->length(); i++)
	w->a_no_check(i) = output->track[i];

    item = u->create_relation("Wave")->append();
    item->set_val("wave",est_val(w));

    delete_output(output);

    return utt;
}

LISP FT_reslpc_resynth(LISP file)
{
    EST_Utterance *u = new EST_Utterance;
    EST_Item *item = 0;
    DIPHONE_ACOUSTIC *as;
    DIPHONE_OUTPUT *output;

    if (di_db == 0)
    {
	cerr << "Diphone: no diphone database loaded" << endl;
	festival_error();
    }

    as = walloc(DIPHONE_ACOUSTIC,1);
    memset(as,0,sizeof(DIPHONE_ACOUSTIC));
    output = make_output(di_db,0);

    reslpc_resynth(get_c_string(file),di_db,as,output);

    wfree(as);

    // Add wave into utterance
    EST_Wave *w = new EST_Wave;
    w->resize(output->o_sz-(2*di_db->sig_band),1);
    for (int i=0; i<w->length(); i++)
	w->a_no_check(i) = output->track[di_db->sig_band+i];
    w->set_sample_rate(di_db->samp_freq);
    delete_output(output);

    item = u->create_relation("Wave")->append();
    item->set_val("wave",est_val(w));

    return siod(u);
}

static void make_silence(DIPHONE_DATABASE *db,DIPHONE_SPN *ps,DIPHONE_OUTPUT *o)
{
    // Make the output directly as silence
    int sil_size = (int)((float)db->samp_freq * 0.020);

    if ((ps->p_sz > 0) &&
	(ps->cum_dur[ps->p_sz-1] > sil_size))
	sil_size = ps->cum_dur[ps->p_sz-1];

    if (o->o_max < (sil_size+2*db->sig_band))
    {
	wfree(o->track);
	o->track = walloc(short,(sil_size+2*db->sig_band));
	o->o_max = (sil_size+2*db->sig_band);
    }

    memset(o->track,0,o->o_max*sizeof(short));
    o->o_sz = o->o_max;

}

static void merge_silences(EST_Utterance &u)
{
    //  Remove multiple silences
    EST_Item *s, *ns;
    
    for (s=u.relation("Segment")->first(); s != 0; s=ns)
    {
	ns = inext(ns);
	if ((ns != 0) &&
	    (ph_is_silence(s->name())) &&
	    (s->name() == ns->name())) // same *type* of silence
	{
	    // delete this segment
	    remove_item(s,"Segment");
	}
    }
}

static void di_diphones(DIPHONE_SPN *ps, DIPHONE_DATABASE *database)
{
    int ph=0;

    ps->cum_dur[0] = 0;

    for(ph=0;ph<ps->p_sz;ph++) 
    {
	ps->cum_dur[ph+1] = ps->duration[ph] + ps->cum_dur[ph];
    }

    for(ph=0;ph<ps->p_sz-1;ph++) 
    {
	sprintf(ps->diphs[ph],"%s-%s",ps->phons[ph],ps->phons[ph+1]);
	ps->ref[ph] = lookupd(database,ps->phons[ph],ps->phons[ph+1]);
	*cdebug << ps->diphs[ph] << " " << 
	    ((database->indx[ps->ref[ph]]->file == 0) ? "grouped" :
	     database->indx[ps->ref[ph]]->file) << endl;
	load_pitch_file(database,ps->ref[ph],di_direct); // ensure its loaded
    }
}

static int lookupd(DIPHONE_DATABASE *database,char *p1, char *p2)
{
    // This should be made much faster
    int i;
    char diphone[20];
    LISP alt1, alt2;

    sprintf(diphone,"%s-%s",p1,p2);
    for(i=0;i<database->nindex;i++) 
    {
	if(!strcmp(database->indx[i]->diph,diphone)) 
	    return i;
    }

    // Try alternates
    alt2 = siod_assoc_str(p2,database->alternates_after);
    if (alt2 != NIL)
    {
	sprintf(diphone,"%s-%s",p1,get_c_string(car(cdr(alt2))));
	for(i=0;i<database->nindex;i++) 
	    if(!strcmp(database->indx[i]->diph,diphone)) 
	    {
		*cdebug << "Diphone alternate: " << diphone
		    << " substituted for " << p1 << "-" << p2 << endl;
		return i;
	    }
    }
    alt1 = siod_assoc_str(p1,database->alternates_before);
    if (alt1 != NIL)
    {
	sprintf(diphone,"%s-%s",get_c_string(car(cdr(alt1))),p2);
	for(i=0;i<database->nindex;i++) 
	    if(!strcmp(database->indx[i]->diph,diphone)) 
	    {
		*cdebug << "Diphone alternate: " << diphone
		    << " substituted for " << p1 << "-" << p2 << endl;
		return i;
	    }
    }

    if ((alt2 != NIL) && (alt1 != NIL))
    {
	sprintf(diphone,"%s-%s",
		get_c_string(car(cdr(alt1))),
		get_c_string(car(cdr(alt2))));
	for(i=0;i<database->nindex;i++) 
	    if(!strcmp(database->indx[i]->diph,diphone)) 
	    {
		*cdebug << "Diphone alternate: " << diphone
		    << " substituted for " << p1 << "-" << p2 << endl;
		return i;
	    }
    }

    // If underscores and dollars exist do it again without them
    if ((EST_String(p1).contains(RXdi_ud)) ||
	(EST_String(p2).contains(RXdi_ud)))
    {
	EST_String dp1=p1, dp2=p2;
	// fprintf(stddebug,"Diphone: removing _ and $\n");
	dp1.gsub(RXdi_ud,"");
	dp2.gsub(RXdi_ud,"");
	return lookupd(database,dp1,dp2);
    }

    if ((database->default_diphone != NULL) &&
	(!streq(database->default_diphone,"")))
    {
	fprintf(stderr,"Diphone: using default diphone %s for %s-%s\n",
		database->default_diphone,p1,p2);
	for(i=0;i<database->nindex;i++) 
	    if(!strcmp(database->indx[i]->diph,database->default_diphone)) 
	    {
		*cdebug << "Diphone alternate: " << diphone
		    << " substituted for " << p1 << "-" << p2 << endl;
		return i;
	    }
    }

    fprintf(stderr,"Diphone: diphone (or alternates) not found: %s\n",diphone);
    festival_error();
    return 0;
}
 
static DIPHONE_OUTPUT *make_output(DIPHONE_DATABASE *database,int samps)
{
    // Alloc the output buffer
    int nos;
    DIPHONE_OUTPUT *o = walloc(DIPHONE_OUTPUT,1);

    // estimate the number of samples
    nos = (int)((float)samps*1.1)+(2*database->sig_band);
    o->o_sz = 0;
    o->o_max = nos;
    o->track = walloc(short,nos);

    return o;
}

static void delete_output(DIPHONE_OUTPUT *o)
{

    wfree(o->track);
    wfree(o);

}

static DIPHONE_SPN *make_spn(EST_Utterance &u)
{
    //  Build initial structure for donovan code
    DIPHONE_SPN *ps = walloc(DIPHONE_SPN,1);
    EST_Relation *seg = u.relation("Segment");
    EST_Relation *targ = u.relation("Target");
    EST_Item *s;
    LISP cps;
    const char *ph_name;
    EST_Item *rt;
    int i;
    float pos,seg_start,seg_dur,seg_end;

    ps->p_sz = seg->length();
    ps->p_max = ps->p_sz+1;
    ps->t_sz = 0;
    ps->phons = walloc(char *,ps->p_max);
    ps->duration = walloc(int,ps->p_max);
    ps->cum_dur = walloc(int,ps->p_max);
    ps->diphs = walloc(char *,ps->p_max);
    ps->ref = walloc(int,ps->p_max);
    ps->pm_req = walloc(int,ps->p_max);
    ps->pm_giv = walloc(int,ps->p_max);
    ps->pm_chg_diph = walloc(int,ps->p_max);
    for (i=0; i<ps->p_sz; i++)
	ps->diphs[i] = walloc(char,20);

    ps->t_max = num_leaves(targ->head())+4;
    ps->pc_targs = walloc(int,ps->t_max);
    ps->targ_phon = walloc(int,ps->t_max);
    ps->targ_freq = walloc(int,ps->t_max);
    ps->abs_targ = walloc(int,ps->t_max);
    
    // Ensure there is a target at the start
    if ((targ->length() == 0) || (targ->first_leaf()->F("pos") != 0))
    {
	ps->targ_phon[0] = 0;
	if (targ->length() == 0)
	    ps->targ_freq[0] = di_db->def_f0;
	else
	    ps->targ_freq[0] = targ->first()->I("f0");
	ps->pc_targs[0] = 0;
	ps->t_sz++;
    }
    seg_end = 0;
    for (i=0,s=seg->first(); s != 0; s=inext(s),i++)
    {
	seg_start = seg_end;
	seg_end = s->F("end");
	seg_dur = seg_end - seg_start;
	EST_String pph_name;  // possibly calculated phone name
	if ((pph_name=s->f("diphone_phone_name").string()) != "0")
	    ph_name = pph_name;
	else if (((cps=ft_get_param("PhoneSet")) == NIL) ||
	    ((streq(get_c_string(cps),di_db->phoneset))))
	    ph_name = s->name();
	else
	    ph_name = map_phone(s->name(),get_c_string(cps),di_db->phoneset);
	ps->phons[i] = wstrdup(ph_name);
	ps->duration[i] = (int)(seg_dur * di_db->samp_freq);  /* in frames */
	if (i>0)
	    ps->cum_dur[i] = ps->cum_dur[i-1];
	else
	    ps->cum_dur[i] = 0;
	ps->cum_dur[i] += ps->duration[i];
	for (rt = daughter1(s,"Target");
	     rt != 0;
	     rt = inext(rt),ps->t_sz++)
	{
	    ps->targ_phon[ps->t_sz] = i;
	    ps->targ_freq[ps->t_sz] = rt->I("f0");
	    pos = ((rt->F("pos") - seg_start) / seg_dur) * 99.9;
	    ps->pc_targs[ps->t_sz] = (int)pos;
	}
    }
    // Ensure there is a target at the end 
    if ((targ->length() == 0) ||
	(targ->last_leaf()->F("pos") != seg->last_leaf()->F("end")))
    {
	ps->targ_phon[ps->t_sz] = i-1;
	if (targ->length() == 0)
	    ps->targ_freq[ps->t_sz] = di_db->def_f0;
	else
	    ps->targ_freq[ps->t_sz] = targ->last_leaf()->I("f0");
	ps->pc_targs[ps->t_sz] = 100;
	ps->t_sz++;
    }


    return ps;

}

static void delete_spn(DIPHONE_SPN *ps)
{
    // claim back the space from ps
    int i;

    if (ps == NULL)
	return;

    for (i=0; i<ps->p_sz; i++)
    {
	wfree(ps->diphs[i]);
	wfree(ps->phons[i]);
    }
    wfree(ps->diphs);
    wfree(ps->phons);
    wfree(ps->duration);
    wfree(ps->cum_dur);
    wfree(ps->ref);
    wfree(ps->pm_req);
    wfree(ps->pm_giv);
    wfree(ps->pm_chg_diph);

    wfree(ps->pc_targs);
    wfree(ps->targ_phon);
    wfree(ps->targ_freq);
    wfree(ps->abs_targ);

    wfree(ps);

    return;
}

static DIPHONE_ACOUSTIC *make_as(DIPHONE_SPN *ps)
{
    DIPHONE_ACOUSTIC *as = walloc(DIPHONE_ACOUSTIC,1);
    int npp = (int)((float)ps->cum_dur[ps->p_sz-1]/
		    ((float)di_db->samp_freq/1000.0));

    as->p_sz = 0;
    as->p_max = npp;
	
    as->pitch = walloc(int,npp);
    as->cum_pitch = walloc(int,npp+1);
    as->inwidth = walloc(int,npp+1);
    as->outwidth = walloc(int,npp+1);
    as->phon_ref = walloc(int,npp);
    as->phon_mark = walloc(int,npp);
    as->diph_ref = walloc(int,npp);
    as->diph_mark = walloc(int,npp);

    return as;
}

static void delete_as(DIPHONE_ACOUSTIC *as)
{
    
    if (as == NULL)
	return;
    wfree(as->pitch);
    wfree(as->cum_pitch);
    wfree(as->inwidth);
    wfree(as->outwidth);
    wfree(as->phon_ref);
    wfree(as->phon_mark);
    wfree(as->diph_ref);
    wfree(as->diph_mark);
    wfree(as);
    
    return;
}

void di_add_diphonedb(DIPHONE_DATABASE *db)
{
    // Add this to list of loaded diphone dbs and select it
    LISP lpair;
    DIPHONE_DATABASE *ddb;

    if (diphone_dbs == NIL)
	gc_protect(&diphone_dbs);
	
    lpair = siod_assoc_str(db->name,diphone_dbs);

    if (lpair == NIL)
    {   // new diphone db of this name
	diphone_dbs = cons(cons(rintern(db->name),
				cons(siod(db),NIL)),
			   diphone_dbs);
    }
    else
    {	// already one of this name, don't know howto free it
	cerr << "Diphone: warning redefining diphone database "
	    << db->name << endl;
	ddb = diphone_db(car(cdr(lpair)));
	delete_diphone_db(ddb);
	setcar(cdr(lpair),siod(db));
    }
    
    di_db = db;
    
}

LISP FT_Diphone_select(LISP name)
{
    // Select diphone set
    LISP lpair;
    
    lpair = siod_assoc_str(get_c_string(name),diphone_dbs);
    
    if (lpair == NIL)
    {
	cerr << "Diphone: no diphone database named " << get_c_string(name)
	    << " defined\n";
	festival_error();
    }
    else
    {
	DIPHONE_DATABASE *db = diphone_db(car(cdr(lpair)));
	di_db = db;
    }

    return name;
}

LISP FT_Diphone_group(LISP name,LISP lfilename)
{
    
    FT_Diphone_select(name);
    di_save_grouped_db(get_c_string(lfilename),di_db);

    return NIL;
}

static LISP FT_Diphone_list_dbs(void)
{
    // List names of currently loaded dbs 
    LISP names,n;

    for (names=NIL,n=diphone_dbs; n != NIL; n=cdr(n))
	names = cons(car(car(n)),names);
    return reverse(names);
}

void festival_diphone_init(void)
{

    // New diphone synthesizer
    proclaim_module("diphone", &di_description);
#ifdef SUPPORT_PSOLA_TM
    // Only available for research
    proclaim_module("di_psolaTM");
#endif

    init_subr_1("Diphone_Init",FT_Diphone_Load_Diphones,
    "(Diphone_Init PARAMS)\n\
  Initialize a general diphone database.  PARAMS are an assoc list\n\
  of parameter name and value. [see Diphone_Init]");
    festival_def_utt_module("Diphone_Synthesize",FT_Diphone_Synthesize_Utt,
    "(Diphone_Synthesize UTT)\n\
  Synthesize a waveform using the currently selected diphone database.\n\
  This is called from Synthesize when the Synth_Method Parameter has the\n\
  value Diphone. [see Diphone synthesizer]");
//    proclaim_module("Diphone_Synthesize",&di_description);
//    init_module_subr("Diphone_Synthesize",FT_Diphone_Synthesize_Utt,
//		     &di_description);
    init_subr_1("Diphone.select",FT_Diphone_select,
    "(Diphone.select DB_NAME)\n\
  Select a preloaded diphone set named DB_NAME, diphone sets are\n\
  identified by their name parameter.");
    init_subr_2("Diphone.group",FT_Diphone_group,
    "(Diphone.group DB_NAME GROUPFILE)\n\
  Create a group file for DB_NAME in GROUPFILE.  A group file is a saved\n\
  image of the database containing only the necessary information.\n\
  It is an efficient way for loading and using diphone databases\n\
  and is the recommended format for diphone databases for normal\n\
  use. [see Group files]");
    init_subr_1("reslpc_resynth",FT_reslpc_resynth,
    "(reslpc_resynth FILENAME)\n\
  Resynthesize FILENAME using LPC plus residual.  The current diphone\n\
  database must have definitions for LPC for this to work.");
    init_subr_0("Diphone.list",FT_Diphone_list_dbs,
    "(Diphone.list)\n\
  List the names of the currently loaded diphone databases.");
    init_subr_2("diphone.oc",find_optimal_coupling,
    "(diphone.oc TABLE WEIGHTS)\n\
  For a table of units (diphones) find the optimal coupling points.  WEIGHTS\n\
  is a list of weights to apply to the vector coefficients used in measuring\n\
  the goodness of the joins.");

}
