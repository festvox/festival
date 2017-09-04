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
/*             Author :  Alan W Black (Steve Isard and Alistair Conkie)  */
/*             Date   :  July 1996                                       */
/*-----------------------------------------------------------------------*/
/*                                                                       */
/* Interface to Donovan (Isard) LPC diphone code                         */
/*                                                                       */
/* Uses the FreeSpeech version of the code.  Those .c files are          */
/* with both systems and hopefully will continue to be so.  Only the     */
/* necessary parts of FreeSpeech are included (the waveform synthesizer) */
/* The rest is plugged in from this file                                 */
/*                                                                       */
/* Note the FreeSpeech code is GNU Copyright not like the above          */
/*                                                                       */
/*=======================================================================*/
#include <iostream>
#include <fstream>
#include "festival.h"
#include "donovan.h"

static CONFIG *make_config(void);
static void delete_config(CONFIG *config);
static SPN *make_spn(EST_Utterance &u);
static void delete_spn(SPN *ps);
static ACOUSTIC *make_as(SPN *ps);
static void delete_as(ACOUSTIC *ss);

static CONFIG *don_config=0;
static short *outbuff = 0;
static int outpos = 0;
static int outmax = 0;

static void don_make_silence(int duration);

LISP FT_Donovan_Load_Diphones(LISP params)
{
    if (don_config != 0)
	delete_config(don_config);
    don_config = make_config();

    don_config->index_file=wstrdup(get_param_str("index_file",params,"index"));
    don_config->diphone_file =
	wstrdup(get_param_str("diphone_file",params,"diphs"));

    if (load_speech(don_config) != 0)  // load in the diphones
	festival_error();

    return NIL;
}

static void delete_config(CONFIG *config)
{
    (void) config;
}

static CONFIG *make_config(void)
{
    // Allocate a new config structure 
    CONFIG *config = walloc(CONFIG,1);
    config->input_file = 0;
    config->output_file = 0;
    config->index_file = 0;
    config->diphone_file = 0;
    config->hash_file = 0;
    config->format = 0;
    config->ifd = 0;
    config->ofd = 0;
    config->xfd = 0;
    config->dfd = 0;

    return config;
}

LISP FT_Donovan_Synthesize_Utt(LISP utt)
{
    EST_Utterance *u = get_c_utt(utt);
    EST_Item *item = 0;
    SPN *ps;
    ACOUSTIC *as;

    if (nindex == 0)
    {
	cerr << "Festival: no donovan diphones loaded\n";
	festival_error();
    }

    don_random_seed = 1;  // so resynthesizing an utterance is always the same

    /* Build structure */
    ps = make_spn(*u);   // get info from utterance structure

    /* This doesn't work if there are less than two phones so */
    /* deal with those cases explicitly                       */
    if (ps->p_sz < 1)
	outpos = 0;
    else if (ps->p_sz < 2)
	don_make_silence(ps->duration[0]);
    else
    {
	as = make_as(ps);

	/* do the actual synthesis */
	phonstoframes(ps,as);
	durations(ps,as);       
	calc_pitch(ps,as);      
	makewave(don_config,as); 
	
	delete_as(as);
    }
    delete_spn(ps);

    // Add wave as stream into utterance
    EST_Wave *w = new EST_Wave;
    w->resize(outpos,1);
    for (int i=0; i<w->length(); i++)
	w->a_no_check(i) = outbuff[i];
    w->set_sample_rate(SR);

    item = u->create_relation("Wave")->append();
    item->set_val("wave",est_val(w));

    return utt;
}

static void don_make_silence(int duration)
{
    // Make silence for size of the first (and only) phone
    // duation is in sample points
    int i;
    short *buff = walloc(short,duration);
    for (i=0; i < duration; i++)
	buff[i] = 0;
    audio_play(buff,sizeof(short),duration,NULL);
    wfree(buff);
}

static SPN *make_spn(EST_Utterance &u)
{
    //  Build initial structure for donovan code
    SPN *ps = walloc(SPN,1);
    EST_Relation *seg = u.relation("Segment");
    EST_Relation *targ = u.relation("Target");
    EST_Item *s;
    LISP cps;
    const char *ph_name;
    EST_Item *rt;
    int i,j;
    float pos,seg_start,seg_dur;

    ps->p_sz = seg->length();
    ps->p_max = ps->p_sz+1;
    ps->t_sz = num_leaves(targ->head());
    ps->t_max = ps->t_sz+1;
    ps->phons = walloc(char *,ps->p_max);
    ps->duration = walloc(int,ps->p_max);
    ps->cum_dur = walloc(int,ps->p_max);
    ps->pb = walloc(int,ps->p_max);
    ps->scale = walloc(float,ps->p_max);
    ps->diphs = walloc(char *,ps->p_max);
    for (i=0; i<ps->p_sz; i++)
	ps->diphs[i] = walloc(char,8);

    ps->pc_targs = walloc(int,ps->t_max);
    ps->targ_phon = walloc(int,ps->t_max);
    ps->targ_freq = walloc(int,ps->t_max);
    ps->abs_targ = walloc(int,ps->t_max);
    
    for (j=i=0,s=seg->first(); s != 0; s=inext(s),i++)
    {
	if (((cps=ft_get_param("PhoneSet")) == NIL) ||
	    ((streq(get_c_string(cps),"holmes"))))
	    ph_name = s->name();
	else
	    ph_name = map_phone(s->name(),get_c_string(cps),"holmes");
	ps->phons[i] = wstrdup(ph_name);
	seg_start = ffeature(s,"segment_start");
	seg_dur = ffeature(s,"segment_duration");
	ps->duration[i] = (int)(seg_dur * 10000);  /* in frames */
	if (i>0)
	    ps->cum_dur[i] = ps->cum_dur[i-1];
	else
	    ps->cum_dur[i] = 0;
	ps->cum_dur[i] += ps->duration[i];
	for (rt = daughter1(s,"Target");
	     rt != 0;
	     rt = inext(rt),j++)
	{
	    ps->targ_phon[j] = i;
	    ps->targ_freq[j] = rt->I("f0");
	    pos = ((rt->F("pos") - seg_start) / seg_dur) * 99.9;
	    ps->pc_targs[j] = (int)pos;
	}
    }

    // makewave thinks its writing to a file but its actually writting 
    // to an incore buffer.  We allocate it here and make it bigger if
    // necessary
    if (outbuff != NULL)
	wfree(outbuff);
    if (i == 0)
	outmax = 10;
    else
	outmax = (int)(ps->cum_dur[i-1]*1.1);
    outbuff = walloc(short,outmax);
    outpos = 0;

    return ps;

}

static void delete_spn(SPN *ps)
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
    wfree(ps->phons);
    wfree(ps->duration);
    wfree(ps->cum_dur);
    wfree(ps->pb);
    wfree(ps->scale);
    wfree(ps->diphs);

    wfree(ps->pc_targs);
    wfree(ps->targ_phon);
    wfree(ps->targ_freq);
    wfree(ps->abs_targ);

    wfree(ps);

    return;
}

static ACOUSTIC *make_as(SPN *ps)
{
    ACOUSTIC *as = walloc(ACOUSTIC,1);
    int nframes = ps->cum_dur[ps->p_sz-1];
    int npp = nframes * 2;

    as->p_sz = 0;
    as->f_sz = 0;
    as->p_max = npp;
    as->f_max = nframes;
	
    as->mcebuf = walloc(FRAME*,nframes);
    as->duration = walloc(short,nframes);
    as->pitch = walloc(short,npp);

    return as;
}

static void delete_as(ACOUSTIC *as)
{
    
    if (as == NULL)
	return;
    wfree(as->mcebuf);
    wfree(as->duration);
    wfree(as->pitch);
    wfree(as);
    
    return;
}

void as_realloc(int nframes, int npp, ACOUSTIC *as)
{
    // I don't think this will ever be called in Festival
    (void)nframes;
    (void)npp;
    (void)as;

    cerr << "Donovan diphones: as_realloc called unexpectedly\n";
    festival_error();

}

void audio_play(short *start,int sz,int number,CONFIG *config)
{
    // The lower level system thinks its calling an original function
    // but here we intercept the output and put in a EST_Wave class
    // This function will be called a number of times, but we know 
    // (roughly) what the maximum size will be, but just in case
    // we make it bigger if necessary
    (void)config;

    if (outpos+number > outmax)
    {
	int noutmax = (int)((float)(outpos+number)*1.1);
	short *noutbuf = walloc(short,noutmax);
	memmove(noutbuf,outbuff,sizeof(short)*outpos);
	wfree(outbuff);
	outbuff = noutbuf;
	outmax = noutmax;
    }

    memmove(&outbuff[outpos],start,number*sz);
    outpos += number;
}

void festival_donovan_init(void)
{

    // Donovan (Isard) LPC diphone set

    proclaim_module("donovan");

    init_subr_1("Donovan_Init",FT_Donovan_Load_Diphones,
    "(Donovan_Init PARAMS)\n\
  Initialize the Donovan LPC diphone database.  PARAMS are an assoc list\n\
  of parameter name and value.  The two parameters are index_file (value is\n\
  a pathname for \"diphlocs.txt\") and diphone_file (value is a pathname\n\
  for \"lpcdiphs.bin\").  [see LPC diphone synthesizer]");
    festival_def_utt_module("Donovan_Synthesize",FT_Donovan_Synthesize_Utt,
    "(Donovan_Synthesize UTT)\n\
  Synthesize a waveform using the Donovan LPC diphone synthesizer.\n\
  This is called from Synthesize when the Synth_Method Parameter has the\n\
  value Donovan. [see LPC diphone synthesizer]");

}



