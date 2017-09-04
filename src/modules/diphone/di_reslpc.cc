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
/*             Author :  Alan W Black with all the magic stuff from      */
/*                       Alistair Conkie, Steve Isard and Paul Taylor    */
/*             Date   :  October 1996                                    */
/*-----------------------------------------------------------------------*/
/*                                                                       */
/* Residual excited LPC synthesis                                        */
/*                                                                       */
/* From "Issues in High Quality LPC Analysis and Synthesis"              */
/* Melvyn Hunt, Dariusz Zweirzynski and Raymond Carr, Eurospeech 89      */
/* vol 2 pp 348-351, Paris 1989                                          */
/*                                                                       */
/*=======================================================================*/
#include <cstdio>
#include <cstdlib>
#include <cstring>
#include <cmath>
#include "festival.h"
#include "diphone.h"

static void add_lpc_coeff(int nth,float *p_coeffs,EST_Track &lpc_coeff);
static void add_residual(int insize, int outsize, int position,
			 short *p_residual,
			 short *residual);
static void lpc_resynth(EST_Track &lpc_coeffs,
			short *residual,
			DIPHONE_ACOUSTIC *as,
			DIPHONE_OUTPUT *output);
static void lpc_resynth_fixp(EST_Track &lpc_coeffs,
			     short *residual,
			     DIPHONE_ACOUSTIC *as,
			     DIPHONE_OUTPUT *output);
static int min(int a, int b);
static void ref2lpc_fixp(const int *rfc, int *lpc, int order);

void reslpc_resynth(const EST_String &file,
		    DIPHONE_DATABASE *db,
		    DIPHONE_ACOUSTIC *as,
		    DIPHONE_OUTPUT *output)
{
    // Resynthesize a named file from the current 
    // This is not fully general, just for my testing
    EST_Track lpc_coeffs;
    EST_Wave residual;
    int i,frame_len;

    if (lpc_coeffs.load(EST_String(db->lpc_dir)+file+EST_String(db->lpc_ext))
	!= format_ok)
    {
	cerr << "Diphone: lpc resynthesis, failed to read lpc file" 
	    << endl;
	festival_error();
    }
    if (residual.load(EST_String(db->lpc_dir)+file+EST_String(db->lpc_res_ext)) !=
	format_ok)
    {
	cerr << "Diphone: lpc resynthesis, failed to read residual file" 
	    << endl;
	festival_error();
    }
    if (db->lpc_pitch_synch)
    {
	as->outwidth = walloc(int,lpc_coeffs.num_frames());
	output->o_max = 0;
	for (i=0; i<lpc_coeffs.num_frames(); i++)
	{
	    // first coeffs in time in samples
	    frame_len = (int)lpc_coeffs(i,0);
	    if (i==0)
		frame_len = (int)(lpc_coeffs.t(i)*residual.sample_rate());
	    else
		frame_len = (int)((lpc_coeffs.t(i)-lpc_coeffs.t(i-1))
				  *residual.sample_rate());
	    as->outwidth[i] = frame_len;
	    output->o_max += frame_len;

	    // Get rid of extra first coeff (time position)
//	    for (int j=0; j<lpc_coeffs.num_channels()-1 ; j++)
//	      lpc_coeffs(i,j) = lpc_coeffs(i,j+1);
	}
	output->o_max *= 2;
	output->track = walloc(short,output->o_max);
//	lpc_coeffs.set_num_channels(lpc_coeffs.num_channels()-1);
    }
    else
    {
	as->outwidth = walloc(int,lpc_coeffs.num_frames());
	frame_len = (int)(db->lpc_frame_shift*db->samp_freq);
	for (i=0; i<lpc_coeffs.num_frames(); i++)
	    as->outwidth[i] = frame_len;
	output->o_max = i*frame_len*2;
	output->track = walloc(short,output->o_max);
    }

    short *shresidual = walloc(short,residual.num_samples());
    for (i=0; i < residual.num_samples(); i++)
	shresidual[i] = residual.a_no_check(i);
    
    // Reconstruct the waveform
    if (siod_get_lval("debug_reslpc",NULL) != NIL)
    {
	residual.save("res.wav","nist");
	lpc_coeffs.save("lpc.ascii","ascii");
    }
    if (siod_get_lval("lpc_fixedpoint",NULL) != NIL)
	lpc_resynth_fixp(lpc_coeffs,shresidual,as,output);
    else
	lpc_resynth(lpc_coeffs,shresidual,as,output);

    wfree(shresidual);
}

void di_reslpc(DIPHONE_DATABASE *db, 
	       DIPHONE_ACOUSTIC *as, 
	       DIPHONE_OUTPUT *output)
{
    int i;
    int curr_diph;
    int curr_mark;
    int max_marks;
    EST_Track lpc_coeffs(as->p_sz-1,db->lpc_order);
    short *residual;

    residual = walloc(short,as->cum_pitch[as->p_sz-1]+db->sig_band);
    memset(residual,0,sizeof(short)*as->cum_pitch[as->p_sz-1]+db->sig_band);

    /* original widths  */
    for(i=0;i<as->p_sz-1;i++) 
    {
	curr_diph = as->diph_ref[i];
	curr_mark = as->diph_mark[i];
	max_marks = db->pm[curr_diph]->nmark;
	if (curr_mark == 0) 
	{
	    as->inwidth[i] = 
		db->pm[curr_diph]->mark[1]-db->pm[curr_diph]->mark[0];
	} 
	else if (curr_mark == max_marks-1) 
	{
	    as->inwidth[i] = db->pm[curr_diph]->mark[curr_mark]
		-db->pm[curr_diph]->mark[curr_mark-1];
	} 
	else 
	{
	    as->inwidth[i] = min(db->pm[curr_diph]->mark[curr_mark+1]
				 -db->pm[curr_diph]->mark[curr_mark],
				 db->pm[curr_diph]->mark[curr_mark]
				 -db->pm[curr_diph]->mark[curr_mark-1]);
	}
    }

    /* new widths       */
    as->outwidth[0] = as->pitch[0];
    for(i=1;i<as->p_sz;i++)
	as->outwidth[i] = min(as->pitch[i],as->pitch[i-1]);

    int res_pos=0;
    /* For each pitch period, copy the lpc coeffs and construct a residual */
    for(i=0;i<as->p_sz-1;i++)
    {
	curr_diph = as->diph_ref[i];
	curr_mark = as->diph_mark[i];

	if (as->outwidth[i] <= 5)
	    continue;   // too wee to worry about 

	/* get coeffs for this pitch period */
	add_lpc_coeff(i,
		      di_get_diph_lpc_mark(curr_diph,curr_mark,db),
		      lpc_coeffs);

	/* get residual for this pitch period (and slice it) */
	add_residual(as->inwidth[i],    
		     as->outwidth[i],
		     res_pos,
		     di_get_diph_res_mark(curr_diph,curr_mark,as->inwidth[i],db),
		     residual);
	res_pos += as->outwidth[i];
    }
    if (siod_get_lval("debug_reslpc",NULL) != NIL)
    {
//	residual.save("res.wav","nist");
	lpc_coeffs.save("lpc.ascii","ascii");
    }
    // Reconstruct the waveform
    if (siod_get_lval("lpc_fixedpoint",NULL) != NIL)
	lpc_resynth_fixp(lpc_coeffs,residual,as,output);
    else
	lpc_resynth(lpc_coeffs,residual,as,output);

    wfree(residual);
}

static void add_lpc_coeff(int nth,float *p_coeffs,EST_Track &lpc_coeff)
{
    // Add coeffs frame to lpc_coeff at position

    lpc_coeff.copy_frame_in(nth, p_coeffs);

}

static void add_residual(int insize, int outsize, int position,
			 short *p_residual,
			 short *residual)
{
    // Slice/pad this pitch's residual to outsize and add to full residual
    // at position.  The mid points should align
    int i;

    int ibp = insize/2;
    int obp = outsize/2;
    if (insize < outsize)
    {   // its too short so place it with 50% points aligned
	memmove(&residual[position+(obp-ibp)],
		p_residual,
		insize*sizeof(short));
    }
    else // insize > outside
    {   // Its too long so only take part of it
	memmove(&residual[position],
		&p_residual[ibp-obp],
		outsize*sizeof(short));
	// Do  power reduction 
	float factor = (float)outsize/(float)insize;
	for (i=0; i < outsize; i++)
	    residual[position+i] = (int)((float)residual[position+i]*factor);
    }

#if 0

    // Windowing (see if it makes a difference)
#define DI_PI 3.14159265358979323846
    float *window = walloc(float,outsize);
    int i;
    for (i=0; i < outsize/5; i++)
	window[i] = (0.5-(0.5*cos((double)(DI_PI*2*i)/(double)((2*outsize)/5-1))));
    for (i=outsize/5; i < outsize; i++)
	window[i] = (0.5-(0.5*cos((double)(DI_PI*2*(i+(outsize/5)*3))/
				  (double)(8*outsize/5-1))));
#if 0
    for (i=0,f=0; i < outsize/2+1; i++,f+=incr)
	window[i] = f;
    incr = 2.0/((outsize/2));
    for (i=0,f=0; i < (outsize/2)+1; i++,f+=incr)
	window[outsize-1-i] = f;
#endif

    for (i=0; i < outsize; i++)
    {
	residual.data()[position+i] = 
	    (short)(window[i]*(float)residual.data()[position+i]);
    }
    wfree(window);
#endif

}
void ref2lpc(const float *rfc, float *lpc, int order)
{
    // Here we use Christopher Longet Higgin's algorithm converted to 
    // an equivalent by awb. It doesn't have hte reverse order or
    // negation requirement.
    float a,b;
    int n,k;
    
    for (n=0; n < order; n++)
    {
	lpc[n] = rfc[n];
	for (k=0; 2*(k+1) <= n+1; k++)
	{
	    a = lpc[k];
	    b = lpc[n-(k+1)];
	    lpc[k] = a-b*lpc[n];
	    lpc[n-(k+1)] = b-a*lpc[n];
	}
    }
}

static void lpc_resynth(EST_Track &lpc_coeffs,
			short *residual,
			DIPHONE_ACOUSTIC *as,
			DIPHONE_OUTPUT *output)
{
    // Reconstruct the waveform from the coefficients and residual
    // This is copied from Steve's donovan code with minor changes
    float *ptbuf;
    float *rbuf;
    int i,j,ci,cz;
    int zz,r;

    int NCOEFFS = lpc_coeffs.num_channels()-1;  // lpc_power is 0
    rbuf = walloc(float,NCOEFFS);
    
    ptbuf = walloc(float,output->o_max);
    memset(ptbuf,0,sizeof(float)*NCOEFFS);
    zz=NCOEFFS;
    r=0;

    float *tmm_coefs = new float[NCOEFFS+1];

    for(i=0;i<lpc_coeffs.num_frames();i++) 
    {
	lpc_coeffs.copy_frame_out(i, tmm_coefs);
	ref2lpc(tmm_coefs+1,rbuf,NCOEFFS);
//	for (int k=1; k<lpc_coeffs.num_channels(); k++)
//	    rbuf[k-1] = tmm_coefs[k];

	if (zz+as->outwidth[i] >= output->o_max)
	{
	    cerr << "Diphone: output buffer overflow " << endl;
	    break;
	}
	for(j=0 ;j < as->outwidth[i];j++,zz++,r++) 
	{
	    ptbuf[zz] = (float)residual[r];

	    // This loop is where all the time is spent
	    for (ci=0,cz=zz-1; ci < NCOEFFS; ci++,cz--)
		ptbuf[zz] += rbuf[ci] * ptbuf[cz];
	}
    }
    // Clipping is required because of some overflows which really points
    // to a lower level problem.  Probably windowing the residual is what
    // I should really do.
    for (i=NCOEFFS,r=0; i < zz; i++,r++)
	if (ptbuf[i] > 32766)
	    output->track[r] = (short)32766;
	else if (ptbuf[i] < -32766)
	    output->track[r] = (short)-32766;
	else
	    output->track[r] = (short)ptbuf[i];
    output->o_sz = r;
    wfree(ptbuf);
    wfree(rbuf);
    delete [] tmm_coefs;
}

static void lpc_resynth_fixp(EST_Track &lpc_coeffs,
			     short *residual,
			     DIPHONE_ACOUSTIC *as,
			     DIPHONE_OUTPUT *output)
{
    // Reconstruct the waveform from the coefficients and residual
    // This is copied from Steve's donovan code with minor changes
    // This is the fixed point (sort of integer) version of the above
    // The coeffs are in the range -32768 to +32767 and where possible
    // integer calculations are done.  This is primarily to allow 
    // this to work on machine swithout a floating point processor
    // (i.e. awb's palmtop)
    int *ptbuf;
    int *rbuf;
    int i,j,ci,cz;
    int zz,r;

    int NCOEFFS = lpc_coeffs.num_channels()-1;  // lpc_power is 0
    rbuf = walloc(int,NCOEFFS);
    
    ptbuf = walloc(int,output->o_max);
    memset(ptbuf,0,sizeof(int)*NCOEFFS);
    zz=NCOEFFS;
    r=0;

    int *tmm_coefs = new int[NCOEFFS+1];

    for(i=0;i<lpc_coeffs.num_frames();i++) 
    {
	for (int k=0; k < NCOEFFS+1; k++)
	    tmm_coefs[k] = (int)lpc_coeffs.a_no_check(i,k);
	ref2lpc_fixp(tmm_coefs+1,rbuf,NCOEFFS);

	if (zz+as->outwidth[i] >= output->o_max)
	{
	    cerr << "Diphone: output buffer overflow " << endl;
	    break;
	}
	for(j=0 ;j < as->outwidth[i];j++,zz++,r++) 
	{
	    ptbuf[zz] = ((int)residual[r]);

	    // This loop is where all the time is spent
	    for (ci=0,cz=zz-1; ci < NCOEFFS; ci++,cz--)
		ptbuf[zz] += (rbuf[ci] * ptbuf[cz]) / 32768;
	}
    }
    // Clipping is required because of some overflows which really points
    // to a lower level problem.  Probably windowing the residual is what
    // I should really do.
    for (i=NCOEFFS,r=0; i < zz; i++,r++)
    {
	if (ptbuf[i] > 32766)
	    output->track[r] = (short)32766;
	else if (ptbuf[i] < -32766)
	    output->track[r] = (short)-32766;
	else
	    output->track[r] = (short)ptbuf[i];
    }
    output->o_sz = r;
    wfree(ptbuf);
    wfree(rbuf);
    delete [] tmm_coefs;
}

static int min(int a, int b)
{
    return((a<b)?a:b);
}

static void ref2lpc_fixp(const int *rfc, int *lpc, int order)
{
    // Here we use Christopher Longet Higgin's alrgorithm converted to 
    // an equivalent by awb.  Its doesn't have the reverse order or
    // negation requirement.
    // Further modify to accept treat the values in rfc as
    // being multiplied by 32768
    int a,b;
    int n,k;
    long long x,y;

    for (n=0; n < order; n++)
    {
	lpc[n] = rfc[n];
	for (k=0; 2*(k+1) <= n+1; k++)
	{
	    a = lpc[k];
	    b = lpc[n-(k+1)];
	    x = b;
	    x *= lpc[n];
	    x /= 32768;
	    lpc[k] = a-x;
	    y = a;
	    y *= lpc[n];
	    y /= 32768;
	    lpc[n-(k+1)] = b-y;
	}
    }
}
