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
/*  The groupfile stuff is all awb's fault.                              */
/*                                                                       */
/*************************************************************************/
#include <cstdio>
#include "EST_unix.h"
#include <cstdlib>
#include "festival.h"
#include "diphone.h"

static unsigned int DIPHONE_MAGIC=0x46544449;  /* FTDI */

static void load_index(DIPHONE_DATABASE *database);
static void load_diphs(DIPHONE_DATABASE *database);
static void load_lpc_file(DIPHONE_DATABASE *db,int diph,int mode);
static void lpc2ref(const float *lpc, float *rfc, int order);
static void extract_lpc_frames(DIPHONE_DATABASE *db, int diph, EST_Track &lpc);
static void load_signal_file(DIPHONE_DATABASE *db, int i, int mode);
static void database_malloc(int ndiphs, DIPHONE_DATABASE *database);
static void di_group_load_signal(DIPHONE_DATABASE *db);
static void di_group_load_lpc_params(DIPHONE_DATABASE *db);
static void di_group_load_pm(DIPHONE_DATABASE *db);
 
void di_load_database(DIPHONE_DATABASE *database)
{
    // Load the ungrouped form
    database_malloc(database->ndiphs,database);
    
    load_index(database); 
    load_diphs(database);

}

static void database_malloc(int ndiphs, DIPHONE_DATABASE *database)
{
    // So why am I not using all those cute C++ classes ?
    // well I suppose I just don't know enough about binary loading
    // and saving to trust them, but that's a poor excuse.
    int i;
 
    database->nindex = 0;
    database->zone = 0;
 
    database->indx = walloc(DI_INDEX *,ndiphs);
    database->vox = walloc(DI_VOX *,ndiphs);
    database->pm = walloc(DI_PM *,ndiphs);
    database->lpc = walloc(DI_LPC *,ndiphs);
 
    for(i=0;i<ndiphs;i++) 
    {
	database->indx[i] = walloc(DI_INDEX,1);
	database->vox[i] = walloc(DI_VOX,1);
	database->vox[i]->signal = 0;
	database->pm[i] = walloc(DI_PM,1);
	database->pm[i]->mark = 0;
	database->lpc[i] = walloc(DI_LPC,1);
	database->lpc[i]->f = 0;
    }

}

static void load_index(DIPHONE_DATABASE *database)
{
    EST_TokenStream ts;
    int i;
    EST_String line;

    if (ts.open(database->index_file) == -1)
    {
	cerr << "Diphone: Can't open file " << database->index_file << endl;
	festival_error();
    }

    for (i=0; (!ts.eof()) && (i<database->ndiphs);)
    {
	line = ts.get_upto_eoln();
	if ((line.length() > 0) && (line[0] != ';'))
	{
	    EST_TokenStream ls;
	    ls.open_string(line);
	    database->indx[i]->diph = wstrdup(ls.get().string());
	    database->indx[i]->file = wstrdup(ls.get().string());
	    database->indx[i]->beg = atof(ls.get().string());
	    database->indx[i]->mid = atof(ls.get().string());
	    database->indx[i]->end = atof(ls.get().string());
	    ls.close();
	    i++;
	}
    }

    if (i == database->ndiphs)
    {
	cerr << "Diphone: too many diphones in DB" << endl;
	festival_error();
    }

    database->nindex = i;
    database->ndiphs = i;

    ts.close();
}

static void load_diphs(DIPHONE_DATABASE *database)
{
    int i;

    for(i=0;i<database->nindex;i++) 
    {
	load_signal_file(database,i,database->sig_access_type);
//	if (database->type = di_lpc)
//	    load_lpc_file(database,i,database->sig_access_type);
	load_pitch_file(database,i,database->sig_access_type);
    }
}
 
static void load_lpc_file(DIPHONE_DATABASE *db,int diph,int mode)
{
    // Load LPC coefficients 
    EST_String lpc_file;
    EST_Track lpc;

    if (db->lpc[diph]->f != 0)
	return;   // already loaded

    if (mode == di_direct)
    {
	lpc_file = EST_String(db->lpc_dir) + 
	    db->indx[diph]->file + db->lpc_ext;

	if (lpc.load(lpc_file) != format_ok)
	{
	    cerr << "Diphone: failed to read lpc file " <<
		lpc_file << endl;
	    festival_error();
	}
	if (lpc.num_channels() != db->lpc_order)
	{
	    cerr << "Diphone: lpc file " <<
		lpc_file << " has order " << lpc.num_channels() << 
		    " while database has " << db->lpc_order << endl;
	    festival_error();
	}
	// Extract frames (pitch synchronously)
	extract_lpc_frames(db,diph,lpc);
    }

    return;
}

static void ref2lpc(const float *rfc, float *lpc, int order)
{
    // Here we use Christopher Longet Higgin's algorithm converted to 
    // an equivalent by awb.  Its doesn't have hte reverse order or
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

static void extract_lpc_frames(DIPHONE_DATABASE *db, int diph, EST_Track &lpc)
{
    // Extract LPC frames from lpc, one for each pitch mark
    int frame_num;
    float pos,factor;
    float ps_pos;
    int i,j,k;

    db->lpc[diph]->f = walloc(float *,db->pm[diph]->nmark);
    float *lpcs = walloc(float,lpc.num_channels());
    for (i=0; i < db->pm[diph]->nmark; i++)
    {
	if (db->lpc_pitch_synch)
	{
	    db->lpc[diph]->f[i] = walloc(float,lpc.num_channels());
	    pos = (((float)db->pm[diph]->mark[i]-db->sig_band)/
		   (float)db->samp_freq)
		+ (db->indx[diph]->beg/1000.0);
	    for (j=1,ps_pos=0; j<lpc.num_frames(); j++)
	    {
		ps_pos = lpc.t(j);
		if (fabs(pos-ps_pos) < 0.003)  // within 3 ms
		{
		    for (k=0; k < lpc.num_channels(); k++)
			lpcs[k] = lpc(j,k);
		    // need to get reflection coefficients
		    db->lpc[diph]->f[i][0] = lpcs[0];
		    lpc2ref(&lpcs[1],&db->lpc[diph]->f[i][1],
			    lpc.num_channels()-1);
		    break;
		}
	    }
	    if (j==lpc.num_frames())
	    {
		cerr << "Diphone: lpc access, failed to find lpc coeffs" 
		    << endl;
		festival_error();
	    }
	}
	else
	{   // Not pitch synchronous so find closest frames and
	    // interpolate between them
	    db->lpc[diph]->f[i] = walloc(float,lpc.num_channels());
	    // position of current mark in seconds
	    pos = (((float)db->pm[diph]->mark[i]-db->sig_band)/
		   (float)db->samp_freq)
		+ (db->indx[diph]->beg/1000.0);
	    // Convert to frames, rounding and subtracting start offset
	    frame_num =  (int)((pos/db->lpc_frame_shift))
		           - db->lpc_frame_offset;
	    if (frame_num+1 < lpc.num_frames())
	    {   //  Interpolate between them
		factor = (pos - ((1+frame_num)*db->lpc_frame_shift))/
		    db->lpc_frame_shift;
		for (j=0; j < lpc.num_channels(); j++)
		{
		    db->lpc[diph]->f[i][j] = 
			lpc(frame_num,j) +
			    (factor * (lpc(frame_num+1,j)-
				       lpc(frame_num,j)));
		}
	    }
	    if (frame_num >= lpc.num_frames())
	    {
		cerr << "Diphone: LPC frame past end of file \"" << 
		    db->indx[diph]->file << "\"" << endl;
		memset(db->lpc[diph]->f[i],0,sizeof(float)*lpc.num_channels());
	    }
	    else  // Last one so just take it as is
	      {
		lpc.copy_frame_out(frame_num, db->lpc[diph]->f[i], 
				   0, lpc.num_channels());
	      }
	}
    }
    db->lpc[diph]->nframes = db->pm[diph]->nmark;
    wfree(lpcs);
//    db->lpc_order = lpc.num_channels();

}

static void lpc2ref(const float *lpc, float *rfc, int order)
{
    // LPC to reflection coefficients 
    // from code from Borja Etxebarria
    int i,j;
    float f,ai;
    float *vo,*vx;
    float *vn = new float[order];

    i = order-1;
    rfc[i] = ai = lpc[i];
    f = 1-ai*ai;
    i--;

    for (j=0; j<=i; j++)
	rfc[j] = (lpc[j]+((ai*lpc[i-j])))/f;

    /* vn=vtmp in previous #define */
    vo=rfc;

    for ( ;i>0; ) 
    {
	ai=vo[i];
	f = 1-ai*ai;
	i--;
	for (j=0; j<=i; j++)
	    vn[j] = (vo[j]+((ai*vo[i-j])))/f;
	
	rfc[i]=vn[i];

	vx = vn;
	vn = vo;
	vo = vx;
    }

    delete [] vn;
}

static void load_signal_file(DIPHONE_DATABASE *db, int i, int mode)
{
    //  Load signal (or lpc residual) file)
    int beg_samp,end_samp,zone,nsamples;
    EST_String signal_file;
    EST_String sig_type;
    int offset,error;
    beg_samp = 0;
    zone = 0;
    nsamples = 0;

    if (db->gtype == di_ungrouped)
    {
	beg_samp = (int)((db->indx[i]->beg)/1000.0*db->samp_freq);
	end_samp = (int)((db->indx[i]->end)/1000.0*db->samp_freq);
	
	nsamples = end_samp - beg_samp;
	
	zone = db->sig_band;
	db->zone = zone;
	db->vox[i]->nsamples = nsamples+(2*zone);
	db->vox[i]->signal = 0;
    }

    if (mode == di_direct)
    {
	if (db->gtype == di_ungrouped)
	{   
	    EST_Wave w;
	    if (db->type == di_lpc)
	    {
		signal_file = EST_String(db->lpc_dir) +
		    EST_String(db->indx[i]->file) +
			EST_String(db->lpc_res_ext);
		sig_type = db->lpc_res_type;
		// Different LPC techniques will leave various offsets
		// in the residule, you have to specify this explicitly
		beg_samp -= (int)(db->lpc_res_offset * db->samp_freq);
	    }
	    else
	    {
		signal_file = EST_String(db->signal_dir) +
		    EST_String(db->indx[i]->file) +
			EST_String(db->signal_ext);
		sig_type = db->signal_type;
	    }
	    offset = beg_samp-zone;
	    if (offset < 0)
		offset = 0;
	    if (w.load_file(signal_file,sig_type,
			    db->samp_freq, "short", EST_NATIVE_BO,
			    1, offset, nsamples+2*zone) != format_ok)
	    {
		cerr << "Diphone: failed to read " << sig_type 
		    << " format signal file " << signal_file << endl;
		festival_error();
	    }
	    db->vox[i]->signal = walloc(short,w.num_samples());
	    if (beg_samp-zone < 0)   // wasn't enough space at beginning
		error = abs(beg_samp-zone);
	    else
		error = 0;
	    memset(db->vox[i]->signal,0,error*sizeof(short));
	    for (int j=0; j < w.num_samples()-error; j++)
		db->vox[i]->signal[error+j] = w(j);
	    db->vox[i]->nsamples = w.num_samples()-error;
	}
	else //  grouped so have to access the group file
	{
	    if (db->gfd == NULL)
	    {
		cerr << "Diphone: can no longer access the group file" << endl;
		festival_error();
	    }
	    if (db->group_encoding == di_raw)
	    {
		db->vox[i]->signal = walloc(short,db->vox[i]->nsamples);
		fseek(db->gfd,db->gsignalbase+(db->offsets[i]*2),SEEK_SET);
		fread(db->vox[i]->signal,sizeof(short),
		      db->vox[i]->nsamples,db->gfd);
		if (db->swap)
		    swap_bytes_short(db->vox[i]->signal,db->vox[i]->nsamples);
	    }
	    else if (db->group_encoding == di_ulaw)
	    {
		unsigned char *ulaw = 
		    walloc(unsigned char,db->vox[i]->nsamples);
		db->vox[i]->signal = walloc(short,db->vox[i]->nsamples);
		fseek(db->gfd,db->gsignalbase+(db->offsets[i]),SEEK_SET);
		fread(ulaw,sizeof(unsigned char),db->vox[i]->nsamples,db->gfd);
		ulaw_to_short(ulaw,db->vox[i]->signal,db->vox[i]->nsamples);
		wfree(ulaw);
	    }
	    else
	    {
		cerr << "Diphone: unknown group type" << endl;
		festival_error();
	    }
	}
    }

}

void load_pitch_file(DIPHONE_DATABASE *database, int i, int mode)
{
    // load files from newer Track format
    int mark[5000];
    EST_String pitch_file;
    EST_Track pms;
    float fnum;
    int k,k1,k2,m,zone,beg_samp,p;

    if ((database->pm[i]->mark != 0) ||
	(mode != di_direct))
	return;

    pitch_file = EST_String(database->pitch_dir)+database->indx[i]->file+
	database->pitch_ext;
    if (pms.load(pitch_file) != format_ok)
    {
	cerr << "Diphone: Can't open pitch file " << pitch_file << endl;
	festival_error();
    }
    /* assumptions.. only those within the limits of the diphone  */

    beg_samp = (int)((database->indx[i]->beg)/1000.0*database->samp_freq);
 
    zone = database->sig_band;

    k = 0;
    k1 = 0;
    k2 = 0;
    for (p=0; p < pms.num_frames(); p++)
    {
	fnum = pms.t(p)*1000.0;
	if((fnum>database->indx[i]->beg) && (fnum<database->indx[i]->mid)) 
	{
	    mark[k] = (int)(fnum/1000.0*database->samp_freq - beg_samp + zone);
	    if ((mark[k] >= database->vox[i]->nsamples+zone) ||
		(mark[k] > 64534))
	    {
		fprintf(stderr,"Diphone: Mark out of range -- too large %s\n",
			(const char *)pitch_file);
		k--; k1--;
	    }
	    if(mark[k] < zone) 
	    {
		fprintf(stderr,"Diphone: Mark out of range -- too small %s\n",
			(const char *)pitch_file);
		k--; k1--;
	    }
	    k++;
	    k1++;
	} 
	else if((fnum>=database->indx[i]->mid) && 
		(fnum<database->indx[i]->end)) 
	{
	    mark[k] = (int)(fnum/1000.0*database->samp_freq - beg_samp + zone);
	    if ((mark[k] >= database->vox[i]->nsamples+zone) ||
		(mark[k] > 64534))
	    {
		fprintf(stderr,"Diphone: Mark out of range -- too large %s\n",
			(const char *)pitch_file);
		k--; k2--;
	    }
	    if(mark[k] < zone) 
	    {
		fprintf(stderr,"Diphone: Mark out of range -- too small %s\n",
			(const char *)pitch_file);
		k--; k2--;
	    }
	    k++;
	    k2++;
	}
    }
    database->pm[i]->mark = walloc(unsigned short,k);
    for(m=0;m<k;m++) 
	database->pm[i]->mark[m] = (unsigned short)mark[m];

    database->pm[i]->nmark = (unsigned short)k;
    database->pm[i]->lmark = (unsigned short)k1;
    database->pm[i]->rmark = (unsigned short)k2;
    if (database->pm[i]->rmark == 0)
    {
	*cdebug << "Diphone: modifying edge pms for " 
	    << database->indx[i]->diph << endl;
	database->pm[i]->rmark = 1;
	database->pm[i]->lmark -= 1;
    }
    if (database->pm[i]->nmark <= 0)
    {
	cerr << "Diphone: diphone " << database->indx[i]->diph << 
          " has 0 pitchmarks" << endl;
	festival_error();
    }

}

#if 0
void load_pitch_file(DIPHONE_DATABASE *database, int i, int mode)
{
    char s[100];
    int mark[5000];
    EST_String pitch_file;
    FILE *pfd;
    float fnum;
    int k,k1,k2,m,zone,beg_samp;

    if ((database->pm[i]->mark != 0) ||
	(mode != di_direct))
	return;

    pitch_file = EST_String(database->pitch_dir)+database->indx[i]->file+
	database->pitch_ext;
    if((pfd=fopen(pitch_file,"rb")) == NULL) 
    {
	cerr << "Diphone: Can't open pitch file " << pitch_file << endl;
	festival_error();
    }
    /* assumptions.. only those within the limits of the diphone  */

    beg_samp = (int)((database->indx[i]->beg)/1000.0*database->samp_freq);
 
    zone = database->sig_band;

    k = 0;
    k1 = 0;
    k2 = 0;
    while(fgets(s,100,pfd) != NULL) 
    {
	sscanf(s,"%f",&fnum);
	if((fnum>database->indx[i]->beg) && (fnum<database->indx[i]->mid)) 
	{
	    mark[k] = (int)(fnum/1000.0*database->samp_freq - beg_samp + zone);
	    if ((mark[k] >= database->vox[i]->nsamples+zone) ||
		(mark[k] > 64534))
	    {
		fprintf(stderr,"Diphone: Mark out of range -- too large %s\n",
			(const char *)pitch_file);
		k--; k1--;
	    }
	    if(mark[k] < zone) 
	    {
		fprintf(stderr,"Diphone: Mark out of range -- too small %s\n",
			(const char *)pitch_file);
		k--; k1--;
	    }
	    k++;
	    k1++;
	} 
	else if((fnum>=database->indx[i]->mid) && 
		(fnum<database->indx[i]->end)) 
	{
	    mark[k] = (int)(fnum/1000.0*database->samp_freq - beg_samp + zone);
	    if ((mark[k] >= database->vox[i]->nsamples+zone) ||
		(mark[k] > 64534))
	    {
		fprintf(stderr,"Diphone: Mark out of range -- too large %s\n",
			(const char *)pitch_file);
		k--; k2--;
	    }
	    if(mark[k] < zone) 
	    {
		fprintf(stderr,"Diphone: Mark out of range -- too small %s\n",
			(const char *)pitch_file);
		k--; k2--;
	    }
	    k++;
	    k2++;
	}
    }
    database->pm[i]->mark = walloc(unsigned short,k);
    for(m=0;m<k;m++) 
	database->pm[i]->mark[m] = (unsigned short)mark[m];

    database->pm[i]->nmark = (unsigned short)k;
    database->pm[i]->lmark = (unsigned short)k1;
    database->pm[i]->rmark = (unsigned short)k2;
    if (database->pm[i]->rmark == 0)
    {
	*cdebug << "Diphone: modifying edge pms for " 
	    << database->indx[i]->diph << endl;
	database->pm[i]->rmark = 1;
	database->pm[i]->lmark -= 1;
    }
    if (database->pm[i]->nmark <= 0)
    {
	cerr << "Diphone: diphone " << database->indx[i]->diph << 
          " has 0 pitchmarks" << endl;
	festival_error();
    }

    fclose(pfd);
}
#endif

/* Buffer to hold current diphone signal when using ondemand access   */
/* method.  It remembers the last phone accessed as typical access is */
/* the same one for a few times                                       */
static short *diph_buffer = 0;
static int diph_max_size = 0;
static int last_diph = -1;
static DIPHONE_DATABASE *last_db = 0;

short *di_get_diph_signal(int diph,DIPHONE_DATABASE *db)
{
    // Get the diphone signal (or residual) from wherever
    
    if (db->sig_access_type == di_direct)         // all pre-loaded
	return db->vox[diph]->signal;
    else if (db->sig_access_type == di_dynamic)   // Load and keep
    {   
	if (db->vox[diph]->signal == 0)
	    load_signal_file(db,diph,di_direct);
	return db->vox[diph]->signal;
    }
    else if (db->sig_access_type == di_ondemand)  // Load and free afterwards
    {   // Loads it into a common buffer, over written each time
	if ((diph == last_diph) &&
	    (db == last_db))    // ensure db hasn't changed
	    return diph_buffer;
	load_signal_file(db,diph,di_direct);
	if (diph_max_size < db->vox[diph]->nsamples)
	{
	    wfree(diph_buffer);
	    diph_buffer = walloc(short,db->vox[diph]->nsamples);
	    diph_max_size = db->vox[diph]->nsamples;
	}
	memmove(diph_buffer,db->vox[diph]->signal,
		db->vox[diph]->nsamples*sizeof(short));
	wfree(db->vox[diph]->signal);
	db->vox[diph]->signal = 0;
	last_db = db; last_diph = diph;
	return diph_buffer;
    }
    else
    {
	cerr << "Diphone: unknown diphone signal access strategy" << endl;
	festival_error();
    }
    return NULL;
}

/* The buffer used to hold the requested frame */
static float frame_buff[128];

float *di_get_diph_lpc_mark(int diph,int mark,DIPHONE_DATABASE *db)
{
    // Get the coeff frame for diph at mark

    load_lpc_file(db,diph,di_direct);

    memmove(frame_buff,
	    db->lpc[diph]->f[mark],
	    sizeof(float)*db->lpc_order);
    
    return frame_buff;
}

short *di_get_diph_res_mark(int diph,int mark,int size,DIPHONE_DATABASE *db)
{
    // Get the residual for diph at mark, use the signal field
    // to hold it as they are so similar.
    short *residual;

    residual = di_get_diph_signal(diph,db);

    // Take residual around this midpoint

    int pos_samp = db->pm[diph]->mark[mark] - size/2;

    if (pos_samp < 0)
    {
	pos_samp = 0;
	*cdebug << "DIPHONE: sig_band too short to the left" << endl;
    }
    if (pos_samp+size >= db->vox[diph]->nsamples)
    {
	pos_samp = db->vox[diph]->nsamples - size;
	*cdebug << "DIPHONE: sig_band too short to the right" << endl;
    }

    return &residual[pos_samp];
}

void di_load_grouped_db(const EST_Pathname &filename, DIPHONE_DATABASE *db,
			LISP global_params)
{
    // Get index file and saved data from grouped file
    int i,j;
    unsigned int magic;
    int strsize;
    char *diphnames;
    LISP params;

    if ((db->gfd=fopen(filename,"rb")) == NULL)
    {
	cerr << "Diphone: cannot open group file " <<
	    filename << " for reading" << endl;
	festival_error();
    }

    fread(&magic,sizeof(int),1,db->gfd);
    if (magic == SWAPINT(DIPHONE_MAGIC))
	db->swap = TRUE;
    else if (magic != DIPHONE_MAGIC)
    {
	cerr << "Diphone: " << filename << " not a group file" << endl;
	festival_error();
    }

    params = lreadf(db->gfd);  // read the parameters in LISP form

    di_general_parameters(db,params);  // some may be reset later
    di_fixed_parameters(db,params);
    di_general_parameters(db,global_params);  // reset some params

    database_malloc(db->ndiphs,db);
    db->nindex = db->ndiphs;  // we can trust that number this time

    fread(&strsize,sizeof(int),1,db->gfd);  // number of chars in diph names
    if (db->swap)
	strsize = SWAPINT(strsize);
    diphnames = walloc(char,strsize);
    fread(diphnames,sizeof(char),strsize,db->gfd);
    for (j=i=0;i<db->nindex;i++)
    {
	db->indx[i]->diph = &diphnames[j];
	db->indx[i]->file = 0;
	for ( ; diphnames[j] != '\0'; j++) // skip to next diphname
	    if (j > strsize)
	    {
		cerr << "Diphone: group file diphone name table corrupted"
		    << endl;
		festival_error();
	    }
	j++;
    }

    // Diphone signals
    di_group_load_signal(db);
    // Diphone LPC parameters 
    if (db->type == di_lpc)
	di_group_load_lpc_params(db);
    // Diphone Pitch marks
    di_group_load_pm(db);

    if (db->sig_access_type == di_direct)
    {
	fclose(db->gfd);  // read eveything 
	db->gfd = 0;
    }
    
}

static void di_group_load_signal(DIPHONE_DATABASE *db)
{
    int i;
    unsigned short *samp_counts;
    int sample_offset,totsamples;

    samp_counts = walloc(unsigned short,db->nindex);
    fread(samp_counts,sizeof(unsigned short),db->nindex,db->gfd);
    if (db->swap) swap_bytes_ushort(samp_counts,db->nindex);
    fread(&totsamples,sizeof(int),1,db->gfd);
    if (db->swap)
	totsamples = SWAPINT(totsamples);
    if (db->sig_access_type == di_direct)
    {
	if (db->group_encoding == di_raw)
	{
	    db->allsignal = walloc(short,totsamples);
	    fread(db->allsignal,sizeof(short),totsamples,db->gfd);
	    if (db->swap)
		swap_bytes_short(db->allsignal,totsamples);
	}
	else if (db->group_encoding == di_ulaw)
	{
	    db->allulawsignal = walloc(unsigned char,totsamples);
	    fread(db->allulawsignal,sizeof(unsigned char),totsamples,db->gfd);
	}
    }
    else
    {
	db->gsignalbase = ftell(db->gfd);
	db->offsets = walloc(int,db->nindex);
    }

    sample_offset = 0;
    for (i=0; i < db->nindex; i++)
    {
	db->vox[i]->nsamples = samp_counts[i];
	if (db->sig_access_type == di_direct)
	{
	    if (db->group_encoding == di_raw)
		db->vox[i]->signal = &db->allsignal[sample_offset];
	    else if (db->group_encoding == di_ulaw)
	    {
		db->vox[i]->signal = walloc(short,samp_counts[i]);
		ulaw_to_short(&db->allulawsignal[sample_offset],
			      db->vox[i]->signal,samp_counts[i]);
	    }
	    else
	    {
		cerr << "Diphone: unknown group type to unpack" << endl;
		festival_error();
	    }
	}
	else 
	{
	    db->offsets[i] = sample_offset;
	    db->vox[i]->signal = 0;
	}
	sample_offset += samp_counts[i];
    }
    if (db->sig_access_type != di_direct)
	if (db->group_encoding == di_ulaw)
	    fseek(db->gfd,(long)sample_offset,SEEK_CUR);
	else
	    fseek(db->gfd,(long)sample_offset*sizeof(short),SEEK_CUR);
    wfree(samp_counts);
}

static void di_group_load_lpc_params(DIPHONE_DATABASE *db)
{
    // LPC params are always fully loaded
    int totframes;
    int i,j,k;
    unsigned short *frame_counts;
    int frame_offset;
    int this_frame;
    
    frame_counts = walloc(unsigned short, db->nindex);
    fread(frame_counts,sizeof(unsigned short),db->nindex,db->gfd);
    if (db->swap) swap_bytes_ushort(frame_counts,db->nindex);
    fread(&totframes,sizeof(int),1,db->gfd);
    if (db->swap) totframes = SWAPINT(totframes);
    if (db->group_encoding == di_raw) // its as floats
    {
	db->allframes = walloc(float,totframes*db->lpc_order);
	fread(db->allframes,sizeof(float),
	      totframes*db->lpc_order,db->gfd);
	if (db->swap) 
	    swap_bytes_float(db->allframes,totframes*db->lpc_order);
    }
    else if (db->group_encoding == di_ulaw) // its as shorts
    {
	db->allframesshort = walloc(short,totframes*db->lpc_order);
	fread(db->allframesshort,sizeof(short),
	      totframes*db->lpc_order,db->gfd);
	if (db->swap)
	    swap_bytes_short(db->allframesshort,
			     totframes*db->lpc_order);
    }
    frame_offset = 0;
    for (i=0; i < db->nindex; i++)
    {
	db->lpc[i]->nframes = frame_counts[i];
	db->lpc[i]->f = walloc(float *,frame_counts[i]);
	if (db->group_encoding == di_raw)
	    for (j=0;j<db->lpc[i]->nframes;j++)
		db->lpc[i]->f[j] = 
		    &db->allframes[(frame_offset+j)*db->lpc_order];
	else if (db->group_encoding == di_ulaw)
	{
	    int fixedpoint = FALSE;
	    if (siod_get_lval("lpc_fixedpoint",NULL) != NIL)
		fixedpoint = TRUE;
	    for (j=0;j<db->lpc[i]->nframes;j++)
	    {
		db->lpc[i]->f[j] = walloc(float,db->lpc_order);
		this_frame = (frame_offset+j)*db->lpc_order;
		if (fixedpoint)
		    for (k=0;k<db->lpc_order;k++)
			db->lpc[i]->f[j][k] = 
			    (float)db->allframesshort[this_frame+k];
		else
		    for (k=0;k<db->lpc_order;k++)
			db->lpc[i]->f[j][k] = 
			    (float)db->allframesshort[this_frame+k]/32766.0;
	    }
	}
	else
	{
	    cerr << "Diphone: unknown group type to unpack" << endl;
	    festival_error();
	}
	frame_offset += frame_counts[i];
    }
    wfree(db->allframesshort);
    db->allframesshort = 0;      // don't really need this any more
    wfree(frame_counts);
}

static void di_group_load_pm(DIPHONE_DATABASE *db)
{
    unsigned short *pm_info;
    int i,j;
    
    pm_info = walloc(unsigned short,db->nindex*3);
    if (fread(pm_info,sizeof(unsigned short),db->nindex*3,db->gfd) != 
	(unsigned int)(db->nindex*3))
    {
	cerr << "DIPHONE: short group file, can't read pm\n";
	festival_error();
    }
    if (db->swap)
	for (i=0; i < db->nindex*3; i++)
	    pm_info[i] = SWAPSHORT(pm_info[i]);
    for (i=0; i < db->nindex; i++)
    {
	db->pm[i]->mark = walloc(unsigned short,pm_info[i*3]);
	db->pm[i]->nmark = pm_info[i*3];
	db->pm[i]->lmark = pm_info[(i*3)+1];
	db->pm[i]->rmark = pm_info[(i*3)+2];
	fread(db->pm[i]->mark,sizeof(unsigned short),db->pm[i]->nmark,db->gfd);
	if (db->swap)
	    for (j=0; j < db->pm[i]->nmark; j++)
		db->pm[i]->mark[j] = SWAPSHORT(db->pm[i]->mark[j]);
    }
}

static LISP di_enlispen_params(DIPHONE_DATABASE *db)
{
    // Return lisp representation of the parameters in db
    
    return cons(make_param_str("name",db->name),
	   cons(make_param_str("type",db->type_str),
	   cons(make_param_str("index_file",db->index_file),
	   cons(make_param_str("signal_dir",db->signal_dir),
	   cons(make_param_str("signal_ext",db->signal_ext),
	   cons(make_param_str("signal_type",db->signal_type),
	   cons(make_param_str("pitch_dir",db->pitch_dir),
	   cons(make_param_str("pitch_ext",db->pitch_ext),
	   cons(make_param_str("lpc_dir",db->lpc_dir),
	   cons(make_param_str("lpc_ext",db->lpc_ext),
	   cons(make_param_str("lpc_res_ext",db->lpc_res_ext),
	   cons(make_param_str("lpc_type",db->lpc_type),
	   cons(make_param_str("lpc_res_type",db->lpc_res_type),
	   cons(make_param_float("lpc_res_offset",db->lpc_res_offset),
	   cons(make_param_int("lpc_frame_offset",db->lpc_frame_offset),
	   cons(make_param_int("lpc_order",db->lpc_order),
	   cons(make_param_float("lpc_frame_shift",db->lpc_frame_shift),
	   cons(make_param_int("samp_freq",db->samp_freq),
	   cons(make_param_str("phoneset",db->phoneset),
	   cons(make_param_str("access_type",db->sig_access_type_str),
	   cons(make_param_str("group_encoding",db->group_encoding_str),
	   cons(make_param_int("num_diphones",db->nindex),
	   cons(make_param_int("sig_band",db->sig_band),
	   cons(make_param_int("def_f0",db->def_f0),
	   cons(make_param_str("default_diphone",db->default_diphone),
	   cons(make_param_lisp("alternates_before",db->alternates_before),
	   cons(make_param_lisp("alternates_after",db->alternates_after),
	   NIL)))))))))))))))))))))))))));
}

void di_save_grouped_db(const EST_Pathname &filename, DIPHONE_DATABASE *db)
{
    // Get index file and saved data from grouped file
    FILE *fd;
    LISP params;
    int strsize,totsamples,totframes;
    int i,j,k;

    if ((fd=fopen(filename,"wb")) == NULL)
    {
	cerr << "Diphone: cannot open group file " <<
	    filename << " for saving" << endl;
	festival_error();
    }
    
    fwrite(&DIPHONE_MAGIC,sizeof(int),1,fd);
    params = di_enlispen_params(db); // get lisp representation of parameters
    lprin1f(params,fd);
    
    // Only need to dump the diphone names, not the rest of the indx info
    strsize = 0;
    for (i=0;i<db->nindex;i++)
	strsize += strlen(db->indx[i]->diph)+1;
    fwrite(&strsize,sizeof(int),1,fd);
    for (i=0;i<db->nindex;i++)
	fwrite(db->indx[i]->diph,sizeof(char),strlen(db->indx[i]->diph)+1,fd);
    
    // Diphone Signals
    // Dump the signal sizes first to make reading easier
    totsamples = 0;
    for (i=0;i<db->nindex;i++)
    {
	if (db->vox[i]->signal == 0) // in case it isn't loaded yet
	{
	    load_pitch_file(db,i,di_direct);
	    load_signal_file(db,i,di_direct);
	}
	fwrite(&db->vox[i]->nsamples,sizeof(unsigned short),1,fd);
	totsamples += db->vox[i]->nsamples;
    }
    fwrite(&totsamples,sizeof(int),1,fd);
    // Dump signals (compressed if necessary)
    for (i=0;i<db->nindex;i++)
    {
	if (db->group_encoding == di_raw)
	    fwrite(db->vox[i]->signal,sizeof(short),db->vox[i]->nsamples,fd);
	else if (db->group_encoding == di_ulaw)
	{
	    unsigned char *ulaw = walloc(unsigned char,db->vox[i]->nsamples);
	    short_to_ulaw(db->vox[i]->signal,ulaw,db->vox[i]->nsamples);
	    fwrite(ulaw,sizeof(unsigned char),db->vox[i]->nsamples,fd);
	    wfree(ulaw);
	}
	else
	{
	    cerr << "Diphone: unknown group type for dumping" << endl;
	    festival_error();
	}
	
    }
    
    // Diphone LPC parameters 
    if (db->type == di_lpc)
    {
	for (i=0;i<db->nindex;i++)	// ensure they are all loaded
	    load_lpc_file(db,i,di_direct);
	totframes = 0;
	for (i=0;i<db->nindex;i++)
	{
	    fwrite(&db->lpc[i]->nframes,sizeof(unsigned short),1,fd);
	    totframes += db->lpc[i]->nframes;
	}
	fwrite(&totframes,sizeof(int),1,fd);
	for (i=0;i<db->nindex;i++)
	{
	    if (db->group_encoding == di_raw) // saved as floats
	    {
		for (j=0; j<db->lpc[i]->nframes; j++)
		    fwrite(db->lpc[i]->f[j],sizeof(float),db->lpc_order,fd);
	    }
	    else if (db->group_encoding == di_ulaw) // saved as shorts
	    {   
		short *sh = new short[db->lpc_order];
		
		for (j=0; j<db->lpc[i]->nframes; j++)
		{
		    for (k=0; k<db->lpc_order; k++)
			sh[k] = (short)(db->lpc[i]->f[j][k]*32766.0);
		    fwrite(sh,sizeof(short),db->lpc_order,fd);
		}
		delete sh;
	    }
	    else
	    {
		cerr << "Diphone: unknown group type for dumping" << endl;
		festival_error();
	    }
	}
    }
    
    // Diphone Pitch Marks
    for (i=0;i<db->nindex;i++)
    {
	fwrite(&db->pm[i]->nmark,sizeof(unsigned short),1,fd);
	fwrite(&db->pm[i]->lmark,sizeof(unsigned short),1,fd);
	fwrite(&db->pm[i]->rmark,sizeof(unsigned short),1,fd);
    }
    for (i=0;i<db->nindex;i++)
    {
	fwrite(db->pm[i]->mark,sizeof(unsigned short),db->pm[i]->nmark,fd);
	i=i;
    }
    
    fclose(fd);
    
}

