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

#ifndef __DIPHONE_H_
#define __DIPHONE_H_

#include "EST_Pathname.h"

enum di_sigaccess_t {di_direct, di_dynamic, di_ondemand};
enum di_db_type_t {di_pcm, di_lpc};
enum di_group_encode_t {di_raw, di_ulaw };
enum di_group_t {di_grouped, di_ungrouped};

typedef struct {
	char *diph;
	char *file;
	float beg;
	float mid;
	float end;
} DI_INDEX;

typedef struct {
    unsigned short nsamples;
    short *signal;
} DI_VOX;

typedef struct {
	unsigned short nmark;
	unsigned short lmark;
	unsigned short rmark;
	unsigned short *mark;
} DI_PM;

typedef struct {
    unsigned short nframes;
    float **f;
} DI_LPC;

typedef struct DD_STRUCT {
    char *name;
    char *type_str;
    enum di_db_type_t type;  /* pcm or lpc */
    enum di_group_t gtype;    /* grouped or ungrouped */
    char *index_file;
    char *signal_dir;
    char *signal_ext;
    char *signal_type;
    char *lpc_dir;
    char *lpc_ext;
    char *lpc_res_ext;
    char *lpc_res_type;
    float lpc_res_offset;
    int lpc_frame_offset;
    EST_String lpc_type;
    int lpc_order;
    float lpc_frame_shift;  /* in seconds */
    char *pitch_dir;
    char *pitch_ext;
    char *sig_access_type_str;
    enum di_sigaccess_t sig_access_type;
    char *group_encoding_str;
    enum di_group_encode_t group_encoding;
    LISP alternates_before;
    LISP alternates_after;
    int def_f0;
    FILE *xfd;
    FILE *gfd;        /* the group file */
    int gsignalbase;  /* offset in group file where signals start */
    int gframebase;   /* offset in group file where frames start */
    int samp_freq;
    int sig_band;	/* the safety zone in samples round each signal  */
    char *phoneset;
    int ndiphs;
    int swap;
    char *default_diphone;  /* when all else fails */
    int lpc_pitch_synch;    /* True if lpc frames are pitch synchronous */

    short *allsignal;                /* used in group files */
    unsigned char *allulawsignal;
    float *allframes;
    short *allframesshort;

    int nindex;
    int zone;	/* bytes  */
    int *offsets;      /* used for indexing signals in group files */
    int *frameoffsets; /* used for indexing frames in group files */
    DI_INDEX **indx;
    DI_VOX **vox;
    DI_PM **pm;
    DI_LPC **lpc;

} DIPHONE_DATABASE;

typedef struct {
        int p_sz;
        int p_max;
        int p_tmp;
        int *pitch;
        int *cum_pitch;
	int *phon_ref;
	int *phon_mark;
	int *diph_ref;
	int *diph_mark;
	int *inwidth;
	int *outwidth;
} DIPHONE_ACOUSTIC;

typedef struct {
        int p_sz;
        int p_max;
        char **phons;
        int *duration;
        int *cum_dur;
        int *ref;
	int *pm_req;
	int *pm_giv;
	int *pm_chg_diph; /* phoneme-based info  */

        char **diphs;

        int t_sz;
        int t_max;
        int *pc_targs;
        int *targ_phon;
        int *targ_freq;
        int *abs_targ; /* maybe in samples  */

} DIPHONE_SPN;

typedef struct {
        int o_sz;
        int o_max;
        short *track;
} DIPHONE_OUTPUT;

void di_load_database(DIPHONE_DATABASE *database);
void di_calc_pitch(DIPHONE_DATABASE *config, 
		   DIPHONE_SPN *ps, 
		   DIPHONE_ACOUSTIC *as);
void di_psola_tm(DIPHONE_DATABASE *db,
		 DIPHONE_ACOUSTIC *as, 
		 DIPHONE_OUTPUT *output);
void di_reslpc(DIPHONE_DATABASE *db, 
	       DIPHONE_ACOUSTIC *as, 
	       DIPHONE_OUTPUT *output);
void reslpc_resynth(const EST_String &file,
		    DIPHONE_DATABASE *db,
		    DIPHONE_ACOUSTIC *as,
		    DIPHONE_OUTPUT *output);
void di_frame_select(DIPHONE_DATABASE *db,
		     DIPHONE_SPN *ps,
		     DIPHONE_ACOUSTIC *as);
short *di_get_diph_signal(int diph,DIPHONE_DATABASE *db);
float *di_get_diph_lpc_mark(int diph,int mark,DIPHONE_DATABASE *db);
//short *di_get_diph_res_mark(int diph,int mark,DIPHONE_DATABASE *db);
short *di_get_diph_res_mark(int diph,int mark,int size,DIPHONE_DATABASE *db);

void di_save_grouped_db(const EST_Pathname &filename, DIPHONE_DATABASE *db);
void di_load_grouped_db(const EST_Pathname &filename, DIPHONE_DATABASE *db,
			LISP global_params);
void di_fixed_parameters(DIPHONE_DATABASE *db,LISP params);
void di_general_parameters(DIPHONE_DATABASE *db,LISP params);
void load_pitch_file(DIPHONE_DATABASE *database, int i, int mode);
LISP find_optimal_coupling(LISP table, LISP weights);

void di_add_diphonedb(DIPHONE_DATABASE *db);
DIPHONE_DATABASE *make_diphone_db(void);

#endif /* __DIPHONE_H__ */
