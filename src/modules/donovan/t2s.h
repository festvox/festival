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
/*             Author :  Alistair Conkie and Steve Isard                 */
/*-----------------------------------------------------------------------*/

#ifndef _T2S_H_
#define _T2S_H_
#define NDIPHS 3000
#define NFRAMES 23000
#define FR_DATA 16  /* shorts per frame, coeffs + assorted  */

#define FW 1
#define CW 2
#define PUNCT 3

#define DEF_F0 125
#define SR 10000	/* sample rate  */
#define FR_SZ 132	/* standard frame size  */

/* malloc defaults  */
#define DEF_BUFFER 1024
#define DEF_LING_LIST 100
#define DEF_SPL 100
#define DEF_PHONS 100
#define DEF_TARGS 100
#define DEF_FRAMES 100
#define DEF_PM 100

#define PHON_SZ 5
#define DIPH_SZ 10

#define OUT_BUF 2048

#define NCOEFFS 12

/* non-rhotic vowel classification for assim.c */
#define V_DEL_R 0
#define V_AIR 1
#define V_EER 2
#define V_OOR 3
#define V_R2SCHWA 4

/* various typedefs  */

typedef struct {
	char *input_file;
	char *output_file;
	char *index_file;
	char *diphone_file;
	char *hash_file;
	char *format;
	int type;	/* format by any other name  */
	FILE *ifd;
	FILE *ofd;
	FILE *xfd;
	FILE *dfd;
	void *db;
	int fw_num;
	int sonority_num;
	int dur0_num;
} CONFIG;

typedef struct {
	int max;
	int sz;
	char *ptr;
} BUFFER;

typedef struct {
	char *word;
	int type;
	char *transcription;
} LING;

typedef struct {
	int max;
	int sz;
	LING **text;
} LING_LIST;

typedef struct key {
	char *keyword;
	int keycount;
} KEY;

typedef struct {
	char phoneme[5];
	int syll;
	int dur;
	char *sprosod1;
	char *sprosod2;
	float strength1;
	float strength2;	/* for combined elements  */
} SPROSOD;

typedef struct {
	int max;
	int sz;
	SPROSOD **phoneme;
} SPROSOD_LIST;

typedef struct {
        char diph[10];
        int beg;
        int mid;
        int end;
} ENTRY;

typedef struct {
        short frame[FR_DATA]; 
} FRAME;

typedef struct {
	int p_sz;
	int p_max;
	int t_sz;
	int t_max;
	char **phons;
	int *duration;
	int *cum_dur;
	int *pc_targs;
	int *targ_phon;
	int *targ_freq;
	int *abs_targ; /* maybe in samples  */
	int *pb;
	float *scale;
	char **diphs;
} SPN;

typedef struct {
	int f_sz;
	int p_sz;
	int f_max;
	int p_max;
	FRAME **mcebuf;
	short *duration; /* since variants may be required  */
	short *pitch;
} ACOUSTIC;


extern KEY fw[];
extern KEY son[];
extern KEY dur0[];

/* now definitions of global data  */

/* awb -- deleted */
/* extern ENTRY indx[NDIPHS]; */
/* extern FRAME dico[NFRAMES]; */
extern int nindex;
extern char *dbName;

/* program prototypes  */

/* audio.c  */
void audio_open(CONFIG *config);
void audio_play(short *start,int sz,int number,CONFIG *config);
void audio_close(CONFIG *config);
void audio_flush(CONFIG *config);

/* makewave.c  */
void makewave(CONFIG *config, ACOUSTIC *as);

/* coeffs.c  */
void rfctolpc(float *buf);

/* conv.c  */
void conv(CONFIG *config, LING_LIST *ling_list, SPROSOD_LIST *spl);
void spl_cpy(int index,int syll, char *phon, int dur, char *type, float strength, SPROSOD_LIST *spl);
void spl_cat(int index,char *type, float strength, SPROSOD_LIST *spl);
int vowel(char *ph) ;

/* durations.c  */
void durations(SPN *ps, ACOUSTIC *as);

/* excitation.c  */
float iexc(short voiced, ACOUSTIC *as, short *wkspace);

/* go.c  */
void go(CONFIG *config, BUFFER *buffer, LING_LIST *ling_list, SPROSOD_LIST *spl, SPN *ps, ACOUSTIC *as);

/* grammar.c  */
void grammar(LING_LIST *ling_list);

/* interface.c  */
char *nrl_rules(char *in);

/* load_diphs.c  */
int load_speech(CONFIG *config);
int lookup(char *diph);
void phonstoframes(SPN *ps, ACOUSTIC *as);

/* nrl_edin.c  */
void nrl_edin_conv(char *str, char *str2);

/* pitch.c  */
void calc_pitch(SPN *ps, ACOUSTIC *as);

/* prosody.c  */
void prosody(SPROSOD_LIST *spl, SPN *ps);

/* space.c  */
void init(CONFIG *config, BUFFER *buffer, LING_LIST *ling_list, SPROSOD_LIST *spl, SPN *ps, ACOUSTIC *as);
void terminate(CONFIG *config, BUFFER *buffer, LING_LIST *ling_list, SPROSOD_LIST *spl, SPN *ps, ACOUSTIC *as);
void buffer_malloc(int num,BUFFER *buffer);
void buffer_realloc(int num, BUFFER *buffer);
void buffer_free(BUFFER *buffer);
void ling_list_malloc(int num, LING_LIST *ling_list);
void ling_list_realloc(int num, LING_LIST *ling_list);
void ling_list_free(LING_LIST *ling_list);
void spl_malloc(int num, SPROSOD_LIST *spl);
void spl_realloc(int num, SPROSOD_LIST *spl);
void spl_free(SPROSOD_LIST *spl);
void ps_malloc(int nphons, int ntargs, SPN *ps);
void ps_realloc(int nphons, int ntargs, SPN *ps);
void ps_free(SPN *ps);
void as_malloc(int nframes, int npp, ACOUSTIC *as);
void as_realloc(int nframes, int npp, ACOUSTIC *as);
void as_free(ACOUSTIC *as);

/* syllab.c  */
char *syllabify(char *string, CONFIG *config);
char *stress(char *input);

/* t2s.c  */
void process_sentence(CONFIG *config, BUFFER *buffer, LING_LIST *ling_list, SPROSOD_LIST *spl, SPN *ps, ACOUSTIC *as);

/* tags.c  */
void tags(CONFIG *config, BUFFER *buffer, LING_LIST *ling_list);

/* transcribe.c  */
void transcribe(CONFIG *config, LING_LIST *ling_list);

/* ulaw.c  */
unsigned char linear2ulaw(int sample);
int ulaw2linear(unsigned char ulawbyte);

/* utils.c  */
char **split(char *in);
void tidy_split(char **root);
KEY *binary(char *word, KEY tab[], int n);

/* library prototypes 
int fprintf(FILE *stream, char *format, ...  );
int printf(const char *format, ...  );
int getopt(int argc,char **argv, char *optstring);
int sscanf(char *s,char * format, ... );
int fread (char *ptr, int size, int nitems, FILE *stream);
int fwrite (char *ptr, int size, int nitems, FILE *stream);
int fclose(FILE *stream);
*/


#endif /* _T2S_H_ */
