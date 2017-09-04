/************************************************************************************/
/* Copyright (c) 2012 The Department of Arts and Culture,                           */
/* The Government of the Republic of South Africa.                                  */
/*                                                                                  */
/* Contributors:  Meraka Institute, CSIR, South Africa.                             */
/*                                                                                  */
/* Permission is hereby granted, free of charge, to any person obtaining a copy     */
/* of this software and associated documentation files (the "Software"), to deal    */
/* in the Software without restriction, including without limitation the rights     */
/* to use, copy, modify, merge, publish, distribute, sublicense, and/or sell        */
/* copies of the Software, and to permit persons to whom the Software is            */
/* furnished to do so, subject to the following conditions:                         */
/* The above copyright notice and this permission notice shall be included in       */
/* all copies or substantial portions of the Software.                              */
/*                                                                                  */
/* THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR       */
/* IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,         */
/* FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE      */
/* AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER           */
/* LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,    */
/* OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN        */
/* THE SOFTWARE.                                                                    */
/*                                                                                  */
/************************************************************************************/
/*                                                                                  */
/* AUTHOR  : Aby Louw                                                               */
/* DATE    : 14 May 2012                                                            */
/*                                                                                  */
/************************************************************************************/
/*                                                                                  */
/* Added mixed excitation, see code at end of file.                                 */
/*                                                                                  */
/************************************************************************************/


#ifndef HTS_VOCODER_ME_H
#define HTS_VOCODER_ME_H

#ifdef __cplusplus
#define HTS_VOCODER_ME_H_START extern "C" {
#define HTS_VOCODER_ME_H_END   }
#else
#define HTS_VOCODER_ME_H_START
#define HTS_VOCODER_ME_H_END
#endif                          /* __CPLUSPLUS */

HTS_VOCODER_ME_H_START;

/* hts_engine libraries */
#include <HTS_hidden.h>

/* HTS_Vocoder_ME: structure for setting of vocoder (mixed excitation) */
typedef struct _HTS_Vocoder_ME {
  HTS_Vocoder *v;                /* hts engine vocoder structure */

  /* mixed excitation */
  double  *xp_sig;         /* pulse signal, the size of this should be the filter order */
  double  *xn_sig;         /* noise signal, the size of this should be the filter order */
  double  *hp;             /* pulse shaping filter, size of the filter order  
                            */
  double  *hn;             /* noise shaping filter, size of the filter order      
                            */
  int      num_filters;    /* number of filters                               
                            */
  int      filter_order;   /* filter order                                    
                            */
  double **h;              /* filter coefficients                             
                            */
} HTS_Vocoder_ME;

/* HTS_Vocoder_initialize_me: initialize vocoder (mixed excitation) */
void HTS_Vocoder_initialize_me(HTS_Vocoder_ME * v_me, const int m,
                               const int stage,
                               HTS_Boolean use_log_gain,
                               const int rate,
                               const int fperiod,
                               int num_filters, int filter_order,
                               double **h,
                               double *xp_sig, double *xn_sig,
                               double *hp, double *hn);

/* HTS_Vocoder_synthesize_me: mixed excitation and MLSA/MGLSA filster based waveform   synthesis */
void HTS_Vocoder_synthesize_me(HTS_Vocoder_ME * v_me, const int m,
                               double lf0,
                               double *spectrum,
                               double *strengths,
                               size_t nlpf, double* lpf,
                               double alpha,
                               double beta, double volume,
                               double *rawdata, HTS_Audio * audio);

/* HTS_Vocoder_clear_me: clear vocoder (mixed excitation) */
void HTS_Vocoder_clear_me(HTS_Vocoder_ME * v_me);


HTS_VOCODER_ME_H_END;

#endif                          /* !HTS_VOCODER_ME_H */
