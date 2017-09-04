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
/* ----------------------------------------------------------------- */
/*           The HMM-Based Speech Synthesis Engine "hts_engine API"  */
/*           developed by HTS Working Group                          */
/*           http://hts-engine.sourceforge.net/                      */
/* ----------------------------------------------------------------- */
/*                                                                   */
/*  Copyright (c) 2001-2012  Nagoya Institute of Technology          */
/*                           Department of Computer Science          */
/*                                                                   */
/*                2001-2008  Tokyo Institute of Technology           */
/*                           Interdisciplinary Graduate School of    */
/*                           Science and Engineering                 */
/*                                                                   */
/* All rights reserved.                                              */
/*                                                                   */
/* Redistribution and use in source and binary forms, with or        */
/* without modification, are permitted provided that the following   */
/* conditions are met:                                               */
/*                                                                   */
/* - Redistributions of source code must retain the above copyright  */
/*   notice, this list of conditions and the following disclaimer.   */
/* - Redistributions in binary form must reproduce the above         */
/*   copyright notice, this list of conditions and the following     */
/*   disclaimer in the documentation and/or other materials provided */
/*   with the distribution.                                          */
/* - Neither the name of the HTS working group nor the names of its  */
/*   contributors may be used to endorse or promote products derived */
/*   from this software without specific prior written permission.   */
/*                                                                   */
/* THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND            */
/* CONTRIBUTORS "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES,       */
/* INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF          */
/* MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE          */
/* DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT OWNER OR CONTRIBUTORS */
/* BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL,          */
/* EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED   */
/* TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE,     */
/* DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON */
/* ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY,   */
/* OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY    */
/* OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE           */
/* POSSIBILITY OF SUCH DAMAGE.                                       */
/* ----------------------------------------------------------------- */
/*                                                                                  */
/* Added mixed excitation, see code at end of file.                                 */
/*
  Alok Parlikar modified this file for use with HTS Engine 1.07
 */
/************************************************************************************/

#ifndef HTS_VOCODER_ME_C
#define HTS_VOCODER_ME_C

#ifdef __cplusplus
#define HTS_VOCODER_ME_C_START extern "C" {
#define HTS_VOCODER_ME_C_END   }
#else
#define HTS_VOCODER_ME_C_START
#define HTS_VOCODER_ME_C_END
#endif                          /* __CPLUSPLUS */

HTS_VOCODER_ME_C_START;

#include "./HTS_vocoder_me.h"
#include "../hts_engine/HTS_vocoder.c"

/* HTS_Vocoder_initialize_me: initialize vocoder (mixed excitation) */
void HTS_Vocoder_initialize_me(HTS_Vocoder_ME * v_me,
                               const int m, const int stage,
                               HTS_Boolean use_log_gain,
                               const int rate,
                               const int fperiod,
                               int num_filters, int filter_order,
                               double **h,
                               double *xp_sig, double *xn_sig,
                               double *hp, double *hn) {
  int i;
  HTS_Vocoder *v = v_me->v; /* access to original HTS_Vocoder structure */

  /* set parameter */
  v->stage = stage;
  if (stage != 0)
    v->gamma = -1.0 / v->stage;
  else
    v->gamma = 0.0;
  v->use_log_gain = use_log_gain;
  v->fprd = fperiod;
  v->next = SEED;
  v->gauss = GAUSS;
  v->rate = rate;
  v->p1 = -1.0;
  v->sw = 0;
  v->x = 0x55555555;
  /* init buffer */
  v->freqt_buff = NULL;
  v->freqt_size = 0;
  v->gc2gc_buff = NULL;
  v->gc2gc_size = 0;
  v->lsp2lpc_buff = NULL;
  v->lsp2lpc_size = 0;
  v->postfilter_buff = NULL;
  v->postfilter_size = 0;
  v->spectrum2en_buff = NULL;
  v->spectrum2en_size = 0;
  if (v->stage == 0) {         /* for MCP */
    v->c = (double *) HTS_calloc(m * (3 + PADEORDER) + 5 * PADEORDER + 6, sizeof(double));
    v->cc = v->c + m + 1;
    v->cinc = v->cc + m + 1;
    v->d1 = v->cinc + m + 1;
  } else {                     /* for LSP */
    v->c = (double *) HTS_calloc((m + 1) * (v->stage + 3), sizeof(double));
    v->cc = v->c + m + 1;
    v->cinc = v->cc + m + 1;
    v->d1 = v->cinc + m + 1;
  }
  /* ABY: think this is not required */
  v->pulse_buff = NULL;
  v->pulse_size = 0;

  /* initialize mixed excitation variables */
  /*---------------------------------------*/

  v_me->num_filters = num_filters;
  v_me->filter_order = filter_order;
  v_me->xp_sig = xp_sig;
  v_me->xn_sig = xn_sig;

  /* initialise xp_sig and xn_sig */
  for (i = 0; i < v_me->filter_order; i++)
  {
    v_me->xp_sig[i] = 0.0;
    v_me->xn_sig[i] = 0.0;
  }

  /* allocate memory for pulse and noise shaping filters */
  v_me->hp = hp;
  v_me->hn = hn;


  /* get filter coefs */
  v_me->h = h;

}


/* HTS_Vocoder_synthesize_me: mixed excitation and MLSA/MGLSA filster
 * based waveform synthesis */
void HTS_Vocoder_synthesize_me(HTS_Vocoder_ME * v_me,
                               const int m, double lf0,
                               double *spectrum, double *strengths,
                               size_t nlpf, double *lpf,
                               double alpha,
                               double beta, double volume,
                               double *rawdata, HTS_Audio * audio)
{
    double x = 0.0;
    int i, j;
    int k;
    short xs;
    int rawidx = 0;
    double p;
    HTS_Vocoder *v = v_me->v; /* access to original HTS_Vocoder struct */
    double xpulse;
    double xnoise;
    double fxpulse;
    double fxnoise;
    double e1, e2;


    /* Copy in str's and build pulse and noise shaping filter for this frame */
    for (i = 0; i < v_me->filter_order; i++)
    {
        v_me->hp[i] = v_me->hn[i] = 0.0;
        for (j = 0; j < v_me->num_filters; j++)
        {
            v_me->hp[i] += strengths[j] * v_me->h[j][i];
            v_me->hn[i] += (1 - strengths[j]) * v_me->h[j][i];
        }
    }

    /* lf0 -> pitch */
    if (lf0 == LZERO)
        p = 0.0;
    else
        p = v->rate / exp(lf0);

    /* first time */
    if (v->p1 < 0.0) {
        HTS_Vocoder_initialize_excitation(v, 0);
        if (v->stage == 0) {      /* for MCP */
            HTS_mc2b(spectrum, v->c, m, alpha);
        } else {                  /* for LSP */
            if (v->use_log_gain)
                v->c[0] = LZERO;
            else
                v->c[0] = ZERO;
            for (i = 1; i <= m; i++)
                v->c[i] = i * PI / (m + 1);
            HTS_lsp2mgc(v, v->c, v->c, m, alpha);
            HTS_mc2b(v->c, v->c, m, alpha);
            HTS_gnorm(v->c, v->c, m, v->gamma);
            for (i = 1; i <= m; i++)
                v->c[i] *= v->gamma;
        }
    }

    HTS_Vocoder_start_excitation(v, p, 0);
    if (v->stage == 0) {         /* for MCP */
        HTS_Vocoder_postfilter_mcp(v, spectrum, m, alpha, beta);
        HTS_mc2b(spectrum, v->cc, m, alpha);
        for (i = 0; i <= m; i++)
            v->cinc[i] = (v->cc[i] - v->c[i]) * IPERIOD / v->fprd;
    } else {                     /* for LSP */
        HTS_Vocoder_postfilter_lsp(v, spectrum, m, alpha, beta);
        HTS_check_lsp_stability(spectrum, m);
        HTS_lsp2mgc(v, spectrum, v->cc, m, alpha);
        HTS_mc2b(v->cc, v->cc, m, alpha);
        HTS_gnorm(v->cc, v->cc, m, v->gamma);
        for (i = 1; i <= m; i++)
            v->cc[i] *= v->gamma;
        for (i = 0; i <= m; i++)
            v->cinc[i] = (v->cc[i] - v->c[i]) * IPERIOD / v->fprd;
    }


    for (j = 0, i = (IPERIOD + 1) / 2; j < v->fprd; j++)
    {
        if (v->stage == 0) {      /* for MCP */
            if (v->p1 == 0.0)
            {
                x = HTS_white_noise(v);

                /* MIXED EXCITATION */
                xnoise = x;
                xpulse = 0.0;
            }
            else
            {
                if ((v->pc += 1.0) >= v->p1)
                {
                    x = sqrt(v->p1);
                    v->pc = v->pc - v->p1;
                }
                else
                {
                    x = 0.0;
                }

                /* MIXED EXCITATION */
                xpulse = x;
                xnoise = HTS_mseq(v);  /* ABY: plus or minus 1 */
            }

            /* MIXED EXCITATION */
            /* The real work -- apply shaping filters to pulse and noise */
            fxpulse = fxnoise = 0.0;
            for (k = v_me->filter_order - 1; k > 0; k--)
            {
                fxpulse += v_me->hp[k] * v_me->xp_sig[k];
                fxnoise += v_me->hn[k] * v_me->xn_sig[k];

                v_me->xp_sig[k] = v_me->xp_sig[k-1];
                v_me->xn_sig[k] = v_me->xn_sig[k-1];
            }

            fxpulse += v_me->hp[0] * xpulse;
            fxnoise += v_me->hn[0] * xnoise;
            v_me->xp_sig[0] = xpulse;
            v_me->xn_sig[0] = xnoise;

            x = fxpulse + fxnoise; /* excitation is pulse plus noise */

            x *= exp(v->c[0]);
            x = HTS_mlsadf(x, v->c, m, alpha, PADEORDER, v->d1);

        } else {                  /* for LSP */
            if (!NGAIN)
                x *= v->c[0];
            x = HTS_mglsadf(x, v->c, m, alpha, v->stage, v->d1);
        }

        x *= volume;

        /* output */
        if (rawdata)
            rawdata[rawidx++] = x;
        if (audio) {
            if (x > 32767.0)
                xs = 32767;
            else if (x < -32768.0)
                xs = -32768;
            else
                xs = (short) x;
            HTS_Audio_write(audio, xs);
        }

        if (!--i) {
            for (i = 0; i <= m; i++)
                v->c[i] += v->cinc[i];
            i = IPERIOD;
        }
    }

    HTS_Vocoder_end_excitation(v, nlpf);
    HTS_movem(v->cc, v->c, m + 1);
}

/* HTS_Vocoder_clear_me: clear vocoder (mixed excitation) */
void HTS_Vocoder_clear_me(HTS_Vocoder_ME * v_me)
{
  HTS_Vocoder *v = v_me->v; /* access to original HTS_Vocoder structure */

  if ((v_me != NULL)
      && (v != NULL))
  {
    /* free buffer */
    if (v->freqt_buff != NULL) {
      HTS_free(v->freqt_buff);
      v->freqt_buff = NULL;
    }
    v->freqt_size = 0;
    if (v->gc2gc_buff != NULL) {
      HTS_free(v->gc2gc_buff);
      v->gc2gc_buff = NULL;
    }
    v->gc2gc_size = 0;
    if (v->lsp2lpc_buff != NULL) {
      HTS_free(v->lsp2lpc_buff);
      v->lsp2lpc_buff = NULL;
    }
    v->lsp2lpc_size = 0;
    if (v->postfilter_buff != NULL) {
      HTS_free(v->postfilter_buff);
      v->postfilter_buff = NULL;
    }
    v->postfilter_size = 0;
    if (v->spectrum2en_buff != NULL) {
      HTS_free(v->spectrum2en_buff);
      v->spectrum2en_buff = NULL;
    }
    v->spectrum2en_size = 0;
    if (v->c != NULL) {
      HTS_free(v->c);
      v->c = NULL;
    }
    v->pulse_size = 0;
    if (v->pulse_buff != NULL){
      HTS_free(v->pulse_buff);
      v->pulse_buff = NULL;
    }

    v_me->num_filters = 0;
    v_me->filter_order = 0;
    v_me->xp_sig = NULL;
    v_me->xn_sig = NULL;
    v_me->hp = NULL;
    v_me->hn = NULL;
    v_me->h = NULL;
  }
}

HTS_VOCODER_ME_C_END;

#endif                          /* !HTS_VOCODER_ME_C */
