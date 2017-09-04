/*********************************************************************/
/*                                                                   */
/*                  Language Technologies Institute                  */
/*                     Carnegie Mellon University                    */
/*                         Copyright (c) 2013                        */
/*                        All Rights Reserved.                       */
/*                                                                   */
/*  Permission is hereby granted, free of charge, to use and         */
/*  distribute this software and its documentation without           */
/*  restriction, including without limitation the rights to use,     */
/*  copy, modify, merge, publish, distribute, sublicense, and/or     */
/*  sell copies of this work, and to permit persons to whom this     */
/*  work is furnished to do so, subject to the following conditions: */

/*   1. The code must retain the above copyright notice, this list   */
/*      of conditions and the following disclaimer.                  */
/*   2. Any modifications must be clearly marked as such.            */
/*   3. Original authors' names are not deleted.                     */
/*   4. The authors' names are not used to endorse or promote        */
/*      products derived from this software without specific         */
/*      prior written permission.                                    */
/*                                                                   */
/*  CARNEGIE MELLON UNIVERSITY AND THE CONTRIBUTORS TO THIS WORK     */
/*  DISCLAIM ALL WARRANTIES WITH REGARD TO THIS SOFTWARE, INCLUDING  */
/*  ALL IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS, IN        */
/*  NO EVENT SHALL CARNEGIE MELLON UNIVERSITY NOR THE CONTRIBUTORS   */
/*  BE LIABLE FOR ANY SPECIAL, INDIRECT OR CONSEQUENTIAL DAMAGES     */
/*  OR ANY DAMAGES WHATSOEVER RESULTING FROM LOSS OF USE, DATA       */
/*  OR PROFITS, WHETHER IN AN ACTION OF CONTRACT, NEGLIGENCE OR      */
/*  OTHER TORTIOUS ACTION, ARISING OUT OF OR IN CONNECTION WITH      */
/*  THE USE OR PERFORMANCE OF THIS SOFTWARE.                         */
/*                                                                   */
/*********************************************************************/
/*             Author:  Alok Parlikar (aup@cs.cmu.edu)               */
/*               Date:  January 2013                                 */
/*********************************************************************/
/*
  Interface to the MLSA code within HTS Engine API
  (in the src/modules/hts_engine directory)

  Written for HTS Engine API 1.07 (Jan 2013)
*/

#include <sys/types.h>

#include <EST_walloc.h>
#include <festival.h>
#include <HTS_hidden.h>
#include "./vc.h"
#include "./HTS_vocoder_me.h"



DVECTOR synthesis_body(DMATRIX mcep,      // input mel-cep sequence
                       DVECTOR f0v,       // input F0 sequence
                       EST_Track *str,    // str for mixed excitation
                       EST_Track *filtertrack, 
                       double framerate,  // sampling frequency (Hz)
                       int framem,        // FFT length
                       double alpha,
                       double beta);

LISP mlsa_resynthesis(LISP ltrack, LISP strtrack, LISP filtrack) {
  EST_Track *t;
  EST_Track *str = 0;
  EST_Track *filter_track = 0;
  EST_Wave *wave = 0;

  DVECTOR w;
  DMATRIX mcep;
  DVECTOR f0v;

  int framerate = 16000;
  int i, j;
  int shift;
  double ALPHA = 0.42;
  double BETA = 0.0;

  LISP lispframerate = siod_get_lval("framerate",
                                     NULL);
  if (lispframerate == NIL) {
    framerate = 16000;
  } else {
    framerate = FLONM(lispframerate);
  }

  ALPHA = FLONM(siod_get_lval("mlsa_alpha_param",
                              "mlsa: mlsa_alpha_param not set"));
  BETA = FLONM(siod_get_lval("mlsa_beta_param",
                             "mlsa: mlsa_beta_param not set"));

  if ((ltrack == NULL) ||
      (TYPEP(ltrack, tc_string) &&
       (streq(get_c_string(ltrack), "nil"))))
    return siod(new EST_Wave(0, 1, framerate));

  t = track(ltrack);

  if (strtrack != NULL) {
    /* We have to do mixed-excitation */
    str = track(strtrack);
  }

  if (filtrack != NULL) {
    filter_track = track(filtrack);
  }

  f0v = xdvalloc(t->num_frames());
  mcep = xdmalloc(t->num_frames(), t->num_channels()-1);

  for (i = 0; i < t->num_frames(); i++) {
    f0v->data[i] = t->a(i, 0);
    for (j = 1; j < t->num_channels(); j++)
      mcep->data[i][j-1] = t->a(i, j);
  }

  if (t->num_frames() > 1) {
    // Hacky way to get floats doing the right thing
    // Seems to work for 16K and 48KHz -- aup

    shift = (100000.0 * t->t(1)) - (100000.0 * t->t(0));
    shift = shift/10;
    if (shift % 10 > 5)
      shift = ceil(shift/10.0);
    else
      shift = floor(shift/10.0);
  } else {
    shift = 5.0;
  }

  if (shift == 0)
    shift = 5.0;

  w = synthesis_body(mcep, f0v, str, filter_track,
                     framerate, shift, ALPHA, BETA);

  wave = new EST_Wave(w->length, 1, framerate);

  for (i = 0; i < w->length; i++)
    wave->a(i) = (int16_t)w->data[i];  //NOLINT

  xdmfree(mcep);
  xdvfree(f0v);
  xdvfree(w);

  return siod(wave);
}

DVECTOR synthesis_body(DMATRIX mcep,      // input mel-cep sequence
                       DVECTOR f0v,       // input F0 sequence
                       EST_Track *str,    // str for mixed excitation
                       EST_Track *filter_track,  // Track for LPF or
                                                 // Mixed Excitation
                                                 // Filters
                       double framerate,  // sampling frequency (Hz)
                       int framem,     // FFT length
                       double alpha,
                       double beta) {
  int64_t t, pos;
  int framel;
  double f0;
  HTS_Vocoder v;
  HTS_Vocoder_ME v_me;
  DVECTOR xd = NODATA;

  HTS_Boolean use_log_gain = FALSE;

  size_t stage = 0;  // MGC or LSP

  size_t nlpf = 0;
  double *lpf = NULL;

  double volume = 1.0;

  // Mixed Excitation Stuff
  int i, j;
  int me_num_filters = 0;
  int me_filter_order = 0;
  double **me_filter = NULL;
  double *xp_sig = NULL;
  double *xn_sig = NULL;
  double *hp = NULL;
  double *hn = NULL;
  double *strengths;

  if (str == NULL) {
    // Not using mixed excitation, so use the lpf that HTS uses
    // in the SLT voice as of Jan 2013 -- aup

    // Check if LPF filter is to be applied to signal.
    if (filter_track != NULL) {
      if (filter_track->num_frames() != 1) {
        printf("Warning: Wrong filter passed. Ignoring LPF\n");
        printf("Expected single row for pulse-noise-excitation voice");
      } else {
        nlpf = (filter_track->num_channels() - 1)/2;
        lpf = (double*) calloc(filter_track->num_channels(), sizeof(double));
        for (i = 0; i < filter_track->num_channels(); i++) {
          lpf[i] = filter_track->a(0,i);
        }
        //        printf("aup_debug: Using LPF filter of nlpf %d\n", nlpf);
      }
    }
  }

  // floats may not do the right thing, but this seems to work
  // for 16KHz and 48KHz -- aup
  framel = framem * framerate/1000.0;

  if (str == NULL) {
    // Not Mixed Excitation
    HTS_Vocoder_initialize(&v, mcep->col - 1,
                           stage, use_log_gain,
                           framerate, framel);
  } else {
    // Mixed Excitation
    if (filter_track != NULL) {
      me_num_filters = filter_track->num_frames();
      me_filter_order = filter_track->num_channels();
      me_filter = walloc(double*, me_num_filters);

      for (i = 0; i < me_num_filters; i++) {
        me_filter[i] = walloc(double, me_filter_order);
        for (j = 0; j < me_filter_order; j++) {
          me_filter[i][j] = filter_track->a(i, j);
        }
      }
    } else {
      printf("Warning: Attempting to use Mixed Excitation without Filters");
    }

    xp_sig =  (double*) calloc(me_filter_order, sizeof(double));
    xn_sig = (double*) calloc(me_filter_order, sizeof(double));
    hp = (double*) calloc(me_filter_order, sizeof(double));
    hn = (double*) calloc(me_filter_order, sizeof(double));

    v_me.v = &v;
    HTS_Vocoder_initialize_me(&v_me,
                              mcep->col -1,
                              stage, use_log_gain,
                              framerate, framel,
                              me_num_filters,
                              me_filter_order,
                              me_filter,
                              xp_sig, xn_sig, hp, hn);
  }

  // synthesize waveforms by MLSA filter
  xd = xdvalloc(mcep->row * (framel + 2));
  for (t = 0, pos = 0; t < mcep->row; t++) {
    if (t >= f0v->length)
      f0 = LZERO;
    else
      f0 = f0v->data[t];

    if (f0 == 0)
      f0 = LZERO;
    else
      f0 = log(f0);

    if (str == NULL) {
      // Not Mixed Excitation
      //      printf("aup_debug %d %d %d\n", t, pos, framel);
      HTS_Vocoder_synthesize(&v, mcep->col - 1,
                             f0, mcep->data[t],
                             nlpf, lpf,
                             alpha, beta, volume,
                             &xd->data[pos], NULL);
    } else {
      strengths =  (double*) calloc(me_filter_order, sizeof(double));
      for (i = 0; i < me_num_filters; i++) {
        strengths[i] = str->a((int)t, i);
      }
      HTS_Vocoder_synthesize_me(&v_me, mcep->col - 1,
                                f0, mcep->data[t],
                                strengths,
                                nlpf, lpf,
                                alpha, beta, volume,
                                &xd->data[pos], NULL);
      free(strengths);
    }

    pos += framel;
  }

  if (lpf != NULL)
    free(lpf);
  
  if (str != NULL) {
    // Mixed Excitation
    free(xp_sig);
    free(xn_sig);
    free(hp);
    free(hn);
  }

  HTS_Vocoder_clear(&v);

  return xd;
}

