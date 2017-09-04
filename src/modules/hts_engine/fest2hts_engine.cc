/* ----------------------------------------------------------------- */
/*           The HMM-Based Speech Synthesis Module for Festival      */
/*           "festopt_hts_engine" developed by HTS Working Group     */
/*           http://hts-engine.sourceforge.net/                      */
/* ----------------------------------------------------------------- */
/*                                                                   */
/*  Copyright (c) 2001-2013  Nagoya Institute of Technology          */
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

/* standard C libraries */
#include <cstdio>
#include <cstdlib>
#include <cstring>
#include <cctype>
#include "festival.h"

/* header of hts_engine API */
#include "HTS_engine.h"
#include "HTS_hidden.h"

/* Getfp: wrapper for fopen */
static FILE *Getfp(const char *name, const char *opt) {
  FILE *fp = fopen(name, opt);

  if (fp == NULL) {
    cerr << "Getfp: Cannot open " << name << endl;
    festival_error();
  }

  return (fp);
}

/* HTS_Synthesize_Utt: generate speech from utt by using hts_engine API */
static LISP HTS_Synthesize_Utt(LISP utt) {
  EST_Utterance *u = get_c_utt(utt);
  EST_Item *item = 0;
  LISP hts_engine_params = NIL;
  LISP hts_output_params = NIL;

  char **fn_voices = NULL;
  char *fn_htsvoice = NULL;

  HTS_Engine engine;

  FILE *labfp = NULL;
  FILE *rawfp = NULL, *durfp = NULL;

  int sampling_rate;
  double speech_speed;

  /* get params */
  hts_engine_params =
      siod_get_lval("hts_engine_params",
                    "festopt_hts_engine: no parameters set for module");
  hts_output_params =
      siod_get_lval("hts_output_params",
                    "festopt_hts_engine: no output parameters set for module");

  /* get model file names */
  fn_voices = (char **) malloc(sizeof(char *));
  fn_htsvoice = (char *) get_param_str("-m",
                                       hts_engine_params,
                                       "hts/htsvoice");
  fn_voices[0] = fn_htsvoice;

  /* get speech speed */
  speech_speed = (double) get_param_float("-r", hts_engine_params, 1.0);

  /* open input file pointers */
  labfp =
      Getfp(get_param_str("-labelfile", hts_output_params, "utt.feats"), "r");

  /* open output file pointers */
  rawfp = Getfp(get_param_str("-or", hts_output_params, "tmp.raw"), "wb");
  durfp = Getfp(get_param_str("-od", hts_output_params, "tmp.lab"), "wb");

  /* initialize */
  HTS_Engine_initialize(&engine);

  /* load models */
  HTS_Engine_load(&engine, fn_voices, 1);
  free(fn_voices);

  /* turn off audio output */
  HTS_Engine_set_audio_buff_size(&engine, 0);

  /* get sampling frequency */
  sampling_rate = HTS_Engine_get_sampling_frequency(&engine);

  /* set speech speed */
  HTS_Engine_set_speed(&engine, speech_speed);

  /* generate speech */
  if (u->relation("Segment")->first()) {       /* only if there segments */
    HTS_Engine_synthesize_from_fn(&engine,
                                  get_param_str("-labelfile",
                                                hts_output_params,
                                                "utt.feats"));
    if (rawfp != NULL)
      HTS_Engine_save_generated_speech(&engine, rawfp);
    if (durfp != NULL)
      HTS_Engine_save_label(&engine, durfp);
    HTS_Engine_refresh(&engine);
  }

  /* free */
  HTS_Engine_clear(&engine);

  /* close output file pointers */
  if (rawfp != NULL)
    fclose(rawfp);
  if (durfp != NULL)
    fclose(durfp);

  /* close input file pointers */
  if (labfp != NULL)
    fclose(labfp);

  /* Load back in the waveform */
  EST_Wave *w = new EST_Wave;
  w->resample(sampling_rate);

  if (u->relation("Segment")->first()) /* only if there segments */
    w->load_file(get_param_str("-or", hts_output_params, "tmp.raw"), "raw",
                 sampling_rate, "short", str_to_bo("native"), 1);

  item = u->create_relation("Wave")->append();
  item->set_val("wave", est_val(w));

  /* Load back in the segment times */
  EST_Relation *r = new EST_Relation;
  EST_Item *s, *o;

  r->load(get_param_str("-od", hts_output_params, "tmp.lab"), "htk");

  for (o = r->first(), s = u->relation("Segment")->first();
       (o != NULL) && (s != NULL); o = inext(o), s = inext(s))
    if (o->S("name").before("+").after("-").matches(s->S("name")))
      s->set("end", o->F("end"));
    else
      cerr << "HTS_Synthesize_Utt: Output segment mismatch";

  delete r;

  return utt;
}

#define HTS_VERSION   "1.07"
#define HTS_URL       "http://hts-engine.sourceforge.net/"
#define HTS_NCOPYRIGHT 1

/* HTS_get_copyright: write copyright to string */
void HTS_get_copyright(char *str) {
  int i, nCopyright = HTS_NCOPYRIGHT;
  char url[] = HTS_URL, version[] = HTS_VERSION;
  const char *copyright[] = { HTS_COPYRIGHT };

  sprintf(str,
           "\nThe HMM-Based Speech Synthesis Engine \"hts_engine API\"\n");

  sprintf(str,
           "%shts_engine API version %s (%s)\n", str, version, url);
  for (i = 0; i < nCopyright; i++) {
    if (i == 0)
      sprintf(str,
               "%sCopyright (C) %s\n", str, copyright[i]);
    else
      sprintf(str,
               "%s              %s\n", str, copyright[i]);
  }
  sprintf(str, "%sAll rights reserved.\n", str);

  return;
}


void festival_hts_engine_init(void) {
  char buf[4096];

  HTS_get_copyright(buf);
  proclaim_module("hts_engine", buf);

  festival_def_utt_module("HTS_Synthesize", HTS_Synthesize_Utt,
                          "(HTS_Synthesis UTT)\n  Synthesize a waveform using the hts_engine and the current models");
}
