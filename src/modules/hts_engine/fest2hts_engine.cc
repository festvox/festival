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
#include <sstream>

/* header of hts_engine API */
#include "HTS_engine.h"
#include "HTS_hidden.h"

using namespace std;

/* Getfp: wrapper for fopen */
static FILE *Getfp(const char *name, const char *opt) {
  if (name == NULL) {
    return NULL;
  }
  FILE *fp = fopen(name, opt);

  if (fp == NULL) {
    cerr << "Getfp: Cannot open " << name << endl;
    festival_error();
  }

  return (fp);
}

static HTS_Engine *engine = NULL;
static const char *cached_voice = NULL;

/* HTS_Engine_save_label_ostream: save label with time */
void HTS_Engine_save_label_ostream(HTS_Engine * engine, std::ostream &os)
{
   size_t i, j;
   size_t frame, state, duration;

   HTS_Label *label = &engine->label;
   HTS_SStreamSet *sss = &engine->sss;
   size_t nstate = HTS_ModelSet_get_nstate(&engine->ms);
   double rate = engine->condition.fperiod * 1.0e+07 / engine->condition.sampling_frequency;

   for (i = 0, state = 0, frame = 0; i < HTS_Label_get_size(label); i++) {
      for (j = 0, duration = 0; j < nstate; j++)
         duration += HTS_SStreamSet_get_duration(sss, state++);
      os << (unsigned long) (frame * rate) << " "
         << (unsigned long) ((frame + duration) * rate) << " "
         << HTS_Label_get_string(label, i) << endl;
      frame += duration;
   }
}

/* HTS_Engine_copy_wave: Copy generated wave to a short int* array */
static short int * HTS_Engine_copy_wave(HTS_Engine * engine)
{
   double x;
   short int xs;
   size_t i;
   short int *rawwave = new short[HTS_Engine_get_nsamples(engine)];
   for (i=0;i<HTS_Engine_get_nsamples(engine);++i) {
     x = HTS_Engine_get_generated_speech(engine,i);
     if (x > 32767.0)
       xs = 32767;
     else if (x < -32768.0)
       xs = -32768;
     else
       xs = (short) x;
     rawwave[i] = xs;
   }
   return rawwave;
}

/* HTS_Synthesize_Utt: generate speech from utt by using hts_engine API */
static LISP HTS_Synthesize_Utt(LISP utt) {
  EST_Utterance *u = get_c_utt(utt);
  EST_Item *item = 0;
  LISP hts_engine_params = NIL;
  LISP hts_output_params = NIL;

  char *fn_htsvoice = NULL;

  LISP label_string_list = NIL;
  const char **label_string_array = NULL;
  size_t i, numlabels = 0;

  char *labfn = NULL;
  FILE *rawfp = NULL, *durfp = NULL;

  int sampling_rate;
  double speech_speed;

  const char* current_voice = NULL;

  /* get current voice name */
  current_voice = get_c_string(siod_get_lval("current-voice", NULL));
   
  /* get params */
  hts_engine_params =
      siod_get_lval("hts_engine_params",
                    "festopt_hts_engine: no parameters set for module");
  hts_output_params =
      siod_get_lval("hts_output_params",
                    "festopt_hts_engine: no output parameters set for module");

  /* get model file names */
  fn_htsvoice = (char *) get_param_str("-m",
                                       hts_engine_params,
                                       "hts/htsvoice");

  /* get speech speed */
  speech_speed = (double) get_param_float("-r", hts_engine_params, 1.0);

  /* open input file pointers */
  labfn =  (char *) get_param_str("-labelfile", hts_output_params, NULL);
  /* get input label as string */
  label_string_list = 
       (LISP) get_param_lisp("-labelstring", hts_output_params, NULL);
  if (label_string_list != NULL) {
    numlabels = siod_llength(label_string_list);
    label_string_array = new const char*[numlabels];
    LISP b;
    for (i=0, b=label_string_list; b != NIL; b=cdr(b),i++)
      label_string_array[i] = get_c_string(car(b));
  }
  /* open output file pointers */
  rawfp = Getfp(get_param_str("-or", hts_output_params, NULL), "wb");
  durfp = Getfp(get_param_str("-od", hts_output_params, NULL), "wb");

   std::stringstream labelstream(std::stringstream::in|std::stringstream::out);
   
  /* initialize */
  /* If voice name has not changed, keep cached parameters and models */
  if ( cached_voice != NULL && current_voice != NULL && \
    strcmp(cached_voice, current_voice)==0 ) {
    HTS_Engine_refresh(engine);
  } else {
    if (cached_voice != NULL)
      HTS_Engine_clear(engine);
    HTS_Engine_initialize(engine);

    /* load models */
    HTS_Engine_load(engine, &fn_htsvoice, 1);
    cached_voice = current_voice;
  }

  /* turn off audio output */
  HTS_Engine_set_audio_buff_size(engine, 0);

  /* get sampling frequency */
  sampling_rate = HTS_Engine_get_sampling_frequency(engine);

  /* set speech speed */
  HTS_Engine_set_speed(engine, speech_speed);

  /* generate speech */
  if (u->relation("Segment")->first()) {       /* only if there are segments */
    /* Load label from file pointer or from string */
    if ( labfn != NULL ) {
      HTS_Engine_synthesize_from_fn(engine, labfn);
    } else if ( label_string_array != NULL ) {
      HTS_Engine_synthesize_from_strings(engine, label_string_array, numlabels);
    } else {
      cerr << "No input label specified" << endl;
      HTS_Engine_refresh(engine);
      if (rawfp != NULL)
         fclose(rawfp);
      if (durfp != NULL)
         fclose(durfp);
      return utt;
    }
    if (rawfp != NULL)
      HTS_Engine_save_generated_speech(engine, rawfp);
    if (durfp != NULL)
      HTS_Engine_save_label(engine, durfp);
    HTS_Engine_save_label_ostream(engine, labelstream);
  }

  /* free (keep models in cache) */
  /* HTS_Engine_clear(engine); */

  /* close output file pointers */
  if (rawfp != NULL)
    fclose(rawfp);
  if (durfp != NULL)
    fclose(durfp);

  /* Load back in the waveform */
  const int numsamples = HTS_Engine_get_nsamples(engine);
  short int * rawwave = HTS_Engine_copy_wave(engine);
  EST_Wave *w;
  if (u->relation("Segment")->first()) /* only if there segments */
  {
    w = new EST_Wave(numsamples, 1 /* one channel */,
                     rawwave, 0, sampling_rate, 1 /* deallocate when destroyed */);
  } else {
    w = new EST_Wave;
    w->resample(sampling_rate);
  }

  item = u->create_relation("Wave")->append();
  item->set_val("wave", est_val(w));

  /* Load back in the segment times */
  EST_TokenStream ts_label;
  ts_label.open(labelstream);
  EST_Relation *r = new EST_Relation;
  EST_Item *s, *o;
  ts_label.seek(0);
  r->load("tmp.lab", ts_label, "htk");

  for (o = r->first(), s = u->relation("Segment")->first();
       (o != NULL) && (s != NULL); o = inext(o), s = inext(s))
    if (o->S("name").before("+").after("-").matches(s->S("name")))
      s->set("end", o->F("end"));
    else
      cerr << "HTS_Synthesize_Utt: Output segment mismatch" << endl;
  delete r;
  HTS_Engine_refresh(engine);
  ts_label.close();
  delete[] label_string_array;
  return utt;
}

#define HTS_VERSION   "1.07"
#define HTS_URL       "http://hts-engine.sourceforge.net/"
#define HTS_NCOPYRIGHT 1

/* HTS_get_copyright: write copyright to string */
void HTS_get_copyright(char *str) {
  sprintf(str,
           "\nThe HMM-Based Speech Synthesis Engine \"hts_engine API\"\n"
           "hts_engine API version %s (%s)\n"
           "Copyright (C) %s\n"
           "All rights reserved.\n", HTS_VERSION, HTS_URL, HTS_COPYRIGHT);

  return;
}


void festival_hts_engine_init(void) {
  char buf[4096];
  engine = new HTS_Engine;
  HTS_get_copyright(buf);
  proclaim_module("hts_engine", buf);

   festival_def_utt_module("HTS_Synthesize", HTS_Synthesize_Utt,
                           "(HTS_Synthesis UTT)\n  Synthesize a waveform using the hts_engine and the current models");
}
