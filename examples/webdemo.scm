;;;
;;;  Sentences presynthesized on demo web page
;;;

(set! utt1 
(Utterance Text
"
This is a short introduction to the Festival Speech Synthesis System.
Festival was developed by Alan Black and Paul Taylor, at the Centre
for Speech Technology Research, University of Edinburgh.
"))

(set! utt2
(Utterance Text
"
Festival currently uses a diphone synthesizer, both 
residual excited LPC and PSOLA methods are supported.
The upper levels, duration and intonation, are generated from
statistically trained models, built from databases of natural speech.
The architecture of the system is designed to be flexible, including
various tools, which allow new modules to be added easily.
"))

(define (make_waves)
"Synthesize the two examples and save them in the desired formats"
 (Synth utt1)
 (Parameter.set 'Wavefiletype 'riff)
 (utt.save.wave utt1 "intro.wav")
 (Parameter.set 'Wavefiletype 'ulaw)
 (utt.save.wave utt1 "intro.au")

 (Synth utt2)
 (Parameter.set 'Wavefiletype 'riff)
 (utt.save.wave utt2 "intro2.wav")
 (Parameter.set 'Wavefiletype 'ulaw)
 (utt.save.wave utt2 "intro2.au")
)

(set! welsh1
(Utterance Text 
"Dwi'n gallu llefaru pob llinell heb atal, oherwydd does dim tafod gyda fi."))

(define (make_welsh)
 (voice_welsh_hl)
 (Synth welsh1)
 (Parameter.set 'Wavefiletype 'riff)
 (utt.save.wave welsh1 "welsh1.wav")
 (Parameter.set 'Wavefiletype 'ulaw)
 (utt.save.wave welsh1 "welsh1.au"))

(set! spanish1
(Utterance Text
"m'uchos 'a~nos despu'es, fr'ente al pelot'on de fusilami'ento, el
coron'el aureli'ano buend'ia hab'ia de record'ar de aqu'el d'ia
lej'ano, en que su p'adre lo llev'o a conoc'er el hi'elo."))

(define (make_spanish)
 (voice_spanish_el)
 (Synth spanish1)
 (Parameter.set 'Wavefiletype 'riff)
 (utt.save.wave spanish1 "spanish1.wav")
 (Parameter.set 'Wavefiletype 'ulaw)
 (utt.save.wave spanish1 "spanish1.au"))


(set! utt_pos (Utterance Text 
"My cat who lives dangerously had nine lives. "))

(set! utt_Bdi (Utterance Text 
"He wanted to go for a drive in."))
(set! utt_Bditc (Utterance Text 
"He wanted to go for a drive in the country."))

(define (make_others)
 (Synth utt_pos)
 (Synth utt_Bdi)
 (Synth utt_Bditc)
 (Parameter.set 'Wavefiletype 'riff)
 (utt.save.wave utt_pos "cat.wav")
 (utt.save.wave utt_Bdi "Bdi.wav")
 (utt.save.wave utt_Bditc "Bditc.wav")
 (Parameter.set 'Wavefiletype 'ulaw)
 (utt.save.wave utt_pos "cat.au")
 (utt.save.wave utt_Bdi "Bdi.au")
 (utt.save.wave utt_Bditc "Bditc.au"))

(set! utt_diph (Utterance Text
"This is a short introduction to the Festival Speech Synthesis System."))
(set! utt_sucs (Utterance Text
"This is a short introduction to the Festival Speech Synthesis System."))

(define (make_diphsbs)
 (Synth utt_diph)
 (Parameter.set 'Wavefiletype 'riff)
 (utt.save.wave utt_diph "diph1.wav")
 (Parameter.set 'Wavefiletype 'ulaw)
 (utt.save.wave utt_diph "diph1.au")
 (voice_gsw_450)
 (Synth utt_sucs)
 (Parameter.set 'Wavefiletype 'riff)
 (utt.save.wave utt_sucs "sbs1.wav")
 (Parameter.set 'Wavefiletype 'ulaw)
 (utt.save.wave utt_sucs "sbs1.au"))
