;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;                                                                       ;;
;;;                Centre for Speech Technology Research                  ;;
;;;                     University of Edinburgh, UK                       ;;
;;;                       Copyright (c) 1996,1997                         ;;
;;;                        All Rights Reserved.                           ;;
;;;                                                                       ;;
;;;  Permission is hereby granted, free of charge, to use and distribute  ;;
;;;  this software and its documentation without restriction, including   ;;
;;;  without limitation the rights to use, copy, modify, merge, publish,  ;;
;;;  distribute, sublicense, and/or sell copies of this work, and to      ;;
;;;  permit persons to whom this work is furnished to do so, subject to   ;;
;;;  the following conditions:                                            ;;
;;;   1. The code must retain the above copyright notice, this list of    ;;
;;;      conditions and the following disclaimer.                         ;;
;;;   2. Any modifications must be clearly marked as such.                ;;
;;;   3. Original authors' names are not deleted.                         ;;
;;;   4. The authors' names are not used to endorse or promote products   ;;
;;;      derived from this software without specific prior written        ;;
;;;      permission.                                                      ;;
;;;                                                                       ;;
;;;  THE UNIVERSITY OF EDINBURGH AND THE CONTRIBUTORS TO THIS WORK        ;;
;;;  DISCLAIM ALL WARRANTIES WITH REGARD TO THIS SOFTWARE, INCLUDING      ;;
;;;  ALL IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS, IN NO EVENT   ;;
;;;  SHALL THE UNIVERSITY OF EDINBURGH NOR THE CONTRIBUTORS BE LIABLE     ;;
;;;  FOR ANY SPECIAL, INDIRECT OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES    ;;
;;;  WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN   ;;
;;;  AN ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION,          ;;
;;;  ARISING OUT OF OR IN CONNECTION WITH THE USE OR PERFORMANCE OF       ;;
;;;  THIS SOFTWARE.                                                       ;;
;;;                                                                       ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;  This contains an example of how to use Festival with a 
;;;  a talking head.  This is a combination of various answers given
;;;  to different groups who have been using Festival with talking
;;;  heads
;;;
;;;  This version has not actually been used by any talking head
;;;  but just serves as an example.
;;;
;;;  The basic mode produces /tmp/th.ph (phone info) and /tmp/th.com
;;;  (commands: smile frown) for each utterance in the file.  These
;;;  files are produced and the program makefaces called before
;;;  waveform synthesis for each utterance.  The play command then
;;;  calls xanim with the generate animation and waveform.
;;;
;;;  There are probabaly better way to do this.  Using Festival as a 
;;;  server to generate the phone and command files might
;;;  be more reasonable.  Note festival not supports the returning
;;;  of Lisp data to the client as well as waveform data.
;;;  In that case you'd want to change th_output_info to use
;;;  the send_cleint command and package the phone info into an
;;;  s-expression.

(defvar th-prepare-prog "makefaces"
  " A program that takes phones and other data and produces the 
animated face.")

(define (utt.save.phonedata utt filename)
"(utt.save.mydata UTT FILE)
  Saves phone, duration, stress, F0 word pos."
  (let ((fd (fopen filename "w")))
    (mapcar
     (lambda (seg)
       (format fd "%s %2.4f %s %s" 
	       (item.feat seg "name")
	       (item.feat seg "segment_duration")
	       (item.feat seg "R:SylStructure.parent.stress")
	       (item.feat seg "R:Target.daughter1.name"))
       ;; output word name and part of speech if start of word
       (if (and (not (item.relation.next seg "SylStructure"))
		(not (item.next
		      (item.relation.parent seg "SylStructure"))))
	   (format fd " %s %s"
		   (item.feat seg "R:SylStructure.parent.parent.name")
		   (item.feat seg "R:SylStructure.parent.parent.pos")))
       (format fd "\n"))
     (utt.relation.items utt 'Segment))
    (fclose fd)
    utt))

(define (utt.save.commands utt filename)
"(utt.save.commands UTT FILE)
  Save commands with time stamps.  Commands are those tokens which 
start and end with an asterisk."
  (let ((fd (fopen filename "w")))
    (format fd "#\n")
    (mapcar
     (lambda (tok_item)
       (if (string-matches (item.name tok_item) "\\*.+\\*")
	   (format fd "%2.4f 100 %s\n" 
		   (find_com_time utt tok_item)
		   (item.name tok_item))))
     (utt.relation.items utt 'Token))
    (fclose fd)
    utt))

(define (find_com_time utt tok_item)
"Returns time of tok_item.  Looks backward for first token that
is related to a word and returns the end time of that word."
  (cond
   ((item.daughtern tok_item)
    (item.feat (item.daughtern tok_item) "word_end"))
   ((not (item.prev tok_item))  ;; start of stream
    0.0)
   (t
    (find_com_time utt (item.prev tok_item)))))

(define (th_output_info utt)
  "(th_output_info utt)
This is called after linguistic analysis but before waveform synthesis.
It collects the phone and duration data and also any th commands
found in the utterance.  The file names are then passed to some
external program which will process them for the talking head."
  (set! th-current-file "/tmp/th") ;; this should have a process id in it
  (utt.save.phonedata utt (string-append th-current-file ".ph"))
  (utt.save.commands utt (string-append th-current-file ".com"))
  ;; It would be good to background this process as long as you
  ;; resync at play time
  (system (format nil "%s %s %s"
		  th-prepare-prog
		  (string-append th-current-file ".ph")
		  (string-append th-current-file ".ph")))
  utt)

;;;
;;;  Define a new text mode for talking heads
;;;

(define (th_init_func)
 "Called on starting talking head text mode."
 (set! th_previous_t2w_func token_to_words)
 (set! th_previous_after_analysis_hooks after_analysis_hooks)
 (set! after_analysis_hooks (list th_output_info))
 (set! english_token_to_words th_token_to_words)
 (set! token_to_words th_token_to_words)

 ;; We assume the prepare talking head program generates a movie
 ;; that can be played by something, so we redefie the audio
 ;; player to play the generated animation and waveform.
 (set! th_previous_Parameter Parameter)
 (audio_mode 'sync)  ;; ensure new Audio command gets passed to new audiosp
 (Parameter.set 'Audio_Required_Format 'riff)
 (Parameter.set 'Audio_Command "xanim /tmp/th.anime $FILE")
 (Parameter.set 'Audio_Method 'Audio_Command)
 (audio_mode 'async)
)

(define (th_exit_func)
 "Called on exit talking head text mode."
 (set! token_to_words th_previous_t2w_func)
 (set! english_token_to_words th_previous_t2w_func)
 (set! after_analysis_hooks th_previous_after_analysis_hooks)

 (audio_mode 'sync) ;; so we can reset the audio
 (set! Parameter th_previous_Parameter)
)

(define (th_token_to_words token name)
"(th_token_to_words TOKEN NAME)
Talking head specific token to word rules."
 (cond
  ((string-matches name "\\*.*\\*")
   ;;  Symbols started and ended with an asterisk as treated as commands
   ;;  and not rendered as speech
   nil)
  (t
   (th_previous_t2w_func token name))))

(set! tts_text_modes
   (cons
    (list
      'th           ;; mode name
      (list         ;; ogimarkup mode params
       (list 'init_func th_init_func)
       (list 'exit_func th_exit_func)))
    tts_text_modes))

(provide 'th-mode)
