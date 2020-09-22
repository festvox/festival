;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;-*-mode:scheme-*-
;;                                                                       ;;
;;                Centre for Speech Technology Research                  ;;
;;                     University of Edinburgh, UK                       ;;
;;                       Copyright (c) 1996,1997                         ;;
;;                        All Rights Reserved.                           ;;
;;                                                                       ;;
;;  Permission is hereby granted, free of charge, to use and distribute  ;;
;;  this software and its documentation without restriction, including   ;;
;;  without limitation the rights to use, copy, modify, merge, publish,  ;;
;;  distribute, sublicense, and/or sell copies of this work, and to      ;;
;;  permit persons to whom this work is furnished to do so, subject to   ;;
;;  the following conditions:                                            ;;
;;   1. The code must retain the above copyright notice, this list of    ;;
;;      conditions and the following disclaimer.                         ;;
;;   2. Any modifications must be clearly marked as such.                ;;
;;   3. Original authors' names are not deleted.                         ;;
;;   4. The authors' names are not used to endorse or promote products   ;;
;;      derived from this software without specific prior written        ;;
;;      permission.                                                      ;;
;;                                                                       ;;
;;  THE UNIVERSITY OF EDINBURGH AND THE CONTRIBUTORS TO THIS WORK        ;;
;;  DISCLAIM ALL WARRANTIES WITH REGARD TO THIS SOFTWARE, INCLUDING      ;;
;;  ALL IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS, IN NO EVENT   ;;
;;  SHALL THE UNIVERSITY OF EDINBURGH NOR THE CONTRIBUTORS BE LIABLE     ;;
;;  FOR ANY SPECIAL, INDIRECT OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES    ;;
;;  WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN   ;;
;;  AN ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION,          ;;
;;  ARISING OUT OF OR IN CONNECTION WITH THE USE OR PERFORMANCE OF       ;;
;;  THIS SOFTWARE.                                                       ;;
;;                                                                       ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;           Author:  Alan W Black
;;;           Date:    November 1997
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;  Text to a single waveform like festival_client but without
;;;  starting hte server
;;;

;;; Because this is a --script type file I has to explicitly
;;; load the initfiles: init.scm and user's .festivalrc
(load (path-append libdir "init.scm"))

;;; Process command line arguments
(define (text2wave_help)
  (format t "%s\n"
  "text2wave [options] textfile
  Convert a textfile to a waveform
  Options
  -mode <string>  Explicit tts mode.
  -o ofile        File to save waveform (default is stdout).
  -otype <string> Output waveform type: alaw, ulaw, snd, aiff, riff, nist etc.
                  (default is riff)
  -F <int>        Output frequency.
  -scale <float>  Volume factor
  -eval <string>  File or lisp s-expression to be evaluated before
                  synthesis.
")
  (quit))

;;; No gc messages
(gc-status nil)

;;; Default argument values
(defvar fp nil)
(defvar totalnumsamples 0)
(defvar outfile "-")
(defvar output_type 'riff)
(defvar frequency nil)  ;; default is no frequency modification
(defvar text_files '("-"))
(defvar mode nil)
(defvar volume "1.0")
(defvar wavefiles nil)
(defvar an_utt nil)

;;; Get options
(define (get_options)

  (let ((files nil)
	(o argv))
    (if (or (member_string "-h" argv)
	    (member_string "-help" argv)
	    (member_string "--help" argv)
	    (member_string "-?" argv))
	(text2wave_help))
    (while o
      (begin
	(cond
	 ((string-equal "-o" (car o))
	  (if (not (cdr o))
	      (text2wave_error "no output file specified"))
	  (set! outfile (car (cdr o)))
	  (set! o (cdr o)))
	 ((string-equal "-otype" (car o))
	  (if (not (cdr o))
	      (text2wave_error "no output filetype specified"))
	  (set! output_type (car (cdr o)))
	  (set! o (cdr o)))
	 ((or (string-equal "-f" (car o)) ;; for compatibility and memory loss
	      (string-equal "-F" (car o)))
	  (if (not (cdr o))
	      (text2wave_error "no frequency specified"))
	  (set! frequency (car (cdr o)))
	  (set! o (cdr o)))
	 ((string-equal "-scale" (car o))
	  (if (not (cdr o))
	      (text2wave_error "no scale specified"))
	  (set! volume (car (cdr o)))
	  (set! o (cdr o)))
	 ((string-equal "-mode" (car o))
	  (if (not (cdr o))
	      (text2wave_error "no mode specified"))
	  (set! mode (car (cdr o)))
	  (set! o (cdr o)))
	 ((string-equal "-eval" (car o))
	  (if (not (cdr o))
	      (text2wave_error "no file specified to load"))
	  (if (string-matches (car (cdr o)) "^(.*")
	      (eval (read-from-string (car (cdr o))))
	      (load (car (cdr o))))
	  (set! o (cdr o)))
	 (t
	  (set! files (cons (car o) files))))
	(set! o (cdr o))))
    (if files
	(set! text_files (reverse files)))))

(define (text2wave_error message)
  (format stderr "%s: %s\n" "text2wave" message)
  (text2wave_help))


(define (save_record_wave_fp utt)
"Saves the waveform and records its so it can be joined into a 
a single waveform at the end."
    (if frequency
	   (utt.wave.resample utt (parse-number frequency))
  )
  (if (eq? totalnumsamples 0)
        (wave.save.header fp (utt.wave utt) output_type nil
                (list (list "numsamples" 0)))
  )
  (set! totalnumsamples (+ totalnumsamples
                           (get_param 'num_samples (wave.info (utt.wave utt)) 0)
                        )
  )
    (if (not (equal? volume "1.0"))
	(begin
	    (utt.wave.rescale utt (parse-number volume))
    )
  )
  (wave.save.data.fp (utt.wave utt) fp output_type nil)
  (set! an_utt utt)
)

;;;
;;; Redefine what happens to utterances during text to speech 
;;;
(set! tts_hooks (list utt.synth save_record_wave_fp))

(define (main)
  (get_options)

  (set! fp (fopen outfile "wb"))

  ;; do the synthesis
  (mapcar
   (lambda (f) 
     (if mode
	 (tts_file f mode)
	 (tts_file f (tts_find_text_mode f auto-text-mode-alist))))
   text_files)

  ;; Now update the header
  (fseek fp 0 0)
  (wave.save.header fp (utt.wave an_utt) output_type nil
                (list (list "numsamples" totalnumsamples))
  )
  (fclose fp)
)

;;;  Do the work
(main)
