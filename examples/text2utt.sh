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

;;; Because this is a --script type file I has to explicitly
;;; load the initfiles: init.scm and user's .festivalrc
(load (path-append libdir "init.scm"))

;;; Process command line arguments
(define (text2utt_help)
  (format t "%s\n"
  "text2utt textfile
  Convert a textfile to a uttrancefile
")
  (quit))

;;; No gc messages
(gc-status nil)

;;; Default argument values
(defvar outfile "-")
(defvar text_files '("-"))

;;; Get options
(define (get_options)

  (let ((files nil)
	(o argv))
    (if (or (member_string "-h" argv)
	    (member_string "-help" argv)
	    (member_string "--help" argv)
	    (member_string "-?" argv))
	(text2utt_help))
    (while o
      (begin
	(cond
	 (t
	  (set! files (cons (car o) files))))
	(set! o (cdr o))))
    (if files
	(set! text_files (reverse files)))))

(define (text2utt_error message)
  (format stderr "%s: %s\n" "text2utt" message)
  (text2utt_help))

(define (save_record_utt utt)
"Saves the utterancefiles"
  (let ((fn (make_tmp_filename)))
    (utt.save utt outfile 'est_ascii)
    utt))

;;;
;;; Redefine what happens to utterances during text to speech 
;;;
(set! tts_hooks (list utt.synth save_record_utt))

(define (main)
  (get_options)

  ;; do the synthesis
  (mapcar
   (lambda (f) 
	 (tts_file f (tts_find_text_mode f auto-text-mode-alist))
   )
   text_files)
)

;;;  Do the work
(main)
