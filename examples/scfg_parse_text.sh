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
;;;           Date:    October 1997
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;  Parse arbitrary text using the given SCFG.  
;;;
;;;  Tokenizes given text file and runs the part of speech tagger on it
;;;  Parses it with respect to a grammar trained from the UPenn WSJ
;;;  tree bank (you may optionall specify a different grammar).
;;;
;;;  This may be slow for long sentences as there is |w|^3 factor
;;;  involved in parsing algorithm.

;;; Because this is a --script type file I has to explicitly
;;; load the initfiles: init.scm and user's .festivalrc
(load (path-append libdir "init.scm"))

(require 'scfg)

;;; Process command line arguments
(define (scfg_parse_text_help)
  (format t "%s\n"
  "scfg_parse_text [options] textfile
  Parse arbitrary text 
  Options
  -o ofile        File to save parses (default is stdout).
  -grammar ifile  Alternative grammar, default uses standard grammar
                  from Festival distribution.
  -full_parse     Output full parse with probabilities rather than
                  simplified form (which is the default).")
  (quit))

;;; No gc messages
(gc-status nil)

;;; Default argument values
(defvar grammarfile (path-append libdir "scfg_wsj_wp20.gram"))
(defvar outfile "-")
(defvar outfd t)
(defvar parse_type 'brackets_only)
(defvar text_files '("-"))

;;; Get options
(define (get_options)

  (let ((files nil)
	(o argv))
    (if (or (member_string "-h" argv)
	    (member_string "-help" argv)
	    (member_string "--help" argv)
	    (member_string "-?" argv))
	(scfg_parse_text_help))
    (while o
      (begin
	(cond
	 ((string-equal "-o" (car o))
	  (if (not (cdr o))
	      (scfg_error "no output file specified"))
	  (set! outfile (car (cdr o)))
	  (set! outfd (fopen outfile "w"))
	  (set! o (cdr o)))
	 ((string-equal "-grammar" (car o))
	  (if (not (cdr o))
	      (scfg_error "no grammar file specified"))
	  (set! grammarfile (car (cdr o)))
	  (set! o (cdr o)))
	 ((string-equal "-full_parse" (car o))
	  (set! parse_type 'full_parse))
	 (t
	  (set! files (cons (car o) files))))
	(set! o (cdr o))))
    (if files
	(set! text_files (reverse files)))))

(define (scfg_error message)
  (format stderr "%s: %s\n" "scfg_parse_text" message)
  (scfg_parse_text_help))

;;;  Functions that do the work
(define (find-parse utt)
"Main function for processing TTS utterances.  Tokenizes, predicts POS and
then parses."
  (Token utt)
  (POS utt)
  (Phrasify utt)  ;; cause it maps the POS tags
  (ProbParse utt)
)

(define (output-parse utt)
"Output the parse tree for each utt"
 (if (equal? parse_type 'brackets_only)
     (pprintf (scfg_simplify_relation_tree
	       (utt.relation_tree utt 'Syntax)) outfd)
     (pprintf (utt.relation_tree utt 'Syntax) outfd))
 (format outfd "\n")
 utt)

;;;
;;; Redefine what happens to utterances during text to speech 
;;;
(set! tts_hooks (list find-parse output-parse))

(define (main)
  (get_options)

  ;; Load the grammar
  (set! scfg_grammar (load grammarfile t))

  ;; Parse the files
  (mapcar
   (lambda (f) (tts_file f))
   text_files))

;;;  Do the work
(main)
