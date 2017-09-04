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
;;;           Date:    December 1997
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;  Dump features from a list of utterances
;;;

;;; Because this is a --script type file it has to explicitly
;;; load the initfiles: init.scm and user's .festivalrc
(load (path-append libdir "init.scm"))

(define (dumpfeats_help)
  (format t "%s\n"
  "Usage: dumpfeats [options] <utt_file_0> <utt_file_1> ...
  Dump features from a set of utterances
  Options
  -relation <string>
             Relation from which the features have to be dumped from
  -output <string>
             If output parameter contains a %s its treated as a skeleton
             e.g feats/%s.feats and multiple files will be created one
             each utterance.  If output doesn't contain %s the output
             is treated as a single file and all features and dumped in it.
  -feats <string>
             If argument starts with a \"(\" it is treated as a list of
             features to dump, otherwise it is treated as a filename whose
             contents contain a set of features (without parenetheses).
  -eval <ifile>
             A scheme file to be loaded before dumping.  This may contain
             dump specific features etc.  If filename starts with a left
             parenthis it it evaluated as lisp.
  -from_file <ifile>
             A file with a list of utterance files names (used when there
             are a very large number of files.
")
  (quit))

;;; Default options values
(defvar utt_files nil)  ;; utterance files to dump from
(defvar desired_relation nil)
(defvar output "-")
(defvar desired_features nil)
(defvar extra-file nil)

;;; Get options
(define (get_options)
  (let ((files nil)
	(o argv))
    (if (or (member_string "-h" argv)
	    (member_string "-help" argv)
	    (member_string "--help" argv)
	    (member_string "-?" argv))
	(dumpfeats_help))
    (while o
      (begin
	(cond
	 ((string-equal "-relation" (car o))
	  (if (not (cdr o))
	      (dumpfeats_error "no stream file specified"))
	  (set! desired_relation (car (cdr o)))
	  (set! o (cdr o)))
	 ((string-equal "-output" (car o))
	  (if (not (cdr o))
	      (dumpfeats_error "no output file/skeleton specified"))
	  (set! output (car (cdr o)))
	  (set! o (cdr o)))
	 ((string-equal "-feats" (car o))
	  (if (not (cdr o))
	      (dumpfeats_error "no feats list/file specified"))
	  (if (string-matches (car (cdr o)) "^(.*")
	      (set! desired_features (read-from-string (car (cdr o))))
	      (set! desired_features (load (car (cdr o)) t)))
	  (set! o (cdr o)))
	 ((string-equal "-from_file" (car o))
	  (if (not (cdr o))
	      (durmeanstd_error "no file of utts names file specified"))
          (set! files 
                (append
                 (reverse (load (car (cdr o)) t)) files))
	  (set! o (cdr o)))
	 ((string-equal "-eval" (car o))
	  (if (not (cdr o))
	      (dumpfeats_error "no file specified to load"))
	  (if (string-matches (car (cdr o)) "^(.*")
	      (eval (read-from-string (car (cdr o))))
	      (load (car (cdr o))))
	  (set! o (cdr o)))
	 (t
	  (set! files (cons (car o) files))))
	(set! o (cdr o))))
    (if files
	(set! utt_files (reverse files)))))

(define (dumpfeats_error message)
  (format stderr "%s: %s\n" "dumpfeats" message)
  (dumpfeats_help))

;;; No gc messages
(gc-status nil)

(define (dump_all_features relname feats names outskeleton)
"(dump_all_features relname feats names outskeleton)
Dump all names features in RELNAME from utterances in NAMES
to a files or files specified by outskeleton."
  (let (fd)
    (if (not (string-matches outskeleton ".*%s.*"))
	(set! fd (fopen outskeleton "w")))
    (mapcar
     (lambda (uttfile)
       (if (cdr names)  ;; only output the utt name if there is more than one
           (format stderr "%s\n" uttfile))
       ;; change fd to new file if in skeleton mode
       (if (string-matches outskeleton ".*%s.*")
	   (set! fd (fopen (format nil outskeleton
				   (string-before 
				    (basename uttfile) "."))
			   "w")))
       (unwind-protect
        (extract_feats 
         relname 
         feats 
         (utt.load nil uttfile)
         fd)
        nil)
       (if (string-matches outskeleton ".*%s.*")
	   (fclose fd))
       t)
     names)
    (if (not (string-matches outskeleton ".*%s.*"))
	(fclose fd))))

(define (extract_feats relname feats utt outfd)
 "(extract_feats relname feats utt outfd)
Extract the features and write them to the file descriptor."
  (mapcar
   (lambda (si)
     (mapcar 
      (lambda (f) 
	(set! fval (unwind-protect (item.feat si f) "0"))
	(if (or (string-equal "" fval)
		(string-equal " " fval))
	    (format outfd "%l " fval)
	    (format outfd "%s " fval)))
      feats)
     (format outfd "\n")
     t)
   (utt.relation.items utt relname))
  t)

(define (get_utt fname)
  (utt.load nil fname))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;   The main work
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define (main)
  (get_options)

  (dump_all_features 
   desired_relation
   desired_features 
   utt_files
   output)
)

(main)
