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
;;;  A search for token occurrences in buckets of text
;;;  
;;;  This is only an example to aid you, this actually depends on
;;;  the availability of databases we don't have permission to
;;;  distribute.

(set! text_dir "/home/awb/data/text/")

;;;  The databases themselves are identified by a file which names all
;;;  the files in that databases.  e.g. This expects bin/gutenberg.files
;;;  to exists which should contain something like
;;;      gutenberg/etext90/bill11.txt
;;;      gutenberg/etext90/const11.txt
;;;      gutenberg/etext90/getty11.txt

(set! db_names
      '("gutenberg"     ;; books from gutenberg              21906570
        "desktopshop"   ;; books, documents etc              23090463
        "time"          ;; Time Magazine 1990-1994            6770175
        "hutch"         ;; Hutchinson Encyclopedia            1715268
        "dicts"         ;; Dictionaries and Encyclopedias     4248109
        "stw-ref"       ;; Standard Reference libraries       3330448
	"treebank"      ;; WSJ articles from PENN treebank    1109895
	"email"         ;; awb's email
       ))

;;; Identify the tokens you want extracted
;;; Tokens may be regular expressions
(set! desired_tokens
      '(lead wound tear axes Jan bass Nice Begin Chi Colon
        St Dr III IV V X VII II "[0-9]+"))

;;; First pass: to get examples and context for labelling
(set! desired_feats
      '(filepos
	p.p.p.p.name p.p.p.name p.p.name p.name 
	name 
	n.name nn.name n.n.n.name n.n.n.n.name))
;;; Second: pass to get desried features for tree building
;;; Typically this has to be specific for a particular homograph
;;; so you'll probably want to do multiple second passes one for each
;;; homograph type
;(set! desired_feats
;      '(filepos
;	lisp_tok_rex
;	p.punc
;	punc
;	n.punc
;	pp.cap p.cap n.cap nn.cap
;	))

(define (tok_search_db dbname)
"Search through DB for named tokens and save found occurrences."
  (let ((outfile (string-append text_dir "fullhgs/" dbname ".out")))
    (delete-file outfile)
    (mapcar
     (lambda (fname)  ;; for each file in the database
       (extract_tokens  ;; call internal function to extract tokens
	(string-append text_dir fname)  ;; full pathname to extract from
	(mapcar                         ;; list of tokens and features 
	 (lambda (t)                    ;;    to extract
	   (cons t desired_feats)) 
	 desired_tokens)
	outfile))
     (load (string-append text_dir "bin/" dbname ".files") t))
    t))

(define (tok_do_all)
"Search all dbs for desired tokens."
  (mapcar 
   (lambda (db)
     (print db)
     (tok_search_db db))
   db_names)
  t)

