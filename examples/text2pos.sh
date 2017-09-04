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
;;;           Date:    August 1996
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;  Reads in text from stdin and outputs text/pos on stdout
;;;
;;;  Designed to show how simple filters can be written in Festival
;;;
;;;  First we defined a function that processes an utterance enough
;;;  to predict part of speech, namely, tokenize it, find the words
;;;  and then run the POS tagger on it.
;;;  Then we define a function to extract the word and pos tag itself
;;;
;;;  We redefine the basic functions run on utterances during text to
;;;  speech to be our two newly-defined function and then simply
;;;  run tts on standard input.
;;;

;;; Because this is a --script type file I has to explicitly
;;; load the initfiles: init.scm and user's .festivalrc
(load (path-append libdir "init.scm"))

(define (find-pos utt)
"Main function for processing TTS utterances.  Predicts POS and
prints words with their POS"
  (Token utt)
  (POS utt)
)

(define (output-pos utt)
"Output the word/pos for each word in utt"
 (mapcar
  (lambda (word)
    (format t "%l/%l\n" 
            (item.feat word "name")
            (item.feat word "pos")))
  (utt.relation.items utt 'Word)))

;;;
;;; Redefine what happens to utterances during text to speech 
;;;
(set! tts_hooks (list find-pos output-pos))

;;; Stop those GC messages
(gc-status nil)

;;; Do the work
(tts_file "-")



