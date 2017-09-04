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
;;;  Simple test of a single utterance

(define (remove_ids utt)
  "(remove_ids utt)
To all diffs on saved files this removed the _id feature from
every item in utt so other utts will look the same as this if
regenerated."
  (mapcar
   (lambda (r)
     (mapcar
      (lambda (i)
	(item.remove_feature i "id"))
      (utt.relation.items utt r)))
   (utt.relationnames utt))
  utt)

(set! utt1 
      (Utterance Text 
       "On May 5 1985, around 1985 people joined Mr. Black's project."))

(utt.synth utt1)
(utt.save.words utt1 "-")
(print (utt.features utt1 'Word '(name R:Token.parent.name R:SylStructure.daughtern.name)))
;; tidy the utt up so it'll look the same on resynthesis
(remove_ids utt1)
(utt.set_feat utt1 "max_id" 0)
(utt.set_feat utt1 "filename" 0)
(utt.save utt1 "tmp/fest2.utt")
(utt.save.segs utt1 "-")

;;; Test Utterance i/o
(set! utt2 (utt.load nil "tmp/fest2.utt"))
(utt.synth utt2)
(remove_ids utt2)
(utt.set_feat utt2 "max_id" 0)
(utt.set_feat utt2 "filename" 0)
(utt.save utt2 "tmp/fest3.utt")





