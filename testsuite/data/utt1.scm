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
;;;  Walk about an utterance using features, relations etc.

(set! utt1 (Utterance Text "hello, this example has three syllables."))
(utt.synth utt1)

(format t "num syllables: %d\n"
	(length (utt.features utt1 'Syllable '(name))))

(set! segs (utt.relation.items utt1 'Segment))
(set! s1 (item.relation.parent (nth 2 segs) 'SylStructure))
(set! s2 (item.relation.parent (nth 3 segs) 'SylStructure))
(set! s3 (item.relation.parent (nth 4 segs) 'SylStructure))

(if (equal? s1 s2)
    (print "syls match: ok")
    (print "syls don't match: error"))
(if (not (equal? s2 s3))
    (print "syls don't match: ok")
    (print "syls match: error"))

(print
 (list
  (item.feat s3 'name)
  (item.feat s3 'R:SylStructure.daughter1.name)
  (item.feat s3 'R:Syllable.n.R:SylStructure.daughter1.name)
  (item.feat s3 'R:Syllable.p.R:SylStructure.daughter1.name)
  (item.feat s3 'R:SylStructure.parent.name)
  (item.feat s3 'R:SylStructure.parent.daughter1.daughtern.name)
  (item.feat s3 'R:SylStructure.parent.R:Token.parent.whitespace)
  (item.feat s3 'R:SylStructure.parent.R:Word.n.R:Token.parent.p.punc)
))



