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
;;;  Some specific tokens etc that might cause problems

(require 'festtest)

;;; Test the tokenization
(test_words "e.g. 12,000 pounds")
(test_words "It costs $12 million")
(test_words "Prussia's influence (1864-87) will be discussed at a conference
             May 2-10.  Call (203) 450-3343, ($1.43 a minute) for details.")
(test_words "23/06/97 at 03:45")
(test_words "During the 1950's and 60s, 1.45% took AB123.")
;;; Money, money, money, (must be funny ...)
(test_words "$10, $10,000, $1.00, $1.23, $1.03, $2.56.")
(test_words "HK$100 million, \\10,000, Y1.2345, £1.23, M$1.03, C$2.56.")
(test_words "A$1.25, £650.00, C$1.23 billion, Y10,000, #1.23.")
;;; Various special symbols
(test_words "I think that is No 123.")
(test_words "It was exactly 12:45:23.")
(test_words "The date will be 3/3/04.")
(test_words "Its on the 1st, and 2nd on 185th and Cornell.")
;;; Fractions 
(test_words "About 2/3 of the stocks increased by more than 1/16%, while the other 1/2 didn't.")
;;; Abbreviations
(test_words "The U.S. government, EU and NASA are involved.")
;;; Roman numerals
(test_words "Henry V: Part I Act II Scene XI: Mr X is I believe, V I Lenin, 
             and not Charles I.")
;;; Saint Street Doctor Drive
(test_words "Dr Taylor is at 12 High St. Edinburgh.")
(test_words "Dr Taylor is at St Andrew's St, Edinburgh.")
(test_words "Dr Taylor is at St Andrew's, St Albans.")
(test_words "Dr Taylor is at Dr Johnson Dr, West Linton.")
(test_words "Dr Taylor is with Dr Black Dr Caley and St Erasmus.")
(test_words "Dr Taylor is at Dr Black Dr near the bus station.")

;; Test the phrase break mechanism
(test_phrases "The man wanted to go for a drive in.")
(test_phrases "The man wanted to go for a drive in the country.")
(test_phrases "The man wanted to go for a drive-in the country.")
(test_phrases "The man wanted to go for a drive--in the country.")
(test_phrases "He gave the big boys' lunch in the park.")
(test_phrases "He gave the `big boys' lunch in the park.")
(test_phrases "That is it---unless you want more.")
(test_phrases "That is it -- unless you want more.")

;; Some tests of utterance/punctuation boundary
(test_segments "They called him Mr. Black though he preferred Alan.")
(test_segments "They called him Mr.  Black was the colour of his beard.")
(test_segments "(They called him Mr.) Black was the colour of his beard.")
(test_segments "The U.S. Secretary didn't arrive in time.")

(test_segments "My cat who lives in Edinburgh has nine lives.")

;;; This was showed up different durations on different platforms 
;;; sees ok now.  Problem was lexicon was in different order 
;;; (i.e. qsort did different things on different platforms)
(test_segments "Prussia's influence (1864-87) will be discussed at a conference
                May 2-10.  Call (203) 450-3343, ($1.43 a minute) for details.")


