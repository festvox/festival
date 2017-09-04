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
;;;           Date:    September 1996
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;  Gets the a summary of the latest news from Pathfinder.com (Time
;;;  magazine and synthesizes it).  My first web based speech application.
;;;
;;;  This is far too dependent on Time's latest news pages format and
;;;  not really very general, but its a start.  Also they seem to change
;;;  both the format and the name of the pages regularly so this probably
;;;  no longer works.
;;;
;;;  Note the news in Copyright Reuters, and should not be used except
;;;  for personal use.  This program can be viewed simply as a web
;;;  browser (for one particular page) and does not itself contain any
;;;  information under Time or Reuter copyright.
;;;

;;; Because this is a --script type file I has to explicitly
;;; load the initfiles: init.scm and user's .festivalrc
(load (path-append libdir "init.scm"))

(audio_mode 'async)  ;; play waves while continuing synthesis

;;; Give short introduction, so something can happen while we're 
;;; getting the news
(SayText "And here is the news.")
(SayText "News stories are courtesy of Time Warners' Path finder Magazine
          and Reuters News Media.")

(format t "Getting news from Pathfinder Magazine ... \n")
(fflush nil)
;;;  First get the page

(set! tmpfile (make_tmp_filename))
(set! tmpfile2 (string-append tmpfile "_2"))

(get_url "http://www.pathfinder.com/news/latest" tmpfile)

(format t "done\n")

;; This has to be powerful awk, not the original awk.  GNU awk or nawk
;; are what I'm looking for, but they have such random names, and may or
;; may not be on your system.
(if (string-matches *ostype* ".*Linux.*")
    (defvar GOOD_AWK "awk")
    (defvar GOOD_AWK "nawk"))

;; Should now use some HTML to SSML conversion but hack it just now
(system 
 (string-append
  GOOD_AWK " '{ if ($1 == \"<dl>\")
	inlist = 1;
       if (inlist == 1)
       {
          if ($1 == \"<dt>\")   # title
	  {
	       getline  # skip href
	       getline
	       line = $0
	       sub(/^.*<b>/,\"\",line);
	       sub(/ *<.b>.*$/,\"\",line);
               printf(\"%s, \",line);
          }
          else if ($1 == \"<dd>\") # summary
          {
               getline
               line = $0
               sub(/\(.. ... .... ..:.. ...\)/,\"\",line) # remove time stamp
               printf(\"%s\\n\\n\",line);
          }
          else if ($1 == \"</dl>\")
               inlist = 0;
       }
     }' < " tmpfile " > " tmpfile2))

;;  Say the news
(tts_file tmpfile2 nil)

(system (string-append "rm -f " tmpfile " " tmpfile2))
(audio_mode 'close)  ;; close gracefully


