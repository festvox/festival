;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;                                                                       ;;
;;;                   Carnegie Mellon University and                      ;;
;;;                Centre for Speech Technology Research                  ;;
;;;                     University of Edinburgh, UK                       ;;
;;;                       Copyright (c) 1998-2001                         ;;
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
;;;  THE UNIVERSITY OF EDINBURGH, CARNEGIE MELLON UNIVERSITY AND THE      ;;
;;;  CONTRIBUTORS TO THIS WORK DISCLAIM ALL WARRANTIES WITH REGARD TO     ;;
;;;  THIS SOFTWARE, INCLUDING ALL IMPLIED WARRANTIES OF MERCHANTABILITY   ;;
;;;  AND FITNESS, IN NO EVENT SHALL THE UNIVERSITY OF EDINBURGH, CARNEGIE ;;
;;;  MELLON UNIVERSITY NOR THE CONTRIBUTORS BE LIABLE FOR ANY SPECIAL,    ;;
;;;  INDIRECT OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES WHATSOEVER          ;;
;;;  RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN  AN ACTION   ;;
;;;  OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF     ;;
;;;  OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.       ;;
;;;                                                                       ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;  Finding features and acoustic distance measture for a set of 
;;;  segments in a database of utterances
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;  This is primarily implement for the cluster unit selection method
;;;  but may be use in other unit selection schemes.
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;  There are five stages to this
;;;     Load in all utterances
;;;     Load in their coefficients
;;;     Collect together the units of the same type
;;;     build distance tables from them
;;;     dump features for them
;;;

;; Everything is moved to lib/clunits_build.scm and 
;; lib/clunits.scm

(require_module 'clunits)
(require 'clunits_build)

(provide 'acost)
