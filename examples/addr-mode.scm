;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;                                                                       ;;
;;;                Centre for Speech Technology Research                  ;;
;;;                     University of Edinburgh, UK                       ;;
;;;                         Copyright (c) 1998                            ;;
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
;;;  An address mode for reading lists of names, addresses and
;;;  telephone numbers.  Takes quite an aggressive view of the data.
;;;
;;;  This was used for the CSTR's entry in the evaluations at the
;;;  ESCA workshop on Speech Synthesis in Janolen Caves, Blue Mountains,
;;;  NSW, Australia.
;;;
;;;  This can read things like
;;;  Brown, Bill,  6023 Wiser Rd, Austin, TX 76313-2837,  817-229-7849 
;;;  Green, Bob,  3076 Wabash Ct, Fort Worth, TX 76709-1368,  (817)292-9015 
;;;  Smith, Bobbie Q,  3337 St Laurence St, Fort Worth, TX 71611-5484,  (817)839-3689
;;;  Jones, Billy,  5306 Dr Dana Lynn Dr, Fort Worth, TX 71637-2547,  817 845 6154 
;;;  Henderson, Bryan J,  5808 Sycamore Creek Rd Apt R, Fort Worth, TX 76134-1906,  (817)239-4634 
;;;  Black, Alan W, 130 S 18th St #3, Pittsburgh, PA 15205, (412)268-8189
;;;  Bowman, K,  2610 W Bowie St, El Paso, TX 76019-1712,  (817)268-7257 
;;;  Sydney, Aaron A,  1521 NW Ballard Way, Seattle, WA 91807-4712,  (206)783-8645
;;;  Anderson, A,  12012 Pinehurst Way NE, Seattle, NE 98125-5108,  (212)404-9988 

;; New lines without trailing continuation punctuation signal EOU
(defvar addr_eou_tree 
'((n.whitespace matches ".*\n.*") ;; any new line
  ((punc in ("," ":"))
   ((0))
   ((1)))))

(set! addr_phrase_cart_tree
'
((pbreak is "B")
 ((B))
 ((pbreak is "BB")
  ((BB))
  ((lisp_token_end_punc in ("?" "." ":" "'" "\"" "," ";"))
   ((B))
   ((n.name is 0)  ;; end of utterance
    ((BB))
    ((NB)))))))

(define (addr_init_func)
 "Called on starting addr text mode."
 (Parameter.set 'Phrase_Method 'cart_tree)
 (set! phrase_cart_tree addr_phrase_cart_tree)
 (set! int_lr_params
       '((target_f0_mean 105) (target_f0_std 12)
	 (model_f0_mean 170) (model_f0_std 34)))
 (Parameter.set 'Duration_Stretch 1.1)
 (set! addr_previous_t2w_func english_token_to_words)
 (set! english_token_to_words addr_token_to_words)
 (set! token_to_words addr_token_to_words)
 (set! addr_previous_eou_tree eou_tree)
 (set! eou_tree addr_eou_tree))

(define (addr_exit_func)
 "Called on exit addr text mode."
 (Parameter.set 'Duration_Stretch 1.0)
 (set! token_to_words addr_previous_t2w_func)
 (set! english_token_to_words addr_previous_t2w_func)
 (set! eou_tree addr_previous_eou_tree))

(set! addr_regex_ZIPCODE2 "[0-9][0-9][0-9][0-9][0-9]-[0-9][0-9][0-9][0-9]")

(set! addr_regex_USPHONE3 "[0-9][0-9][0-9])[0-9][0-9][0-9]-[0-9][0-9][0-9][0-9]")
(set! addr_regex_USPHONE1 "[0-9][0-9][0-9]-[0-9][0-9][0-9]-[0-9][0-9][0-9][0-9]")

(set! addr_tiny_break (list '(name "<break>") '(pbreak mB)))
(set! addr_small_break (list '(name "<break>") '(pbreak B)))
(set! addr_large_break (list '(name "<break>") '(pbreak BB)))

(define (addr_within_name_part token)
  "(addr_within_name_part token)
Heuristic guess if we are still in the name (i.e. pre-address)
this is desgined to stop Mr W Smith becoming West."
  (cond
   ((addr_preceding_number token)
    ;; any preceding token with a digit
    nil)
   (t
    t)))

(define (addr_preceding_number tok)
  (cond
   ((null tok) nil)
   ((string-matches (item.name tok) ".*[0-9].*")
    t)
   (t (addr_preceding_number (item.prev tok)))))

(define (addr_succeeding_non_number tok)
  (cond
   ((null tok) nil)
   ((string-matches (item.name tok) ".*[A-Za-z].*")
    t)
   (t (addr_succeeding_non_number (item.next tok)))))

(define (addr_token_to_words token name)
  "(addr_token_to_words token name)
Address specific text reading mode.  Lots of address specific abbreviations
and phrasing etc."
  (set! utt_addr (item.get_utt token))
  (let ((type (item.feat token "token_type")))
    (cond
     ((string-matches name "A")
      (list (list '(name "a") '(pos nn))))
     ((addr_within_name_part token)
       (builtin_english_token_to_words token name)
      )
     ((string-matches name "\\([dD][Rr]\\|[Ss][tT]\\)")
      (if (string-equal (item.feat token "token_pos") "street")
	  (if (string-matches name "[dD][rR]")
	      (list "drive")
	      (list "street"))
	  (if (string-matches name "[dD][rR]")  ;; default on title side
	      (list "doctor")
	      (list "saint"))))
     ((string-matches name addr_regex_ZIPCODE2)
      ;; Zip code 
      (item.set_feat token "token_pos" "digits")
      (append
       (builtin_english_token_to_words token (string-before name "-"))
       (list addr_small_break)
       (builtin_english_token_to_words token (string-after name "-"))
       (list addr_large_break)))
     ((string-matches name addr_regex_USPHONE3)
      (item.set_feat token "token_pos" "digits")
      (append
       (builtin_english_token_to_words token (string-before name ")"))
       (list addr_small_break)
       (builtin_english_token_to_words token 
	 (string-after (string-before name "-") ")"))
       (list addr_small_break)
       (builtin_english_token_to_words token (string-after name "-"))))
     ((string-matches name addr_regex_USPHONE1)
      (item.set_feat token "token_pos" "digits")
      (append
       (builtin_english_token_to_words token (string-before name "-"))
       (list addr_small_break)
       (builtin_english_token_to_words token 
	 (string-before (string-after name "-") "-"))
       (list addr_small_break)
       (builtin_english_token_to_words token 
	 (string-after (string-after name "-") "-"))))
     ((string-equal name "NE")
      (cond
       ((string-matches (item.feat token "n.name") addr_regex_ZIPCODE2)
	(list "Nebraska"))
       ;; could check if there is a state following it
       (t
	(list "North" "East"))))
     ((set! full (addr_undo_abbrev name addr_addr_abbrevs))
      (cdr full))
     ((string-matches name "#.*")
      (cons
       "number"
       (builtin_english_token_to_words token (string-after name "#"))))
     ((string-matches name "[0-9][0-9][0-9][0-9][0-9]+")
      ;; long number
      (item.set_feat token "token_pos" "digits")
      (builtin_english_token_to_words token name))
     ((or (string-matches name "[0-9]0[0-9][0-9]")
	  (string-matches name "[0-9][0-9]0[0-9]"))
      (item.set_feat token "token_pos" "digits")
      (mapcar
       (lambda (a)
	 (if (string-equal a "zero")
	     "oh"
	     a))
       (builtin_english_token_to_words token name)))
     ((and
       (addr_succeeding_non_number token)
       (string-matches name "[0-9][0-9][0-9][0-9]"))
      ;; four digit number 
      (let (block number)
	(item.set_feat token "token_pos" "number")
	(set! block 
	      (builtin_english_token_to_words 
	       token (substring name 0 2)))
	(if (string-equal (nth 2 (symbolexplode name)) "0")
	    (item.set_feat token "token_pos" "digits")
	    (item.set_feat token "token_pos" "number"))
	(set! number
	      (builtin_english_token_to_words 
	       token (substring name 2 2)))
	(append
	 block 
	 (list addr_tiny_break)
	 number)))
     ((and 
       (addr_succeeding_non_number token)
       (string-matches name "[0-9][0-9][0-9]"))
      ;; four digit number 
      (let (block number)
	(item.set_feat token "token_pos" "number")
	(set! block 
	      (builtin_english_token_to_words 
	       token (substring name 0 1)))
	(if (string-equal (nth 1 (symbolexplode name)) "0")
	    (item.set_feat token "token_pos" "digits")
	    (item.set_feat token "token_pos" "number"))
	(set! number
	      (builtin_english_token_to_words 
	       token (substring name 1 2)))
	(append
	 block number)))
     ((string-matches name "[0-9]+")
      (item.set_feat token "token_pos" "digits")
      (builtin_english_token_to_words token name))
     (t  ;; for all other cases
      (addr_previous_t2w_func token name)))))

(define (addr_undo_abbrev name abbrevs)
"(addr_undo_abbrev name abbrevs)
General abbreviation undoer.  Looks for name in reverse assoc
list and returns the value."
  (cond
   ((null abbrevs) nil)
   ((member_string name (car (car abbrevs)))
    (car abbrevs))
   (t
    (addr_undo_abbrev name (cdr abbrevs)))))

(set! tts_text_modes
   (cons
    (list
      'addr         ;; mode name
      (list         ;; addr mode params
       (list 'init_func addr_init_func)
       (list 'exit_func addr_exit_func)))
    tts_text_modes))

(set! addr_us_states
      '((( AL Ala ) Alabama )
	(( AK ) Alaska )
	(( AR ) Arkansas )
	(( AZ Ariz ) Arizona )
	(( CA Cal Calif ) California )
	(( CO Colo ) Colorado )
	(( CT Conn ) Connecticutt )
	(( DC ) DC )
	(( DE Dela ) Delaware )
	(( FL Fla ) Florida )
	(( GA ) Georgia )
	(( HI ) Hawaii )
	(( IA Ind ) Indiana )
	(( ID ) Idaho )
	(( IL Ill ) Illinois )
	(( KS Kans ) Kansas )
	(( KY ) Kentucky )
	(( LA Lou Lous) Louisiana )
	(( MA Mass ) Massachusetts )
	(( MD ) Maryland )
	(( ME ) Maine )
	(( MI Mich ) Michigan )
	(( MN Minn ) Minnesota )
	(( MS Miss ) Mississippi )
	(( MT ) Montana )
	(( MO ) Missouri )
	(( NC ) North Carolina )
	(( ND ) North Dakota )
	(( NE Neb ) Nebraska )
	(( NH ) New Hampshire)
	(( NV Nev ) Nevada )
	(( NY ) New York )
	(( OH ) Ohio )
	(( OK Okla ) Oklahoma )
	(( Or Ore ) Oregon )
	(( PA Penn ) Pennsylvania )
	(( RI ) Rhode Island )
	(( SC ) Sourth Carolina )
	(( SD ) Sourth Dakota )
	(( TN  Tenn ) Tennessee )
	(( TX Tex ) Texas )
	(( UT ) Utah )
	(( VA Vir ) Virginia )
	(( VT ) Vermont )
	(( WA Wash ) Washington )
	(( WI Wisc ) Wisconsin )
	(( WV ) West Virginia )
	(( WY Wyom ) Wyoming )
	(( PR ) Puerto Rico )
	))

(set! addr_compass_points
      '(((S So Sth) South)
	((N No Nor) North)
	((E) East)
	((W) West)
	((NE) North East)
	((NW) North West)
	((SE) South East)
	((SW) South West)))

(set! addr_streets
      '(((Hwy) Highway)
	((Rt Rte) Root)
	((Ct) Court)
	((Pl) Place)
	((Blvd Bld) Boulevard)
	((Ave) Avenue)
	((Rd) Road)
	((Apt App Appt) Apartment)
	((Cntr Ctr) Center)
	((Ter Terr Tr) Terrace)
	((Ln) Lane)
	((PO) pea oh)
	))

(set! addr_uk_counties
      '((( Hants ) Hampshire)
	(( Soton ) Southampton )
	(( Berks ) Berkshire )
	(( Yorks ) Yorkshire )
	(( Leics ) Leicestershire )
	(( Shrops ) Shropshire )
	(( Cambs ) Cambridgeshire )
	(( Oxon ) Oxfordshire )
	(( Notts ) Nottinghamshire )
	(( Humbers ) Humberside )
	(( Glams ) Glamorganshire )
	(( Pembs ) Pembrookeshire )
	(( Lancs ) Lancashire )
	(( Berwicks ) Berwickshire )
	))

(set! addr_addr_abbrevs
      (append
       addr_us_states
       addr_compass_points
       addr_streets))

(provide 'addr-mode)
