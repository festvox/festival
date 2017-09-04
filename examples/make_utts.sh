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
;;;           Date:    November 1997
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;  Build a utterance from a number of stream label files including
;;;  building the links between the stream items
;;;
;;;  This used to be a shell script but that was just soooo slow
;;;  and inflexible it was better to do it in Festival
;;;

;;; Because this is a --script type file it has to explicitly
;;; load the initfiles: init.scm and user's .festivalrc
(if (not (symbol-bound? 'caar))
    (load (path-append libdir "init.scm")))

;;;  Some parts are potentially editable
(defvar basic_relations '((Phrase segmental ())
			  (Word segmental (Phrase))
			  (Syllable segmental (Word) )
			  (Segment segmental (Syllable))
			  (IntEvent point (Syllable) )
			  (Target point (Segment))  ;; virtually unused
			  )
  "The basic relations that exist and need to be combined into a single
utterance.  Also their type, that is if they describe segments of
the utterance or points in the utterance.")

(require 'tilt)

(define (make_utts_help)
  (format t "%s\n"
  "make_utts [options] festival/relations/Segment/*.Segment
  Build utterance forms for sets of stream_item labels
  Options
  -utt_dir <string>
             Directory where utterances will be saved (default
             is festival/utts/)
  -label_dir <string>
             The directory which contains subdirectories containing
             label files for each relation, default is festival/relations/
  -style <string>
             What style of utterances, classic or unisyn
  -tilt_events 
             IntEvent files are tilt event so uses special information
             in the syllink feature to link to syllables.
  -eval <file>
             Load in scheme file with run specific code, if file name
             starts with a left parent the string itsefl is interpreted
  -tokens
             Overly non-general method to load in Tokens and markup
  -pos
             Do part of speech assignment
  -phoneset <string>
             Specify the phoneset name, this is required for -tilt_events.
")
  (quit))

;;; Default options values
(defvar seg_files nil)  ;; files to build from
(defvar label_dir "festival/relations/")
(defvar style 'classic)
(defvar tilt_events nil)
(defvar with_tokens nil)
(defvar unisyn_build_with_silences t)
(defvar do_pos nil)
(defvar do_syn nil)
(defvar utt_dir "festival/utts/")

;; may be redefined by user
(define (make_utts_user_function utt) utt)

;;; Get options
(define (get_options)
  (let ((files nil)
	(o argv))
    (if (or (member_string "-h" argv)
	    (member_string "-help" argv)
	    (member_string "--help" argv)
	    (member_string "-?" argv))
	(make_utts_help))
    (while o
      (begin
	(cond
	 ((string-equal "-label_dir" (car o))
	  (if (not (cdr o))
	      (make_utts_error "no label_dir file specified"))
	  (set! label_dir (car (cdr o)))
	  (set! o (cdr o)))
	 ((string-equal "-utt_dir" (car o))
	  (if (not (cdr o))
	      (make_utts_error "no utt_dir file specified"))
	  (set! utt_dir (car (cdr o)))
	  (set! o (cdr o)))
	 ((string-equal "-phoneset" (car o))
	  (if (not (cdr o))
	      (make_utts_error "no phoneset specified"))
	  (load_library (string-append (car (cdr o)) "_phones.scm"))
	  (set! o (cdr o)))
	 ((string-equal "-eval" (car o))
	  (if (not (cdr o))
	      (make_utts_error "no file specified to load"))
	  (if (string-matches (car (cdr o)) "^(.*")
	      (eval (read-from-string (car (cdr o))))
	      (load (car (cdr o))))
	  (set! o (cdr o)))
	 ((string-equal "-tilt_events" (car o))
	  (set! tilt_events t))
	 ((string-equal "-style" (car o))
	  (if (not (cdr o))
	      (make_utts_error "no style specified"))
	  (set! style (car (cdr o)))
	  (set! o (cdr o)))
	 ((string-equal "-tokens" (car o))
	  (set! with_tokens t))
	 ((string-equal "-pos" (car o))
	  (set! do_pos t))
	 (t
	  (set! files (cons (car o) files))))
	(set! o (cdr o))))
    (if files
	(set! seg_files (reverse files)))))

(define (make_utts_error message)
  (format stderr "%s: %s\n" "make_utts" message)
  (make_utts_help))

;;; No gc messages
(gc-status nil)

(define (make_utt name relations)
  "(make_utt dir name relations)
Build an utterance from the stream_item label files in 
dir/RELATION/name.RELATION and return it.  This also creates 
relations between each base relation."
  (let (utt)
    (cond
     ((equal? 'classic style)
      (set! utt (make_utt_classic name relations)))
     ((equal? 'unisyn style)
      (set! utt (make_utt_unisyn name relations)))
     (t
      (err "make_utts: unknown style" style)))

    (utt.set_feat utt "fileid" name)
    (if do_pos
	(find_pos utt))
    (if do_syn
	(find_syn utt))

    utt)
)

;; These should probably be somewhere else
(defvar met_insertion 7)
(defvar met_deletion 7)
(defvar met_substitution 7)

(define (make_utt_unisyn name relation)
  "(make_utt_classic name relations)
Build utterance, with xml, metrical tree and tilt relations."
  (let ((utt (Utterance nil nil)))

    (set! utt (wsi_build utt name label_dir tilt_events))

    (add_xml_relation 
     utt (string-append label_dir "xml/rhet/" name ".xrhet"))
    (add_xml_relation 
     utt (string-append label_dir "xml/syn/" name ".xsyn"))
    (add_xml_relation 
     utt (string-append label_dir "xml/anaph/" name ".xana"))
    (add_xml_relation 
     utt (string-append label_dir "xml/info/" name ".xinf"))

   (syntax_to_metrical_words utt)
   (extend_tree 
    utt (list 'MetricalTree 'MetricalWord 'WordStructure 'Syllable))
   (extend_tree 
    utt (list 'ProsodicTree 'MetricalTree 'SylStructure 'Segment))

   (add_match_features utt)
   utt)
)

(define (wsi_build utt file pathname do_il)

   (add_trans_word utt (string-append pathname "wrd/" file ".wrd"))
   (add_trans_segment utt (string-append pathname "lab/" file ".lab"))
   (if do_il
       (add_trans_intonation utt (string-append pathname "tilt/" file ".tilt"))
       nil
       )
   utt
)

(define (make_utt_classic name relations)
  "(make_utt_classic name relations)
Build utterance in classic style for name and named relations."
  (let (utt)
    (if with_tokens
	(set! utt (utt.load 
		   nil
		   (string-append label_dir "/" "Token" "/" name ".Token")))
	(set! utt (Utterance Text nil)))
    (set! current_utt utt)
    (mapcar
     (lambda (s) 
       (utt.relation.load 
	utt (car s) (string-append label_dir "/" 
				   (car s) "/" name "." (car s))))
     relations)
    ;; Now link them 
    (make_syl_structure utt)
    (make_target_links utt)
    (make_phrase_structure utt)
    (if tilt_events
	(tilt_link_syls utt)
	(intevent_link_syls utt))

    (if with_tokens
	(relate_tokens_to_words utt))
    (make_utts_user_function utt)
    utt)
)

(define (find_pos utt)
  "(find pos utt)
Assign part of speech using standard POS tagger.  Also produce
standard reduced tagset in phr_pos.  Irrelevant extra POS features
are removed.  This assumes a POS tagger is set up at this point,
this can most easily be done be setting up a relevant voice."
  (POS utt)
  (mapcar 
   (lambda (w)
     (item.set_feat 
      w "phr_pos" 
      (map_pos (item.feat w "pos") english_pos_map_wp39_to_wp20))
     (item.remove_feature w "pos_index")
     (item.remove_feature w "pos_index_score")
     )
   (utt.relation.items utt 'Word))
  utt)

(define (map_pos pos map)
  (cond
   ((null map) pos)
   ((member_string pos (car (car map)))
    (car (cdr (car map))))
   (t
    (map_pos pos (cdr map)))))

(define (make_target_links utt)
  "(make_target_links utt)
Make targets that fall within a segment.  Targets contains all segments
and have that actual Targets as daughters."
  (let ((targets (utt.relation.items utt 'Target))
	(segs (utt.relation.items utt 'Segment))
	tt1)
    (utt.relation.create utt 'TTarget)
    (mapcar
     (lambda (tt) 
       (set! tt1 (utt.relation.append utt 'TTarget))
       ;; covert the target values to the newer naming convention
       (item.set_feat tt1 "pos" (item.feat tt "end"))
       (item.set_feat tt1 "f0" (parse-number (item.feat tt "name")))
       (item.relation.remove tt 'Target))
     targets)
    (set! targets (utt.relation.items utt 'TTarget))
    (set! TARGSEGFACTOR 0.010)
    (while segs
     (utt.relation.append utt 'Target (car segs))
     (while (and targets (< (item.feat (car targets) "pos")
			  (+ (item.feat (car segs) "end")
			     TARGSEGFACTOR)))
       (item.relation.append_daughter (car segs) 'Target (car targets))
       (set! targets (cdr targets)))
     (set! segs (cdr segs)))
    (utt.relation.delete utt 'TTarget)
))
    

(define (make_phrase_structure utt)
  "(make_phrase_structure utt)
Add words into phrases."
  (let ((phrases (utt.relation.items utt 'Phrase))
	(words (utt.relation.items utt 'Word)))
    (set! WORDPHRASEFACTOR 0.200)
    (while phrases
     (while (and words (< (item.feat (car words) 'end)
			  (+ (item.feat (car phrases) 'end)
			     WORDPHRASEFACTOR)))
       (item.relation.append_daughter (car phrases) 'Phrase (car words))
       (set! words (cdr words)))
     (set! phrases (cdr phrases)))))

(define (relate_tokens_to_words utt)
"(relate_tokens_to_words utt)
A specific function for aligning the token stream to word stream."
 (convert_token_stream utt)
 (let ((tokens (utt.relation.items utt 'Token))
       (words (utt.relation.items utt 'Word)))
   (link_tokens_words tokens words)
   utt)
)

(define (convert_token_stream utt)
  "(convert_token_stream utt)
Replace Token Stream with Token relation. -- won't be needed when things
are properly converted."
  (utt.relation.create utt 'Token)
  (mapcar
   (lambda (tok) 
     (utt.relation.append utt 'Token tok))
   (utt.stream utt 'Token))
  (utt.stream.delete utt 'Token)
  )

(define (link_tokens_words tokens words)
  "(link_tokens_words tokens words)
Advance through the tokens and words aligning them as required."
  (cond
   ((null words)
    t)
   ((null tokens)
    (error (format nil "Extra words: %l\n" (mapcar item.name words))))
   ((or (string-equal "1"
		  (item.feat (car tokens) "punct-elem"))
	(member_string (item.name (car tokens))
		       '("(" ")")))
    (link_tokens_words (cdr tokens) words))
   ((string-equal "SPEECH-OMITTED"
		  (item.feat (car tokens) "R:SOLEML.parent.TYPE"))
    (link_tokens_words (cdr tokens) words))
   ((and (string-matches (item.name (car words)) ".*'.*")
	 (string-equal "APOSTROPHE"
		       (item.feat (car tokens) "R:Token.n.TYPE")))
    (item.relation.append_daughter (car tokens) 'Token (car words))
    (item.relation.append_daughter (car (cdr tokens)) 'Token (car words))
    (if (string-matches (item.name (car words)) ".*'")
	(link_tokens_words (cdr (cdr tokens)) (cdr words))
	(begin
	  (item.relation.append_daughter 
	   (car (cdr (cdr tokens))) 'Token (car words))
	  (link_tokens_words (cdr (cdr (cdr tokens))) (cdr words)))))
   ((string-equal (downcase (item.name (car tokens)))
		  (downcase (item.name (car words))))
    (item.relation.append_daughter (car tokens) 'Token (car words))
    (link_tokens_words (cdr tokens) (cdr words)))
   ;; there going to be more here !!!
   (t
    (error (format nil "Mismatch of tokens and words \n  %l\n  %l\n"
		   (mapcar item.name tokens)
		   (mapcar item.name words))))))

(define (do_utt name)
  (let ((utt (make_utt name basic_relations)))
    (utt.save utt (string-append utt_dir "/" name ".utt") 'est_ascii)
    t))

(define (make_syl_structure utt)
  "(make_syl_structure utt)
Make SylStructure relation linking Words, Syllables and Segments."
  (let ((words (utt.relation.items utt 'Word))
	(syls (utt.relation.items utt 'Syllable))
	(segs (utt.relation.items utt 'Segment)))
    (set! SYLWORDFACTOR 0.025)
    (set! SEGSYLFACTOR 0.02)
    (utt.relation.create utt 'SylStructure)
    (while words
     (utt.relation.append utt 'SylStructure (car words))
     (while (and syls (< (item.feat (car syls) 'end)
			 (+ (item.feat (car words) 'end)
			    SYLWORDFACTOR)))
       (item.relation.append_daughter (car words) 'SylStructure (car syls))
       (while (and segs (< (item.feat (car segs) 'end)
			 (+ (item.feat (car syls) 'end)
			    SEGSYLFACTOR)))
	(if (not (phone_is_silence (item.name (car segs))))
	    (item.relation.append_daughter 
	     (car syls) 'SylStructure (car segs)))
	(set! segs (cdr segs)))
       (set! syls (cdr syls)))
     (set! words (cdr words)))))

(define (tilt_link_syls utt)
"(tilt_link_syls utt)
Link syls to IntEvents, for Tilt.  In this case the feature syllink 
specifies the word.sylnum that the event should be linked to."
  (let ((syls (utt.relation.items utt 'Syllable)))
    (utt.relation.create utt 'Intonation)
    (mapcar 
     (lambda (ie)
       (let ((name (item.name ie))
	     (syllink (item.feat ie "syllink"))
	     syl)
	 (cond
	  ((member_string name '("phrase_start" "phrase_end"))
	   ;; relate this IntEvent to silence segment
;	   (if (string-equal name "phrase_start")
;	       (set! syl (find_ie_phrase_syl utt ie 'syllable_start))
;	       (set! syl (find_ie_phrase_syl utt ie 'syllable_end)))
;	   (utt.relation.append utt 'Intonation syl)
;	   (item.relation.append_daughter syl 'Intonation ie)
	   )
	  ((and (string-equal (item.feat ie "int_event") "1")
		(set! syl (find_related_syl utt syls syllink)))
	   (if (not (member 'Intonation (item.relations syl)))
	       (utt.relation.append utt 'Intonation syl))
	   (item.relation.append_daughter syl 'Intonation ie)
	   (set_rel_peak_pos utt ie syl)))))
     (utt.relation.items utt 'IntEvent))  ;; the IntEvents
     ))

(define (intevent_link_syls utt)
"(intevent_link_syls utt)
Non-tilt link of syllables to intevents through the Intonation relation."
  (let ((syls (utt.relation.items utt 'Syllable)))
    (utt.relation.create utt 'Intonation)
    (mapcar 
     (lambda (ie)
       (let ((syl (find_container_syl ie syls)))
	 (if (not (member 'Intonation (item.relations syl)))
	     (utt.relation.append utt 'Intonation syl))
	 (item.relation.append_daughter syl 'Intonation ie)))
     (utt.relation.items utt 'IntEvent))  ;; the IntEvents
     ))

(define (find_container_syl ie syls)
  "(find_container_syl ie syls)
Find the syl thats cloests to the time on this ie."
 (let ((pos (item.feat ie 'end))
       (ss syls)
       syl)
   (while (and ss (not syl))
     (let ((ss_start (item.feat (car ss) 'syllable_start))
	   (ss_end (item.feat (car ss) 'syllable_end)))
       (if (and (> pos ss_start)
		(< pos (+ ss_end 0.030)))
	   (set! syl (car ss)))
       (set! ss (cdr ss))))
   (if (not syl)
       (error "Failed to find related syllable for IntEvent at" pos))
   syl))

(define (find_ie_phrase_syl utt ie direction)
"(find_ie_phrase_syl utt ie pos direction)
Find the syllable that should be related to this IntEvent.
As at this stage no real relations can be relied on this blindly
searches the Syllable stream for a segment at the right time
point."
 (let ((syls (utt.relation.items utt 'Syllable))
       (pos (item.feat ie 'position))
       syl)
   (while (and syls (not syl))
     (if (or (approx-equal? pos (item.feat (car syls) direction) 0.04)
	     (and (not (item.relation.next ie 'IntEvent))
		  (not (cdr syls))))
	 (set! syl (car syls)))
     (set! syls (cdr syls)))
   (if (not syl)
       (error "Failed to find related syllable for phrase IntEvent at" pos))
   syl))

(define (set_rel_peak_pos utt ie syl)
"(set_rel_peak_pos ie syl)
Set the feature tilt:rel_pos to the distance from the start of 
of the vowel in syl"
 (item.set_feat
  ie
  "tilt:rel_pos"
  (- (- (item.feat ie 'end)
	(* (- 1.0 (item.feat ie 'tilt:tilt))
	   (item.feat ie 'tilt:dur) 
	   0.5))
     (syl_vowel_start syl))))

(define (find_related_syl utt syls syllink)
"(find_related_syl utt syls syllink)
Find the syllable name by sylllink, which is of the form x[.y].
x the word number and y is the syllable number."
  (unwind-protect 
   (let (wordlab sylnum word syls syl)
     (if (string-matches syllink ".*\\..*")
	 (begin 
	   (set! wordlab (string-before syllink "."))
	   (set! sylnum (- (parse-number (string-after syllink ".")) 1)))
	 (begin 
	   (set! wordlab syllink)
	   (set! sylnum 0)))
     (set! word (find_word_labelled 
		 utt (utt.relation.items utt 'Word) wordlab))
     (if (not word)
	 (error "Failed to find word labelled:" wordlab))
     (set! syls (item.relation.daughters word 'SylStructure))
     (set! syl (nth sylnum syls))
     (if syl 
	 syl
	 (car (last syls))))
   (begin
     (error "Failed to find syllable labelled:" syllink))))

(define (find_word_labelled utt words lab)
"(find_word_labelled words lab)
Find the word whose label is lab."
 (cond
  ((null words) nil)
  ((string-equal lab (item.feat (car words) "wordlab"))
   (car words))
  (t
   (find_word_labelled utt (cdr words) lab))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;   The main work
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define (main)
  (get_options)

  (mapcar
   (lambda (f)
     (format t "%s\n" f)
     (unwind-protect
      (do_utt (path-basename f))
      (format stderr "utterance build or save failed\n")))
   seg_files))

(main)
