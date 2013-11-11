;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;                                                                       ;;
;;;          Department of General Linguistics / Suopuhe project          ;;
;;;                      University of Helsinki, FI                       ;;
;;;                  Copyright (c) 2000, 2001, 2002, 2003                 ;;
;;;                        All Rights Reserved.                           ;;
;;;                                                                       ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; This program is distributed under Gnu Lesser General Public License (cf. the
; file LICENSE in distribution).
 
; This program is free software; you can redistribute it and/or modify
; it under the terms of the GNU Lesser General Public License as published by
; the Free Software Foundation; either version 2 of the License, or
; (at your option) any later version.
 
; This program is distributed in the hope that it will be useful,
; but WITHOUT ANY WARRANTY; without even the implied warranty of
; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
; GNU Lesser General Public License for more details.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;                                                                       ;;
;;; Author(s):                                                            ;;
;;;                                                                       ;;
;;;          Nicholas Volk                                                ;;
;;;  e-mail: nvolk@ling.helsinki.fi                                       ;;
;;; address: Department of General Linguistics                            ;;
;;;          PL 9 (Siltavuorenpenger 20A)                                 ;;
;;;          00014 University of Helsinki                                 ;;
;;;          FINLAND                                                      ;;
;;;                                                                       ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; This file contains the suopuhe mode. It's based on the Festival's
;; Sable mode, though certain typical text-to-speech mark-up
;; language tags have been omitted, and some more linguistics stuff
;; has been added. 
;; (I think that for example. F0/speed etc. are properties of the voice,
;; and thus I don't allow any mark-up writer to modify it. 
;; Thus, no Donald Ducks!)
;;
;; WISH LIST:
;;
;; Add male/female-option into SPEAKER-tag
;; Audio plays local files, how about WWW (compare with SABLE)...
;;
;; KNOWN BUGS
;;

;; XML-mode's output directory
(if (not (boundp 'suopuhe_output_file_prefix))
    (set! suopuhe_output_file_prefix "/tmp/"))

(if (not (boundp 'suopuhe_default_voice))
    (set! suopuhe_default_voice 'voice_hy_fi_mv_diphone))

;; the maximum number of sentences to synthetize & save
(if (not (boundp 'suopuhe_max_sentences))
    (set! suopuhe_max_sentences 1000))

;; are the SGML utterances spoken or not?
(if (not (boundp 'suopuhe_aloud))
    (set! suopuhe_aloud t))


(if hy_debug
    (format stderr "Using %s as the output directory!\n" suopuhe_output_file_prefix))


(require_module 'rxp) ;; Richard Tobin's XML parser


(define (list2string list)
  "(list2string LIST)
Construct a string from a list of symbols"
  (let ((str ""))
    (mapcar (lambda (x)
	      (set!  str (string-append str x)))
	    list)
    str))

;; Add Suopuhe-mode to mode-from-suffix list
(set! auto-text-mode-alist
      (cons
       (cons "\\.suo$" 'suopuhe)
       (cons
	(cons "\\.suopuhe$" 'suopuhe)
	auto-text-mode-alist)))


(xml_register_id "-//SUOPUHE//DTD SUOPUHE speech mark up//EN"
		 (cond
;		  (t
;		   "/home/nvolk/public_html/cgi-bin/festival/lib/voices/finnish/festvox/suopuhe.dtd")
		  ((boundp 'hy_fi_mv_dir)
		   (path-append hy_fi_mv_dir "festvox/suopuhe.dtd"))
		  ((boundp 'suo_fi_lj_dir)
		   (path-append suo_fi_lj_dir "festvox/suopuhe.dtd"))
		  (t
		   nil)))

(xml_register_id "-//SABLE//ENTITIES Added Latin 1 for SABLE//EN"
		 (path-append libdir  "sable-latin.ent"))


;; select the filter for suopuhe
(set! suopuhe_filter 
       (cond 
	((boundp 'hy_fi_mv_dir)
	 (string-append hy_fi_mv_dir "festvox/suopuhe_filter.perl"))
	((boundp 'suo_fi_lj_dir)
	 (string-append suo_fi_lj_dir "festvox/suopuhe_filter.perl"))
	(t
	 nil)))

(define (suopuhe_init_globals)
  "(suopuhe_init_globals)
Initialize various global variables."
  (set! suopuhe_language "finnish")
  (set! suopuhe_file_number 1) ;; utts are stored into separate files
  (set! utts nil)
  (set! suopuhe_vol_type 'no_change)
  (set! suopuhe_vol_factor 1.0)
  (set! suopuhe t)
  (set! suopuhe_within_token 0)
  (set! suopuhe_pos_stack nil)
  (set! suopuhe_accent_stack nil)
)


;; TOKENIZATION
(define (suopuhe_token_to_words token name)
  "(suopuhe_token_to_words token name)
SUOPUHE mode token specific analysis."
  (let ((skip (car suopuhe_skip_stack)))
    ;; (format stderr "skip \"%s\"? %s\n" name skip)
    (set! suopuhe_skip_stack (cdr suopuhe_skip_stack))
    (cond
     (skip 
      (format stderr "suopuhe_mode: removed %s\n" name)
      nil)
     
     ;; default: do the standard t2w stuff:
     (t
      (finnish_token_to_words token name)))))
      ;; (original_token_to_words token name)))))





(defvar suopuhe_elements
'(
  ("(suopuhe" (ATTLIST UTT)
   ;(eval (list suopuhe_current_voice))  ;; so we know what state we start in
   (eval (list suopuhe_default_voice))
   (suopuhe_voice_param_setup)
   UTT ;; or nil
   )
  (")suopuhe" (ATTLIST UTT)
   (if (not (null? UTT))
       (begin
	 (xxml_synth UTT)
	 (suopuhe_voice_param_setup)))
   nil
   )
  ("(speaker" (ATTLIST UTT)
   (let ((name (list2string (xxml_attval "name" ATTLIST)))
	 (age (list2string (xxml_attval "age" ATTLIST)))
	 (gender (list2string (xxml_attval "gender" ATTLIST))))
     ;; (print name)
     ;; (print age)
     ;; (print gender)

     ;;; turn the appropriate age/gender/name on
     (cond
      ((and (string-equal name "martti")	    
	    (not (string-equal current-voice "hy_fi_mv_diphone")))
       (hy_fi_mv_diphone))
      ;; we should check whether the voice exits!
      ((and (string-equal name "suo_fi_lj_diphone")
	    ;; not yet on
	    (not (string-equal current-voice "suo_fi_lj_diphone")))
       (suo_fi_lj_diphone))
      ((and (string-equal gender "female")
	    (not (string-equal current-voice "suo_fi_lj_diphone")))
       (suo_fi_lj_diphone))
      ((and (string-equal gender "male")
	    (not (string-equal current-voice "hy_fi_mv_diphone")))
       (hy_fi_mv_diphone))
      ((not (string-equal age "")
	    (format stderr  "The age-parameter is not yet supported!")))
      (t
       nil))
     (set! token_to_words suopuhe_token_to_words)
   UTT))
  (")speaker" (ATTLIST UTT)
   (if (not (null? UTT))
       (begin
	 (xxml_synth UTT)
	 (suopuhe_voice_param_setup)))
   nil
   )
  ;; PAUSE: does not create a file, instead uses unix's sleep command
  ;; to doze off for a while
  ("pause" (ATTLIST UTT)
   (if (not (null? UTT))
       (begin
	 (let ((dur (car (xxml_attval "duration" ATTLIST))))
	   (set! dur (string-append "sleep " dur))
	   ;; since we are not currently creating empty wavs
	   ;; there no point dozing off when we are in the
	   ;; silent mode (suopuhe_aloud is nil)
	   (if (and suopuhe_aloud
		    (string-matches dur "sleep [1-9][0-9]*"))
	       (system dur)))))

	 
   nil)
   

  ("audio" (ATTLIST UTT)
   (if (not (null? UTT))
       (begin
	 (xxml_synth UTT)
	 (suopuhe_voice_param_setup)))  ;; synthesizing anything ready to be synthesized
   (wave.play (wave.load (car (xxml_attval "file" ATTLIST)) "wav"))
   nil)

  ("(utterance" (ATTLIST UTT)
   (if (not (null? UTT))
       (begin
	 (xxml_synth UTT)
	 (suopuhe_voice_param_setup)))
   nil)
 
  (")utterance" (ATTLIST UTT) 
   (if (not (null? UTT))
      (begin
	 (xxml_synth UTT)
	 (suopuhe_voice_param_setup)))
   nil
   )
;;;;;;;;;;;; TOKENS
  ("(token" (ATTLIST UTT)
   (set! suopuhe_within_token 1)
   ;; some variables affecting intonation and stuff...
   (let ((tokpos (list2string (xxml_attval "pos"    ATTLIST)))
	 (tokacc (list2string (xxml_attval "accent" ATTLIST)))
	 (toklis (list2string (xxml_attval "list"   ATTLIST)))
	 )
     (set! xxml_word_features 
	   (cons (list "pos" tokpos) xxml_word_features))
     
     (cond
      ((string-equal tokpos "punc")
       ;; add pause to the end of last word       
       (let ((last_token (utt.relation.last UTT 'Token)))
	 (if (and last_token
		  (string-equal (item.feat last_token "pbreak") "0"))
	     (begin
	       (item.set_feat last_token "pbreak" "B"))))
       ;; delete the actual string
       (set! suopuhe_skip_stack (append suopuhe_skip_stack (list t))))
      ;; no punc, no (must) pause:
      (t
       (push_pos tokpos)
       (push_accent tokacc)
       (set! suopuhe_skip_stack (append suopuhe_skip_stack (list nil)))
       ;; list intonation
       (if (string-equal toklis "yes")
	   (let ((last_token (utt.relation.last UTT 'Token)))
	     (if last_token
		 (begin
		   (item.set_feat last_token "pbreak" "LB")
		   (if hy_debug
		       (format t "ListBreak after %s\n" (item.name last_token)))))))
       )))
   
   
   UTT)
  
  (")token" (ATTLIST UTT)
   (set! suopuhe_within_token 0)
   UTT)

  ("break" (ATTLIST UTT)
   ;; check that there really is a word after which the pause will be put
   (if (utt.relation.last UTT 'Token)
       (let ((blevel (list2string (xxml_attval "level" ATTLIST)))
	     (last_token (utt.relation.last UTT 'Token)))      
	 
	 ;; some stack manipulation
	 (if (> suopuhe_within_token 0)
	     (begin
	       (set! suopuhe_skip_stack (append suopuhe_skip_stack (list nil)))
	       (push_pos (car suopuhe_pos_stack))
	       (push_accent (car suopuhe_accent_stack))))
	 ;; since PUNC-tags are deleted, 
	 ;; we make this a property of the previous token
	 ;; hope it's a word!
	 (if (string-equal "punc" (item.feat last_token "pos"))
	     (set! last_token (item.prev last_token)))
	 (cond
	  ((string-equal blevel "1")
	   (if last_token
	       (begin
		 (if hy_debug
		     (format t "Adding a B pause after %s.\n" (item.name last_token)))
		 
		 (item.set_feat last_token "pbreak" "B"))))
	  ((string-equal blevel "0")
	   ;; add a no-break marker
	   ;; but do we honour it? can't remember...
	   (if last_token
	       (item.set_feat last_token "pbreak" "NB")))
	  (t
	   (print (string-append "Unexpected value blevel: " blevel))
	   ))))
   UTT)
      
  
  ("phrase" (ATTLIST UTT)
   ;; add a phrase boundary
   ;; marks a phrase command in the dujisaki model
   ;; but does not result in a pause (if I remember correctly)
   (let ((last_token (utt.relation.last UTT 'Token)))
     ;; if within a token, that token consists of many words and
     ;; thus many pos and accent data: copy the information for all of them
     (if (> suopuhe_within_token 0)
	 (begin
	   (set! suopuhe_skip_stack (append suopuhe_skip_stack (list nil)))
	   (push_pos (car suopuhe_pos_stack))
	   (push_accent (car suopuhe_accent_stack))))

     (if last_token
	 (begin
	   (if hy_debug
	       (format t "Lis‰t‰‰n tauko %s-sanan per‰‰n (PB)\n" (item.name last_token)))
	   (item.set_feat last_token "pbreak" "PB"))))

   
   ;; duplicate stack properties
   (if (> suopuhe_within_token 0)
       (begin
	 (push_pos (car suopuhe_pos_stack))
	 ;; (push_ini (car suopuhe_ini_stack))
	 (push_accent (car suopuhe_accent_stack))))

   ;; add a phrase command?
   UTT)


))



(define (suopuhe_init_func)
  "(suopuhe_init_func)
Initialisation for SUOPUHE mode"
  (suopuhe_init_globals)
  (set! original_xxml_elements xxml_elements) ; fixed
  (set! xxml_elements suopuhe_elements)       ; fixed
  (set! original_token_to_words token_to_words) 
  (set! original_english_token_to_words english_token_to_words) ; new
  (set! english_token_to_words suopuhe_token_to_words) 
  (set! token_to_words suopuhe_token_to_words))


(define (suopuhe_exit_func) ; VALMIS
  "(suopuhe_exit_func)
Exit function for SUOPUHE mode"
  (set! xxml_elements original_xxml_elements) ; ok
  (set! token_to_words original_token_to_words) 
  (set! english_token_to_words original_english_token_to_words)
  (set! suopuhe nil))


(define (= x y)
  "(= N1 N2)
Return true if N1 and N2 are equal numbers."
  (and (<= x y) (>= x y)))

(define (suopuhe_voice_param_setup)
  "(suopuhe_voice_param_setup)
Set up original values for various voice parameters."
  (set! suopuhe_pos_stack nil) ;; tieto sanaluokasta/sana
  (set! suopuhe_accent_stack nil) ;; tieto aksentista
  (set! suopuhe_skip_stack nil) ;; t: remove nil: don't
  (set! pos_reversed nil)
  (set! suopuhe_pitch_base_original (cadr (assoc 'target_f0_mean int_lr_params)))
  (set! suopuhe_pitch_med_original (cadr (assoc 'target_f0_mean int_lr_params)))
  (set! suopuhe_pitch_range_original (cadr (assoc 'target_f0_std int_lr_params)))
  (set! suopuhe_rate_speed_original 1.0)

  (if (and after_synth_hooks (not (consp after_synth_hooks)))
      (set! after_synth_hooks 
	    (cons after_synth_hooks (list suopuhe_adjust_volume)))
      (set! after_synth_hooks 
	    (append after_synth_hooks (list suopuhe_adjust_volume)))))

(define (suopuhe_adjust_volume utt)
  "(suopuhe_adjust_volume utt)
Amplify or attenutate signale based on value of suopuhe_vol_factor
and suopuhe_vol_type (absolute or relative)."
  (set! utts (cons utt utts))
  (cond
   ((equal? suopuhe_vol_type 'no_change)
    utt)
   ((equal? suopuhe_vol_type 'absolute)
    (utt.wave.rescale utt suopuhe_vol_factor 'absolute))
   ((equal? suopuhe_vol_type 'relative)
    (utt.wave.rescale utt suopuhe_vol_factor))
   (t
    (format stderr "SUOPUHE: volume unknown type \"%s\"\n" suopuhe_vol_type)
    utt))
   utt)



;;; Declare the new mode to Festival
;;; Keep this at the end of the file!
(set! tts_text_modes
   (cons
    (list
      'suopuhe   ;; mode name
      (list         
       (list 'init_func suopuhe_init_func)
       (list 'exit_func suopuhe_exit_func)
       (list 'filter (format nil "%s" suopuhe_filter)) 
       ;; now we can use suopuhe-mode from anywhere
       '(analysis_type xml)
       ))
    tts_text_modes))

(define (tts file mode)
  "(tts FILE MODE)
  Convert FILE to speech.  MODE identifies any special treatment
  necessary for FILE.  This is simply a front end to tts_file but 
  puts the system in sync audio mode first. [see TTS]"
  (audio_mode 'sync)
  ;; (print (tts_find_text_mode file auto-text-mode-alist))
  (if (and mode 
	   (not (string-equal mode "suopuhe")))
      (tts_file file mode)
      (if (string-equal "suopuhe" 
			(tts_find_text_mode file auto-text-mode-alist))
	  (begin
	    (suopuhe_setup)
	    (tts_file file 'suopuhe))
	  (tts_file file (tts_find_text_mode file auto-text-mode-alist))))
  (audio_mode 'sync) ;; Hmm this is probably bad
  )
;; korvaava versio tts-viritelmiin...
;(define (utt.play utt)
;  "(utt.play UTT)
;Play waveform in utt by current audio method."
;  (print (utt.relationnames utt))
;  (wave.play (utt.wave utt))
;  utt)




(define (push_pos alkio)
  "(push_pos alkio)
Adds alkio to the top of the suopuhe_pos_stack."
  (set! suopuhe_pos_stack
	(cons alkio suopuhe_pos_stack)))

(define (pop_pos)
  "(pop_pos)
Removes and returns the topmost whatever from the suopuhe_pos_stack." 
  (let ((alkio (car suopuhe_pos_stack)))
    (set! suopuhe_pos_stack (cdr suopuhe_pos_stack))
    alkio))

(define (push_accent alkio)
  "(push_accent alkio)
Adds alkio to the top of the suopuhe_accent_stack."
  (set! suopuhe_accent_stack
	(cons alkio suopuhe_accent_stack)))

(define (pop_accent)
  "(pop_accent)
Removes and returns the topmost whatever from the suopuhe_accent_stack."  
  (let ((alkio (car suopuhe_accent_stack)))
    (set! suopuhe_accent_stack (cdr suopuhe_accent_stack))
    alkio))





;; kukin lause talletetaan omaan tiedostoonsa
;; lausenumero on nelilukuinen merkkijono
;; levytilan s‰‰st‰miseksi talletetaan vain 100 erinimist‰ tiedostoa
;; eli 99 ensimm‰ist‰ ja viimeinen...
(define (file_id int)
  (if (> int suopuhe_max_sentences)
      suopuhe_max_sentences
      int))





(define (suopuhe_setup)
  "(suopuhe_setup)
Reset tts hooks for suopuhe mode."
  (set! tts_hooks
	(append 
	 tts_hooks
	 (list save_suopuhe_output)))
  ;; remove (utt.play) from tts_hooks 
  (if (not suopuhe_aloud)
      (set! tts_hooks (remove (car (list utt.play)) tts_hooks)))
  )

(define (save_suopuhe_output utt)
  "(save_suopuhe_output UTT)
Saves the UTT and some textual data about it to files."
  ;; (format stderr "%s" suopuhe_file_number)
  (if (> suopuhe_file_number suopuhe_max_sentences)
      (begin
	(format stderr "Muuttuja suopuhe_file_number ylitt‰‰ muuttujan suopuhe_max_sentences\nasenttaman raja-arvon! Tiedostoa ei talleteta!\n")
	(if (not suopuhe_aloud)
	    (begin
	      (SayText "Muuttuja suopuhe_file_number ylitti muuttujan
suopuhe_max_sentences\nasenttaman raja-arvon")
	      (SayText "Tiedostojen tallennus lopetetaan ja poistutaan ohjelmasta.")
	      (SayText "N‰kemiin!")
	      (exit 0)))))
	
  (if (and utt
	   (or suopuhe_aloud
	       (< (+ suopuhe_max_sentences 1) suopuhe_file_number)))
      (begin
	(let ((file (string-append
		     suopuhe_output_file_prefix
		     "suopuhe-"
		     (file_id suopuhe_file_number)
		     ".wav")))
	  (format stderr "suopuhe: saving files into %s!\n" file)
	  (utt.save.wave utt file "wav")
	  (system (string-append "chmod 622 " file)))
	(if hy_debug 
	    (begin
	      (print "SANA===LUOKKA==TAUKO===ALKU===LOPPU===")
	      (print (parts_of_speech utt))))
	;; write phone durations into a file
	(let ((file (string-append
		     suopuhe_output_file_prefix
		     "suopuhe-sanakestot-"
		     (file_id suopuhe_file_number)
		     ".txt"))
	      suopuhe_fp)
	  (set! suopuhe_fp (fopen file "w"))
	  (fwrite
	   (word_durations (utt.relation.first utt 'Word))
	   suopuhe_fp)
	  (fclose suopuhe_fp)
	  (system (string-append "chmod 622 " file)))
	;; <= kirjoitus p‰‰ttyy
	
	(if hy_debug (print "===== SYLLABLE STRESS  ====="))
	(if hy_debug (print (syllable_stress utt)))
	;;(print "==FOONIT (aannekestot.txt)")
	;; kirjoita aannekestot tiedostoon:
	(let ((file (string-append
		     suopuhe_output_file_prefix
		     "suopuhe-aannekestot-"
		     (file_id suopuhe_file_number)
		     ".txt"))
	      suopuhe_fp)
	  (set! suopuhe_fp (fopen file "w"))
	  (fwrite
	   (phone_durations (utt.relation.first utt 'Segment))
	   suopuhe_fp);
	  (fclose suopuhe_fp)
	  (system (string-append "chmod 622 " file)))))
	
	;; <= kirjoitus p‰‰ttyy
	;;(print (aannekestot2 (utt.relation.first utt 'Segment)))
	;; do something more with utt, if you want...
  (set! suopuhe_file_number (+ suopuhe_file_number 1)))
  


(define (word_durations word)
  "(word durations2 WORD)
Returns the name and start and end times of the given word.
Then recursively calls the next word if any and finally
returns a string consisting of data from all words visited."
  (string-append
   (item.feat word "name") "\t"
   ;; (item.feat word "pos") "\t"
   ;; (item.feat word "pbreak") "\t" 
   (item.feat word "word_start") "\t" 
   (item.feat word "word_end") "\n" 
   (if (item.next word)
       (word_durations (item.next word))
       "")))


(define (phone_durations phone)
  "(phone_durations PHONE)
Prints the name and duration of a given PHONE.
The recursively goes through the next phones and does the same for them also."
  (string-append
   ;; (item.features (item.next phone)););)
   (item.feat phone "name") "\t"
   (item.feat phone "end") "\n"
   ;; (item.feat phone "source_end") "\t" ;; dunno what this is...
   ;; (item.feat phone "dur_factor") "\n" ))		
   (if (item.next phone)
       (phone_durations (item.next phone))
       "")))

(define (parts_of_speech utt)
  "(parts_of_speech UTT)
Returns a list of sublists. Each sublist consists of a word's name
and part-of-speech based on the UTTerance."
  (utt.features utt 'Word '(name pos)))

(define (syllable_stress utt)
  "(syllable stress UTT)
Returns a list of sublists. Each sublist consists of a syllable name
and a syllable stress value based on the UTTerance."
  (utt.features utt 'Syllable '(name stress)))


(provide 'suopuhe_mode)