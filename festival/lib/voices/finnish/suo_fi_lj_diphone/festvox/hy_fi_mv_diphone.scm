;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;                                                                       ;;
;;;                          Suopuhe project                              ;;
;;;     Department of General Linguistics, University of Helsinki, FI     ;;
;;;                Copyright (c) 2000, 2001, 2002, 2003                   ;;
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
;                                                                          ;
; Authors:                                                                 ;
;                                                                          ;
;          Martti Vainio                                                   ;
;  e-mail: martti.vainio@helsinki.fi                                       ;
; address: Department of General Linguistics                               ;
;          PL 9 (Siltavuorenpenger 20A)                                    ;
;          00014 University of Helsinki                                    ;
;          FINLAND                                                         ;
;                                                                          ;
;          Nicholas Volk                                                   ;
;  e-mail: nvolk@ling.helsinki.fi                                          ;
; address: Department of General Linguistics                               ;
;          PL 9 (Siltavuorenpenger 20A)                                    ;
;          00014 University of Helsinki                                    ;
;          FINLAND                                                         ;
;                                                                          ;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;  THE MASTER FILE OF FINNISH VOICES
;;;  =================================
;;;
;;;  contains:
;;;  The Finnish adult male voice "MV"
;;;  The Finnish adlut female voice "LJ"
;;;
;;;  The files have been combined since they share a lot
;;;  and combining them makes updating easier
;;;
;;; The female voice is linked to this file via two symbolic links
;;; first in the finnish/VOICE_NAME   
;;; and then in festvox/VOICE_NAME.scm 

(if (not (boundp 'suopuhe_output_file_prefix))
    (set! suopuhe_output_file_prefix "/tmp/"))

;; unless the debug mode is already on, set debug mode off...
(if (not (boundp 'hy_debug))
    (set! hy_debug nil))

;; Determine the location of the Finnish male voice:
;; The location is stored in variable hy_fi_mv_dir.
;; If it isn't present, the voice is not loaded!
(cond 
 ;; 0th: use my absolute path in my www demo
 ((probe_file "/home/nvolk/public_html/cgi-bin/festival/lib/voices/finnish/hy_fi_mv_diphone/festvox/hy_fi_mv_diphone.scm")
  (defvar hy_fi_mv_dir "/home/nvolk/public_html/cgi-bin/festival/lib/voices/finnish/hy_fi_mv_diphone/")
  ;; this one is for my www demo
  (set! suopuhe_output_file_prefix "/home/nvolk/public_html/cgi-bin/lavennin/tmp/"))
 ;; 1st: try default place in the Festival hierarchy:
 ((assoc 'hy_fi_mv_diphone voice-locations)
  (defvar hy_fi_mv_dir
    (cdr (assoc 'hy_fi_mv_diphone voice-locations))))
 ;; 2nd: use (my) absolute path in www demo
 ((probe_file "/home/n/v/nvolk/festival/lib/voices/finnish/hy_fi_mv_diphone/festvox/hy_fi_mv_diphone.scm")
  (defvar hy_fi_mv_dir "/home/n/v/nvolk/festival/lib/voices/finnish/hy_fi_mv_diphone/"))
 (t
  (format stderr "The location of (hy_fi_mv_diphone) could not be determined!\n")
  ))



;; Determine the location of the Finnish female voice:
;; The location is stored in variable suo_fi_lj_dir.
;; If it isn't present, the voice is not loaded!
(cond 
 ;; 1st: try default place in the Festival hierarchy:
 ((assoc 'suo_fi_lj_diphone voice-locations)
  (defvar suo_fi_lj_dir
    (cdr (assoc 'suo_fi_lj_diphone voice-locations))))
 ;; 2nd: use (my) absolute path
 ((probe_file "/home/n/v/nvolk/festival/lib/voices/finnish/suo_fi_lj_diphone/festvox/suo_fi_lj_diphone.scm")
  (defvar hy_fi_mv_dir "/home/n/v/nvolk/festival/lib/voices/finnish/suo_fi_lj_diphone/"))
 (t
  (format stderr "The location of (suo_fi_lj_diphone) could not be determined!\n")
  ))



;; add the finnish stuff to Festival's load-path variable
(if (boundp 'hy_fi_mv_dir)
    (set! load-path (cons (string-append hy_fi_mv_dir "festvox/") load-path)))
(if (boundp 'suo_fi_lj_dir)
    (set! load-path (cons (string-append suo_fi_lj_dir "festvox/") load-path)))


;;;=== THE OTHER FILES WE NEED ================================================
(require_module 'UniSyn)

(if (or (boundp 'hy_fi_mv_dir)
	(boundp 'suo_fi_lj_dir))
    (begin
      ;; (require 'finnish_aux_funcs) ;; this one makes writes to /tmp a system call (calling wavesurfer, fix paths if you need to use (wavesurf UTT) function
      (require 'finnish_phones)
      (require 'finnish_lex)
      (require 'finnish_lts) ;; handles also the tokens
      (require 'finnish_mv_int)
      (require 'finnish_duration)
      (require 'finnish_mv_phrase)
      (require 'suopuhe_mode))) ;; suopuhe, our TTS mode
;;;========================================================================


;; if one uses the bitlips modifications to Festival's C++ code
;; alternatives (a a:) (b b:) etc. could be also used safely...
;; I've informed the festival-talk lisy  about this improvement, but
;; since the current standard version Festival lacks the improvement
;; in alternative diphone selecection, I'm still using the old code 
(set! suopuhe_alternatives '(
			     (## #)
			     (a: a) ;; (a a:)
			     (b: b)
			     (d: d)
			     (D: D)
			     (e: e) ;; (e e:)
			     (f: f)
			     (g: g)
			     (h: h)
			     (i: i)
			     (j: j)
			     (k: k)
			     (l: l)
			     (m: m)
			     (n: n)
			     (N: N)
			     (o: o) ;; (o o:) 
			     (p: p)
			     (r: r)
			     (s: s)
			     (S: S)
			     (t: t)
			     (T: t)
			     (T  t)
			     ;; (T: T)
			     (u: u) ;; (u u:)
			     (v: v)
			     (w: w)
			     (y: y) ;; (y y:)
			     (7: 7) ;; (7 7:)
			     (@: @) ;; (@ @:)
			     ))			    


;;  hy_fi_mv_diphone grouped lpc data
(if (boundp 'hy_fi_mv_dir)
    (set! hy_fi_mv_lpc_group 
	  (list
	   '(name "mv_lpc_group")
	   (list 'index_file 
		 (path-append hy_fi_mv_dir "group/mvlpc.group"))
	   '(grouped "true")
	   (list 'alternates_left suopuhe_alternatives)
	   (list 'alternates_right suopuhe_alternatives)
	   (list 'default_diphone "#-#"))))

;; suo_fi_lj_diphone grouped voice
(if (boundp 'suo_fi_lj_dir)
    (set! suo_fi_lj_lpc_group 
	  (list
	   '(name "lj_lpc_group")
	   (list 'index_file 
		 (path-append suo_fi_lj_dir "group/ljlpc.group"))
	   '(grouped "true")
	   (list 'alternates_left suopuhe_alternatives)
	   (list 'alternates_right suopuhe_alternatives)
	   (list 'default_diphone "#-#"))))

(if (boundp 'hy_fi_mv_dir) (set! hy_fi_mv_db_name (us_diphone_init hy_fi_mv_lpc_group)))
(if (boundp 'suo_fi_lj_dir) (set! suo_fi_lj_db_name (us_diphone_init suo_fi_lj_lpc_group)))

;;;========================================================================


(if (boundp 'hy_fi_mv_dir)
    (define (hy_fi_mv_diphone)
      "(hy_fi_mv_diphone)
Switch to the Finnish male voice,
Suopuhe's support for Festival's in-build voice-locating mechanism."
      (voice_hy_fi_mv_diphone)))

(if (boundp 'suo_fi_lj_dir)
    (define (voice_suo_fi_lj_diphone)
      "(suo_fi_lj_diphone)
Switch to the Finnish female voice.
Suopuhe's support for Festival's in-build voice-locating mechanism."
      (voice_suo_fi_lj_diphone)))
    
(if (boundp 'suo_fi_lj_dir)
    (define (voice_suo_fi_lj_diphone)
      "(voice_suo_fi_lj_diphone) 
Suopuhe Finnish female voice "
      (general_fi_diphone)
      
      ;; local fujisaki params (and others) for female voice;
      (set! *alpha* 2)
      (set! *beta* 15)
      (set! *f_min* 115.0)
      (set! *f_max* 300.0)
      (us_db_select suo_fi_lj_db_name)
      (if (not (string-equal current-voice "suo_fi_lj_diphone"))
	  (set! current-voice 'suo_fi_lj_diphone))
      'suo_fi_lj_diphone
      ))

(if (boundp 'hy_fi_mv_dir)
    (define (voice_hy_fi_mv_diphone)
      "(voice_hy_fi_mv_diphone) 
Suopuhe Finnish male voice "
      (general_fi_diphone)
      ;; local fujisaki params;
      (set! *alpha* 2)
      (set! *beta* 15)
      (set! *f_min* 55.0) 
      (set! *f_max* 120.0)
      ;; if-lause est‰‰ ‰‰nen tuplak‰ynnist‰misen siteinit,scm:n yhteydess‰
      (us_db_select hy_fi_mv_db_name)
      (if (not (string-equal current-voice "hy_fi_mv_diphone"))
	  (set! current-voice 'hy_fi_mv_diphone))
      'hy_fi_mv_diphone))


(define (general_fi_diphone) 
  "(general_fi_diphone) 
Shared settings for all Suopuhe voices."
  (voice_reset)
  (Parameter.set 'Language 'finnish)
  (Parameter.set 'PhoneSet 'finnish) 
  (PhoneSet.select 'finnish)

  (Parameter.set 'Token_Method 'Token_Any) 
  (Parameter.set 'Phrase_Method 'cart_tree) 

  ;; we use are our own redifined "default" duration method
  ;; (which is probably a bad idea since it may cause headaches
  ;; for other languages)
  (Parameter.set 'Duration_Method 'Duration_Finnish)

  (Parameter.set 'Int_Target_Method 'General)
  (Parameter.set 'Int_Method 'General)
  ;; Numeric expansion of sorts
  ;; These should be handled externaly
  (set! token_to_words finnish_token_to_words) 


  ;;  No POS prediction (get it from TTS mode or from lexicon)...
  (set! pos_lex_name nil)
  (set! pos_supported nil)
  ;; Phrasing
  (set! phrase_cart_tree finnish_phrase_cart_tree)
  
  ;; Lexicon and postlexical rules
  (lex.select "finnish") 
 
  ;; Duration
  (set! duration_cart_tree hy_fi_mv2::zdur_tree)
  (set! duration_ph_info hy_fi_mv2::phone_durs)

  ;; Accent
  (set! int_accent_cart_tree finnish_accent_cart_tree)
  ;;(set! multfactor 0.88 )


  ;; FUJISAKI as the intonation model
  (set! int_general_params (list (list 'targ_func fujisaki_targ_func)))


  ;; TTS might complain if these are not set...
  (set! int_lr_params
 	'((target_f0_mean 110) (target_f0_std 29) ; was 100 and 19
 	  ;; the standard value for target_f0_mean 124
 	  (model_f0_mean 170) (model_f0_std 34)))

  (set! guess_pos nil) ;; no educaterd guesses


  ;; Waveform synthesizer: diphones
  (set! UniSyn_module_hooks nil)
  (set! us_abs_offset 0.0)
  (set! window_factor 1.0)
  (set! us_rel_offset 0.0)
  (set! us_gain 0.9)

  (Parameter.set 'Synth_Method 'UniSyn)
  (Parameter.set 'us_sigpr 'lpc)
  (Parameter.set 'Duration_Stretch 1.0) ;; we are speeding up... 

  ;; pause insertion and also initial doubling
  (Parameter.set 'Pause_Method suopuhe_add_break)



  (set! suopuhe nil) ;; tts-mode off. SHOULD WE USE BOUNDP here? (nv)

  ;; Initializing global variables (sigh!) for storing Fujisaki's
  ;; accent and phrase commands
  (set! suopuhe_accent nil)
  (set! suopuhe_phrase nil)

  (Parameter.set 'Language 'finnish)
  )


(if (symbol-bound? 'hy_fi_mv_dir)
    (proclaim_voice
     'hy_fi_mv_diphone
     '((language finnish)
       (gender male)
       (dialect helsinki)
       (description
	"Festival diphone voice for Finnish"
	)
       (builtwith festvox-1.2))))


(if (symbol-bound? 'hy_fi_mv_dir)
    (proclaim_voice
     'suo_fi_lj_diphone
     '((language finnish)
       (gender male)
       (dialect karjala)
       (description
	"Festival diphone voice for Finnish"
	)
       (builtwith festvox-1.2))))

;; this is the shared main file for all finnish voices
(if (boundp 'hy_fi_mv_dir)
    (provide 'hy_fi_mv_diphone))

(if (boundp 'suo_fi_lj_dir)
    (provide 'suo_fi_lj_diphone))

