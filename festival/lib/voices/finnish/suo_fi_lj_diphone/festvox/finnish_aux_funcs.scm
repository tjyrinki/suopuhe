;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;                                                                       ;;
;;;          Department of General Linguistics / Suopuhe project          ;;
;;;                      University of Helsinki, FI                       ;;
;;;                 Copyright (c) 2000, 2001, 2002, 2003                  ;;
;;;                        All Rights Reserved.                           ;;
;;;                                                                       ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;                                                                       ;;
;;;  Authors:                                                             ;;
;;;                                                                       ;;
;;;          Martti Vainio                                                ;;
;;;  e-mail: martti.vainio@helsinki.fi                                    ;;
;;; address: Department of General Linguistics                           ;;
;;;          PL 9 (Siltavuorenpenger 20A)                                 ;;
;;;          00014 University of Helsinki                                 ;;
;;;          FINLAND                                                      ;;
;;;                                                                       ;;
;;;          Nicholas Volk                                                ;;
;;;  e-mail: nvolk@ling.helsinki.fi                                       ;;
;;; address: Department of General Linguistics                            ;;
;;;          PL 9 (Siltavuorenpenger 20A)                                 ;;
;;;          00014 University of Helsinki                                 ;;
;;;          FINLAND                                                      ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Auxiliary functions for development work; nothing for end users here
;;; 



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

(define (phones utt)
  "(phones UTT)
Prints a list of phone names in UTTerance and returns the original utterance
unchanged.."
  (print (utt.features utt 'Segment '(name)))
  utt)




(define (wavesurf utt)
  "(wavesurf UTT)
Saves the utterance UTT as a wav file 
and the creates the corresponding lab file.
The files are placed in the /tmp dir.
Then starts the wavesurfer-program for speech analysis."
;; WISH LIST:
;; 1) optinal file name
;; 2) optional path (compare with SUOPUHE-mode)
  (utt.save.wave utt "/tmp/xxx.wav" "wav")
  (system "chmod a+rw /tmp/xxx.wav") ;; I read&write, other just (over)write


  ;; create the .lab file
;  (let ((file "/tmp/xxx.lab" fp))
  (let ((file "/tmp/xxx.lab")
	fp)
    (set! fp (fopen file "w"))
    (fwrite
     (string-append
      "separator ;\n"
      "nfields 1\n"
      "#\n"  
      (lab_body (utt.relation.first utt 'Segment)))
     fp)
    (fclose fp))
  (system "chmod a+rw /tmp/xxx.lab")
  
  (system "wavesurfer -config WAVES /tmp/xxx.wav &")
  nil)


(define (extract_feats relname feats utt)
 "(extract_feats relname feats utt outfd)
Extract the features and write them to the screen."
  (mapcar
   (lambda (si)
     (mapcar 
      (lambda (f) 
        (format t  "%s " (item.feat si f)))
      feats)
     (format t "\n"))
   (utt.relation.items utt relname)))



;; NV's redefinition of SayText
(define (SayText text)
  "(SayText TEXT)
TEXT, a string, is rendered as speech. (Suopuhe redifinion)"
  (let ((utter (utt.synth (eval (list 'Utterance 'Text text)))))
    (if utter
	(utt.play utter)
	nil)))

	 








(define (lab_header UTT)
  "(lab_header UTT)
Returns the contents for the to-be *.lab file based on the UTTerance.
Saving the output to a file is done somewhere higher."
  (string-append
   "separator ;\n"
   "nfields 1\n"
   "#\n"
   (lab_body (utt.relation.first UTT 'Segment))))

(define (lab_body PHONE)
  "(lab_body PHONE)
Writes the LAB data for a given phone and recursively call
itself while there are phones left (ortographically right:).
The header is done in lab_header procedure which typically
is the caller of this function." 
  (string-append
   ;;(item.features (item.next PHONE)););)
   "\t"
   (item.feat PHONE "end")
   "\t121\t"
   (item.feat PHONE "name") 
   " ;\n"
   (if (item.next PHONE)
       (lab_body (item.next PHONE))
       "")))




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; UNUSED STUFF FROM OTHER DIRECTORIES
;; PLACED HERE SINCE NO BETTER PLACES CAME INTO MIND
(define (one_by_one word)
  "(one_by_one WORD)
Speaks the letters in the given WORD one by one."
  (let ((output "")
	(grapheme nil))
    (while (> (string-length word) 0)
	   (set! grapheme (substring word 0 1) ". ")
	   (cond
	    ((string-equal grapheme ".")
	     (set! output (string-append output "pisteel. ")))
	    ((string-equal grapheme ",")
	     (set! output (string-append output "pilkkuul. ")))
	    
	    (t
	     (set! output (string-append output grapheme ". "))))
	   (set! word (substring word 1 (- (string-length word) 1))))
    ;; (print output)
    (SayText output)))


;;; Martti did this functions. Dunno for what though...
; (define (remove_long_consonants utt)
; "(remove_long_consonants UTT)
; Remove long consonant markers (:) from the segment names"
;    (mapcar
;    (lambda (s)
;      (let ((name (item.name s)))
;        (if (and (string-equal "-" (item.feat s "ph_vc")) ;;; consonant
; 		(string-equal "l" (item.feat s "ph_len")))  ;;; long
; 	   (item.set_name s (substring name 0 1)))
;        ))
;    (utt.relation.items utt 'Segment))
;   utt)

; (define (klusiili phone)
;   (if (string-matches phone "^[bdgkpt]:?$" )
;       t
;       nil))

; (define (konsonantti phone)
;   (if (string-matches phone "^[bdfghjklmnprstvNT]:?$" )
;       t
;       nil))


; (define (lyhyt phone)
;   (if (string-matches phone "^[abdefghijklmnoprstuvy@7NT]$" )
;       t
;       nil))

; (define (nasaali phone)
;   (if (string-matches phone "^[mnN]:?$" )
;       t
;       nil))



; (define (pitka phone)
;   (if (string-matches phone "^[abdefghijklmnoprstuvy@7NT]:$" )
;       t
;       nil))

; (define (vokaali phone)
;   (if (string-matches phone "^[aeiouy@7]:?$" )
;       t
;       nil))


(define (intro-finnish)
  "(intro-finnish)
Synthesize an introduction to the Festival Speech Synthesis System
in Finnish."
  (if (or (string-equal "hy_fi_mv_diphone" current-voice)
	  (string-equal "suo_fi_lj_diphone" current-voice))
      (tts (path-append hy_fi_mv_dir "fi-intro.txt") nil)
      (format stderr "Please set the (hy_fi_mv_diphone) voice first")))


	       
(define (language_finnish)
  "(language_finnish)
Set up language parameters for Finnish."
  (set! male1 hy_fi_mv_diphone)
  (set! female1 suo_fi_lj_diphone)
  ;; LISÄÄ TÄHÄN MYÖHEMMÄN DIFONITIETOKANNAT...
  (set! language "finnish")
  (male1)
  (Parameter.set 'Language 'finnish)
)



;; alla olevaa kamaa käytettiin kestomallin opetukseen:

; (define (puhunnoksen_alku phone)
;   (cond
;    ;; on itse tauko
;    ((not (item.relation phone 'SylStructure))
;     0)
;    ;; edellinen on tauko
;    ((not (item.prev
; 	  (item.relation 
; 	   (item.parent
; 	    (item.parent
; 	     (item.relation phone 'SylStructure)))
; 	   'Word)))
;     1)
;    ((not (item.prev
; 	  (item.prev
; 	   (item.relation 
; 	    (item.parent
; 	     (item.parent
; 	      (item.relation phone 'SylStructure)))
; 	    'Word))))
;     2)
;    (t
;     0)))

; (define (puhunnoksen_loppu phone)
;   (cond
;    ;; on itse tauko
;    ((not (item.relation phone 'SylStructure))
;     0)
;    ;; seuraava on tauko
;    ((not (item.next
; 	  (item.relation 
; 	   (item.parent
; 	    (item.parent
; 	     (item.relation phone 'SylStructure)))
; 	   'Word)))
;     1)
;    ((not (item.next
; 	  (item.next
; 	   (item.relation 
; 	    (item.parent
; 	     (item.parent
; 	      (item.relation phone 'SylStructure)))
; 	    'Word))))
;     2)
;    (t
;     0)))

; (define (painotettu_eka_tavu phone)
;   (if (and (= (item.feat phone 'R:SylStructure.parent.pos_in_word) 0)
; 	   (not (string-equal (item.name phone) "#"))
; 	   (string-equal (item.feat phone 'R:SylStructure.parent.parent.gpos)
; 			 "content"))
;       (begin 
; ;;	(print (string-append (item.name phone) " 1")) 
; 	1)
;       (begin 
; ;;	(print (string-append (item.name phone) " 0")) 
; 	0)))

; (define (eka_tavu phone)
;   (if (and  (= (item.feat phone 'R:SylStructure.parent.pos_in_word) 0)
; 	    (not (string-equal (item.name phone) "#")))
;       (begin 
; ;;	(print (string-append (item.name phone) " 1")) 
; 	1)
;       (begin 
; ;;	(print (string-append (item.name phone) " 0")) 
; 	0)))




; (define (fraasin_alku phone)
;   (cond
;    ;; on itse tauko
;    ((or (not (item.relation phone 'SylStructure)))
;     0)
;    ;; seuraava on tauko
;    ((or 
; 	(not (item.prev
; 	      (item.relation 
; 	       (item.parent
; 		(item.parent
; 		 (item.relation phone 'SylStructure)))
; 	       'Word)))
; 	(string-matches (item.name
; 			 (item.prev
; 			  (item.relation
; 			   (item.first_leaf
; 			    (item.parent
; 			     (item.parent
; 			      (item.relation phone 'SylStructure))))
; 			   'Segment)))
; 			"^#+$"))
;     1)
;    ((or 
; 	(not (item.prev
; 	      (item.prev
; 	       (item.relation 
; 		(item.parent
; 		 (item.parent
; 		  (item.relation phone 'SylStructure)))
; 		'Word))))
; 	(string-matches (item.name
; 			 (item.prev
; 			  (item.relation
; 			   (item.first_leaf
; 			    (item.relation
; 			     (item.prev
; 			      (item.relation
; 			       (item.parent
; 				(item.parent
; 				 (item.relation phone 'SylStructure)))
; 			       'Word))
; 			     'SylStructure))
; 			   'Segment)))
; 			"^#+"))
;     2)
;    (t
;     0)))




;  (item.prev
;   (item.relation
;    (item.parent
;     (item.parent
;      (item.relation phone 
;		     'SylStructure)))
;    'Word)))



; (define (round number)
;   "(round number)
; Removes some \"extra\" decimals from floats...
; Keeps log decimals manageable..."
;   (let ((pos 1)
; 	(koko 0))
;     (if (< number 0)
; 	(begin 
; 	  (set! number (* -1 number))
; 	  (set! pos -1)))
   
;     (while (> number 1)
; 	   (set! koko (+ koko 1))
; 	   (set! number (- number 1)))
;     ;; (print pos) (print koko) (print number)
;     (* pos (+ koko (read-from-string (format nil "%.2f" number))))))

; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(provide 'finnish_aux_funcs)












