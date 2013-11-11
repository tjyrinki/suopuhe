;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;                                                                       ;;
;;;          Department of General Linguistics / Suopuhe project          ;;
;;;                      University of Helsinki, FI                       ;;
;;;                   Copyright (c) 2000,2001,2002,2003                   ;;
;;;                        All Rights Reserved.                           ;;
;;;                                                                       ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;                                                                       ;
;;; Authors:                                                              ;
;;;                                                                       ;
;;;          Martti Vainio                                                ;
;;;  e-mail: martti.vainio@helsinki.fi                                    ;
;;; address: Department of General Linguistics                            ;
;;;          PL 9 (Siltavuorenpenger 20A)                                 ;
;;;          00014 University of Helsinki                                 ;
;;;          FINLAND                                                      ;
;;;                                                                       ;
;;;          Nicholas Volk                                                ;
;;;  e-mail: nvolk@ling.helsinki.fi                                       ;
;;; address: Department of General Linguistics                            ;
;;;          PL 9 (Siltavuorenpenger 20A)                                 ;
;;;          00014 University of Helsinki                                 ;
;;;          FINLAND                                                      ;
;;;                                                                       ;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;  Accent and F0 prediction
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Intonation


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

;;; (SayText "sanoisin että en sano" )
(define (monotone utt syl)
  "(monotone UTT SYL)
Monotonous intonation model."
  (let ((middle (/ (+ (item.feat syl 'syllable_start) (item.feat syl 'syllable_end)) 2)))
    (list (list middle 100))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Intonation
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(set! finnish_accent_cart_tree
      '((stress is 2)
	((R:SylStructure.parent.pos is function)
	 ((NONE))
	 ((R:SylStructure.parent.pos is COP)
	  ((NONE))
	  ((R:SylStructure.parent.pos is PRON)
	   ((NONE))
	   ((R:SylStructure.parent.pos is COORD)
	    ((NONE))

	    ((Accented))))))
	((NONE))))




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Fujisaki model
;;

; The model and algorith is as follows:
;-----------------------------------------------------------------------
; Variables:
;                ...
; points = once in very 50 ms (points where (time_in_seconds * 1000 % 50 == 0)
;         but atleast once in each syllable
; alpha = defined in voice_WHATEVER
; beta = defined in voice_WHATEVER
; f_min = defined in voice_WHATEVER
;-----------------------------------------------------------------------       
; Algorithm:
;   for each utterance
;    accent_levels = predict accent levels
;    accent_types  = predict accent type
;    accent_list   = calculate_accent_commands(accent_levels, accent_types)
;    phrase_list   = calculate_phrase_commands(utterance, pharases)
;    syllables     = syllables(utterance)
;    calculate fujisaki_contour(segments, phrase_list, accent_list)
;
; fujisaki_contour:
;   for syllable in  syllables:
;     for point in points:
;       ph_level = calculate_phrase_level(point, phrase_list, alpha)
;       ac_level = calculate_accent_level(point, accent_list, beta)
;       f0 = exp(ph_level + ac_level + f_min) 

;; global parameters for the Fujisaki model
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; we should use the Parameter.set etc. methods:


;; Some auxiliary functions, which should be somewhere else
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; this is not called anywhere?
;;(define (incf var val)
;;  (+ var val))

(define (sqr number)
  "(sqr NUM)
NUM ** 2."
  (* number number))

(define (neg number)
  "(neg number)
Negates a number -- Festival SIOD doesn't understand (- number), but
requires TWO arguments to the '-' operator"
  (* -1 number))


(define (min num1 num2)
  "(min num1 num2)
Returns the smaller of the two."
  (cond ((<= num1 num2)
        num1)
        (t num2)))

(define (max num1 num2)
  "(max num1 num2)
Returns the greater of the two."
  (cond ((<= num1 num2)
        num2)
        (t num1)))


(define (accented_p syl)
  "(accented_p SYL)
Sees if the syllable is accented..." 
  (cond
   ((not (equal? (item.feat syl "R:Intonation.daughter1.name") "Accented"))
    nil)
   ((string-equal (item.feat syl 'R:SylStructure.parent.pos) "function")
    nil)   
   ((string-equal (item.feat syl 'R:SylStructure.parent.pos) "COP")
    nil)
   ((string-equal (item.feat syl 'R:SylStructure.parent.pos) "PRON")
    nil)
   (t
    t)))
;   
;
;  (and (string-equal (item.feat syl 'R:SylStructure.parent.pos) "content")
;       ;; eka tavu
;       (equal? (item.feat syl "R:Intonation.daughter1.name") "Accented")))


;; Accent and Phrase parameter prediction
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (get_accent_list utt)
  "(get_accent_list UTT)"
  (let ((syllables (utt.relation.items utt 'Syllable))
	(ennakko 0.00)
	(no_accent nil) ;; debug variable: removes accent commands
        (ac_list nil))
    (mapcar
     (lambda (syl) ;; for each syllable
       
       (let ((onset  (- (item.feat syl 'syllable_start) ennakko))
	     (offset (item.feat syl 'syllable_end)))

	 (cond
	  (no_accent
	   nil)
	  ;; only syllable
	  ((and (not (item.next syl))
		(not (item.prev syl)))
	   (set! ac_list (append ac_list (list (list onset offset 0.1)))))
	  ;; only word, first syllable
	  ((and (not (item.prev syl))
		(not (item.next (item.parent (item.relation syl 'SylStructure)))))
	   (set! ac_list (append ac_list (list (list onset offset 0.25)))))
	  ;; first syllable on an accented word
	  ((and (accented_p syl)
		(or (not (item.prev syl))
		    (not (string-equal
			  (item.feat syl 'R:SylStructure.parent.name)
			  (item.feat syl "p.R:SylStructure.parent.name")))))
	   
	   ;; minimum of (120ms * stretch)
	   (if (< (- offset onset) (* 0.120 (Param.get 'Duration_Stretch)))
	       (set! offset (+ onset (* 0.120 (Param.get 'Duration_Stretch)))))
	   
	   (set! ac_list (append ac_list (list (list onset offset 0.4)))))
	  ;; unstressed last syllable: negative accent.
	  ((and (not (accented_p syl))
		(not (item.next syl)))
	   (set! onset (- onset (/ (- onset (item.feat (item.prev syl) 'syllable_start)) 2)))
	   
	   (set! ac_list
		 (append ac_list 
			 (list (list 
				onset 
				(+ offset (* 0.2 (Param.get 'Duration_Stretch )))
				-0.3)))))
	  ;; default: do nothing
	  (t 
	   nil))))
     syllables)
    ac_list))

;; find all phrases within an utterance:
(define (find_phrases utt)
  "(define (find_phrases utt)
Returns a list of phrases from an utterance in the form
of lists whose car is the break level followed by start and end times"
  (let ((wrds (utt.relation.items utt 'Word))
	;; end is problematic if (SayText UTT) contains more
	;; than one sentence. SEP..
	(end (item.feat (utt.relation.last utt 'Word) 'word_end))
        (result nil)
        (start -0.2))
 
    (mapcar
     (lambda (wrd)
;;       (set! end (item.feat wrd 'word_end))
       (let ((break (item.feat wrd 'pbreak))
;	     (end (item.feat wrd 'word_end))
	     (next (item.next wrd)))
	 (cond 
	  ;; ordinary break
	  ((or (equal? break "B")
	       (equal? break "BB")
	       (equal? break "PB")) ;; Phrasal Break
	   (set! result (append result (list (list break start end))))
	   (set! start (item.feat wrd 'word_end)))
	   ;; break within list
	  ((equal? break "LB")
	   (set! result (append result (list (list break start end))))
	   (set! start
		 (if next 
		     (max
		      (- (item.feat wrd 'word_end) 0.1)
		      (/ (+ (item.feat wrd 'word_start)
			    (item.feat wrd 'word_end))
			 2))
		     "no_start")))
	  (t nil))))
     wrds)
    
    result))

(define (utt.length utt)
  "(utt.length utt)
Returns the length of an utterance in syllables"
  (length (utt.relation.items utt 'Syllable)))

;;; produce a list of phrase commands:
(define (get_phrase_list utt)
  "(get_phrase_list utt)
Returns a list of phrase commands for an utterance. The first command
is higher than the following ones."
  (let ((phrases (find_phrases utt))
        (phrase_list nil)
        (len nil))
    (set! len (length phrases))
    (set! phrase_list
          (mapcar
           (lambda (phrase)
             (let ((b_type (car phrase))
                   (start (nth 1 phrase))
                   (end (nth 2 phrase))
                   (level 0))
               (cond ((or (and (= len 1) (> (utt.length utt) 5))
                          (and (= (position phrase phrases) 0)
                               (not (string-equal b_type "BB")))) ;; first phrase of more than one
		      (if (string-equal current-voice "suo_fi_lj_diphone")
			  (list start end 0.4)
			  ;; male voice
			  (list start end 0.6))) ;; was 0.7
		     ((or (string-equal b_type "LB")
			  (string-equal b_type "PB"))
		      ;; list intonation 
		      (list start end 0.05))
                     (t
                      (list start end 0.3)))))
           phrases))
    phrase_list))


;;; this is the actual Fujisaki equation:
;;;
;;; calculate accent amplitude for a given point:
(define (get_amplitude time beta)
  "(get_amplitude point beta)
Calculate the amplitude from time and beta information:"
  (if (< time 0) 
      0 ;; <- THEN
      (min 0.9 ;; <- ELSE
	   (- 1 (* (+ 1 (* beta time))
		   (exp (* (neg beta) time)))))))

;;; calculate phrase amplitude for a given point:
(define (get_phrase_amplitude time alpha)
  "(get_phrase_amplitude time alpha)
calculate phrase amplitude for a given point in time."
  (cond ((>= time 0)
	 (begin
	   (* (sqr alpha) 
	      time
	      (exp (* (neg alpha) time)))))
	(t 0.0)))


;;; calculate the actual f0 for a given time (point):
(define (calculate_fujisaki accent_list phrase_list point alpha beta f_min)
  " (calculate_fujisaki acc_list phrase list point alpha beta f_min)
Calculates the Fujisaki parameter values for a given point. Returns an absolute
Hertz value"
;;  (print point)
  (let (ph_level
        ac_level
	speed_level
        result)
    (set! ph_level
          (let ((Sum_Pa 0.0))
            (mapcar
             (lambda (p)
               (let ((onset (+ (car p) (/ -1 beta))) ;;
                     (offset (nth 1 p))
                     (amp (nth 2 p)))
                 (set! Sum_Pa
                       (+ Sum_Pa
                          (* amp
                             (get_phrase_amplitude
			      (if (> point offset)
				  0.0
				  (- point onset))
			      alpha ))))))
             phrase_list)
	    Sum_Pa))
    (set! ac_level
          (let ((Sum_Aa 0.0))
            (mapcar
             (lambda (ac)
               (let ((onset (car ac))    ;; onset of the accent command
                     (offset (nth 1 ac)) ;; offset of the acc. com.
                     (amp (nth 2 ac)))    ;; amplitude of the  acc. com.
                 (set! Ga_T1
                       (get_amplitude (- point onset) beta))
                 (set! Ga_T2
                       (get_amplitude (- point offset) beta))
                 (set! Sum_Aa
                       (+ Sum_Aa (* amp (- Ga_T1 Ga_T2))))))
             accent_list)
            Sum_Aa))
    ;;; speed level raises f0 in fast speech
    (set! speed_level (* 100 (- 1 (min (Parameter.get 'Duration_Stretch) 1))))
    ;; maximum raise of 40dB
    (set! speed_level (min 40 speed_level))


    ;; fast speech is also more monotonous...
    (set! result (+ speed_level
		    (exp 
		     (+ (* ph_level (min (Parameter.get 'Duration_Stretch) 1)) 
			(* ac_level (min (Parameter.get 'Duration_Stretch) 1))

			(log f_min)))))
    result))

;;; calculate the local f0 contour for a syllable
;;; for a given number of points, determined by
;;; the parameter *points*
(define (fujisaki_targ_func utt syl)
  "(fujisaki_targ_func UTT STREAMITEM)
Returns a list of targets for the given syllable."
  ;; (if hy_debug (print "Fujisaki_targ_func"))

  
  (baptize syl) ;; give names to syllables

  (begin
    (let ((start (item.feat syl 'syllable_start))
	  (end (item.feat syl 'syllable_end))
	  (accent_list (or suopuhe_accent
			   ;; wierd way to print status reports (:
			   (if hy_debug (format stderr "------ COUNTING THE F0 CONTOUR ------\n"))
			   (get_accent_list utt)))
	  (phrase_list (or suopuhe_phrase
			   (get_phrase_list utt)))
	  
	  
	  (*points* nil)
	  result dur)
      ;; nint was the easiest way to create an integer, where's abs?
      ;; calculate f0 after every 50 mseconds
      (let ((i (if (< start (nint start))
		   (- (nint start) 1)
		   (nint start))))
	(while (< i end)
	       (begin
		 (if (>= i start)
		     (set! *points* (flatten (list *points* i))))
		 (set! i (+ i 0.050))))) ;; + 50 ms
      ;; if too short a syllable, use start and end times instead.
      (if (not *points*)
	  (set! *points* (list start end))
	  (begin
	    (if (not (item.next syl))
		(set! *points* (flatten (list *points* end (+ end 0.1)))))
	    (if (not (item.prev syl))
		(set! *points* (flatten (list 0.0 *points*))))))

            

      (set! dur (- end start))
      (set! result (mapcar 
		    (lambda (point)
		      ;;			(if (> (+ start (* dur point))
		      ;;			       end)
		      ;;(print (string-append "Dur: " dur " point " (+ start (* dur point)) " end " end))
;;		      (list (+ start (* dur point))
		      (list point
			    (min *f_max*
				 (calculate_fujisaki
				  accent_list
				  phrase_list
				  ;; IN ABSOLUT TIME
				  point
				  ;; THIS WAS IN THE RELATIVE APPROACH:
				  ;;(+ start (* point dur))
				  *alpha* *beta* *f_min*))))
		    *points*))

      (set! suopuhe_accent accent_list)
      (set! suopuhe_phrase phrase_list)
      (if hy_debug
	  (begin

	    (format t "   Time      F0 in syllable \"%s\".\n" (item.name syl))
	    
	    (mapcar (lambda (x)
		    (mapcar (lambda (y)
				(format t "%7.3f " y))
			      x)
		      (format t "\n"))
		    result)))  
      (if (and hy_debug
	       (not (item.next syl))) 
	  (begin
	    (format t "Phrase commands\n   Start    End    Size\n")
	    (mapcar (lambda (x)
		      (mapcar (lambda (y)
				(format t "%7.3f " y))
			      x)
		      (format t "\n"))
		    phrase_list)
	    (format t "Accent commands\n   Start    End    Size\n")
	    (mapcar (lambda (x)
		      (mapcar (lambda (y)
				(format t "%7.3f " y))
			      x)
		      (format t "\n"))
		    
		    accent_list)))
		      
      ;;; (print phrase_list)
      ;;;(print accent_list)

;      (if hy_debug 
;	  (begin
;	    (format t "f0 at the first point of %l = %l\n" (item.name syl) (car result))
;	    (print result)))
      result)))


(define (baptize syl)
  "(baptize SYL)
Baptizes the given syllable by concatenating together the names
of it's daughters (phones). Useful in debugging."
  (item.set_name syl 
		 (let ((str ""))
		   (mapcar 
		    (lambda (x) 
		      (set! str (string-append str (item.feat x "name"))))
		    (item.daughters (item.relation syl 'SylStructure)))
		   str)))

(provide 'finnish_mv_int)







