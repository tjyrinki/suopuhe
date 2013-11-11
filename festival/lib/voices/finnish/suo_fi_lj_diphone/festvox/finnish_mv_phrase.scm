;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;                                                                       ;;
;;;          Department of General Linguistics / Suopuhe project          ;;
;;;                      University of Helsinki, FI                       ;;
;;;                  Copyright (c) 2000,2001,2002,2003                    ;;
;;;                        All Rights Reserved.                           ;;
;;;                                                                       ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;                                                                       ;;
;;; Authors:                                                              ;;
;;;                                                                       ;;
;;;          Martti Vainio                                                ;;
;;;  e-mail: martti.vainio@helsinki.fi                                    ;;
;;; address: Department of General Linguistics                            ;;
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
;;;                                                                       ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;  Phrase break prediction
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

(require 'finnish_lex) ;; causers of initial doubling are in a word list

;; t‰llekin pit‰isi opettaa listaintonaatio
(set! finnish_phrase_cart_tree
'
;; phrasal break
((R:Token.parent.pbreak is PB)
 ((PB))
 ;; list break
 ((R:Token.parent.pbreak is LB)
  ((LB))
  ((R:Token.parent.pbreak is BB)
   ((BB))
   ((lisp_token_end_punc in ("?" "." ":"))
    ((BB))
    ((R:Token.parent.pbreak is B)
     ((B))
     ((lisp_token_end_punc in (";" ","))
      ((B))
      ((n.name is 0)  ;; end of utterance
       ((BB))
       ((NB))))))))))

  
(define (word_has_break? word)
  "(word_has_break? WORD)
True, if the WORD is followed by # or ##  \"phone\"."
  (let ((final
	 (item.relation
	  (item.last_leaf
	   (item.relation word 'SylStructure))
	  'Segment)))
    (and (item.next final)
	 (string-matches (item.name (item.next final)) "^#+$"))))

(define (distance_to_next_pause word)
  "(distance_to_next_pause WORD)
Counts the distance from the end of the word to the next pause in syllables."
  (cond
   ((not (item.next word)) 
    0)
   ((word_has_break? word)      
    0)
   (t
    (+ (syllables_in_word (item.daughter1 (item.relation word 'SylStructure)))
       (distance_to_next_pause (item.next word))))))

(define (syllables_in_word SYL)
  "(syllables_in_word SYL)
Counts the number of syllables in a given word. Actually counts the numebr
of syllables from a given syllable to the end of word."
  (if (item.next SYL) 
      (+ 1 (syllables_in_word (item.next SYL)))
      1)) 

(define (distance_from_prev_pause word)
  "(distance_from_prev_pause WORD)
Distance (in syllables) from the previous word."
  (cond
   ((not (item.prev word))
    0)
   ((word_has_break? (item.prev word))
    0)
   (t
    (+
     (syllables_in_word
      (item.daughter1 (item.relation (item.prev word) 'SylStructure)))
     (distance_from_prev_pause (item.prev word))))))


(define(insert_pause2 word pausetype)
  "(insert_pause2 WORDITEM)
 Insert a little break (pause segment) after the last segment in WORDITEM in UTT."

 (if hy_debug (print (string-append "Lis‰t‰‰n tauko (" pausetype ") " (item.name word) "-sanan per‰‰n.")))
  
  ;; sets the value pbreak feature, not too useful...
 (if (not (string-equal (item.feat word 'pbreak) "BB"))      
     (item.set_feat word 'pbreak pausetype))
  ; ... since the trick is done by adding a #
  (let ((lastseg (find_last_seg word))
        (silence (if (or (string-equal (item.feat word 'pbreak) "PB")
			 (string-equal (item.feat word 'pbreak) "LB"))
		     "##" ;; pikkutauko
		     "#")))
    (if lastseg
        (item.relation.insert 
         lastseg 'Segment (list silence) 'after))))

;(define (lingvistinen_tauon_esto sana)
;  "(lingvistinen_tauon_esto WORD)
;Est‰‰ tauon tulemisen t‰h‰n kohtaan...
;Ei juuri implementoitu viel‰...
;Pit‰‰ v‰h‰n mietti‰...
;Mit‰ tehd‰‰n sanatasolla ja mik‰ muualla?"
;  (let ((thispos (item.feat sana 'pos))
;	(nextpos (if (item.next sana)
;		     (item.feat (item.next sana) 'pos)
;		     nil)))
;    
;    (cond
;     ((and (string-equal thispos "adjective" nextpos "noun"))
;      ;;(print "ei taukoa a+n v‰liin!")
;      t)
;     ((and (string-equal thispos "noun" nextpos "noun"))
;      (print "ei taukoa n+n v‰liin!")
;      t)
;     (t
;      ;;(print (string-append thispos " + " nextpos))
;      nil))))


(define (find_phrase_break word)
  "(find_phrase_break WORD)
If the distance from previous break to the next break is
long enough (now 25 syllables), try to add more pauses
in between. New pauses are added between certain word classes."
  (cond
   ;; something went wrong... return
   ((not word)
    nil)
   ;; less than N syllables (current 25 is an arbitrary choice): no break
   ((< (+ (distance_to_next_pause word) (distance_from_prev_pause word)) 
	     25)
    (find_phrase_break (skip_pause word)))
   ;; break needed
   (t
    (if hy_debug (format t 
			 "Trying to add a break after \"%s\"\n"
			 (upcase (item.name word))))
    
    (find_phrase_break (skip_pause (add_linguistic_break word))))))


(define (add_linguistic_break word)
  "(add_linguistic_break WORD)
Adds a phrase break after the word, if the context is appropriate.
If the word is final return nil. If there already is a pause
return the word itself. Also is pause is added, return the word itself.
If pause couldn't be added, try the same with the next word. Simple!"  
(if (not (item.next word))
      nil     
      (let ((thispos (item.feat word 'pos))
	    (nextpos (item.feat (item.next word) 'pos))) 
	;; (print (string-append thispos "#" nextpos " paired"))
	(cond
	 ;; we already have a pause, so let's not try to add one...
	 ((word_has_break? word)
	  word)
	 ;; not far enough from the phrase beginning
	 ((or (not (item.prev word))
	      (word_has_break? (item.prev word)))
	  (add_linguistic_break (item.next word)))
	 ;; put a break after a PoStPosition
	 ((string-equal thispos "psp")
	  (print (string-append " Pause after PSP \""(item.name word)"\".")) 
	  (insert_pause2 word "B")
	  word)
	 ;; (NOUN|NUM|PROP) # V
	 ((and (or (string-equal thispos "adjective")
		   (string-equal thispos "noun")
		   (string-equal thispos "num")
		   (string-equal thispos "prop"))
	       (string-equal nextpos "verb"))
	  (format t "\nInserted pause after in N#V postition after %s."
		  (upcase (item.name word)))
	  (insert_pause2 word "PB")
	  word)
	 ;; NUM # PROP
	 ((and (or (string-equal thispos "num")
		   (string-equal thispos "noun"))
		   (string-equal nextpos "prop"))
	  (format t "\nInserted pause after in NUM#PROP postition after %s."
		  (upcase (item.name word)))
	  (insert_pause2 word "PB")
	  word)

	 ;; NOUN # CC NOUN 
	 ((and (string-equal thispos "noun")
	       (string-equal nextpos "conj")
	       (item.next (item.next word))
	       (string-equal (item.feat (item.next (item.next word)) 'pos) "noun"))
	  (print (string-append "Inserted pause between NOUN \"" 
				(item.name word) 
				"\" CONJ and \"" 
				(item.name (item.next word)) "\"."))
	  (insert_pause2 word "PB")
	  word)
	 ;; ADJ # CC ADJ 
	 ((and (string-equal thispos "adjective")
	       (string-equal nextpos "conj")
	       (item.next (item.next word))
	       (string-equal (item.feat 
			      (item.next (item.next word)) 'pos) "adjective"))
	  (print (string-append "Inserted pause between ADJ \"" 
				(item.name word) 
				"\" CONJ and \"" 
				(item.name (item.next word)) "\"."))
	  (insert_pause2 word "PB")
	  word)

	 ;; ADJ # ADJ 
	 ((and (string-equal thispos "adjective")
	       (string-equal nextpos "adjective"))

	  (print (string-append "Inserted pause between ADJ \""
				(item.name word) "\" and ADJ \""
				(item.name (item.next word)) "\"."))
	  (insert_pause2 word "PB")
	  word)
	 ;; V # NUM
	 ((and (string-equal thispos "verb") 
	       (string-equal nextpos "num"))
	  
	  (insert_pause2 word "PB")
	  word)
	 (t
	  (add_linguistic_break (item.next word)))))))


(define (skip_pause WORD)
  "(skip_pause WORD)
Returns the word after the next pause or nil if there are no intersentence
pauses left."
  (if (item.next WORD)
      (if (word_has_break? WORD)
	  (item.next WORD)
	  (skip_pause (item.next WORD)))
      nil))

(define (suopuhe_add_break utterance)
  "(suopuhe_add_break UTT)
Adds pauses if necessary to a (overlong) utterance.
Applies some linguistic intelligence in the process..."
  (if hy_debug (format stderr "------- PAUSE-ADDING BEGINS ----------\n"))


  ;; converts punctions in breaks 
  (suopuhe_Pauses utterance)

  ;; suopuhe-mode can provide linguistic information.
  ;; we may be able to place additional breaks based on that information
  (if suopuhe
      (find_phrase_break (utt.relation.first utterance 'Word))
      ;; on the other hand initial doubling is only done (here)
      ;; when not using the suopuhe-mode.
      ;; we only hope that the xml input's creator took care of them...
      ;; A bit optimistic assumption, but doing things twice can cause
      ;; mayhem...
      (begin
	(if hy_debug ( format stderr "------ ALKUKAHDENNUS ALKAA -----------\n"))
	(initial_doubling (utt.relation.first utterance 'Word))))

  ;; these global variables contains the accent and intonation
  ;; commands for the fujisaki model
  ;; we want to calculate them only once
  ;; so these variables tell, whether they have already been counted
  ;; for the given utterance
  (set! suopuhe_accent nil)
  (set! suopuhe_phrase nil)
  
  utterance)

(define (suopuhe_Pauses utt)
  "(suopuhe_Pauses utt)
Converts the punctuation marks into appropriate pauses.
Does the list intonation too."
  (let ((words (utt.relation.items utt 'Word))
	lastword)
    (if words
        (begin
          (insert_initial_pause utt)   ;; always have a start pause
          (set! lastword (car (last words)))
          (mapcar
           (lambda (w)
             (let ((pbreak (item.feat w "pbreak"))
                   (emph (item.feat w "R:Token.parent.EMPH")))

               (cond
		((equal? w lastword)
		 ;; last word is followed by a double pause
		 ;; this is to reduce the effect of an annoying syntesis 
		 ;; feature...
		 (insert_pause utt w)
		 (insert_pause utt w))
                ((or (string-equal "BB" pbreak)
		     (string-equal "LB" pbreak)
		     (string-equal "PB" pbreak)
		     (string-equal "B" pbreak))
		 ;; list intonation
		 (let ((a (equal? (item.feat w "p.p.lisp_token_end_punc") ","))
		       (b (equal? (item.feat w "p.lisp_token_end_punc") ","))
		       (c (equal? (item.feat w "lisp_token_end_punc") ","))
		       (d (equal? (item.feat w "n.lisp_token_end_punc") ","))
		       (e (equal? (item.feat w "n.n.lisp_token_end_punc") ",")))
		   (cond 
		    ((or (and b c)
			 (and c d)
			 (and c (equal? (item.feat w "n.n.pos") "COORD")))
		     (set! pbreak "LB"))))

		 (insert_pause2 w pbreak)))))
           words)))
    utt))

(define (initial_doubling WORD)
  "(initial_doubling WORD)
Checks wheter the current WORD can triggers initial doubling on the
next word or not."
  (let ((name (if WORD (downcase (item.name WORD) nil))))
    (if (and (item.next WORD) 
	     (not (word_has_break? WORD))
	     ;; (not suopuhe) ;; suopuhe-mode deal this personally ???
	     (word_list_entry? name doubler_words))
	(begin
	  (if hy_debug 
	      (format stderr 
		      "%s triggers initial doubling (word list)\n"
		      name))
	  (set! next_initial (item.next (item.relation (item.last_leaf (item.relation WORD 'SylStructure)) 'Segment)))
	  
	  (let ((char (item.name next_initial)))
	    (if hy_debug (print (string-append char " => " char ":")))
	    (cond ;; the following consonants are doubled:
	     ((string-equal "b" char)
	      (item.set_name next_initial "b:"))
	     ((string-equal "d" char)
	      (item.set_name next_initial "d:"))
	     ((string-equal "g" char)
	      (item.set_name next_initial "g:"))
	     ((string-equal "h" char)
	      (item.set_name next_initial "h:"))
	     ((string-equal "j" char)
	      (item.set_name next_initial "j:"))
	     ((string-equal "k" char)
	      (item.set_name next_initial "k:"))
	     ((string-equal "l" char)
	      (item.set_name next_initial "l:"))
	     ((string-equal "m" char)
	      (item.set_name next_initial "m:"))
	     ((string-equal "n" char)
	      (item.set_name next_initial "n:"))
	     ((string-equal "p" char)
	      (item.set_name next_initial "p:"))
	     ((string-equal "r" char)
	      (item.set_name next_initial "r:"))
	     ((string-equal "s" char)
	      (item.set_name next_initial "s:"))
	     ((string-equal "S" char)
	      (item.set_name next_initial "S:"))
	     ((string-equal "t" char)
	      (item.set_name next_initial "t:"))
	     ((string-equal "v" char)
	      (item.set_name next_initial "v:"))))))
    (if (item.next WORD)
	(initial_doubling (item.next WORD)))))

 




(provide 'finnish_mv_phrase)



