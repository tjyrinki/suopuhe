;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;                                                                       ;;
;;;          Department of General Linguistics / Suopuhe project          ;;
;;;                      University of Helsinki, FI                       ;;
;;;                       Copyright (c) 2000-2003                         ;;
;;;                         All Rights Reserved.                          ;;
;;;                                                                       ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;                                                                      ;;;
;;;                                                                      ;;;
;;;  Author: Nicholas Volk                                               ;;;
;;;  e-mail: nvolk@ling.helsinki.fi                                      ;;;
;;; address: Department of General Linguistics                           ;;;
;;;          PL 9 (Siltavuorenpenger 20A)                                ;;;
;;;          00014 University of Helsinki                                ;;;
;;;          FINLAND                                                     ;;;
;;;                                                                      ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;  Finnish LTS-rules, which fake the lexicon
;;; (somewhat similar to the Festival Spanish)
;;;
;;;  This file contains the LTS rules for Finnish.
;;;  The LTS rules also syllabify and mark the stressed syllables

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

(require 'finnish_lex) ;; POS (content vs. function) information is in lex


(define (finnish_lts word)
  "(finnish_lts WORD) 
Creates the lexical entry for the given WORD."
  (if hy_debug (format stderr "Creating the lexical entry for %s.\n" word))

  ;; in suopuhe-mode:
  ;; reverse the stacks, if that has not been done yet...
  (if (and suopuhe
	   (not pos_reversed))
      (begin
	(set! suopuhe_accent_stack (reverse suopuhe_accent_stack))
	(set! suopuhe_pos_stack (reverse suopuhe_pos_stack))
	(set! pos_reversed t)))


  ;; check the string
  ;; if there are unknown characters, replace them with "tuntematonmerkki"
  ;; ("unknowncharacter")
  (set! word 
	(let ((new "")
	      (state 0) ;; 0: initial state 1: prev is ordinary  2: unknown prev
	      char)
	  (while (> (string-length word) 0)
		 (begin
		   (set! char (substring word 0 1))
		   (set! word (substring word 1 (- (string-length word) 1)))
		   (cond 
		    ;; ad hoc: LTS can't replace right bracket   \] "]" 
		    ((string-equal "\]" char)
		     (set! new 
			   (string-append 
			    (if new new "") 
			    (if (> state 0) "-" "") 
			    "oikeahakasulku"))
		     (set! state 2))
		    ((string-matches char "^[ a-zA-Z0-9\"\'\`\$%&\(\)\*\+\-~_\^/\\\?\}\{\@\.:!àáâãäöæçèéêëìíîïðñòóôõöøùúûüýþÀÁÂÃÄÅÆÇÐÈÉÊËÌÍÎÏÑÒÓÔÕÖØÙÚÛÜÝÞ]$" )
		     (set! new 
			   (string-append 
			    (if new new "") 
			    (if (= state 2) "-" "")
			    char))
		     (set! state 1))
		    (t
		     (set! new 
			   (string-append 
			    (if new new "") 
			    (if (> state 0) "-" "") 
			    "tuntematonmerkki"))
		     (set! state 2)))))
	  new))
  

  (cond
   ;; suopuhe mode gets hopefully the pos from a stack
   (suopuhe
    (let ((pos (pop_pos))
	  (acc (pop_accent)))
      (set! pos (if (string-equal "unknown" pos)
		    (cond
		     ((word_list_entry? word finnish_guess_coord)
		      "COORD")
		     ((word_list_entry? word finnish_guess_cop)
		      "COP")
		     ((word_list_entry? word finnish_guess_pron)
		      "PRON")
		     ((word_list_entry? word finnish_guess_pos)
		      "function")
		     (t
		      "content"))))
      ;; anyway... if word is defined as accentless we change
      ;; the POS in the lexical entry to FUNCTION overriding the
      ;; defined POS!
      ;; accented words get POS CONTENT on the same basis...
      (if (string-equal acc "no")
	  (set! pos "function")
	  (if (string-equal acc "yes")
	      (set! pos "content")))
      
      (let ((x (list word (pop_pos) (compound_stress word))))
	x)))
   ;; non-suopuhe mode
   ((word_list_entry? word finnish_guess_coord)
    (list word "COORD" (compound_stress word)))
   ((word_list_entry? word finnish_guess_cop)
    (list word "COP" (compound_stress word)))
   ((word_list_entry? word finnish_guess_pron)
    (list word "PRON" (compound_stress word)))
   ((word_list_entry? word finnish_guess_pos)
    (list word "function" (compound_stress word)))
   (t
    (list word "content" (compound_stress word)))))

(define (finnish_token_to_words token name)
  "(finnish_token_to_words token name)
A few simple ad hoc solutions for the most common simple T2W-problems.
It was much nicer to use an external Perl text normalizer than scheme."
  (cond
   ;; NAN: begins with a 0, thsus sequence of digit 
   ((string-matches name "^0[0-9]*$")
    (symbolexplode name))
   ;; hundereds of millions are read as digits
   ((string-matches name "^[0-9][0-9][0-9][0-9][0-9][0-9][0-9][0-9][0-9][0-9]+$")
    (symbolexplode name))
   ;; ordinary number (no inflections in scheme, use perl)
   ((string-matches name "^[1-9][0-9]*$")
    (list (finnish_number name)))
   ;; Too many initial consonants
   ((string-matches name "^[BCDFGHJKLMNPQRSTVWXZbcdfghjklmnpqrstvwxz][BCDFGHJKLMNPQRSTVWXZbcdfghjklmnpqrstvwxz][BCDFGHJKLMNPQRSTVWXZbcdfghjklmnpqrstvwxz][BCDFGHJKLMNPQRSTVWXZbcdfghjklmnpqrstvwxz].*$")
    (symbolexplode name))
   ;; separate consonant sequence from each other : "KGB" => "K G B"
   ((string-matches name "^\\(.*\-\\)?[BCDFGHJKLMNPQRSTVWXZbcdfghjklmnpqrstvwxz]+\\(\-.*\\)?$")
    (symbolexplode name))
   ;; do nothing
   (t
    (list name))))

(define (finnish_number int)
  "(finnish_number INT)
Converts the INT into a corresponding string.
Very simple. Does not handle cases." 
  (remove_head_and_tail_spaces (make_number int)))

(define (remove_head_and_tail_spaces text)
  "(remove_head_and_tail_spaces string)
Removes unspoken space sequences from the beginning and end of a give string."
  (if (not (string-equal (typeof text) "string"))
      (begin
	(set! text "The input was not a string!")))
  
  ;; remove the initial whitespaces
  (while (and (> (string-length text) 0)
	      (string-matches (substring text 0 1) "^[ \t\n\015]$"))
	 ;; \015 = CTRL-M = carriage return
	 (set! text (substring text 1 (- (string-length text) 1))))
  ;; remove the final whitespaces
  (while (and (> (string-length text) 0)
	      (string-matches 
	       (substring text (- (string-length text) 1) 1)
	       "^[ \t\n\015]$"))
	 (set! text (substring text 0 (- (string-length text) 1))))
  text)


(define (make_number int)
  "(make_number INT)
Splits numbers into 3-letter chunks, which are converted into a number.
Also adds an appropriate  quantifier (like thousands) to each triplet,
thus producing any int between 1 - 999999999."
  (let (head)
    (cond
     ((> (string-length int) 6)
      (set! head (substring int 0 (- (string-length int) 6)))
      (set! int (substring int (string-length head) 6))
      (if (string-equal head "1")
	  (string-append 
	   "miljoona "
	   (make_number int))
	  (string-append
	   (make_number2 head)
	   "miljoonaa "
	   (make_number int))))
     ((> (string-length int) 3)
      (set! head (substring int 0 (- (string-length int) 3)))
      (set! int (substring int (string-length head) 3))
      (cond
       ((string-equal head "000")
	(make_number int))
       ((or (string-equal head "1")
	    (string-equal head "001"))
	(string-append 
	 "tuhat "
	 (make_number int)))
       (t
	(string-append
	 (make_number2 head)
	 "tuhatta "
	 (make_number int)))))     
     (t
      (make_number2 int)))))

(define (make_number2 int)
  "(make_number2 INT)
Converts an up-to-three digit sequence into a number."

  (let (ones dozens hundreds output)
    (cond 
     ((= (string-length int) 3)
      (set! hundreds (substring int 0 1))
      (set! dozens (substring int 1 1))
      (set! ones (substring int 2 1)))
     ((= (string-length int) 2)
      (set! hundreds "0")
      (set! dozens (substring int 0 1))
      (set! ones (substring int 1 1)))
     (t
      (set! hundreds "0")
      (set! dozens "0")
      (set! ones (substring int 0 1))))
     ;; HUNDREDS
    (cond
     ((string-equal hundreds "0")
      (set! output ""))
     ((string-equal hundreds "1")
      (set! output "sata"))
     (t
      (set! output (string-append (number_list hundreds) "sataa"))))
    ;; DOZENS & ONES
    (cond 
     ((string-equal dozens "0")
      (set! output (string-append output (number_list ones))))
     ((string-equal dozens "1")
      (if (string-equal ones "0")
	  (set! output (string-append output "kymmenen"))
	  (set! output (string-append output (number_list ones) "toista"))))
     (t
      (set! output (string-append output 
				  (number_list dozens) "kymmentä"
				  (number_list ones)))))
    output))
    
       

(define (number_list digit)
  "(number_list DIGIT)
Returns the corresponding string for a given DIGIT"
  (cond 
   ((string-equal digit "9") "yhdeksän")
   ((string-equal digit "8") "kahdeksan")
   ((string-equal digit "7") "seitsemän")
   ((string-equal digit "6") "kuusi")
   ((string-equal digit "5") "viisi")
   ((string-equal digit "4") "neljä")
   ((string-equal digit "3") "kolme")
   ((string-equal digit "2") "kaksi")
   ((string-equal digit "1") "yksi")
   (t
    "")))


(define (finnish_tosyl_brackets phones accent)
  "(finnish_tosyl_brackets phones)
Takes a list of phones containing - as syllable boundary.  
Should we add a compound boundary? Probably...
Construct the Festival bracket structure. 
Used by finnish_lts-function."
  (let ((tavu nil) (syls nil) (p phones) (stress 0) (syltotal 1)
	(cvcvcvcv nil)
	(which 1) (secondary_stress 0))
    ;; lasketaan tavujen määrä
    (while p
	   (if (eq? '- (car p))
	       (begin
		 (set! tavu (reverse tavu))
		 ;; if the 3rd syllable has 1 "mora"
		 ;; the secondary stress goes to the 4th syllable
		 (if (and (= syltotal 3)
			  (= (mora2 tavu) 1) 
			  (or (string-matches (cadr p) 
					      "^[abdefghijklmnoprstuvy@7NT]$")
			      (string-matches (cadr p) "^[aeiouy@7]:?$" )))
		     ;; had but 1 mora
		     (begin
		       ;; cvcvcvcv takes care of 4-syl words
		       ;; with a short 3rd syl
		       (set! cvcvcvcv t)
		       (set! secondary_stress 4)))
		 (if (and (= secondary_stress 4)
			  (= syltotal 4)
			  (= (mora2 tavu) 1)
			  (or (string-matches ;; short
			       (cadr p)
			       "^[abdefghijklmnoprstuvy@7NT]$" )
			      (string-matches (cadr p) "^[aeiouy@7]:?$" )))
		     (set! secondary_stress 3))
		 
	       

		 (set! syltotal (+ syltotal 1)) ;; N:s tavu alkaa

		 (set! tavu nil))
	       ;; muuten lisää tavu
	       (set! tavu (cons (car p) tavu)))
	   (set! p (cdr p)))

    
    (set! p phones)
    (if (and (> syltotal 3) ;; secondary_stress is possible
	     (> accent 0)) ;; the word is stressed
	;; ON SECONDARY_STRESS
	(begin
	  ;; 4-tavuinen, jossa 3:s oli lyhyt omaa väärän arvon, korjataan:
	  (if (and (= syltotal 4)
		   cvcvcvcv)
	      (set! secondary_stress 3))
	  ;; tarpeeksi pitkä secondary_stresslliselle, mutta secondary_stress ei päällä:
	  (if (= secondary_stress 0) 
	      (set! secondary_stress 3)));; nyt aina kolmannelle tavulle
	(set! secondary_stress 0))

    (set! which 1)
    (while p
	   (set! tavu nil)
	   (set! stress accent)
	   ;; asetetaan secondary_stress, jos on tarvis
	   
	   (if (and (= which secondary_stress)
		    (> syltotal secondary_stress))
	       (begin
		 (set! secondary_stress (+ secondary_stress 2))
		 (set! stress 1)))
	   
	   (while (and p 
		       (not (eq? '- (car p))))
		  ;;;(set! name (list2string (cons (car p) syl)))
		  (set! tavu (cons (car p) tavu))
		  ;;;(item.set_name syl name)
		  (set! p (cdr p)))
	   (set! which (+ which 1))
	   (set! p (cdr p)) ;; hyppää '-':n yli
	   (set! accent 0) ;; jälkitavuille
	   (set! syls (cons (list (reverse tavu) stress) syls)))
    (reverse syls)))

(define (mora2 list_of_phones)
"(mora2 list_of_phones)
Counts the moras (morae?) in a given list of phones.
Does not support ambisyllabic long consonants."
(let ((onset t)
	(total 0)
	phone)
    
    (while list_of_phones
	   (set! phone (car list_of_phones))
	   ;; as long as we get Cs we're in the onset
	   (if (not (and onset
			 (string-matches phone "^[bdfghjklmnprstvNT]:?$" )))
	       (begin
		 (set! onset nil)
		 (if (string-matches phone "^[abdefghijklmnoprstuvy@7NT]:$" )
		     (set! total (+ total 2)) ;; long
		     (set! total (+ total 1))))) ;; short
	   (set! list_of_phones (cdr list_of_phones)))
    total))



(define (compound_stress compound)
  "(compound_stress WORD)
Handles the exception stress placement in compounds.
The few compounds supported are recognized by the '-' separator.
However secondary word stress did not show up in duration and intonation
models... We could also add boundary detection based on vowel positions
in neighboring syllables."

  (let ((suitable (string-matches compound "^\\([A-ZÅÄÖa-zåäö][A-ZÅÄÖa-zåäö]+\-\\)+[A-ZÅÄÖa-zåäö][A-ZÅÄÖa-zåäö]+$"))
	(word1 "")
	(word2 nil)) ;; word2 will be a list
    (if (not suitable) (set! word1 compound))
    (while (and (> (string-length compound) 0)
		suitable)
	   ;; compound marker has been found yet
	   (if (string-equal (substring compound 0 1) "-")
	       (begin
		 ;; behead the compound
		 (if (= (string-length compound) 1)
		     (set! compound "")
		     (set! compound (substring compound 1 (- (string-length compound) 1))))
		 (if (not (string-equal word1 ""))
		     (set! word2 
			   (append word2 (finnish_tosyl_brackets
					  (lts.apply 
					   (lts.apply 
					    (lts.apply word1 'normalize) 
					    'finnish_cv) 
					   'remove_unneeded) 2))))
		     
		 (set! word1 ""))
	       ;; else
	       (begin
		 (set! word1 (string-append word1 (substring compound 0 1)))
		 (if (= (string-length compound) 1)
		     (set! compound "")
		     (set! compound (substring compound 1 (- (string-length compound) 1))))))) ;; end of while

    (if word2
	(set! word2 (append word2
			    (finnish_tosyl_brackets
			     (lts.apply 
			      (lts.apply 
			       (lts.apply word1 'normalize) 
			       'finnish_cv) 
			      'remove_unneeded) 2)))
	(set! word2 (finnish_tosyl_brackets
		     (lts.apply 
		      (lts.apply 
		       (lts.apply word1 'normalize) 
		       'finnish_cv) 
		      'remove_unneeded) 2)))
    word2))

;;;=======================================================================

(lts.ruleset
 normalize
 ; sets
 ( ( NUMERO 0 1 2 3 4 5 6 7 8 9 ) 
   ( POIS "\"" "\'" "\`" )

   ( CON b c d f g h j k l m n p q r s t v w x z 
	 B C D F G H J K L M N P Q R S T V W X Z)
   ( VOW a e i o u y ä ö 
	 A E I O U Y Ä Ö)
   ( LET A B C D E F G H I J K L M N O P Q R S T U V W X Y Z Å Ä Ö
         a b c d e f g h i j k l m n o p q r s t u v w x y z å ä ö )
   )
;
; ;rules
 (

  ;; a lone " 
  ( # [ "\"" ] # = l a i n a u s m e r k k i )
  ( # [ POIS ] # = h e i t t o m e r k k i )
  ; seuraavat pois:
  ( [ POIS ] = )
  ( [ " " ] = ) ;; tokenisoija ei vielä jaa merkkijonoa wordlistaksi...
  
  ( [ 1 ] = y k s i  )
  ( [ 2 ] = k a k s i  )
  ( [ 3 ] = k o l m e  )
  ( [ 4 ] = n e l j @  )
  ( [ 5 ] = v i i s i  )
  ( [ 6 ] = k u u s i  )
  ( [ 7 ] = s e i t s e m @ n  )
  ( [ 8 ] = k a h d e k s a n  )
  ( [ 9 ] = y h d e k s @ n  )
  ( [ 0 ] = n o l l a  )
  
  ( [ "#" ] = r i s u a i t a )
  ( [ "$" ] = d o l l a r i )
  ( [ "%" ] = p r o s e n t t i )
  ( [ "&" ] =  e t  )
  
  ;; should these be spoken (SayText "(word)")
  ( [ "(" ] = v a s e n  s u l k u )
  ( [ ")" ] = o i k e a  s u l k u )

  ( [ * ] = a s t e r i s k i )
  ( [ + ] = p l u s ) 
  ;( [ "\/" = j a k o v i i v a )
   

  ( # [ - ] # = v i i v a )
  ( LET [ - ] LET = ) ;; morphemeboundary
  ( LET [ - ] # = ) ;; word- => word
  ( # [ - ] LET = ) ;; -word => word
  ( [ - ] NUMERO = m i i n u s - )
  
;;  ( # [ - ] = )
;;  ( [ " " - " " ] = )  
;;  ( " " [ - ] = )
;;  ( [ - ] # = )
;;  ( [ - ] " " = )

  ( [ - ] = v i i v a )

  ( [ "~" ] = a a l t o v i i v a )
  ( [ _ ] = a l a v i i v a )
  ( [ "^" ] = h a t t u )
  ( [ "!" ] = h u u t o m e r k k i )
  ( [ "/" ] = k a u t t a )
  ( [ "\\" ] = k e n o v i i v a ) ;; kenoviiva kannattaa laventaa etukäteen..
  ( [ "?" ] = k y s y m y s m e r k k i ) 
  ( [ \] ] = o i k e a  h a k a s u l k u ) ;; finnish_lts ongelma
  ( [ "}" ] = o i k e a  k a a r i s u l k u )
  ( [ "=" ] = o n  y h t a k u i n )
  ( [ "<" ] = p i e n e m p i  k u i n )
  ( [ "," ] = p i l k k u )
  ( [ "£" ] = p u n t a )
  ( [ ";" ] = p u o l i p i s t e )
  ( [ "|" ] = p y s t y v i i v a )
  ( [ ">" ] = s u u r e m p i  k u i n )
  ( [ "[" ] = v a s e n  h a k a s u l k u ) ;; finnish_lts ongelma
  ( [ "{" ] = v a s e n  k a a r i s u l k u ) ;; "{}" toimii oudosti...
  ( [ "@" ] =  @ t )

  ( [ "." ] = p i s t e  ) ;; token2words temppuilee
  ( [ ":" ] i n # = )
  ( [ ":" ] = k a k s o i s p i s t e )
  ( [ " " ] = " " ) ;; separeted chars use this
  


  ;  # SH-äänne
  ( [ s c h ] = S ) ; schubert
  ( [ S c h ] = S )
  ( t [ s h ] = s ) ; tshetsheeni
  ( T [ s h ] = s )
  ( [ s h ] CON = S ) ; pushkin
  ( [ S h ] = S )
  ( VOW VOW [ s h ] = s h ) ; talOUSHallinto
  ( [ s h ] y = s h ) ; 
  ( [ s h ] ä = s h ) ; mieshän
  ( [ s h ] ö = s h ) ; ykköshörhö
  ( i l a [ s h ] = s h ) ; sotilashenkilö
  ( l l i [ s h ] = s h ) ; paikallishallinto
  ( s k u [ s h ] = s h ) ; keskushallinto
  ( n n u [ s h ] = s h ) ;
  ( t u [ s h ] = s h ) ; opetushallitus
  ( y [ s h ] = s h ) ; yrityshautomo
  ( ä [ s h ] = s h ) ; seiväshyppy
  ( ö [ s h ] = s h ) ; ykköshenkilö
  ( # [ s h ] = S ) ; shakki
  ( [ s h ] # = S ) ; bush
  ( e i [ s h ] = s h ) ; yhteishenki
  ( [ s h ] i # = S ) ; takeshi
  ( w a [ s h ] i n g t = S S ) ; washington
  ( [ s h ] o w  = S )
  ( m u [ s h ] = S ) ; kokoomushan
  ( i h m i [ s h ] = s h ) ; ihmishenki 
  ( [ s h ] = S )

;;  ( [ a h l ] = a a l )
  ( [ t z ] = t s )
 



  ( [ à ] = a ) ( [ À ] = a )
  ( [ á ] = a ) ( [ Á ] = a )
  ( [ â ] = a ) ( [ "Â" ] = a )
  ( [ ã ] = a ) ( [ Ã ] = a )
  ( [ æ ] = @ ) ( [ Æ ] = @ )
  ( [ ç ] = s ) ( [ Ç ] = s )
  ( [ ð ] = t h ) ( [ Ð ] = t h )
  ( [ è ] = e ) ( [ È ] = e )
  ( [ é ] = e ) ( [ É ] = e ) 
  ( [ ê ] = e ) ( [ Ê ] = e )
  ( [ ë ] = e ) ( [ Ë ] = e )
  ( [ ì ] = i ) ( [ Ì ] = i )
  ( [ í ] = i ) ( [ Í ] = i )
  ( [ ï ] = i ) ( [ Ï ] = i )
  ( [ î ] = i ) ( [ Î ] = i )
  ( [ ñ ] = n j ) ( [ Ñ ] = n j )
  ( [ ø ] = 7 ) ( [ Ø ] = 7 )
  ( [ ò ] = o ) ( [ Ò ] = o )
  ( [ ó ] = o ) ( [ ó ] = o )
  ( [ ô ] = o ) ( [ Ô ] = o )
  ( [ õ ] = o ) ( [ Õ ] = o )
  ( [ ß ] = s s )
  ( [ þ ] = t h ) ( [ Þ ] = t h )
  ( [ ù ] = u ) ( [ Ù ] = u )
  ( [ ú ] = u ) ( [ Ù ] = u )
  ( [ û ] = u ) ( [ Û ] = u )
  ( [ ü ] = y ) ( [ Ü ] = y )
  ( [ ý ] = y ) ( [ Ý ] = y ) 
  ( [ ÿ ] = y )

  ; "lowercase"
  ( [ a ] = a ) ( [ b ] = b ) 
  ( [ A ] = a ) ( [ B ] = b )

  ( [ c ] = c ) 
  ( [ C ] = c )

  ( [ d ] = d ) ( [ e ] = e )
  ( [ D ] = d ) ( [ E ] = e ) 
  ( [ f ] = f ) ( [ g ] = g ) ( [ h ] = h ) ( [ i ] = i ) ( [ j ] = j )
  ( [ F ] = f ) ( [ G ] = g ) ( [ H ] = h ) ( [ I ] = i ) ( [ J ] = j )

  ( [ K ] = k ) ( [ L ] = l ) ( [ M ] = m ) 
  ( [ N ] = n )  
  ( [ k ] = k ) ( [ l ] = l ) ( [ m ] = m )
 
  ( [ n ] = n ) 

  ( [ O ] = o ) ( [ P ] = p ) ( [ Q ] = q ) ( [ R ] = r ) ( [ S ] = s ) 
  ( [ o ] = o ) ( [ p ] = p ) ( [ q ] = q ) ( [ r ] = r ) ( [ s ] = s ) 
  ( [ T ] = t ) ( [ U ] = u ) ( [ V ] = v ) ( [ W ] = w ) ( [ X ] = x ) 
  ( [ t ] = t ) ( [ u ] = u ) ( [ v ] = v ) ( [ w ] = w ) ( [ x ] = x ) 
  ( [ Y ] = y ) ( [ Z ] = z ) ( [ Å ] = å ) ( [ Ä ] = @ ) ( [ Ö ] = 7 )
  ( [ y ] = y ) ( [ z ] = z ) ( [ å ] = å ) ( [ ä ] = @ ) ( [ ö ] = 7 )

  )) 

(lts.ruleset
 finnish_cv
 ; sets
 (( LAITONA  e o y @ 7 )
  ( LAITONEI a o y @ 7 )
  ( LAITONOU a e y @ 7 )
  ( LAITONY7 a e o u @ )
  ( LAITON@  a e u o 7 )
  ( VOW a e i o u y @ 7 ö å )
  ( CON b c d f g h j k l m n p q r s t v w x z )
  )
 ;rules
 (
;  ( # [ s h ] = __ S )
;  ( # [ s c h ] = __ S )
  ( # [ t h ] = __ T )
  ( [ t h ] # = __ T ) ;; cheat
  ( [ t h ] CON = __ T )

  ( [ a - a ] = _ a - a )
  ( [ e - e ] = _ e - e )
  ( [ i - i ] = _ i - i )
  ( [ o - o ] = _ o - o )
  ( [ u - u ] = _ u - u )
  ( [ y - i ] = _ y - y )
  ( [ @ - @ ] = _ @ - @ )
  ( [ 7 - 7 ] = _ 7 - 7 )

  
  ( [ a ] LAITONA  = _ a - )
  ;; ( [ e ] a # = _ e )
  ( [ e ] LAITONEI = _ e - )
  ;; ( [ i ] a # = _ i )
  ( [ i ] LAITONEI = _ i - )
  ;;( [ o ] a # = _ o )
  ( [ o ] LAITONOU = _ o - )
  ( [ u ] LAITONOU = _ u - )
  ( [ y ] LAITONY7 = _ y - )
  ;; ( [ 7 ] ä # _ 7 )
  ( [ 7 ] LAITONY7 = _ 7 - )
  ( [ @ ] LAITON@  = _ @ - )

  ;; some assimilations
  ( VOW [ n m ] VOW = __ m: )
  ( VOW [ n ] p VOW = m )
  ( [ n g ] VOW = __ N: )
  ( [ n g ] = __ N )
  ( [ n ] k = __ N )


 
  ( [ a a ] = _ a: )
  ( [ b b ] = "__" b: )
  ( [ c c ] = "__" k: ) ( [ c k ] = k: )
  ( [ d d ] = "__" d: )
  ( [ e e ] = _ e: )
  ( [ f f ] = "__" f: )
  ( [ g g ] = "__" g: )
  ( [ h h ] = "__" h: )
  ( [ i i ] = _ i: )
  ( [ j j ] = "__" j: )
  ( [ k k ] = "__" k: )
  ( [ l l ] = "__" l: )
  ( [ m m ] = "__" m: )
  ( [ n n ] = "__" n: )
  ( [ o o ] = _ o: )
  ( [ p p ] = "__" p: )
  ( [ r r ] = "__" r: )
  ( [ s s ] = "__" s: )
  ( [ S S ] = "__" S: )
  ( [ t t ] = "__" t: )
  ( [ u u ] = _ u: )
  ( [ v v ] = "__" v: )
  ( [ y y ] = _ y: )
  ( [ @ @ ] = _ @: )
  ( [ 7 7 ] = _ 7: )

  ( [ a ] = _ a ) 
  ( [ b ] = __ b ) 
  ( [ c ] e = __ s )
  ( [ c ] = __ k ) 
  ( [ d ] = __ d ) 
  ( [ e ] = _ e )
  ( [ f ] = __ f ) 
  ( [ g ] = __ g ) 
  ( [ h ] = __ h ) 
  
  ( [ i ] = _ i ) 
  ( [ j ] = __ j )
  ( [ k ] = __ k ) 
  ( [ l ] = __ l ) 
  ( [ m ] = __ m ) 
  ( [ n ] = __ n ) 
  ( [ o ] = _ o )
  ( [ p ] = __ p ) 
  ( # [ q u ] = k v )
  ( [ q u ] = - k v )
  ( # [ q ] = k v )
  ( [ q ] = - k v ) ; frek-venssi 
  ( [ r ] = __ r ) 
  ( [ s ] = __ s ) 
  ( [ t ] = __ t )
  ( [ u ] = _ u ) 
  ( [ v ] = __ v ) 
  ( [ w ] = __ v )
  ( [ x ] = __ k __ s ) ; tak-si 
  ( [ y ] = _ y )
  ( # [ z ] = t s )
  ( [ z ] = - t s )
  ( [ å ] = _ o ) 
  ( [ @ ] = _ @ ) 
  ( [ 7 ] = _ 7 ) 
  ( [ - ] = - ) 
  ( [ L ] = __ L ) ;; this is our light /l/
  ( [ S ] = __ S ) ;; this is our palatal /s/
  ( [ " "+ ] = " " )
))

(lts.ruleset remove_unneeded
 ( ; sets
  ( CON b  d  f  g  h  j  k  l  m  n  p  r  s  t  v  N  T  S  L
	b: d: f: g: h: j: k: l: m: n: p: r: s: t: v: N: T: S: L:)
  ( VOW a e i o u y @ 7 a: e: i: o: u: y: @: 7: )
  ( LONG a: e: i: o: u: y: @: 7: )
  ( EOS # " " )
  )
 ( ; rules
  ( [ - _ ] = - )
  ( [ - __ ] = - )

  ( EOS __ CON __ CON __ CON [ __ ] = )
  ( EOS __ CON __ CON [ __ ] = )
  ( EOS __ CON [ __ ] = )
  ( EOS [ __ ] = )
  ( [ __ ] CON _ VOW = - )  
  ( [ __ ] = )

  ( EOS [ _ ] = )
  ( CON [ _ ] = )
  ( - [ _ ] = )
  ( LONG [ _ ] = - )
  ( [ _ a _ ] = a - )
  ( [ _ e _ ] = e - )
  ( [ _ i _ ] = i - )
  ( [ _ o _ ] = o - )
  ( [ _ u _ ] = u - )
  ( [ _ y _ ] = y - )
  ( [ _ @ _ ] = @ - )
  ( [ _ 7 _ ] = 7 - )
  ( [ _ ] = )

  ( [ a: ] = a: )
  ( [ b: ] __ CON = b ) ( [ b: ] # = b ) ( # __ [ b: ] = b ) ( [ b: ] = b: )
  ( [ d: ] __ CON = d ) ( [ d: ] # = d ) ( # __ [ d: ] = d ) ( [ d: ] = d: )
  ( [ e: ] = e: )
  ( [ f: ] __ CON = f ) ( [ f: ] # = f ) ( # __ [ f: ] = f ) ( [ f: ] = f: )
  ( [ g: ] __ CON = g ) ( [ g: ] # = g ) ( # __ [ g: ] = g ) ( [ g: ] = g: )
  ( [ h: ] __ CON = h ) ( [ h: ] # = h ) ( # __ [ h: ] = h ) ( [ h: ] = h: )
  ( [ i: ] = i: )
  ( [ j: ] __ CON = j ) ( [ j: ] # = j ) ( # __ [ j: ] = j )  ( [ j: ] = j: )
  ( [ k: ] __ CON = k ) ( [ k: ] # = k ) ( # __ [ k: ] = k )  ( [ k: ] = k: )
  ( [ l: ] __ CON = l ) ( [ l: ] # = l ) ( # __ [ l: ] = l )  ( [ l: ] = l: )
  ( [ m: ] __ CON = m ) ( [ m: ] # = m ) ( # __ [ m: ] = m )  ( [ m: ] = m: )
  ( [ n: ] __ CON = n ) ( [ n: ] # = n ) ( # __ [ n: ] = n )  ( [ n: ] = n: )
  ( [ N: ] __ CON = N ) ( [ N: ] # = N ) ( # __ [ N: ] = N )  ( [ N: ] = N: )
  ( [ o: ] = o: )
  ( [ p: ] __ CON = p ) ( [ p: ] # = p ) ( # __ [ p: ] = p )  ( [ p: ] = p: )
  ( [ r: ] __ CON = r ) ( [ r: ] # = r ) ( # __ [ r: ] = r )  ( [ r: ] = r: )
  ( [ s: ] __ CON = s ) ( [ s: ] # = s ) ( # __ [ s: ] = s )  ( [ s: ] = s: )
  ( [ t: ] __ CON = t ) ( [ t: ] # = t ) ( # __ [ t: ] = t )  ( [ t: ] = t: )
  ( [ u: ] = u: )
  ( [ v: ] __ CON = v ) ( [ v: ] # = v ) ( # __ [ v: ] = v )  ( [ v: ] = v: )
  ( [ y: ] = y: )
  ( [ @: ] = @: )
  ( [ 7: ] = 7: )
  ( [ L: ] __ CON = L ) ( [ L: ] # = L ) ( # __ [ L: ] = L )  ( [ L: ] = L: ) ;; our light /l/
  ( [ S: ] __ CON = S ) ( [ S: ] # = S ) ( # __ [ S: ] = S )  ( [ S: ] = S: ) ;; our palatal /s/
  ( [ T: ] __ CON = T ) ( [ T: ] # = T ) ( # __ [ T: ] = T )  ( [ T: ] = T: )
  ( [ D: ] __ CON = D ) ( [ D: ] # = D ) ( # __ [ D: ] = D )  ( [ D: ] = D: )

  ;;; short (these are the original rules)
  ( [ a ] = a ) ( [ b ] = b ) ( [ d ] = d ) ( [ e ] = e )
  ( [ f ] = f ) ( [ g ] = g ) ( [ h ] = h ) ( [ i ] = i ) ( [ j ] = j )
  ( [ k ] = k ) ( [ l ] = l ) ( [ m ] = m ) ( [ n ] = n ) ( [ o ] = o )
  ( [ p ] = p ) ( [ q ] = q ) ( [ r ] = r ) ( [ s ] = s ) ( [ t ] = t )
  ( [ u ] = u ) ( [ v ] = v ) ( [ y ] = y )
  ( [ @ ] = @ ) ( [ ö ] = ö ) ( [ 7 ] = 7 ) ( [ z ] = z )
  ( [ - ] = - ) 
  ( [ L ] = L )  ;; this is our light /l/
  ( [ S ] = S ) ;; this is our palatal /s/
  ( [ T ] = T )
  ( [ D ] = D )
  ( [ N ] = N )
  ( [ _ ] = _ ) ( [ __ ] = __ ) 
  ( [ " " ] = "" ) ;; cheat for LSEQs
  ))
  

(provide 'finnish_lts)



