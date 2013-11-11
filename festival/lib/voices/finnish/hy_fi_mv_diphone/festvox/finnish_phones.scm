;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;                                                                       ;;
;;;          Department of General Linguistics / Suopuhe project          ;;
;;;                      University of Helsinki, FI                       ;;
;;;                     Copyright (c) 2000,2001,2002                      ;;
;;;                        All Rights Reserved.                           ;;
;;;                                                                       ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;                                                                       ;
; Authors:                                                              ;
;                                                                       ;
;          Martti Vainio                                                ;
;  e-mail: martti.vainio@helsinki.fi                                    ;
; address:                                                              ;
;                                                                       ;
;          Nicholas Volk                                                ;
;  e-mail: nvolk@ling.helsinki.fi                                       ;
; address: Department of General Linguistics                            ;
;          PL 9 (Siltavuorenpenger 20A)                                 ;
;          00014 University of Helsinki                                 ;
;          FINLAND                                                      ;
;                                                                       ;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Phoneset definition 
;;;
;;; Finnish phonemes according to a simplified WorldBet alphabet


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

(defPhoneSet finnish
  (
   (vc + - 0)               ;; vowel/consonant
   (vlng s l 0)             ;; vowel length
   (vheight 1 2 3 0)        ;; vowel height
   (vfront 1 2 3 0)         ;; vowel frontness
   (vrnd + - 0)             ;; vowel rounding
   (ctype s f a n l r 0)    ;; articulation type
   (cplace l a p b d v g 0) ;; articulation place
   (cvox + - 0)             ;; consonant voicing
   (nasaali + - 0)          ;; nasal
   (klusiili + - 0)         ;; plosive
   (frikatiivi + - 0)       ;; fricative
   (sibilantti + - 0)       ;; sibilant
   (likvida + - 0)          ;; liquid
   (lateraali + - 0)        ;; lateral
   (puolivokaali + - 0)     ;; semivowel
   (clng s l 0)             ;; consonant length
   (cplace2 l a r 0)        ;; Lips Alveolar Rest 0
   )
  (
   ;; VOWELS
   (a   +   s   3   3   -   0   0   0 0 0 0 0 0 0 0 0 0) ;; ei-worldbet
   (e   +   s   2   1   -   0   0   0 0 0 0 0 0 0 0 0 0)
   (i   +   s   1   1   -   0   0   0 0 0 0 0 0 0 0 0 0)
   (o   +   s   2   3   +   0   0   0 0 0 0 0 0 0 0 0 0)
   (u   +   s   1   3   +   0   0   0 0 0 0 0 0 0 0 0 0)
   (y   +   s   1   1   +   0   0   0 0 0 0 0 0 0 0 0 0)
   (@   +   s   3   1   -   0   0   0 0 0 0 0 0 0 0 0 0) ;; ä
   (7   +   s   2   1   +   0   0   0 0 0 0 0 0 0 0 0 0) ;; ö 
   (&   +   s   2   2   -   0   0   0 0 0 0 0 0 0 0 0 0);; schwa
   (a:  +   l   3   3   -   0   0   0 0 0 0 0 0 0 0 0 0) ;; ei-worldbet
   (e:  +   l   2   1   -   0   0   0 0 0 0 0 0 0 0 0 0)
   (i:  +   l   1   1   -   0   0   0 0 0 0 0 0 0 0 0 0)
   (o:  +   l   2   3   +   0   0   0 0 0 0 0 0 0 0 0 0)
   (u:  +   l   1   3   +   0   0   0 0 0 0 0 0 0 0 0 0)
   (y:  +   l   1   1   +   0   0   0 0 0 0 0 0 0 0 0 0)
   (@:  +   l   3   1   -   0   0   0 0 0 0 0 0 0 0 0 0) ;; ä
   (7:  +   l   2   1   +   0   0   0 0 0 0 0 0 0 0 0 0) ;; ö 
   ;; stops
   (p   -   0   0   0   0   s   l   - - + - - - - - s l) 
   (t   -   0   0   0   0   s   a   - - + - - - - - s a)  
   (k   -   0   0   0   0   s   v   - - + - - - - - s r) 
;;   (?   -   s   0   0   0   s   g   -) ;; glottaaliklusiili
   (b   -   0   0   0   0   s   l   + - + - - - - - s l)
   (d   -   0   0   0   0   s   a   + - + - - - - - s a) 
   (g   -   0   0   0   0   s   v   + - + - - - - - s r) 
   ;; fricatives
   (f   -   0   0   0   0   f   b   - - - + - - - - s l)
   (T   -   0   0   0   0   f   d   - - - + - - - - s a) ;; th
;;   (D   -   s   0   0   0   f   d   +)	
   (s   -   0   0   0   0   f   a   - - - + + - - - s a)
;;   (z   -   s   0   0   0   f   a   +)
   (S   -   0   0   0   0   f   p   - - - + + - - - s a) ;; sh
;;   (Z   -   s   0   0   0   f   p   +) ;; zh
   (h   -   0   0   0   0   f   g   - - - + - - - - s r)
   (v   -   0   0   0   0   f   b   + - - + - - - - s l)
   ;; nasals
   (m   -   0   0   0   0   n   l   + + - - - - - - s l)
   (n   -   0   0   0   0   n   a   + + - - - - - - s a)
   (N   -   0   0   0   0   n   v   + + - - - - - - s r) ;; ng
   ;; approximants and liquids
   (l   -   0   0   0   0   l   a   + - - - - + + - s a)
   (L   -   0   0   0   0   l   a   + - - - - + + - s a)
   (r   -   0   0   0   0   r   a   + - - - - + - - s a)
   (j   -   0   0   0   0   r   p   + - - - - - - + s r)
   (w   -   0   0   0   0   r   l   + - - - - - - + s r)
   ;;;;;;;;;;;;;;;
   ;;; long phones
   ;; stops
   (p:   -   0   0   0   0   s   l   - - + - - - - - l l) 
   (t:   -   0   0   0   0   s   a   - - + - - - - - l a) 
   (k:   -   0   0   0   0   s   v   - - + - - - - - l r) 
   (b:   -   0   0   0   0   s   l   + - + - - - - - l l)
   (d:   -   0   0   0   0   s   a   + - + - - - - - l a) 
   (g:   -   0   0   0   0   s   v   + - + - - - - - l r) 
   ;; fricatives
   (f:   -   0   0   0   0   f   b   - - - + - - - - l l)
   (T:   -   0   0   0   0   f   d   - - - + - - - - l a) ;; th
   (D:   -   0   0   0   0   f   d   + - - + - - - - l a) ;; th voiced
   (s:   -   0   0   0   0   f   a   - - - + + - - - l a)
   (z:   -   0   0   0   0   f   a   + - - + + - - - l a)
   (S:   -   0   0   0   0   f   p   - - - + + - - - l a) ;; sh
   (Z:   -   0   0   0   0   f   p   + - - + + - - - l a) ;; zh
   (h:   -   0   0   0   0   f   g   - - - + - - - - l r)
   (v:   -   0   0   0   0   f   b   + - - + - - - - l l)
   ;; nasals
   (m:   -   0   0   0   0   n   l   + + - + - - - - l l)
   (n:   -   0   0   0   0   n   a   + + - + - - - - l a)
   (N:   -   0   0   0   0   n   v   + + - + - - - - l r) ;; ng
   ;; approximants and liquids
   (w:   -   0   0   0   0   r   l   + - - - - - - + l r)
   (l:   -   0   0   0   0   l   a   + - - + - + + - l a)
   (L:   -   0   0   0   0   l   a   + - - + - + + - l a)
   (r:   -   0   0   0   0   r   a   + - - + - + - - l a)
   (j:   -   0   0   0   0   r   p   + - - + - - - + l r)
   ;; pauses
   (##   0   0   0   0   0   0   0   0 0 0 0 0 0 0 0 0 0) ;; phrase break
   (#    0   0   0   0   0   0   0   0 0 0 0 0 0 0 0 0 0)))
  

(PhoneSet.silences '(#))
(provide 'finnish_phones)

