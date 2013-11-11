;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;                                                                       ;;
;;;          Department of General Linguistics / Suopuhe project          ;;
;;;                      University of Helsinki, FI                       ;;
;;;                       Copyright (c) 2000-2003                         ;;
;;;                        All Rights Reserved.                           ;;
;;;                                                                       ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;                                                                       ;;
;;; Finnish lexicon for function word determination                       ;;
;;;                                                                       ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
; Authors: Nicholas Volk & Martti Vainio
;



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

(define (word_list_entry? word lexicon)
  "(word_list_entry? STRING LIST)
Checks whether the STRING is member of any of the sublists of LIST."
  ;; (format stderr "%s in %l?\n" sana (car leksikko))
  (cond
   ((null lexicon)
    nil)
   ((member_string word (cdr (car lexicon)))
    t)
   (t
    (word_list_entry? word (cdr lexicon)))))



(set! doubler_words
  '((a terve tuore )
    (n aihe aste kone laite lause luode n‰yte oire puhe purje tunne vene virhe)
    (v aja auta mene ole ota pue pure syˆ tule ))
  )

(set! finnish_guess_coord
      '((coord ja tai vai)))

(set! finnish_guess_cop
  '(
    (cop
     olen olet on olemme olette ovat olla ole olleet ollut
     olin olit oli olimme olitte olivat 
     olisin olisit olisi olisimme olisitte olisivat
     lienen lienet lienee lienemme lienette lienev‰t liene lienneet)))

(set! finnish_guess_pron
      '(
    (pron 
     min‰ sin‰ h‰n me te he se ne
     minun sinun h‰nen meid‰n teid‰n heid‰n sen niiden niitten 
     minua sinua h‰nt‰ meit‰ teit‰ heit‰ sit‰ niit‰
     minut sinut h‰net meid‰t teid‰t heid‰t
     minusta sinusta h‰nest‰ meist‰ teist‰ heist‰ siit‰ niist‰
     minussa sinussa h‰ness‰ meiss‰        heiss‰ siin‰ niiss‰
     minuun sinuun h‰neen meihin teihin heihin siihen niille
     minulla sinulla h‰nell‰ meill‰ teill‰ heill‰       niill‰
     minulta sinulta h‰nelt‰ meilt‰ teilt‰ heilt‰       niilt‰
     minulle sinulle h‰nelle meille teille heille sille niille
     m‰ mun mua mut mussa       muhun mulla       mulle
     s‰ sun sua sut sussa susta suhun sulla sulta sulle
     t‰m‰ t‰m‰n t‰t‰ t‰ss‰ t‰st‰ t‰h‰n t‰ll‰ t‰lt‰ t‰lle t‰n‰
     n‰m‰ n‰iden n‰itten n‰it‰ n‰iss‰ n‰ist‰ n‰ihin n‰ill‰ n‰ilt‰ n‰ille n‰in‰
     tuo tuon tuota tuossa tuosta tuohon tuolla tuolta tuolle tuona
     nuo noiden noitten noissa noista noihin noilla noilta noille noina
     joka jonka jota jossa josta johon jolla jolle jolta jona
     jotka joiden joita joissa joista joihin joilla joilta joille joina
     mik‰ mink‰ mit‰ miss‰ mist‰ mihin mill‰ milt‰ mille miksi
     minne t‰nne sinne jonne
     kuka ket‰ kenen keness‰ kenest‰ keneen kenell‰ kenelt‰ kenelle 
     ken keit‰ ketk‰ keiden keitten keiss‰ keist‰ keihin keill‰ keilt‰ keille 
     kumpi kumpaa kumman kummassa kummasta kumpaan kummalla kummalta kummalle kumpana
     )))
    
(set! finnish_guess_pos
  '(
    (copula 
     olen olet on olemme olette ovat olla ole olleet ollut
     olin olit oli olimme olitte olivat 
     olisin olisit olisi olisimme olisitte olisivat
     lienen lienet lienee lienemme lienette lienev‰t liene lienneet)
    
    (coord 
     ja tai vai)
    (neg  
     en et ei emme ette eiv‰t)
    (xxx 
     ali alla allaan alle alleen alta
     and
     edelle edelleen edell‰ edell‰‰n edess‰ edess‰mme edess‰ni edess‰‰n edest‰ edest‰‰n
     ehkei
     eli
     ellei ellen
     ennen
     eteen
     ettei etteiv‰t etten
     ett‰
     huolimatta
     ilman
     joko
     jokunen
     jollei jolleiv‰t
     jos
     j‰ljess‰ j‰lkeen j‰lkeens‰
     kanssa
     kautta kauttaan
     kera
     kerran
     keskelle keskell‰ keskelt‰ kesken
     kohti
     koska
     kuin
     kun
     luokse luokseen luona luota luotasi
     l‰helle l‰hell‰ l‰helt‰ l‰hettyville
     l‰htien
     l‰pi
     miksei
     mik‰li
     mutta muttei
     ohella ohessa
     ohi ohitse ohitseni
     olemaan olemassa olemasta olematta oleva olevaa olevaan olevalla olevalle olevamme olevan olevani olevansa olevassa olevasta olevia olevien oleviin oleville olevissa olevista olkoon ollaan ollakseen olleelle olleen olleensa olleeseen olleessa olleiden olleille olleisiin olleissa olleista olleita ollen ollenkaan ollessa ollessaan ollessani ollutta oltaisiin oltaisiinko oltava oltu oltua oltuaan
     paitsi
     pitkin
     poikki
     p‰in p‰‰lle p‰‰lleen p‰‰ll‰ p‰‰lt‰‰n
     riippumatta
     saati
     sek‰
     suhteen suhteensa
     taakse taakseen
     takaa takana takanaan
     vaan
     vaikka vaikkei
     vailla vaille
     varrella varrelta varressa varresta varteen
     vastap‰‰t‰
     vasten vastoin
     yli ylitse
     ymp‰ri
     ynn‰
     ‰lk‰‰ ‰lkˆˆn
     )    
    (punc 
     "." "," ":" "!" "?" "(" ")" "\"")))

;;;========================================================================
(lex.create "finnish")
(lex.set.phoneset "finnish")

;(lex.set.compile.file "/home/n/v/nvolk/festival/lib/voices/finnish/hy_fi_mv_diphone/lex.lex" )
;;(lex.set.compile.file "/home/n/v/nvolk/festival/lib/voices/finnish/hy_fi_mv_diphone/festvox/lex.lex" )
;;;(lex.set.pos.map finnish_pos_map) ; turha...
(lex.set.lts.method 'finnish_lts)
;;;(lex.set.lts.ruleset 'finnish) ; turha...

;;;========================================================================
;;(lex.add.entry '("." "punc" ((( # # ) 0 ))))
;;(lex.add.entry '(":" "punc" ((( # ) 0 ))))
;;(lex.add.entry '(";" "punc" ((( # ) 0 ))))
;;(lex.add.entry '("," "punc" ((( # ) 0 ))))
;;(lex.add.entry '("?" "punc" ((( # # ) 0 ))))
;;(lex.add.entry '("!" "punc" ((( # # ) 0 ))))

;;;========================================================================
(lex.add.entry '("a" "char" ((( a: ) 2)))) 
(lex.add.entry '("b" "char" ((( b e: ) 2))))
(lex.add.entry '("c" "char" ((( s e: ) 2)))) 
(lex.add.entry '("d" "char" ((( d e: ) 2))))
(lex.add.entry '("e" "char" ((( e: ) 2)))) 
(lex.add.entry '("f" "char" ((( @ f ) 2))))
(lex.add.entry '("g" "char" ((( g e: ) 2)))) 
(lex.add.entry '("h" "char" ((( h o: ) 2))))
(lex.add.entry '("i" "char" ((( i: ) 2)))) 
(lex.add.entry '("j" "char" ((( j i: ) 2)))) 
(lex.add.entry '("k" "char" ((( k o: ) 2)))) 
(lex.add.entry '("l" "char" ((( @ l ) 2))))
(lex.add.entry '("m" "char" ((( @ m ) 2)))) 
(lex.add.entry '("n" "char" ((( @ n ) 2))))
(lex.add.entry '("o" "char" ((( o: ) 2)))) 
(lex.add.entry '("p" "char" ((( p e: ) 2))))
(lex.add.entry '("q" "char" ((( k u: ) 2)))) 
(lex.add.entry '("r" "char" ((( @ r ) 2))))
(lex.add.entry '("s" "char" ((( @ s ) 2)))) 
(lex.add.entry '("t" "char" ((( t e: ) 2)))) 
(lex.add.entry '("u" "char" ((( u: ) 2)))) 
(lex.add.entry '("v" "char" ((( v e: ) 2))))
(lex.add.entry '("w" "char" ((( k a k ) 2) (( s o i s ) 0) (( v e: ) 1)))) 
(lex.add.entry '("x" "char" ((( @ k s ) 2))))
(lex.add.entry '("y" "char" ((( y: ) 2)))) 
(lex.add.entry '("z" "char" ((( b e: ) 2))))
(lex.add.entry '("Â" "char" ((( r u o t ) 2 ) (( s a ) 0 ) (( l a i ) 1 ) (( n e n) 0 )(( o: ) 2)))) 
(lex.add.entry '("‰" "char" ((( @: ) 2))))
(lex.add.entry '("ˆ" "char" ((( 7: ) 2)))) 


;;;========================================================================
(provide 'finnish_lex)
