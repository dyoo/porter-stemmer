#lang racket/base

;; Copied from:
;; http://programmingpraxis.codepad.org/Wyj6sW6N

(provide stem)

; from the standard prelude

(define (string-downcase str)
  (list->string
    (map char-downcase
      (string->list str))))

(define-syntax assert
  (syntax-rules ()
    ((assert expr result)
      (when (not (equal? expr result))
          (for-each display `(
            #\newline "failed assertion:" #\newline
            expr #\newline "expected: " ,result
            #\newline "returned: " ,expr #\newline))))))

; porter stemming
; http://tartarus.org/~martin/PorterStemmer/index.html
; word is stored as a list of characters in reverse order

; #t if end of cs is consonant, else #f
(define (consonant? cs)
  (case (car cs)
    ((#\a #\e #\i #\o #\u) #f)
    ((#\y) (if (null? (cdr cs)) #t (vowel? (cdr cs))))
    (else #t)))

; #t if end of cs is vowel, else #f
(define (vowel? cs) (not (consonant? cs)))

; calculate number of instances of vowel followed by consonant
(define (measure cs)
  (let loop ((m 0) (cs cs) (zs '()) (prev? #t))
    (cond ((pair? cs)
            (loop m (cdr cs) (cons (if (consonant? cs) #\c #\v) zs) prev?))
          ((null? zs) m)
          ((char=? (car zs) #\v) (loop m cs (cdr zs) #f))
          ((not prev?) (loop (+ m 1) cs (cdr zs) #t))
          (else (loop m cs (cdr zs) #t)))))

; #t if any character is vowel, else #f
(define (v-stem? cs)
  (let loop ((cs cs))
    (cond ((null? cs) #f)
          ((vowel? cs) #t)
          (else (loop (cdr cs))))))

; #t if cs ends with two identical consonants, else #f
(define (dbl-cons? cs)
  (cond ((or (null? cs) (null? (cdr cs))) #f)
        ((not (char=? (car cs) (cadr cs))) #f)
        (else (consonant? cs))))

; #t if cs ends consonant-vowel-consonant (not wxy), else #f
(define (cvc? cs)
  (cond ((< (length cs) 3) #f)
        ((vowel? cs) #f)
        ((consonant? (cdr cs)) #f)
        ((vowel? (cddr cs)) #f)
        ((char=? (car cs) #\w) #f)
        ((char=? (car cs) #\x) #f)
        ((char=? (car cs) #\y) #f)
        (else #t)))

; remove str from end of cs, else #f
(define (ends? cs str)
  (let loop ((cs cs) (ss (reverse (string->list str))))
    (cond ((null? cs) #f)
          ((null? ss) cs)
          ((char=? (car cs) (car ss))
            (loop (cdr cs) (cdr ss)))
          (else #f))))

(define (q str)
  (lambda (cs)
    (if (not (v-stem? cs))
        (append (reverse (string->list str)) cs)
        (cond ((ends? cs "at") => (s "ate"))
              ((ends? cs "bl") => (s "ble"))
              ((ends? cs "iz") => (s "ize"))
              ((and (dbl-cons? cs)
                    (not (member (car cs) '(#\l #\s #\z))))
                (cdr cs))
              ((and (= (measure cs) 1) (cvc? cs))
                (cons #\e cs))
              (else cs)))))

(define (r str1 str2)
  (lambda (cs)
    (if (< 0 (measure cs))
        (append (reverse (string->list str2)) cs)
        (append (reverse (string->list str1)) cs))))

(define (s str)
  (lambda (cs)
    (append (reverse (string->list str)) cs)))

(define (t str)
  (lambda (cs)
    (if (< 1 (measure cs)) cs
      (append (reverse (string->list str)) cs))))

(define (step1a cs) ; plural
  (cond ((null? cs) cs) ((null? (cdr cs)) cs)
        ((ends? cs "sses") => (s "ss"))
        ((ends? cs "ies" ) => (s "i" ))
        ((ends? cs "ss"  ) => (s "ss"))
        ((ends? cs "s"   ) => (lambda (cs) cs))
        (else cs)))

(define (step1b cs) ; past participle
  (cond ((null? cs) cs) ((null? (cdr cs)) cs)
        ((ends? cs "eed") => (r "eed" "ee"))
        ((ends? cs "ed" ) => (q "ed"))
        ((ends? cs "ing") => (q "ing"))
        (else cs)))

(define (step1c cs) ; clean up
  (cond ((null? cs) cs) ((null? (cdr cs)) cs)
        ((ends? cs "y") =>
          (lambda (cs)
            (if (v-stem? cs)
                (cons #\i cs)
                (cons #\y cs))))
        (else cs)))

(define (step2 cs) ; double suffices
  (if (or (null? cs) (null? (cdr cs))) cs
    (case (cadr cs)
      ((#\a) (cond ((ends? cs "ational") => (r "ational" "ate" ))
                   ((ends? cs "tional" ) => (r "tional"  "tion"))
                   (else cs)))
      ((#\c) (cond ((ends? cs "enci"   ) => (r "enci"    "ence"))
                   ((ends? cs "anci"   ) => (r "anci"    "ance"))
                   (else cs)))
      ((#\e) (cond ((ends? cs "izer"   ) => (r "izer"    "ize" ))
                   (else cs)))
      ((#\g) (cond ((ends? cs "logi"   ) => (r "logi"    "log" ))
                   (else cs)))
      ((#\l) (cond ((ends? cs "bli"    ) => (r "bli"     "ble" ))
                   ((ends? cs "alli"   ) => (r "alli"    "al"  ))
                   ((ends? cs "entli"  ) => (r "entli"   "ent" ))
                   ((ends? cs "eli"    ) => (r "eli"     "e"   ))
                   ((ends? cs "ousli"  ) => (r "ousli"   "ous" ))
                   (else cs)))
      ((#\o) (cond ((ends? cs "ization") => (r "ization" "ize" ))
                   ((ends? cs "ation"  ) => (r "ation"   "ate" ))
                   ((ends? cs "ator"   ) => (r "ator"    "ate" ))
                   (else cs)))
      ((#\s) (cond ((ends? cs "alism"  ) => (r "alism"   "al"  ))
                   ((ends? cs "iveness") => (r "iveness" "ive" ))
                   ((ends? cs "fulness") => (r "fulness" "ful" ))
                   ((ends? cs "ousness") => (r "ousness" "ous" ))
                   (else cs)))
      ((#\t) (cond ((ends? cs "aliti"  ) => (r "aliti"   "al"  ))
                   ((ends? cs "iviti"  ) => (r "iviti"   "ive" ))
                   ((ends? cs "biliti" ) => (r "biliti"  "ble" ))
                   (else cs)))
      (else cs))))

(define (step3 cs) ; -ic, -full, -ness, and others
  (if (or (null? cs) (null? (cdr cs))) cs
    (case (car cs)
      ((#\e) (cond ((ends? cs "icate"  ) => (r "icate"   "ic"  ))
                   ((ends? cs "ative"  ) => (r "ative"   ""    ))
                   ((ends? cs "alize"  ) => (r "alize"   "al"  ))
                   (else cs)))
      ((#\i) (cond ((ends? cs "iciti"  ) => (r "iciti"   "ic"  ))
                   (else cs)))
      ((#\l) (cond ((ends? cs "ical"   ) => (r "ical"    "ic"  ))
                   ((ends? cs "ful"    ) => (r "ful"     ""    ))
                   (else cs)))
      ((#\s) (cond ((ends? cs "ness"   ) => (r "ness"    ""    ))
                   (else cs)))
      (else cs))))

(define (step4 cs) ; -ant, -ence and others with measure > 1
  (if (or (null? cs) (null? (cdr cs))) cs
    (case (cadr cs)
      ((#\a) (cond ((ends? cs "al"     ) => (t "al"   )) (else cs)))
      ((#\c) (cond ((ends? cs "ance"   ) => (t "ance" ))
                   ((ends? cs "ence"   ) => (t "ence" )) (else cs)))
      ((#\e) (cond ((ends? cs "er"     ) => (t "er"   )) (else cs)))
      ((#\i) (cond ((ends? cs "ic"     ) => (t "ic"   )) (else cs)))
      ((#\l) (cond ((ends? cs "able"   ) => (t "able" ))
                   ((ends? cs "ible"   ) => (t "ible" )) (else cs)))
      ((#\n) (cond ((ends? cs "ant"    ) => (t "ant"  ))
                   ((ends? cs "ement"  ) => (t "ement"))
                   ((ends? cs "ment"   ) => (t "ment" ))
                   ((ends? cs "ent"    ) => (t "ent"  )) (else cs)))
      ((#\o) (cond ((ends? cs "ion"    ) => (lambda (cs)
                                              (if (and (or (char=? (car cs) #\s)
                                                           (char=? (car cs) #\t))
                                                       (< 1 (measure cs)))
                                                  cs
                                                  (append (reverse (string->list "ion")) cs))))
                   ((ends? cs "ou"     ) => (t "ou"   )) (else cs)))
      ((#\s) (cond ((ends? cs "ism"    ) => (t "ism"  )) (else cs)))
      ((#\t) (cond ((ends? cs "ate"    ) => (t "ate"  ))
                   ((ends? cs "iti"    ) => (t "iti"  )) (else cs)))
      ((#\u) (cond ((ends? cs "ous"    ) => (t "ous"  )) (else cs)))
      ((#\v) (cond ((ends? cs "ive"    ) => (t "ive"  )) (else cs)))
      ((#\z) (cond ((ends? cs "ize"    ) => (t "ize"  )) (else cs)))
      (else cs))))

(define (step5a cs) ; words ending in #\e
  (if (not (char=? (car cs) #\e)) cs
    (let ((m (measure (cdr cs))))
      (if (< 1 m) (cdr cs)
        (if (and (= m 1) (not (cvc? (cdr cs)))) (cdr cs) cs)))))

(define (step5b cs) ; words ending in double #\l
  (if (and (< 1 (measure cs)) (char=? (car cs) #\l) (char=? (cadr cs) #\l))
      (cdr cs) cs))

(define (stem word)
  (if (< (string-length word) 3) (string-downcase word)
    (list->string (reverse
      (step5b (step5a (step4 (step3 (step2 (step1c (step1b (step1a
        (reverse (string->list (string-downcase word)))))))))))))))

(define (test-stem)
  (define (cs str) (reverse (string->list str)))
  (assert (step1a (cs "caresses"       )) (cs "caress"     ))
  (assert (step1a (cs "ponies"         )) (cs "poni"       ))
  (assert (step1a (cs "ties"           )) (cs "ti"         ))
  (assert (step1a (cs "caress"         )) (cs "caress"     ))
  (assert (step1a (cs "cats"           )) (cs "cat"        ))
  (assert (step1b (cs "feed"           )) (cs "feed"       ))
  (assert (step1b (cs "agreed"         )) (cs "agree"      ))
  (assert (step1b (cs "plastered"      )) (cs "plaster"    ))
  (assert (step1b (cs "bled"           )) (cs "bled"       ))
  (assert (step1b (cs "motoring"       )) (cs "motor"      ))
  (assert (step1b (cs "sing"           )) (cs "sing"       ))
  (assert (step1b (cs "conflated"      )) (cs "conflate"   ))
  (assert (step1b (cs "troubled"       )) (cs "trouble"    ))
  (assert (step1b (cs "sized"          )) (cs "size"       ))
  (assert (step1b (cs "hopping"        )) (cs "hop"        ))
  (assert (step1b (cs "tanned"         )) (cs "tan"        ))
  (assert (step1b (cs "falling"        )) (cs "fall"       ))
  (assert (step1b (cs "hissing"        )) (cs "hiss"       ))
  (assert (step1b (cs "fizzed"         )) (cs "fizz"       ))
  (assert (step1b (cs "failing"        )) (cs "fail"       ))
  (assert (step1b (cs "filing"         )) (cs "file"       ))
  (assert (step1c (cs "happy"          )) (cs "happi"      ))
  (assert (step1c (cs "sky"            )) (cs "sky"        ))
  (assert (step2  (cs "relational"     )) (cs "relate"     ))
  (assert (step2  (cs "conditional"    )) (cs "condition"  ))
  (assert (step2  (cs "rational"       )) (cs "rational"   ))
  (assert (step2  (cs "valenci"        )) (cs "valence"    ))
  (assert (step2  (cs "hesitanci"      )) (cs "hesitance"  ))
  (assert (step2  (cs "digitizer"      )) (cs "digitize"   ))
  (assert (step2  (cs "conformabli"    )) (cs "conformable"))
  (assert (step2  (cs "radicalli"      )) (cs "radical"    ))
  (assert (step2  (cs "differentli"    )) (cs "different"  ))
  (assert (step2  (cs "vileli"         )) (cs "vile"       ))
  (assert (step2  (cs "analogousli"    )) (cs "analogous"  ))
  (assert (step2  (cs "vietnamization" )) (cs "vietnamize" ))
  (assert (step2  (cs "predication"    )) (cs "predicate"  ))
  (assert (step2  (cs "operator"       )) (cs "operate"    ))
  (assert (step2  (cs "feudalism"      )) (cs "feudal"     ))
  (assert (step2  (cs "decisiveness"   )) (cs "decisive"   ))
  (assert (step2  (cs "hopefulness"    )) (cs "hopeful"    ))
  (assert (step2  (cs "callousness"    )) (cs "callous"    ))
  (assert (step2  (cs "formaliti"      )) (cs "formal"     ))
  (assert (step2  (cs "sensitiviti"    )) (cs "sensitive"  ))
  (assert (step2  (cs "sensibiliti"    )) (cs "sensible"   ))
  (assert (step3  (cs "triplicate"     )) (cs "triplic"    ))
  (assert (step3  (cs "formative"      )) (cs "form"       ))
  (assert (step3  (cs "formalize"      )) (cs "formal"     ))
  (assert (step3  (cs "electriciti"    )) (cs "electric"   ))
  (assert (step3  (cs "electrical"     )) (cs "electric"   ))
  (assert (step3  (cs "hopeful"        )) (cs "hope"       ))
  (assert (step3  (cs "goodness"       )) (cs "good"       ))
  (assert (step4  (cs "revival"        )) (cs "reviv"      ))
  (assert (step4  (cs "allowance"      )) (cs "allow"      ))
  (assert (step4  (cs "inference"      )) (cs "infer"      ))
  (assert (step4  (cs "airliner"       )) (cs "airlin"     ))
  (assert (step4  (cs "gyroscopic"     )) (cs "gyroscop"   ))
  (assert (step4  (cs "adjustable"     )) (cs "adjust"     ))
  (assert (step4  (cs "defensible"     )) (cs "defens"     ))
  (assert (step4  (cs "irritant"       )) (cs "irrit"      ))
  (assert (step4  (cs "replacement"    )) (cs "replac"     ))
  (assert (step4  (cs "adjustment"     )) (cs "adjust"     ))
  (assert (step4  (cs "dependent"      )) (cs "depend"     ))
  (assert (step4  (cs "adoption"       )) (cs "adopt"      ))
  (assert (step4  (cs "homologou"      )) (cs "homolog"    ))
  (assert (step4  (cs "communism"      )) (cs "commun"     ))
  (assert (step4  (cs "activate"       )) (cs "activ"      ))
  (assert (step4  (cs "angulariti"     )) (cs "angular"    ))
  (assert (step4  (cs "homologous"     )) (cs "homolog"    ))
  (assert (step4  (cs "effective"      )) (cs "effect"     ))
  (assert (step4  (cs "bowdlerize"     )) (cs "bowdler"    ))
  (assert (step5a (cs "probate"        )) (cs "probat"     ))
  (assert (step5a (cs "rate"           )) (cs "rate"       ))
  (assert (step5a (cs "cease"          )) (cs "ceas"       ))
  (assert (step5b (cs "controll"       )) (cs "control"    ))
  (assert (step5b (cs "roll"           )) (cs "roll"       ))
  (assert (stem       "relate"          )     "relat"       )
  (assert (stem       "probate"         )     "probat"      )
  (assert (stem       "conflate"        )     "conflat"     )
  (assert (stem       "pirate"          )     "pirat"       )
  (assert (stem       "prelate"         )     "prelat"      )
  (assert (stem       "derivate"        )     "deriv"       )
  (assert (stem       "activate"        )     "activ"       )
  (assert (stem       "demonstrate"     )     "demonstr"    )
  (assert (stem       "necessitate"     )     "necessit"    )
  (assert (stem       "renovate"        )     "renov"       )
  (assert (stem       "archprelate"     )     "archprel"    )
  (assert (stem       "generalizations" )     "gener"       )
  (assert (stem       "oscillators"     )     "oscil"       )
)

(test-stem) ; no news is good news