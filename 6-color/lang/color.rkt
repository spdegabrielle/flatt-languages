#lang racket/base
(require parser-tools/lex
         syntax-color/scheme-lexer
         (prefix-in : parser-tools/lex-sre))

(provide color-lexer)

(define-lex-abbrevs 
  [id (:: (:/ #\A #\Z #\a #\z) (:* (:or (:/ #\A #\Z #\a #\z #\0 #\9) #\-)))]

  [digit8 (:/ "0" "7")]
  [digit16 (:/ "af" "AF" "09")]

  [unicode  (:or (:: "u" (:** 1 4 digit16))
                 (:: "U" (:** 1 6 digit16)))]
   
  [str (:: "\"" (:* string-element (:: "\\" unicode)) "\"")]
  [string-element (:or (:~ "\"" "\\")
                       "\\\""
                       "\\\\"
                       "\\a"
                       "\\b"
                       "\\t"
                       "\\n"
                       "\\v"
                       "\\f"
                       "\\r"
                       "\\e"
                       "\\'"
                       (:: "\\" (:** 1 3 digit8))
                       (:: "\\x" (:** 1 2 digit16))
                       (:: "\\" #\newline))])

(define errors
  (lexer
   [any-char
    (values lexeme 'error #f (position-offset start-pos) (position-offset end-pos) 
            1 (list inc-errors 1))]
   [(eof)
    (values lexeme 'eof #f #f #f 0 #f)]))

(define (inc-errors in back)
  (define-values (lexeme type data new-token-start new-token-end backup mode) 
    (errors in))
  (values lexeme type data new-token-start new-token-end 
          (+ 1 (car back))
          (list inc-errors (+ 1 (car back)))))

(define-syntax-rule (lexer/error [pat (_values kind next)] ...)
  (lexer
   [pat
    (values lexeme kind #f (position-offset start-pos) (position-offset end-pos) 0 next)]
   ...
   [any-char
    (values lexeme 'error #f (position-offset start-pos) (position-offset end-pos) 0 errors)]
   [(eof)
    (values lexeme 'eof #f #f #f 0 #f)]))

(define-syntax-rule (lexer/whitespace ws clause ...)
  (lexer/error
   [(:+ whitespace)
    (values lexeme ws)]
   clause ...))

(define section-color 'other)
(define id-color 'symbol)

(define start
  (lexer/whitespace
   #f
   ["===VERBS===" (values section-color verbs)]))

(define verbs
  (lexer/whitespace
   verbs
   [id (values id-color verb-trans)]
   ["===EVERYWHERE===" (values section-color everywhere)]))

(define verb-trans
  (lexer/whitespace
   verb-body
   [" _" (values 'symbol verb-comma)]
   [", " (values 'other verbs)]))

(define verb-comma
  (lexer/whitespace
   verb-body
   [", " (values 'other verbs)]))

(define verb-body
  (lexer/whitespace
   verb-body
   [str (values 'string verbs)]
   [id (values id-color verb-trans)]
   ["===EVERYWHERE===" (values section-color everywhere)]))

(define everywhere
  (lexer/whitespace
   everywhere
   [id (values id-color (list global-action))]
   ["===THINGS===" (values section-color things)]))

(define things
  (lexer/whitespace
   things
   [(:: "---" id "---") (values id-color thing-operations)]
   ["===PLACES===" (values section-color places)]))

(define thing-operations
  (lexer/whitespace
   thing-operations
   [id (values id-color (list thing-action))]
   [(:: "---" id "---") (values id-color thing-operations)]
   ["===PLACES===" (values section-color places)]))

(define places
  (lexer/whitespace
   places
   [(:: "---" id "---") (values id-color place)]))

(define place
  (lexer/whitespace
   place
   [str (values 'string place-things)]))

(define place-things
  (lexer/whitespace
   place-things
   ["[" (values 'string place-thing-seq)]))

(define place-thing-seq
  (lexer/whitespace
   errors
   [id (values id-color place-thing-seq-next)]
   ["]" (values 'string place-operations)]))

(define place-thing-seq-next
  (lexer/whitespace
   errors
   [", " (values 'other place-thing-seq)]
   ["]" (values 'string place-operations)]))

(define place-operations
  (lexer/whitespace
   place-operations
   [(:: "---" id "---") (values id-color place)]
   [id (values id-color (list place-action))]))

(define (global-action in mode)
  (action in mode global-action everywhere))

(define (thing-action in mode)
  (action in mode thing-action thing-operations))

(define (place-action in mode)
  (action in mode place-action place-operations))

(define (action in mode self next)
  (define-values (lexeme type data new-token-start new-token-end status) 
    (scheme-lexer/status in))
  (let ([mode (next-mode mode type data status)])
    (values lexeme 
            (if (eq? mode 'error) 'error type)
            data new-token-start new-token-end 0 
            (cond
             [(list? mode) (cons self mode)]
             [(eq? mode 'error) errors]
             [else next]))))

(define (next-mode mode type data status)
  (case type 
    [(parenthesis)
     (case data
       [(|(|) (cons '|)| mode)]
       [(|[|) (cons '|]| mode)]
       [(|{|) (cons '|}| mode)]
       [else (if (and (pair? mode)
                      (eq? (car mode) data))
                 (if (and (null? (cdr mode))
                          (not (eq? status 'continue)))
                     'done
                     (cdr mode))
                 'error)])]
    [(white-space comment) mode]
    [else (if (and (null? mode)
                   (not (eq? status 'continue)))
              'done
              mode)]))

(define (color-lexer in offset mode)
  (cond
   [(not mode) (start in)]
   [(pair? mode)
    ((car mode) in (cdr mode))]
   [else
    (mode in)]))
