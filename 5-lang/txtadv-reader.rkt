#lang racket
(require syntax/readerr)

(provide (rename-out [txtadv-read-syntax read-syntax]))

(define (txtadv-read-syntax src in)
  (expect-section src in "VERBS")
  (define verbs (in-section src in read-verb))
  (expect-section src in "EVERYWHERE")
  (define actions (in-section src in read-action))
  (expect-section src in "THINGS")
  (define things (in-section src in read-thing))
  (expect-section src in "PLACES")
  (define places (in-section src in read-place))
  (datum->syntax
   #f
   `(module world "txtadv.rkt"
      (define-verbs all-verbs
        ,@verbs)
      (define-everywhere everywhere-actions
        ,actions)
      ,@things
      ,@places
      ,(if (null? places)
           (complain src in "no places defined")
           (cadar places)))))

(define (complain src in msg)
  (define-values (line col pos) (port-next-location in))
  (raise-read-error msg src line col pos 1))

(define (skip-whitespace in)
  (regexp-try-match #px"^\\s+" in))
      
(define (expect-section src in name)
  (skip-whitespace in)
  (unless (regexp-match-peek (pregexp (format "^===~a===\\s" name))
                             in)
    (complain src in (format "expected a ===~a=== section" name)))
  (read-line in)
  (read-line in))

(define (in-section src in reader)
  (skip-whitespace in)
  (if (or (regexp-match-peek #rx"^===" in)
          (eof-object? (peek-byte in)))
      null
      (cons (reader src in)
            (in-section src in reader))))

(define (in-defn src in reader)
  (skip-whitespace in)
  (if (or (regexp-match-peek #rx"^(===|---)" in)
          (eof-object? (peek-byte in)))
      null
      (cons (reader src in)
            (in-defn src in reader))))

(define (read-name src in)
  (if (regexp-match-peek #px"^[A-Za-z-]+(?=:$|\\s|[],])" in)
      (read-syntax src in)
      (complain src in "expected a name")))

(define (read-name-sequence src in transitive)
  (let loop ([names null] [transitive transitive])
    (define s (read-name src in))
    (define is-trans?
      (cond
       [(regexp-match-peek #rx"^ _" in)
        (if (or (eq? transitive 'unknown)
                (eq? transitive #t))
            (begin
              (read-char in)
              (read-char in)
              #t)
            (begin
              (read-char in)
              (complain src in "unexpected underscore")))]
       [else
        (if (eq? transitive #t)
            (complain src in "inconsistent transitivity")
            #f)]))
    (if (regexp-match-peek #rx"^, " in)
        (begin
          (read-char in)
          (read-char in)
          (loop (cons s names) is-trans?))
        (values (reverse (cons s names)) is-trans?))))

(define (read-verb src in)
  (skip-whitespace in)
  (define-values (names is-transitive?)
    (read-name-sequence src in 'unknown))
  (skip-whitespace in)
  (define desc
    (if (regexp-match-peek #rx"^\"" in)
        (read-syntax src in)
        (symbol->string (syntax-e (car names)))))
  `[,(car names) 
    ,@(if is-transitive? '(_) '())
    (= ,@(cdr names))
    ,desc])

(define (read-action src in)
  (skip-whitespace in)
  (define name (read-name src in))
  (define expr (read-syntax src in))
  `[,name ,expr])

(define (read-defn-name src in what)
  (skip-whitespace in)
  (unless (regexp-match-peek #px"^---[A-Za-z][A-Za-z0-9-]*---\\s"
                             in)
    (complain src in (format "expected a ~a definition of the form ---name---" what)))
  (read-string 3 in)
  (define-values (line col pos) (port-next-location in))
  (define name-str (bytes->string/utf-8 (cadr (regexp-match #px"^(.*?)---\\s" in))))
  (datum->syntax #f
                 (string->symbol name-str)
                 (vector src line col pos (string-length name-str))
                 orig-props))
(define orig-props (read-syntax 'src (open-input-string "orig")))

(define (read-thing src in)
  (define name (read-defn-name src in "thing"))
  (define actions (in-defn src in read-action))
  `(define-thing ,name
     ,@actions))

(define (read-place src in)
  (define name (read-defn-name src in "place"))
  (skip-whitespace in)
  (define desc (if (regexp-match-peek #rx"^\"" in)
                   (read-syntax src in)
                   (complain src in "expected description string")))
  (skip-whitespace in)
  (unless (regexp-match-peek #rx"^[[]" in)
    (complain src in "expected a square bracket to start a list of things for a place"))
  (read-char in)
  (define-values (things _) 
    (if (regexp-match-peek #rx"^[]]" in)
        (values null #f)
        (read-name-sequence src in #f)))
  (unless (regexp-match-peek #rx"^[]]" in)
    (complain src in "expected a square bracket to end a list of things for a place"))
  (read-char in)
  (define actions (in-defn src in read-action))
  `(define-place ,name ,desc ,things ,actions))
