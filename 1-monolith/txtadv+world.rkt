#lang racket

;; ============================================================
;; Model:

;; Elements of the world:
(struct verb (aliases       ; list of symbols
              desc          ; string
              transitive?)) ; boolean
(struct thing (name         ; symbol
               [state #:mutable] ; any value
               actions))    ; list of verb--thunk pairs
(struct place (desc         ; string
               [things #:mutable] ; list of things
               actions))    ; list of verb--thunk pairs

;; Tables mapping names<->things for save and load
(define names (make-hash))
(define elements (make-hash))

(define (record-element! name val)
  (hash-set! names name val)
  (hash-set! elements val name))

(define (name->element name) (hash-ref names name #f))
(define (element->name obj) (hash-ref elements obj #f))

;; ============================================================
;; Macros for constructing and registering elements:

(define-syntax-rule (define-verbs all-id
                      [id spec ...] ...)
  (begin
    (define-one-verb id spec ...) ...
    (record-element! 'id id) ...
    (define all-id (list id ...))))

(define-syntax define-one-verb
  (syntax-rules (= _)
    [(define-one-verb id (= alias ...) desc)
     (define id (verb (list 'id 'alias ...) desc #f))]
    [(define-one-verb id _ (= alias ...) desc)
     (define id (verb (list 'id 'alias ...) desc #t))]
    [(define-one-verb id)
     (define id (verb (list 'id) (symbol->string 'id) #f))]
    [(define-one-verb id _)
     (define id (verb (list 'id) (symbol->string 'id) #t))]))


(define-syntax-rule (define-thing id 
                      [vrb expr] ...)
  (begin
    (define id 
      (thing 'id #f (list (cons vrb (lambda () expr)) ...)))
    (record-element! 'id id)))


(define-syntax-rule (define-place id 
                      desc 
                      (thng ...) 
                      ([vrb expr] ...))
  (begin
    (define id (place desc
                      (list thng ...)
                      (list (cons vrb (lambda () expr)) ...)))
    (record-element! 'id id)))


(define-syntax-rule (define-everywhere id ([vrb expr] ...))
  (define id (list (cons vrb (lambda () expr)) ...)))

;; ============================================================
;; The world:

;; Verbs ----------------------------------------
;; Declare all the verbs that can be used in the game.
;; Each verb has a canonical name, a `_' if it needs
;; a thing (i.e., a transitive verb), a set of aliases, 
;; and a printed form.

(define-verbs all-verbs
  [north (= n) "go north"]
  [south (= s) "go south"]
  [east (= e) "go east"]
  [west (= w) "go west"]
  [up (=) "go up"]
  [down (=) "go down"]
  [in (= enter) "enter"]
  [out (= leave) "leave"]  
  [get _ (= grab take) "take"]
  [put _ (= drop leave) "drop"]
  [open _ (= unlock) "open"]
  [close _ (= lock) "close"]
  [knock _]
  [quit (= exit) "quit"]
  [look (= show) "look"]
  [inventory (=) "check inventory"]
  [help]
  [save]
  [load])

;; Global actions ----------------------------------------
;; Handle verbs that work anywhere.

(define-everywhere everywhere-actions
  ([quit (begin (printf "Bye!\n") (exit))]
   [look (show-current-place)]
   [inventory (show-inventory)]
   [save (save-game)]
   [load (load-game)]
   [help (show-help)]))

;; Things ----------------------------------------
;; Each thing handles a set of transitive verbs.

(define-thing cactus
  [get "Ouch!"])

(define-thing door
  [open (if (have-thing? key)
            (begin
              (set-thing-state! door 'open)
              "The door is now unlocked and open.")
            "The door is locked.")]
  [close (begin
           (set-thing-state! door #f)
           "The door is now closed.")]
  [knock "No one is home."])

(define-thing key
  [get (if (have-thing? key)
           "You already have the key."
           (begin
             (take-thing! key)
             "You now have the key."))]
  [put (if (have-thing? key)
           (begin
             (drop-thing! key)
             "You have dropped the key.")
           "You don't have the key.")])

(define-thing trophy
  [get (begin
         (take-thing! trophy)
         "You win!")])

;; Places ----------------------------------------
;; Each place handles a set of non-transitive verbs.

(define-place meadow
  "You're standing in a meadow. There is a house to the north."
  []
  ([north house-front]
   [south desert]))

(define-place house-front
  "You are standing in front of a house."
  [door]
  ([in (if (eq? (thing-state door) 'open)
           room
           "The door is not open.")]
   [south meadow]))

(define-place desert
  "You're in a desert. There is nothing for miles around."
  [cactus key]
  ([north meadow]
   [south desert]
   [east desert]
   [west desert]))

(define-place room
  "You're in the house."
  [trophy]
  ([out house-front]))

;; ============================================================
;; Game state

;; Things carried by the player:
(define stuff null) ; list of things

;; Current location:
(define current-place meadow) ; place

;; Fuctions to be used by verb responses:
(define (have-thing? t)
  (memq t stuff))
(define (take-thing! t) 
  (set-place-things! current-place (remq t (place-things current-place)))
  (set! stuff (cons t stuff)))
(define (drop-thing! t) 
  (set-place-things! current-place (cons t (place-things current-place)))
  (set! stuff (remq t stuff)))

;; ============================================================
;; Game execution

;; Show the player the current place, then get a command:
(define (do-place)
  (show-current-place)
  (do-verb))

;; Show the current place:
(define (show-current-place)
  (printf "~a\n" (place-desc current-place))
  (for-each (lambda (thing)
              (printf "There is a ~a here.\n" (thing-name thing)))
            (place-things current-place)))

;; Get and handle a command:
(define (do-verb)
  (printf "> ")
  (flush-output)
  (let* ([line (read-line)]
         [input (if (eof-object? line)
                    '(quit)
                    (let ([port (open-input-string line)])
                      (for/list ([v (in-port read port)]) v)))])
    (if (and (list? input)
             (andmap symbol? input)
             (<= 1 (length input) 2))
        (let ([cmd (car input)])
            (let ([response
                   (cond
                    [(= 2 (length input))
                     (handle-transitive-verb cmd (cadr input))]
                    [(= 1 (length input))
                     (handle-intransitive-verb cmd)])])
              (let ([result (response)])
                (cond
                 [(place? result)
                  (set! current-place result)
                  (do-place)]
                 [(string? result)
                  (printf "~a\n" result)
                  (do-verb)]
                 [else (do-verb)]))))
          (begin
            (printf "I don't undertand what you mean.\n")
            (do-verb)))))

;; Handle an intransitive-verb command:
(define (handle-intransitive-verb cmd)
  (or
   (find-verb cmd (place-actions current-place))
   (find-verb cmd everywhere-actions)
   (using-verb 
    cmd all-verbs
    (lambda (verb)
      (lambda () 
        (if (verb-transitive? verb)
            (format "~a what?" (string-titlecase (verb-desc verb)))
            (format "Can't ~a here." (verb-desc verb))))))
   (lambda ()
     (format "I don't know how to ~a." cmd))))

;; Handle a transitive-verb command:
(define (handle-transitive-verb cmd obj)
  (or (using-verb 
       cmd all-verbs
       (lambda (verb)
         (and 
          (verb-transitive? verb)
          (cond
           [(ormap (lambda (thing)
                     (and (eq? (thing-name thing) obj)
                          thing))
                   (append (place-things current-place)
                           stuff))
            => (lambda (thing)
                 (or (find-verb cmd (thing-actions thing))
                     (lambda ()
                       (format "Don't know how to ~a ~a."
                               (verb-desc verb) obj))))]
           [else
            (lambda ()
              (format "There's no ~a here to ~a." obj 
                      (verb-desc verb)))]))))
      (lambda ()
        (format "I don't know how to ~a ~a." cmd obj))))

;; Show what the player is carrying:
(define (show-inventory)
  (printf "You have")
  (if (null? stuff)
      (printf " no items.")
      (for-each (lambda (thing)
                  (printf "\n  a ~a" (thing-name thing)))
                stuff))
  (printf "\n"))

;; Look for a command match in a list of verb--response pairs,
;; and returns the response thunk if a match is found:
(define (find-verb cmd actions)
  (ormap (lambda (a)
           (and (memq cmd (verb-aliases (car a)))
                (cdr a)))
         actions))

;; Looks for a command in a list of verbs, and
;; applies `suucess-k' to the verb if one is found:
(define (using-verb cmd verbs success-k)
  (ormap (lambda (vrb)
           (and (memq cmd (verb-aliases vrb))
                (success-k vrb)))
         verbs))

;; Print help information:
(define (show-help)
  (printf "Use `look' to look around.\n")
  (printf "Use `inventory' to see what you have.\n")
  (printf "Use `save' or `load' to save or restore your game.\n")
  (printf "There are some other verbs, and you can name a thing after some verbs.\n"))

;; ============================================================
;; Save and load

;; Prompt the user for a filename and apply `proc' to it,
;; catching errors to report a reasonably nice message:
(define (with-filename proc)
  (printf "File name: ")
  (flush-output)
  (let ([v (read-line)])
    (unless (eof-object? v)
      (with-handlers ([exn? (lambda (exn)
                              (printf "~a\n" (exn-message exn)))])
        (unless (path-string? v)
          (raise-user-error "bad filename"))
        (proc v)))))

;; Save the current game state:
(define (save-game)
  (with-filename
   (lambda (v)
     (with-output-to-file v
       (lambda ()
         (write
          (list
           (map element->name stuff)
           (element->name current-place)
           (hash-map names
                     (lambda (k v)
                       (cons k
                             (cond
                              [(place? v) (map element->name (place-things v))]
                              [(thing? v) (thing-state v)]
                              [else #f])))))))))))

;; Restore a game state:
(define (load-game)
  (with-filename
   (lambda (v)
     (let ([v (with-input-from-file v read)])
       (set! stuff (map name->element (car v)))
       (set! current-place (name->element (cadr v)))
       (for-each
        (lambda (p)
          (let ([v (name->element (car p))]
                [state (cdr p)])
            (cond
             [(place? v) (set-place-things! v (map name->element state))]
             [(thing? v) (set-thing-state! v state)])))
        (caddr v))))))

;; ============================================================
;; Go!

(do-place)
