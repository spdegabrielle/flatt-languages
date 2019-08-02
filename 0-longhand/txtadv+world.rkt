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
;; The world:

;; Verbs ----------------------------------------
;; Declare all the verbs that can be used in the game.
;; Each verb has a canonical name, a set of aliases, 
;; a printed form, and a boolean indincating whether it
;; is transitive.

(define north (verb (list 'north 'n) "go north" #f))
(record-element! 'north north)

(define south (verb (list 'south 's) "go south" #f))
(record-element! 'south south)

(define east (verb (list 'east 'e) "go east" #f))
(record-element! 'east east)

(define west (verb (list 'west 'w) "go west" #f))
(record-element! 'west west)

(define up (verb (list 'up) "go up" #f))
(record-element! 'up up)

(define down (verb (list 'down) "go down" #f))
(record-element! 'down down)

(define in (verb (list 'in 'enter) "enter" #f))
(record-element! 'in in)

(define out (verb (list 'out 'leave) "leave" #f))
(record-element! 'out out)

(define get (verb (list 'get 'grab 'take) "take" #t))
(record-element! 'get get)

(define put (verb (list 'put 'drop 'leave) "drop" #t))
(record-element! 'put put)

(define open (verb (list 'open 'unlock) "open" #t))
(record-element! 'open open)

(define close (verb (list 'close 'lock) "close" #t))
(record-element! 'close close)

(define knock (verb (list 'knock) (symbol->string 'knock) #t))
(record-element! 'knock knock)

(define quit (verb (list 'quit 'exit) "quit" #f))
(record-element! 'quit quit)

(define look (verb (list 'look 'show) "look" #f))
(record-element! 'look look)

(define inventory (verb (list 'inventory) "check inventory" #f))
(record-element! 'inventory inventory)

(define help (verb (list 'help) (symbol->string 'help) #f))
(record-element! 'help help)

(define save (verb (list 'save) (symbol->string 'save) #f))
(record-element! 'save save)

(define load (verb (list 'load) (symbol->string 'load) #f))
(record-element! 'load load)

(define all-verbs
  (list north south east west up down in out
        get put open close knock quit
        look inventory help save load))

;; Global actions ----------------------------------------
;; Handle verbs that work anywhere.

(define everywhere-actions
  (list
   (cons quit (lambda () (begin (printf "Bye!\n") (exit))))
   (cons look (lambda () (show-current-place)))
   (cons inventory (lambda () (show-inventory)))
   (cons save (lambda () (save-game)))
   (cons load (lambda () (load-game)))
   (cons help (lambda () (show-help)))))

;; Things ----------------------------------------
;; Each thing handles a set of transitive verbs.

(define cactus
  (thing 'cactus 
         #f 
         (list (cons get (lambda () "Ouch!")))))
(record-element! 'cactus cactus)

(define door
  (thing 'door
         #f
         (list
          (cons open 
                (lambda ()
                  (if (have-thing? key)
                      (begin
                        (set-thing-state! door 'open)
                        "The door is now unlocked and open.")
                      "The door is locked.")))
          (cons close 
                (lambda ()
                  (begin
                    (set-thing-state! door #f)
                    "The door is now closed.")))
          (cons knock 
                (lambda ()
                  "No one is home.")))))
(record-element! 'door door)

(define key
  (thing 'key
         #f
         (list
          (cons get 
                (lambda ()
                  (if (have-thing? key)
                      "You already have the key."
                      (begin
                        (take-thing! key)
                        "You now have the key."))))
          (cons put 
                (lambda ()
                  (if (have-thing? key)
                      (begin
                        (drop-thing! key)
                        "You have dropped the key.")
                      "You don't have the key."))))))
(record-element! 'key key)

(define trophy
  (thing 'trophy
         #f
         (list
          (cons get 
                (lambda ()
                  (begin
                    (take-thing! trophy)
                    "You win!"))))))
(record-element! 'trophy trophy)

;; Places ----------------------------------------
;; Each place handles a set of non-transitive verbs.

(define meadow
  (place
   "You're standing in a meadow. There is a house to the north."
   (list)
   (list
    (cons north 
          (lambda () house-front))
    (cons south 
          (lambda () desert)))))
(record-element! 'meadow meadow)

(define house-front
  (place
   "You are standing in front of a house."
   (list door)
   (list
    (cons in 
          (lambda ()
            (if (eq? (thing-state door) 'open)
                room
                "The door is not open.")))
    (cons south (lambda () meadow)))))
(record-element! 'house-front house-front)

(define desert
  (place
   "You're in a desert. There is nothing for miles around."
   (list cactus key)
   (list
    (cons north (lambda () meadow))
    (cons south (lambda () desert))
    (cons east (lambda () desert))
    (cons west (lambda () desert)))))
(record-element! 'desert desert)

(define room
  (place
   "You're in the house."
   (list trophy)
   (list (cons out (lambda () house-front)))))
(record-element! 'room room)

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
