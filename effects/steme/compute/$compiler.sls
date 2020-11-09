(library (steme compute $compiler)
  (export compile-computation
          initial-environment
          initial-dynamic-environment
          resume
          suspend
          make-thunk
          thunk-computation
          reset shift)
  (import (rnrs)
          (steme shift))

  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;; The Dynamic Environment ;;
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

  (define initial-dynamic-environment
    `((display . ,(lambda (k v)
                     (display v)
                     (k)))
      (raise . ,(lambda (k v)
                  (raise v)))))

  (define (dynamic-lookup name env)
    (cond ((assq name env) => cdr)
          (else
           (lambda (k . x*)
             (error 'compute "unhandled operation" name)))))

  (define-record-type thunk
    (fields computation)
    (nongenerative))

  (define-syntax suspend
    (syntax-rules ()
      ((suspend c)
       (lambda () c))))

  (define resume
    (lambda (c)
      (c)))

  (define-record-type variable
    (fields location)
    (nongenerative))

  (define-record-type keyword
    (fields syntax)
    (nongenerative))

  (define-record-type function
    (parent keyword)
    (fields procedure)
    (nongenerative)
    (protocol (lambda (n)
                (lambda (proc)
                  ((n 'function) proc)))))

  (define-record-type operator
    (parent keyword)
    (fields name)
    (nongenerative)
    (protocol (lambda (n)
                (lambda (name)
                  ((n 'operator) name)))))

  (define-record-type continuation
    (parent keyword)
    (fields name)
    (nongenerative)
    (protocol (lambda (n)
                (lambda (name)
                  ((n 'continuation) name)))))

  (define initial-environment
    `((+ . ,(make-function #'+))
      (* . ,(make-function #'*))
      (- . ,(make-function #'-))
      (display . ,(make-operator #'display))
      (raise . ,(make-operator #'raise))
      (zero? . ,(make-function #'zero?))
      (begin . ,(make-keyword 'begin))
      (car . ,(make-keyword 'car))
      (cdr . ,(make-keyword 'cdr))
      (cons . ,(make-keyword 'cons))
      (force . ,(make-keyword 'force))
      (if . ,(make-keyword 'if))
      (lambda . ,(make-keyword 'lambda))
      (letrec . ,(make-keyword 'letrec))
      (quote . ,(make-keyword 'quote))
      (receive . ,(make-keyword 'receive))
      (thunk . ,(make-keyword 'thunk))
      (values . ,(make-keyword 'values))
      (with-handler . ,(make-keyword 'with-handler))))

  (define constant?
    (lambda (obj)
      (or (boolean? obj)
          (bytevector? obj)
          (char? obj)
          (number? obj)
          (string? obj)
          (vector? obj))))

  (define lookup
    (lambda (id env)
      (cond ((assq (syntax->datum id) env) => cdr)
            (else #f))))

  (define compile-value
    (lambda (v env)
      (syntax-case v ()
        ((keyword . _)
         (identifier? #'keyword)
         (let ((binding (lookup #'keyword env)))
           (unless binding
             (syntax-violation 'compute "unbound keyword" v #'keyword))
           (unless (keyword? binding)
             (syntax-violation 'compute "not a keyword" v #'keyword))
           (case (keyword-syntax binding)
             ((function)
              (compile-function (function-procedure binding) v env))
             ((quote)
              (compile-quote v env))
             ((thunk)
              (compile-thunk v env))
             (else
              (syntax-violation 'compute "invalid use of keyword" v
                                #'keyword)))))
        (_
         (identifier? v)
         (let ((binding (lookup v env)))
           (unless binding
             (syntax-violation 'compute "unbound variable" v))
           (unless (variable? binding)
             (syntax-violation 'compute "not a variable" v))
           (variable-location binding)))
        (d
         (constant? (syntax->datum #'d))
         #''d)
        (_
         (syntax-violation 'compute "invalid value" v)))))

  (define compile-value*
    (lambda (v* env)
      (map (lambda (v) (compile-value v env)) v*)))

  (define compile-function
    (lambda (proc v env)
      (syntax-case v ()
        ((_ v* ...)
         #`(#,proc #,@(compile-value* #'(v* ...) env)))
        (_
         (syntax-violation 'compute "ill-formed function application" v)))))

  (define compile-quote
    (lambda (v env)
      (syntax-case v ()
        ((_ d)
         #''d)
        (_
         (syntax-violation 'quote "ill-formed quote value" v)))))

  (define compile-thunk
    (lambda (v env)
      (syntax-case v ()
        ((_ c)
         #`(make-thunk (suspend #,(compile-computation #'c env))))
        (_
         (syntax-violation 'thunk "ill-formed thunk value" v)))))

  (define compile-computation
    (lambda (c env)
      (syntax-case c ()
        ((keyword . _)
         (identifier? #'keyword)
         (let ((binding (lookup #'keyword env)))
           (unless binding
             (syntax-violation 'compute "unbound keyword" c #'keyword))
           (unless (keyword? binding)
             (syntax-violation 'compute "not a keyword" c #'keyword))
           (case (keyword-syntax binding)
             ((begin)
              (compile-begin c env))
             ((car)
              (compile-car c env))
             ((cdr)
              (compile-cdr c env))
             ((cons)
              (compile-cons c env))
             ((continuation)
              (compile-continuation (continuation-name binding) c env))
             ((force)
              (compile-force c env))
             ((if)
              (compile-if c env))
             ((lambda)
              (compile-lambda c env))
             ((letrec)
              (compile-letrec c env))
             ((operator)
              (compile-operator (operator-name binding) c env))
             ((receive)
              (compile-receive c env))
             ((values)
              (compile-values c env))
             ((with-handler)
              (compile-with-handler c env))
             (else
              (syntax-violation 'compute "invalid use of keyword"
                                c #'keyword)))))
        ((c v* ...)
         #`(#,(compile-computation #'c env)
            #,@(compile-value* #'(v* ...) env)))
        (_
         (syntax-violation 'compute "invalid computation" c)))))

  (define compile-computation*
    (lambda (c* env)
      (map (lambda (c) (compile-computation c env)) c*)))

  (define (compile-begin c env)
    (syntax-case c ()
      ((_ c* ... c)
       #`(begin #,@(compile-computation* #'(c* ...) env)
                #,(compile-computation #'c env)))
      (_
       (syntax-violation 'begin "ill-formed begin computation" c))))

  (define (compile-car c env)
    (syntax-case c ()
      ((_ c)
       #`(resume (car #,(compile-computation #'c env))))
      (_
       (syntax-violation 'car "ill-formed car computation" c))))

  (define (compile-cdr c env)
    (syntax-case c ()
      ((_ c)
       #`(resume (cdr #,(compile-computation #'c env))))
      (_
       (syntax-violation 'cdr "ill-formed car computation" c))))

  (define (compile-cons c env)
    (syntax-case c ()
      ((_ c1 c2)
       #`(cons (suspend #,(compile-computation #'c1 env))
               (suspend #,(compile-computation #'c2 env))))
      (_
       (syntax-violation 'cons "ill-formed cons computation" c))))

  (define compile-continuation
    (lambda (name c env)
      (syntax-case c ()
        ((_ v* ...)
         #`(#,name #,@(compile-value* #'(v* ...) env)))
        (_
         (syntax-violation 'compute "ill-formed continuation" c)))))

  (define (compile-force c env)
    (syntax-case c ()
      ((_ v)
       #`(resume (thunk-computation #,(compile-value #'v env))))
      (_
       (syntax-violation 'force "ill-formed force computation" c))))

  (define (compile-lambda c env)
    (syntax-case c ()
      ((_ (x* ...) c)
       (valid-bound-identifiers? #'(x* ...))
       (let* ((y* (generate-temporaries #'(x* ...)))
              (env (append (map (lambda (x y)
                                  (cons (syntax->datum x) (make-variable y)))
                                #'(x* ...) y*)
                           env)))
         #`(lambda #,y* #,(compile-computation #'c env))))
      (_
       (syntax-violation 'lambda "ill-formed lambda computation" c))))

  (define (compile-letrec c env)
    (syntax-case c ()
      ((_ ((x* c*) ...) c)
       (valid-bound-identifiers? #'(x* ...))
       (let* ((y* (generate-temporaries #'(x* ...)))
              (env (append (map (lambda (x y)
                                  (cons (syntax->datum x) (make-variable y)))
                                #'(x* ...) y*)
                           env)))
         (with-syntax
             (((y* ...) y*)
              ((c* ...) (compile-computation* #'(c* ...) env)))
           #`(letrec ((y* (make-thunk (suspend c*))) ...)
               #,(compile-computation #'c env)))))
      (_
       (syntax-violation 'letrec "ill-formed letrec computation" c))))

  (define (compile-if c env)
    (syntax-case c ()
      ((_ v c1 c2)
       #`(if (boolean=? #t #,(compile-value #'v env))
             #,(compile-computation #'c1 env)
             #,(compile-computation #'c2 env)))
      (_
       (syntax-violation 'if "ill-formed if computation" c))))

  (define compile-operator
    (lambda (name c env)
      (syntax-case c ()
        ((_ v* ...)
         #`(shift k
             ((dynamic-lookup '#,name env) k #,@(compile-value* #'(v* ...)
                                                                env))))
        (_
         (syntax-violation 'compute "ill-formed operation" c)))))

  (define (compile-receive c env)
    (syntax-case c ()
      ((_ (x* ...) c1 c2)
       (valid-bound-identifiers? #'(x* ...))
       (let* ((y* (generate-temporaries #'(x* ...)))
              (env (append (map (lambda (x y)
                                  (cons (syntax->datum x) (make-variable y)))
                                #'(x* ...) y*)
                           env)))
         #`(let-values ((#,y* #,(compile-computation #'c1 env)))
             #,(compile-computation #'c2 env))))
      (_
       (syntax-violation 'receive "ill-formed receive computation" c))))

  (define (compile-values c env)
    (syntax-case c ()
      ((_ v* ...)
       #`(values #,@(compile-value* #'(v* ...) env)))
      (_
       (syntax-violation 'values "ill-formed values computation" c))))

  (define compile-with-handler
    (lambda (c env)
      (syntax-case c ()
        ((_ (clause* ...) n)
         (let-values (((name* handler* return)
                       (compile-handler-clause* c #'(clause* ...) env)))
           (with-syntax (((name* ...) name*)
                         ((handler* ...) handler*))
             #`(reset
                 (shift handler-k
                   (call-with-values
                       (suspend (reset
                                  (let ((env
                                         (cons* (cons 'name* handler*) ...
                                                env)))
                                    #,(compile-computation #'n env))))
                     #,return))))))
        (_
         (syntax-violation 'with-handler "ill-formed with-handler computation"
                           c)))))

  (define compile-operator-clause
    (lambda (c clause env)
      (syntax-case clause ()
        (((_ k x* ...) m)
         (valid-bound-identifiers? #'(k x* ...))
         (let* ((y* (generate-temporaries #'(x* ...)))
                (c (car (generate-temporaries #'(c))))
                (env (append (map (lambda (x y)
                                    (cons (syntax->datum x)
                                          (make-variable y)))
                                  #'(x* ...) y*)
                             (cons (cons (syntax->datum #'k)
                                         (make-continuation c))
                                   env))))
           #`(lambda (#,c #,@y*)
               (call-with-values
                   (suspend #,(compile-computation #'m env))
                 handler-k))))
        (_
         (syntax-violation 'with-handler "ill-formed with-handler clause" c
                           clause)))))

  (define compile-values-clause
    (lambda (c clause env)
      (syntax-case clause ()
        (((_ x* ...) m)
         (valid-bound-identifiers? #'(k x* ...))
         (let* ((y* (generate-temporaries #'(x* ...)))
                (env (append (map (lambda (x y)
                                    (cons (syntax->datum x)
                                          (make-variable y)))
                                  #'(x* ...) y*)
                             env)))
           #`(lambda #,y*
               #,(compile-computation #'m env))))
        (_
         (syntax-violation 'with-handler "ill-formed with-handler clause" c
                           clause)))))

  (define compile-handler-clause*
    (lambda (c clause* env)
      (let f ((clause* clause*) (name* '()) (handler* '()) (return #f))
        (if (null? clause*)
            (values (reverse name*) (reverse handler*) (or return #'values))
            (let ((clause (car clause*)) (clause* (cdr clause*)))
              (syntax-case clause ()
                (((kwd . _) _)
                 (identifier? #'kwd)
                 (let ((binding (lookup #'kwd env)))
                   (unless binding
                     (syntax-violation 'with-handler "unbound keyword" clause
                                       #'kwd))
                   (unless (keyword? binding)
                     (syntax-violation 'with-handler "not a keyword" clause
                                       #'kwd))
                   (case (keyword-syntax binding)
                     ((values)
                      (when return
                        (syntax-violation 'with-handler
                                          "duplicate with-handler clause" c
                                          clause))
                      (let ((handler
                             (compile-values-clause c clause env)))
                        (f clause* name* handler* handler)))
                     ((operator)
                      (let ((name (operator-name binding)))
                        (when (memp (lambda (n)
                                      (symbolic-identifier=? n name))
                                    name*)
                          (syntax-violation 'with-handler
                                            "duplicate with-handler clause" c
                                            clause))
                        (let ((handler
                               (compile-operator-clause c clause env)))
                          (f clause* (cons name name*) (cons handler handler*)
                             return))))
                     (else
                      (syntax-violation 'with-handler "invalid clause keyword"
                                        clause #'kwd)))))
                (_
                 (syntax-violation 'with-handler
                                   "ill-formed with-handler clause" c
                                   clause))))))))

  ;;;;;;;;;;;;;
  ;; Helpers ;;
  ;;;;;;;;;;;;;

  (define symbolic-identifier=?
    (lambda (id1 id2)
      (symbol=? (syntax->datum id1) (syntax->datum id2))))

  (define valid-bound-identifiers?
    (lambda (x*)
      (or (null? x*)
          (let ((x (car x*)) (x* (cdr x*)))
            (and (identifier? x)
                 (not (memq x x*))
                 (valid-bound-identifiers? x*)))))))

;; Local Variables:
;; mode: scheme
;; End:
