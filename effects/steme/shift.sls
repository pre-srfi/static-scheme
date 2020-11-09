(library (steme shift)
  (export reset shift)
  (import (rnrs))

  ;; See: "Final Shift for Call/cc: Direct Implementation of Shift and
  ;; Reset" (Martin Gasbichler/Michael Sperber).

  ;;;;;;;;;;;;
  ;; Syntax ;;
  ;;;;;;;;;;;;

  (define-syntax reset
    (syntax-rules ()
      ((reset e) (%reset (lambda () e)))))

  (define-syntax shift
    (syntax-rules ()
      ((shift k e) (%shift (lambda (k) e)))))

  ;;;;;;;;;;;;;;;;;;;;;;;
  ;; Helper Procedures ;;
  ;;;;;;;;;;;;;;;;;;;;;;;

  (define *meta-continuation*
    (lambda v*
      (error 'shift "missing reset")))

  (define (abort thunk)
    (call-with-values thunk *meta-continuation*))

  (define (%reset thunk)
    (let ((mc *meta-continuation*))
      (call/cc
       (lambda (k)
         (begin
           (set! *meta-continuation*
             (lambda v*
               (set! *meta-continuation* mc)
               (apply k v*)))
           (abort thunk))))))

  (define (%shift f)
    (call/cc
     (lambda (k)
       (abort (lambda ()
                (f (lambda v*
                     (reset (apply k v*))))))))))

;; Local Variables:
;; mode: scheme
;; End:
