(library (steme compute)
  (export compute)
  (import (rnrs)
          (for (steme compute $compiler) run expand))

  (define-syntax compute
    (lambda (stx)
      (syntax-case stx ()
        ((_ c)
         #`(let ((env initial-dynamic-environment))
             (reset #,(compile-computation #'c initial-environment))))
        (_
         (syntax-violation 'compute "ill-formed compute form" stx))))))

;; Local Variables:
;; mode: scheme
;; End:
