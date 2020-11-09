#! /usr/bin/env scheme-script
#!r6rs
(import (rnrs)
        (steme compute))

(compute
  (with-handler
      (((display k s)
        (begin
          (display s)
          (display "\n")
          (k))))
    (letrec ((fact (lambda (n)
                     (if (zero? n)
                         (values 1)
                         (receive (x)
                             ((force fact) (- n 1))
                           (values (* x n)))))))
      (receive (res)
          ((force fact) 6)
        (display res)))))
