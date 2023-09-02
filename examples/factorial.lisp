(define factorial (lambda (n)
  (if (= n 0)
  1
  (* n (factorial (- n 1))))))

(print (factorial 7)) ;; 5040
