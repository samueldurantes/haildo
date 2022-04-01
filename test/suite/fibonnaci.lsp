(fun fib (x)
    (if (< x 2)
        x
        (+ (fib (- x 1)) (fib (- x 2)))))

(print (fib 10))