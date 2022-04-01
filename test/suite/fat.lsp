(fun fat (x)
    (if (< x 2)
        1
        (* x (fat (- x 1)))))

(print (fat 10))