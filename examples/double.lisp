; For this to work, it's necessary to build `bindings/double.c`
; Use following command to do this: `gcc -Wall -shared lib.c -o lib.o -ldl`

(define lib-double (dlopen "./lib.o"))

(define double (lambda (n) (lambda (m)
  ((dlsym lib-double "int" "double_f") m n))))

(print (double 20 20))
