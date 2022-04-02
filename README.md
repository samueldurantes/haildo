## Haildo programming language

Haildo is a toy programming language

## Goals 

- [x] Interpret something
- [ ] Good error messages
- [ ] Modules system
- [ ] Compile to bytecode

## Example

Hello, World:
```lisp
(print "Hello, World!")
```

Map a list:
```lisp
(import "./std")

(let a (list 1 2 3))

(let b (map (lambda (x)
  (* x 2)
  a)))

(print b)
```
