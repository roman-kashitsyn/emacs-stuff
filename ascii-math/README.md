ascii-math.el
=============

Contains functions to format mathematical expressions.
Actual implementation is the port of the one described in the 
"Programming Scala" book. Here some examples:

    ;; `M-x format-last-sexp' reads last sexp and pretty-print it
    (+ 1 2) =>
    1 + 2
    
    (/ (+ a b) (- a b)) =>
    a + b
    -----
    a - b
    
    (/ (/ a b) (/ c d)) =>
     a
     -
     b
    ---
     c
     -
     d
     
    (* 3 (/ a b)) =>
        a
    3 * -
        b
