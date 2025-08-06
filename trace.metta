; First and second elements of a pair
(= (fst $pair)
   (if (>= (size-atom $pair) 1)
       (car-atom $pair)
       ()))
(= (snd $pair)
   (if (>= (size-atom $pair) 2)
       (let* (($rest (cdr-atom $pair)))
         (if (>= (size-atom $rest) 1)
             (car-atom $rest)
             ()))
       ()))

; Range generator (0 to n-1)
(= (range $start $end)
   (if (>= $start $end)
       ()
       (cons-atom $start (range (+ $start 1) $end))))

; Get element at position j in a row
(= (get-element $row $j) (index-atom $row $j))

; Get a column from matrix (recursive implementation)
(= (column $mat $j)
   (if (== $mat ())
       ()
       (let* (($row (car-atom $mat))
              ($rest (cdr-atom $mat))
              ($element (get-element $row $j))
              ($next (column $rest $j)))
         (cons-atom $element $next))))

; Dot product of two vectors
(= (dot-product $vec1 $vec2)
   (if (or (== $vec1 ()) (== $vec2 ()))
       0
       (let* (($fst (car-atom $vec1))
              ($snd (car-atom $vec2))
              ($rest1 (cdr-atom $vec1))
              ($rest2 (cdr-atom $vec2)))
         (+ (* $fst $snd)
            (dot-product $rest1 $rest2)))))

; Compute single matrix product element
(= (matrix-element $B $C $i $j)
   (dot-product
      (index-atom $B $i)
      (column $C $j)))

; Helper function to compute row recursively
(= (compute-row-helper $B $C $i $j $p)
   (if (== $j $p)
       ()
       (let* (($new_j (+ $j 1))
              ($next (compute-row-helper $B $C $i $new_j $p))
              ($element (matrix-element $B $C $i $j)))
         (cons-atom $element $next))))

; Compute i-th row of matrix product B * C
(= (compute-row $B $C $i $p)
   (compute-row-helper $B $C $i 0 $p))

; Matrix multiplication
(= (matrix-mult-helper $B $C $i $n $p)
   (if (== $i $n)
       ()
       (let* (($new_i (+ $i 1))
              ($row (compute-row $B $C $i $p))
              ($rest (matrix-mult-helper $B $C $new_i $n $p)))
         (cons-atom $row $rest))))

(= (matrix-mult $B $C)
   (let* (($n (size-atom $B))
          ($p (if (> $n 0) (- (size-atom (index-atom $C 0)) 1) 0)))
     (matrix-mult-helper $B $C 0 $n $p)))

(= (identity-row $n $i $j)
   (if (== $j $n)
       ()
       (cons-atom (if (== $i $j) 1 0)
                  (identity-row $n $i (+ $j 1)))))


; Identity matrix
(= (identity-matrix $n)
   (if (== $n 0)
       ()
       (let* (($row (identity-row $n $n 0))
              ($rest (identity-matrix (- $n 1))))
         (cons-atom $row $rest))))


; Matrix power
(= (matrix-pow $A $k)
   (if (== $k 0)
       (identity-matrix (size-atom $A))
       (if (== $k 1)
           $A
           (matrix-mult $A (matrix-pow $A (- $k 1))))))

(= (update-trace-acc $acc $row)
   (let* (($sum (fst $acc))
          ($i (snd $acc))
          ($new_sum (+ $sum (index-atom $row $i)))
          ($new_i (+ $i 1)))
     (cons-atom $new_sum ($new_i))))

(= (trace $matrix)
    (let* (
            ($acc_n (cons-atom 0 (0)))
            ($result (foldl-atom $matrix (0 0) $acc $x (update-trace-acc $acc $x)))
             ($sum (fst $result)))
      $sum))

; Trace of matrix power
(= (trace-pow $A $k)
   (trace (matrix-pow $A $k)))

; Test cases
!(compute-row ((1 2) (3 4)) ((5 6) (7 8)) 0 2)  ; Returns (19 22)
!(matrix-element ((1 2) (3 4)) ((5 6) (7 8)) 0 1)  ; Returns 22
!(matrix-pow ((1 2) (3 4)) 2)  ; Returns ((7 10) (15 22))
!(trace-pow ((1 2) (3 4)) 2)  ; Returns 29