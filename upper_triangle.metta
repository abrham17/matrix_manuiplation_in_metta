; Define fst for accessing the first element of a pair
(= (fst $pair) 
   (if (>= (size-atom $pair) 1)
       (car-atom $pair)
       ()))

; Define snd for accessing the second element of a pair
(= (snd $pair)
   (if (>= (size-atom $pair) 2)
       (let* (($rest (cdr-atom $pair)))
         (if (>= (size-atom $rest) 1)
             (car-atom $rest)
             ()))
       ()))

; Reverse a list using foldl-atom
(= (reverse-atom $lst)
   (foldl-atom $lst () $acc $x (cons-atom $x $acc)))

; Build a row for upper triangular matrix at row index i
(= (build-upper-row $row $i $j $n)
   (if (== $j $n)
       ()
       (let* (($index (index-atom $row $j))
              ($new_j (+ $j 1))
              ($build_upper_row (build-upper-row $row $i $new_j $n)))
         (if (>= $j $i)
             (cons-atom $index $build_upper_row)
             (cons-atom 0 $build_upper_row)))))

; Helper to process a row for upper triangular extraction
(= (upper-row-helper $row $i $n) 
   (if (== $n 0)
       ()
       (build-upper-row $row $i 0 $n)))

; Update accumulator for upper triangular matrix with index
(= (update-upper-acc $acc $row)
   (let* (($i (if (== (size-atom $acc) 0)
                  0
                  (snd $acc))) ; Initialize $i to 0 if $acc is empty
          ($n (size-atom $row))
          ($new_row (build-upper-row $row $i 0 $n))
          ($fst_elem (if (== (size-atom $acc) 0)
                         ()
                         (fst $acc))) ; Initialize $fst_elem to () if $acc is empty
          ($new (cons-atom $new_row $fst_elem))
          ($new_i (+ $i 1)))
     (cons-atom $new ($new_i))))

; Extract upper triangular part of a matrix
(= (upper-triangular $matrix)
   (let* (($result (foldl-atom $matrix () $acc $x (update-upper-acc $acc $x)))
          ($matrix_so_far (fst $result)))
      (reverse-atom $matrix_so_far)))

; Test the upper triangular extraction
!(upper-triangular ((3 2 1) (6 5 4) (7 8 9))) ; Returns ((1 2 3) (0 5 6) (0 0 9))