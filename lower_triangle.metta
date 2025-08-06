; Define fst and snd for pair access
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

; Build lower triangular matrix row
(= (build-lower-row $row $i $j $n)
   (if (== $j $n) 
       ()
       (let* (($element (index-atom $row $j))
              ($new_j (+ $j 1))
              ($rest (build-lower-row $row $i $new_j $n)))
         (if (<= $j $i)
             (cons-atom $element $rest)
             (cons-atom 0 $rest)))))

; Process matrix row
(= (process-row $row $i)
   (build-lower-row $row $i 0 (size-atom $row)))

; Update accumulator with processed row
(= (update-lower-acc $acc $row)
   (let* (($i (if (== (size-atom $acc) 0)
                  0
                  (snd $acc))) ; Initialize $i to 0 if $acc is empty
          ($new_row (process-row $row $i))
          ($matrix_so_far (if (== (size-atom $acc) 0)
                              ()
                              (fst $acc))) ; Initialize $matrix_so_far to () if $acc is empty
          ($new_matrix (cons-atom $new_row $matrix_so_far))
          ($new_i (+ $i 1)))
      (cons-atom $new_matrix ($new_i))))

; Reverse a list
(= (reverse-atom $lst)
   (foldl-atom $lst () $acc $x (cons-atom $x $acc)))

; Main function to extract lower triangular matrix
(= (lower-triangular $matrix)
   (let* (($result (foldl-atom $matrix () $acc $x (update-lower-acc $acc $x)))
          ($matrix_so_far (fst $result)))
      (reverse-atom $matrix_so_far)))

; Test case
!(lower-triangular ((1 2 3) (4 5 6) (7 8 9))) ; Returns ((1 0 0) (4 5 0) (7 8 9))