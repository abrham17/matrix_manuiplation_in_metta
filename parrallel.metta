; Helper function to square a number
(= (square $x) (pow-math $x 2))

(= (sum-of-squares $row) 
(let* (
      ($squared (map-atom $row $x (square $x))))
      (foldl-atom $squared 0 $acc $x (+ $acc $x))
)
)
; Compute the L2 norm of a row
(= (l2-norm $row) (sqrt-math (sum-of-squares $row)))
; Normalize a row given its norm (recursive)
(= (normalize-row $row $norm) 
   (if (== $row ()) 
      ()
   (let*(($normalize (normalize-row (cdr-atom $row) $norm))
         ($fst_ele (car-atom $row))
         ($normalized (/ $fst_ele $norm ))) 
       (cons-atom $normalized $normalize))
   )       
)
; Helper to normalize a row by computing its norm
(= (normalize-row-helper $row) (normalize-row $row (l2-norm $row)))

; Normalize all rows of a matrix
(= (normalize-matrix $matrix)
      (let* (($rows (superpose $matrix) )
            ($normalized_rows (normalize-row-helper $rows)))
         $normalized_rows
      )     
)
!(normalize-matrix ((1 2 4) (2 3 4)))

; Generate a range from 0 to n-1 (recursive)
; Define the range function

; Test the range function
;!(range 4)
; Build a row for upper triangular matrix at row index i


; Test Case 1: Matrix Row Normalization
;!(normalize-matrix ((1 2 2) (3 0 0) (0 4 3)))

