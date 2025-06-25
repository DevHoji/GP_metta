;value weight
!(bind! items (
  ( 2 3);water-bottle
  ( 3 4);sandwich
  ( 4 5);chips
  (5 6);apple
  (7 2);banana
))
!(bind! capacity 10)
!(bind! len 5)
!(bind! population-size 50)
!(bind! generations 10)
!(bind! mutation-rate 0.2)
!(bind! crossover-rate 0.7)
!(bind! tr  5)
(= (create-individual $n) 
(if (>= 0 $n )
()
  (let* (
      ($rnd (random-int &rng 0 2))
      ($tail(create-individual (- $n 1)))
  )
  (cons-atom  $rnd $tail)
)
  
   
)
)
; !(create-individual 5)
 
(= (create-population $size)
(if (>= 0 $size)
()
  (let* (
    ($tail (create-population (- $size 1)))
    ($ind (create-individual len ))
  )
  (cons-atom $ind $tail)
)
)
) 

;!(create-population 10)


(= (fitness $index  $ind $w $c)
(if (>= $index (- len 1))
    $c
  (let* (
    ($item (index-atom items $index))
    (($value $weight) $item)
    
    ($bit (index-atom $ind $index))
     ( ($new-w $new-c)  (if (== $bit 0)
          ($w $c)
        ((+ $w $weight) (+ $c $value))
          ))
  )
  
    (if (<= $new-w capacity)
    (fitness (+ $index 1) $ind $new-w $new-c)
      0
  ))
)
)


; !(index-atom  (1 2 3) 2)
; !(fitness 0 (0 1 0 1 0)  0 0)
; !(fitness 0 (1 1 1 1 1)  0 0)
! (bind! toInt (py-atom int))
(= (selectionSort $list $n) (selectionSort $list $n <))
(= (selectionSort $list $n $op)
    (if (or (== $n 0) (== $list ()))
        $list
     (let* (
        ($fn (if (== $op <) min-atom max-atom))
        ($cur (toInt ($fn $list)))
        ($newList (subtraction-atom $list ($cur)))
        ($rest (selectionSort $newList (- $n 1) $op))
    )
    (cons-atom $cur $rest)
    )))


(= (top-5 $pop $n)
(if (or (== $n 0) (== $pop ()))
()
  (let* (
    ($head (car-atom $pop))
    ($tail (cdr-atom $pop))
    ($rest (top-5 $tail (- $n 1)))
  )
  (cons-atom $head $rest)
)
)
)

; !(top-5 ( 9 8 7 6 5 4 3 2 1) 5)
(= (eva-helper $ind ) ((fitness 0 $ind 0 0) $ind))

;evaluation
(= (evaluate $population)
(if (== $population ())
()
  (let* (
    ($scoredpop (collapse (eva-helper (superpose population))))
    ($sorted-pop (selectionSort $scoredpop population-size >))
    ($top5 (top-5 $sorted-pop 5))
  )
  $top5
)))
 (: splitAt (-> Number Expression (Atom Atom)))
(= (splitAt $n $list)
   (case ($n $list)
     (
       ((0 $list) (() $list))
       (($_ ()) (() ()))
       (($n $list)
          (let*
             (
               
               (($x $xs) (decons-atom $list))
               (($left $right) (splitAt (- $n 1) $xs))
               
             )
             ((cons-atom $x $left) $right))))))

;!(evaluate (create-population 10))
(= (crossover $ind1 $ind2)
(if (or (== $ind1 ()) (== $ind2 ()))
()
  (let* (
    ($rnd (random-int &rng 0 len))


    (= (crossover $p1 $p2)
    (let*
        (
            ($len (size-atom $p1))
            ($rand (random-int &rng 0 $len))
            (($head $tail) (splitAt $rand $p1))
            (($head2 $tail2) (splitAt $rand $p2))
        )
        (union-atom $head $tail2)

    )
)


 (= (mutate $ind $probMutation)
    (if (== $ind ())
        () 
        (let* (
                ($head (car-atom $ind))
                ($tail (cdr-atom $ind))
                ($rand (rd ((py-dot np random.uniform) 0 1) 3))  ;; Generate a random number in [0,1] (rounded to 3 decimal places for precision)
                ($newCodon (if (< $rand $probMutation)            ;; If the random number is less than the probability of mutation, replace the codon with a new random value
                              (rd ((py-dot np random.uniform) 0 1) 3)  
                              $head))  
                ($rest (mutate $tail $probMutation))
            )
            (cons-atom $newCodon $rest)  
        )
    )
)

