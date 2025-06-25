!(bind! items (
  ( 2 3);water-bottle
  ( 3 4);sandwich
  ( 4 5);chips
  (5 6);apple
  (7 2);banana
))
!(bind! capacity 10)
!(bind! len 5)
!(bind! population-size 10)
!(bind! generations 2)
!(bind! mutation-rate 0.2)
!(bind! crossover-rate 0.7)
!(bind! top  5)


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
      (($new-w $new-c)  (if (== $bit 0)
                        ($w $c)
                        ((+ $w $weight) (+ $c $value))
                      )
      )
    )
    (if (<= $new-w capacity)
        (fitness (+ $index 1) $ind $new-w $new-c)
          0
    ) 
    )
)
)


; !(index-atom  (1 2 3) 2)
; !(fitness 0 (0 1 0 1 0)  0 0)
; !(fitness 0 (1 1 1 1 1)  0 0)


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

(= (selectionSort $list $n)
    (if (or (== $n 0) (== $list ()))
        ()
     (let* (
        ($cur (find-max (car-atom $list ) $list))
        ($newList (remove-first $cur $list))
        ($rest (selectionSort $newList (- $n 1)))
    )
    (cons-atom $cur $rest)
    )))

(= (remove-first $elem $list)
    (if (== $list ())
        ()
        (let* (
            ($head (car-atom $list))
            ($tail (cdr-atom $list))
        )
        (if (== $head $elem)
            $tail
            (let $result (remove-first $elem $tail) (cons-atom $head $result))))))

(= (find-max $max $list)
    (if (== $list ())
        $max
        (let* (
          ($head (car-atom $list))
          ($head1 (car-atom $head))
          ($max1 (car-atom $max))
          ($new-max (if (> $head1 $max1)
                        $head
                        $max))
          ($tail (cdr-atom $list))
        )
        (find-max $new-max $tail)
        )
    )
)
; ! (selectionSort ((5 10) (3 20) (8 15) (1 30) (5 10)) 5)
; ! (find-max (5 10) ((3 20) (8 15) (1 30) (5 10)))

; !(top-5 ( 9 8 7 6 5 4 3 2 1) 5)
(= (eva-helper $ind ) ((fitness 0 $ind 0 0) $ind))

;evaluation
(= (evaluate $population)
  (if (== $population ())
      ()
      (let* (
        ($scoredpop (collapse (eva-helper (superpose $population))))
        (() (println! ($scoredpop)))
        ($sorted-pop (selectionSort $scoredpop population-size))
        (() (println! ($sorted-pop)))
        ($top5 (top-5 $sorted-pop 5))
      )
      $top5
)))(= (eva-helper $ind ) ((fitness 0 $ind 0 0) $ind))

;evaluation
(= (evaluate $population)
  (if (== $population ())
      ()
      (let* (
        ($scoredpop (collapse (eva-helper (superpose $population))))
        (() (println! ($scoredpop)))
        ($sorted-pop (selectionSort $scoredpop population-size))
        (() (println! ($sorted-pop)))
        ($top5 (top-5 $sorted-pop 5))
      )
      $top5
)))

! (evaluate ((1 0 0 0 1) (1 1 1 1 1) (0 1 0 0 1) (1 1 1 1 0) (0 1 1 0 0) (0 1 1 0 0) (1 1 0 0 1) (1 1 0 0 1) (0 1 0 1 1) (1 0 1 1 1)))
;!(evaluate (create-population 10))


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

(= (mutate $ind $rate)
  (if (== $ind ())
      ()
      (let* (
          ($head (car-atom $ind))
          ($tail (cdr-atom $ind))
          ($rand (py-call (py-dot random random) ()))
          ($new-head (if (< $rand $rate)
                         (if (== $head 1) 0 1)
                         $head))
          ($new-tail (mutate $tail $rate))
      )
      (cons-atom $new-head $new-tail))
  )
)

(= (generate-pop $size $top5)
  (if (== $size 0)
    ()
    (let* (
      ($par1 (index-atom $top5 (random-int &rng 0 5)))
      ($par2 (index-atom  $top5 (random-int &rng 0 5)))
      ($p1 (cdr-atom $par1))
      ($p2 (cdr-atom $par2))
      (($child) (mutate (crossover $p1 $p2) mutation-rate)) ; mutation added here
      ($rest (generate-pop (- $size 1) $top5))
    )
    (cons-atom $child $rest )
  ))
)

; ! (generate-pop 5 ((1 1 0 1) (1 0 1 0) (0 1 1 1) (0 0 0 1) (1 1 1 0)))
(= (Generation $n $pop $pop-size $top )
  (let* (
    ($top5 (evaluate $pop))
    (() (println! ("top five are " $top5)))
    ($new-pop (generate-pop $pop-size $top5))
    (() (println! ($new-pop)))
  )
  (if (== $n 0) 
     ( The best individual is (car-atom $top5))
      (Generation (- $n 1) $new-pop $pop-size 5)
  )
))

! (Generation generations (create-population 10) 10 5)
