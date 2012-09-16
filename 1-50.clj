; TODO: update the macro to take multiple problem test cases
; not just multiple solutions
(defmacro problem [probArgs probBody & solList]
  `(assert (= 0 (count (filter false? (map (fn ~probArgs
  		(and true ~@probBody)
  ) (list ~@solList))))))
)

; Problem 1
(problem[_] 
	(list (= _ true))
	true
)

; Problem 2
(problem[_]
	(list (= (- 10 (* 2 3)) _))
	4
)

; Problem 3
(problem[_]
	(list (= _ (.toUpperCase "hello world")))
	"HELLO WORLD"
)

; skipping to some more interesting problems...

; Problem 30
; Write a function which removes consecutive duplicates from a sequence.
(problem[_]
	(list
		(= (apply str (_ "Leeeeeerrroyyy")) "Leroy")
	)
	(fn [x] (map first (partition-by identity x)))
)

; Problem 31
; Write a function which packs consecutive duplicates into sub-lists.
(problem[_]
	(list 
		(= (_ [1 1 2 1 1 1 3 3]) '((1 1) (2) (1 1 1) (3 3)))
		(= (_ [:a :a :b :b :c]) '((:a :a) (:b :b) (:c)))
		(= (_ [[1 2] [1 2] [3 4]]) '(([1 2] [1 2]) ([3 4])))
	)
	(fn [x] (partition-by identity x))
)

; Problem 32
; Write a function which duplicates each element of a sequence.
(problem[_]
	(list
		(= (_ [1 2 3]) '(1 1 2 2 3 3))
		(= (_ [:a :a :b :b]) '(:a :a :a :a :b :b :b :b))
		(= (_ [[1 2] [3 4]]) '([1 2] [1 2] [3 4] [3 4]))
		(= (_ [[1 2] [3 4]]) '([1 2] [1 2] [3 4] [3 4]))
	)
	(fn [s] (mapcat identity
    	(map (fn [x] (cons x (cons x '()))) s)))
)

; Problem 33
; Write a function which replicates each element of a sequence a variable number of times.
(problem[_]
	(list 
		(= (_ [1 2 3] 2) '(1 1 2 2 3 3))
		(= (_ [:a :b] 4) '(:a :a :a :a :b :b :b :b))
		(= (_ [4 5 6] 1) '(4 5 6))
		(= (_ [[1 2] [3 4]] 2) '([1 2] [1 2] [3 4] [3 4]))
		(= (_ [44 33] 2) [44 44 33 33])
	)
	(fn [list num]
  		(mapcat #(repeat num %) list))
)