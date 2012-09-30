; http://www.4clojure.com solutions
; @author Tantaman

; Macro for running problems & solutions against their test cases.
(defmacro problem [probArgs probBody & solList]
  `(assert (= 0 (count (filter false? (map (fn ~probArgs
  		(and true ~@probBody)
  ) (list ~@solList))))))
)

; Problem 1 - 14 : skipped.  I'm sure nobody is too interested in those solutions ;)

; Problem 14
; Clojure has many different ways to create functions.
(problem[_]
	; In this file,
	; test cases from the 4clojure problem go in the list
	(list
		(= _ ((fn add-five [x] (+ x 5)) 3))
		(= _ ((fn [x] (+ x 5)) 3))
		(= _ (#(+ % 5) 3))
		(= _ ((partial + 5) 3))
	)
	; any number of solutions follow.
	8 ; first solution
	(mod 17 9) ; second solution
	(- (* 2 8) 8) ; third solution
	; etc...
)

; Problem 15
; Write a function that doubles a number
(problem[_]
	(list
		(= (_ 2) 4)
		(= (_ 3) 6)
		(= (_ 11) 22)
		(= (_ 7) 14)
	)
	(partial * 2)
)

; Problem 16
; Write a function which returns a personalized greeting.
(problem[_]
	(list
		(= (_ "Dave") "Hello, Dave!")
		(= (_ "Jenn") "Hello, Jenn!")
		(= (_ "Rhea") "Hello, Rhea!")
	)
	(fn [name] (str "Hello, " name "!"))
)

; Problem 17
; The map function takes two arguments: a function (f) and a sequence (s). 
; Map returns a new sequence consisting of the result of applying f to each item of s. 
; Do not confuse the map function with the map data structure.
(problem[_]
	(list
		(= _ (map #(+ % 5) '(1 2 3)))
	)
	'(6 7 8)
)

; Problem 18
; The filter function takes two arguments:
;  a predicate function (f) and a sequence (s). 
; Filter returns a new sequence consisting of all the items of s 
; for which (f item) returns true.
(problem[_]
	(list
		(= _ (filter #(> % 5) '(3 4 5 6 7)))
	)
	'(6 7)
)

; Problem 19
; Write a function which returns the last element in a sequence.

; Forbidden: last
(problem[_]
	(list
		(= (_ [1 2 3 4 5]) 5)
		(= (_ '(5 4 3)) 3)
		(= (_ ["b" "c" "d"]) "d")
	)
	(fn [seq] (nth seq (- (count seq) 1)))
)

; Problem 20
; Write a function which returns the second to last element from a sequence.
(problem[_]
	(list
		(= (_ (list 1 2 3 4 5)) 4)
		(= (_ ["a" "b" "c"]) "b")
		(= (_ [[1 2] [3 4]]) [1 2])
	)
	(fn [x] (nth (reverse x) 1))
)

; Problem 21
; Write a function which returns the Nth element from a sequence.

; Forbidden: nth
(problem[_]
	(list
		(= (_ '(4 5 6 7) 2) 6)
		(= (_ [:a :b :c] 0) :a)
		(= (_ [1 2 3 4] 1) 2)
		(= (_ '([1 2] [3 4] [5 6]) 2) [5 6])
	)
	(fn [list idx] (last (take (+ idx 1) list)))
)

; Problem 22
; Write a function which returns the total number of elements in a sequence.

; Forbidden: count
(problem[_]
	(list
		(= (_ '(1 2 3 3 1)) 5)
		(= (_ "Hello World") 11)
		(= (_ [[1 2] [3 4] [5 6]]) 3)
		(= (_ '(13)) 1)
		(= (_ '(:a :b :c)) 3)
	)
	(fn [x] (alength (to-array x))) ; lol... I think this is cheating ;)
)

; Problem 23
; Write a function which reverses a sequence.

; Forbidden: reverse, rseq
(problem[_]
	(list
		(= (_ [1 2 3 4 5]) [5 4 3 2 1])
		(= (_ (sorted-set 5 7 2 7)) '(7 5 2))
		(= (_ [[1 2][3 4][5 6]]) [[5 6][3 4][1 2]])
	)
	(fn [x] (reduce #(conj %1 %2) '() x)) 
	; or more simply: reduce #(conj %1 %2) '()
	; the macro here has a few restriction that require the function wrapper.
	; Looks like that will be my next topic of study.
)

; Problem 24
; Write a function which returns the sum of a sequence of numbers.
(problem[_]
	(list
		(= (_ [1 2 3]) 6)
		(= (_ (list 0 -2 5 5)) 8)
		(= (_ #{4 2 1}) 7)
		(= (_ '(0 0 -1)) -1)
		(= (_ '(1 10 3)) 14)
	)
	; sol 1.
	(fn [x] (reduce + x))
	; sol 2.
	#(apply + %)
)

; Problem 25
; Write a function which returns only the odd numbers from a sequence.
(problem[_]
	(list
		(= (_ #{1 2 3 4 5}) '(1 3 5))
		(= (_ [4 2 1 6]) '(1))
		(= (_ [2 2 4 6]) '())
		(= (_ [1 1 1 3]) '(1 1 1 3))
	)
	(fn [x] (filter (fn [v] (= (mod v 2) 1)) x))
)

; Problem 26
; Write a function which returns the first X fibonacci numbers.
(problem[_]
	(list
		(= (_ 3) '(1 1 2))
		(= (_ 6) '(1 1 2 3 5 8))
		(= (_ 8) '(1 1 2 3 5 8 13 21))
	)
	(fn [x] 
	  (map (fn fib[n] (if (= n 1) 1 (if (<= n 0) 0 (+ (fib (- n 1)) (fib (- n 2))))))
	  (range 1 (+ x 1))))
)

; Problem 27
; Write a function which returns true if the given sequence is a palindrome.
; Hint: "racecar" does not equal '(\r \a \c \e \c \a \r)
(problem[_]
	(list
		(false? (_ '(1 2 3 4 5)))
		(true? (_ "racecar"))
		(true? (_ [:foo :bar :foo]))
		(true? (_ '(1 1 3 3 1 1)))
		(false? (_ '(:a :b :c)))
	)
	(fn [x] (= (reverse (reverse x)) (reverse x)))
)

; Problem 28
; Write a function which flattens a sequence.
(problem[_]
	(list
		(= (_ '((1 2) 3 [4 [5 6]])) '(1 2 3 4 5 6))
		(= (_ ["a" ["b"] "c"]) '("a" "b" "c"))
		(= (_ '((((:a))))) '(:a))
	)
	(fn [x] (filter (complement sequential?)
            (rest (tree-seq sequential? seq x))))
)

; Problem 29
; Write a function which takes a string and returns a new string containing only the capital letters.
(problem[_]
	(list
		(= (_ "HeLlO, WoRlD!") "HLOWRD")
		(empty? (_ "nothing"))
		(= (_ "$#A(*&987Zf") "AZ")
	)
	(fn [string] (apply str (filter (fn [x] (re-matches #"[A-Z]" (str x))) string)))
)

; Problem 30
; Write a function which removes consecutive duplicates from a sequence.
(problem[_]
	(list
		(= (apply str (_ "Leeeeeerrroyyy")) "Leroy")
		(= (_ [1 1 2 3 3 2 2 3]) '(1 2 3 2 3))
		(= (_ [[1 2] [1 2] [3 4] [1 2]]) '([1 2] [3 4] [1 2]))
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

; Problem 34
; Write a function which creates a list of all integers in a given range.
(problem[_]
	(list
		(= (_ 1 4) '(1 2 3))
		(= (_ -2 2) '(-2 -1 0 1))
		(= (_ 5 8) '(5 6 7))
	)
	#(take (- %2 %1) (iterate inc %1))
)

; Problem 35
; Clojure lets you give local names to values using the special let-form.
(problem[_]
	(list
		(= _ (let [x 5] (+ 2 x)))
		(= _ (let [x 3, y 10] (- y x)))
		(= _ (let [x 21] (let [y 3] (/ x y))))
	)
	7
)

; Problem 36
; Can you bind x, y, and z so that these are all true?
; (altproblem[_]
; 	(= 10 (let _ (+ x y)))
; 	(= 4 (let _ (+ y z)))
; 	(= 1 (let _ z))
; 	[x 7 z 1 y 3]
; )

; Problem 37
; Regex patterns are supported with a special reader macro.
(problem[_]
	(list
		(= _ (apply str (re-seq #"[A-Z]+" "bA1B3Ce ")))
	)
	"ABC"
)

; Problem 38
; Write a function which takes a variable number of parameters and returns the maximum value.
(problem[_]
	(list
		(= (_ 1 8 3 4) 8)
		(= (_ 30 20) 30)
		(= (_ 45 67 11) 67)
	)
	(fn [x & xs] (reduce #(if (< %1 %2) %2 %1) x xs))
)

; Problem 39
; Write a function which takes two sequences and returns the first item from each, then the second item from each, then the third, etc.
(problem[_]
	(list
		(= (_ [1 2 3] [:a :b :c]) '(1 :a 2 :b 3 :c))
		(= (_ [1 2] [3 4 5 6]) '(1 3 2 4))
		(= (_ [1 2 3 4] [5]) [1 5])
		(= (_ [30 20] [25 15]) [30 25 20 15])
	)
	#(mapcat vector %1 %2)
)

; Problem 40
; Write a function which separates the items of a sequence by an arbitrary value.
(problem[_]
	(list
		(= (_ 0 [1 2 3]) [1 0 2 0 3])
		(= (apply str (_ ", " ["one" "two" "three"])) "one, two, three")
		(= (_ :z [:a :b :c :d]) [:a :z :b :z :c :z :d])
	)
	(fn [split arg] (drop 1
  		(flatten (map #(list split %1) arg))))
)

; Problem 41
; Write a function which drops every Nth item from a sequence.
(problem[_]
	(list
		(= (_ [1 2 3 4 5 6 7 8] 3) [1 2 4 5 7 8])
		(= (_ [:a :b :c :d :e :f] 2) [:a :c :e])
		(= (_ [1 2 3 4 5 6] 4) [1 2 3 5 6])
	)
	(fn [xs step] 
	  (flatten
	    (reduce
	      #(conj (if (= (count %2) step) (drop-last 1 %2) %2) %1) `()
	      (partition step step `() xs)
	    )
	  )
	)
)

; Problem 42
; Write a function which calculates factorials.
(problem[_]
	(list
		(= (_ 1) 1)
		(= (_ 3) 6)
		(= (_ 5) 120)
		(= (_ 8) 40320)
	)
	
	; recursive soltuion
	(fn fact[n] 
	  (if (= n 1) 
	    n 
	    (* n (fact (- n 1)))
	  )
	)

	; alternative, iterative solution using sequences
	#(apply * (range 1 (+ % 1)))
)

; Problem 43
(problem[_]
	(list
		(= (_ [1 2 3 4 5 6] 2) '((1 3 5) (2 4 6)))
		(= (_ (range 9) 3) '((0 3 6) (1 4 7) (2 5 8)))
		(= (_ (range 10) 5) '((0 5) (1 6) (2 7) (3 8) (4 9)))
	)
	(fn [xs step]
  		(vals (group-by #(mod % step) xs)))
)

; Problem 44
; Write a function which reverses the interleave process into x number of subsequences.
(problem[_]
	(list
		(= (_ 2 [1 2 3 4 5]) '(3 4 5 1 2))
		(= (_ -2 [1 2 3 4 5]) '(4 5 1 2 3))
		(= (_ 6 [1 2 3 4 5]) '(2 3 4 5 1))
		(= (_ 1 '(:a :b :c)) '(:b :c :a))
		(= (_ -4 '(:a :b :c)) '(:c :a :b))
	)
	#(flatten 
	  (if (> %1 0)
	    (cons (drop (mod %1 (count %2)) %2) (take (mod %1 (count %2)) %2))
	    (cons 
	      (drop (- (count %2) (mod (Math/abs %1) (count %2))) %2)
	      (take (- (count %2) (mod (Math/abs %1) (count %2))) %2))
	  )
	)
)

; Problem 45
; The iterate function can be used to produce an infinite lazy sequence
(problem[_]
	(list
		(= _ (take 5 (iterate #(+ 3 %) 1)))
	)
	'(1 4 7 10 13)
)

; Problem 46
; Write a higher-order function which flips the order of the arguments of an input function.
(problem[_]
	(list
		(= 3 ((_ nth) 2 [1 2 3 4 5]))
		(= true ((_ >) 7 8))
		(= 4 ((_ quot) 2 8))
		(= [1 2 3] ((_ take) [1 2 3 4 5] 3))
	)
	(fn [g] 
  		(fn [& xs] (apply g (reverse xs)))
	)
)

; Problem 47
; The contains? function checks if a KEY is present in a given collection. This often leads beginner clojurians to use it incorrectly with numerically indexed collections like vectors and lists.
(problem[_]
	(list
		(contains? #{4 5 6} _)
		(contains? [1 1 1 1 1] _)
		(contains? {4 :a 2 :b} _)
		(not (contains? '(1 2 4) _))
	)
	4
)

; Problem 48
(problem[_]
	(list
		(= _ (some #{2 7 6} [5 6 7 8]))
		(= _ (some #(when (even? %) %) [5 6 7 8]))
	)
	6
)


; Problem 49
; Write a function which will split a sequence into two parts.

; You may not use: split-at
(problem[_]
	(list
		(= (_ 3 [1 2 3 4 5 6]) [[1 2 3] [4 5 6]])
		(= (_ 1 [:a :b :c :d]) [[:a] [:b :c :d]])
		(= (_ 2 [[1 2] [3 4] [5 6]]) [[[1 2] [3 4]] [[5 6]]])
	)
	#(cons (take %1 %2) (list (drop %1 %2)))
)

; Problem 50
; Write a function which takes a sequence consisting of items with different 
; types and splits them up into a set of homogeneous sub-sequences. The internal order of each sub-sequence should be maintained, but the sub-sequences themselves can be returned in any order (this is why 'set' is used in the test cases).
(problem[_]
	(list
		(= (set (_ [1 :a 2 :b 3 :c])) #{[1 2 3] [:a :b :c]})
		(= (set (_ [:a "foo"  "bar" :b])) #{[:a :b] ["foo" "bar"]})
		(= (set (_ [[1 2] :a [3 4] 5 6 :b])) #{[[1 2] [3 4]] [:a :b] [5 6]})
	)
	; Grouping by class
	#(into #{} (vals (group-by class %1)))
	; Grouping by type
	#(into #{} (vals (group-by type %1)))
)

; Problem 51
; Here is an example of some more sophisticated destructuring.
(problem[_]
	(list
		(= [1 2 [3 4 5] [1 2 3 4 5]] (let [[a b & c :as d] _] [a b c d]))
	)
	[1 2 3 4 5]
)


; Problem 52
; Let bindings and function parameter lists support destructuring.

; TODO: figure out how to generalize our macro to handle 
; running problems and solutions of this sort of form.

;(problem[_]
;	(list
;		(= [2 4] (let [[a b c d e f g] (range)] _))
;	)
;	[c e]
; )

; Problem 53
; Given a vector of integers, find the longest consecutive sub-sequence of increasing numbers. If two sub-sequences have the same length, use the one that occurs first. 
; An increasing sub-sequence must have a length of 2 or greater to qualify.
(problem[_]
	(list
		(= (_ [1 0 1 2 3 0 4 5]) [0 1 2 3])
		(= (_ [5 6 1 3 2 7]) [5 6])
		(= (_ [2 3 3 4 5]) [3 4 5])
		(= (_ [7 6 5 4]) [])
	)
	#(let [groups (group-by count (filter (fn [f] (> (count f) 1)) (reduce (fn [l r] 
	  (let [grouping (last l)]
	  (if (or (nil? (last grouping)) (= r (+ (last grouping) 1)))
	    (conj (pop l) (conj grouping r))
	    (conj l (vector r))
	  ))
	) (vector []) %)))] (flatten (groups (last (sort (keys groups))))))
)


; Problem 54
; Write a function which returns a sequence of lists of x items each.
; Lists of less than x items should not be returned.

; You may not use: partition, partition-all
(problem[_]
	(list
		(= (_ 3 (range 9)) '((0 1 2) (3 4 5) (6 7 8)))
		(= (_ 2 (range 8)) '((0 1) (2 3) (4 5) (6 7)))
		(= (_ 3 (range 8)) '((0 1 2) (3 4 5)))
	)
	#(filter (fn [x] (= (count x) %1)) (reverse (map reverse (reduce (fn [l r]
	    (let [grouping (first l)]
	      (if (or (nil? (first grouping)) (< (count grouping) %1))
	        (conj (pop l) (conj grouping r))
	        (conj l (list r))
	    ))
	) (list '()) %2))))
)

; Problem 55
; Write a function which returns a map containing the number of occurences
; of each distinct item in a sequence.

; Forbidden: frequencies
(problem[_]
	(list
		(= (_ [1 1 2 3 2 1 1]) {1 4, 2 2, 3 1})
		(= (_ [:b :a :b :a :b]) {:a 2, :b 3})
		(= (_ [:b :a :b :a :b]) {:a 2, :b 3})
	)
	#(apply assoc {} (mapcat identity (map 
	  (fn [v] (list (first v) (count (second v))))
	  (group-by identity %))))
)

; Problem 56
; Write a function which removes the duplicates from a sequence. Order of the items must be maintained.

; Forbidden: distinct
(problem[_]
	(list
		(= (_ [1 2 1 3 1 2 4]) [1 2 3 4])
		(= (_ [:a :a :b :b :c :c]) [:a :b :c])
		(= (_ '([2 4] [1 2] [1 3] [1 3])) '([2 4] [1 2] [1 3]))
		(= (_ (range 50)) (range 50))
	)
	#(sort-by 
		(fn [x] (.indexOf % x))
		(into [] (into #{} %))
	)
)

; Problem 57
; A recursive function is a function which calls itself. 
; This is one of the fundamental techniques used in functional programming.
(problem[_]
	(list
		(= _ ((fn foo [x] (when (> x 0) (conj (foo (dec x)) x))) 5))
	)
	'(5 4 3 2 1)
)

; Problem 58
; Write a function which allows you to create function compositions.
; The parameter list should take a variable number of functions, 
; and create a function applies them from right-to-left.

; Forbidden: comp
(problem[_]
	(list
		(= [3 2 1] ((_ rest reverse) [1 2 3 4]))
		(= 5 ((_ (partial + 3) second) [1 2 3 4]))
		(= true ((_ zero? #(mod % 8) +) 3 5 7 9))
		(= "HELLO" ((_ #(.toUpperCase %) #(apply str %) take) 5 "hello world"))
	)
	(fn [& fs] 
	  (fn [& args]
	    (first
	      (reduce 
	        #(list (apply %2 %1))
	        args
	        (reverse fs)
	      ))
	  ))
)

; Problem 59
; Take a set of functions and return a new function that takes a variable number of arguments
; and returns a sequence containing the result of applying each 
; function left-to-right to the argument list.

; Forbidden: juxt
(problem[_]
	(list
		(= [21 6 1] ((_ + max min) 2 3 5 1 6 4))
		(= ["HELLO" 5] ((_ #(.toUpperCase %) count) "hello"))
		(= [2 6 4] ((_ :a :c :b) {:a 2, :b 4, :c 6, :d 8 :e 10}))
	)
	(fn [& fs] 
	  (fn [& args]
	      (mapcat identity (rest (reverse (reduce 
	        #(conj %1 (list (apply %2 (last %1))))
	        (list args)
	        fs
	      ))))
	  ))
)


; Problem 60
; Forbidden: reductions
; Write a function which behaves like reduce,
; but returns each intermediate value of the reduction. 
; Your function must accept either two or three arguments, and the return sequence must be lazy.
(problem[_]
	(list
		(= (take 5 (_ + (range))) [0 1 3 6 10])
		(= (_ conj [1] [2 3 4]) [[1] [1 2] [1 2 3] [1 2 3 4]])
		(= (last (_ * 2 [3 4 5])) (reduce * 2 [3 4 5]) 120)
	)
	(fn r 
	  ([f init [x & xs]]
	    (cons init (lazy-seq (when x (r f (f init x) xs))))
	  )
	  ([f coll]
	    (r f (first coll) (rest coll))
	  )
	)
)

; Problem 61
; Write a function which takes a vector of keys and a vector of values
; and constructs a map from them.

; Forbidden: zipmap
(problem[_]
	(list
		(= (_ [:a :b :c] [1 2 3]) {:a 1, :b 2, :c 3})
		(= (_ [1 2 3 4] ["one" "two" "three"]) {1 "one", 2 "two", 3 "three"})
		(= (_ [:foo :bar] ["foo" "bar" "baz"]) {:foo "foo", :bar "bar"})
	)
	#(apply assoc {} (interleave %1 %2))
)

; Problem 62
; Given a side-effect free function f and an initial value x write a function which returns 
; an infinite lazy sequence of x, (f x), (f (f x)), (f (f (f x))), etc.

; Forbidden: iterate
(problem[_]
	(list
		(= (take 5 (_ #(* 2 %) 1)) [1 2 4 8 16])
		(= (take 100 (_ inc 0)) (take 100 (range)))
		(= (take 9 (_ #(inc (mod % 3)) 1)) (take 9 (cycle [1 2 3])))
	)
	(fn pow [f x] (cons x (lazy-seq (pow f (f x)))))
)

; Problem 63
; Given a function f and a sequence s, write a function which returns a map. 
; The keys should be the values of f applied to each item in s. 
; The value at each key should be a vector of corresponding items in the order they appear 
; in s.

; Forbidden: group-by
(problem[_]
	(list
		(= (_ #(> % 5) [1 3 6 8]) {false [1 3], true [6 8]})
		(= (_ #(apply / %) [[1 2] [2 4] [4 6] [3 6]])
   			{1/2 [[1 2] [2 4] [3 6]], 2/3 [[4 6]]})
		(= (_ count [[1] [1 2] [3] [1 2 3] [2 3]])
  		 {1 [[1] [3]], 2 [[1 2] [2 3]], 3 [[1 2 3]]})
	)
	#(reduce
	  (fn [l r]
		  (let [k (%1 r)]
		  	; l is our map
		  	; r is the current value in the array
		  	; k is f(r) => key
		  	; associate k to the existing item at k :: r
		    (assoc l k (conj (get l k []) r))
		  )
	  ) 
	  ; initialize l as a map {}
	  ; pass in the array as r
	  {} %2)
)

; Problem 64
; solution: +

; Problem 65
; Clojure has many sequence types, which act in subtly different ways. 
; The core functions typically convert them into a uniform "sequence" type and work with 
; them that way, but it can be important to understand the behavioral and 
; performance differences so that you know which kind is appropriate for your application.

; Write a function which takes a collection and returns one of :map, :set, :list, or :vector - describing the type of collection it was given.
; You won't be allowed to inspect their class or use the built-in predicates like list? - the point is to poke at them and understand their behavior.

; Forbidden: class, type, Class, vector?, sequential?, list?, seq?, map?, set?, instance?, getClass
(problem[_]
	(list
		(= :map (_ {:a 1, :b 2}))
		(= :list (_ (range (rand-int 20))))
		(= :vector (_ [1 2 3 4 5 6]))
		(= :set (_ #{10 (rand-int 5)}))
		(= [:map :set :vector :list] (map _ [{} #{} [] ()]))
	)
	(fn[coll]
	  (let [obj (new Object)]
	    (let [x (conj coll [1 2])]
	      (cond
	        (empty? (flatten x)) (if (associative? x) :map :set)
	        (= (first (conj x obj)) obj) :list
	        :else :vector
	      )
	)))
)

; Problem 66
; Given two integers, write a function which returns the greatest common divisor.
(problem[_]
	(list
		(= (_ 2 4) 2)
		(= (_ 10 5) 5)
		(= (_ 5 7) 1)
		(= (_ 1023 858) 33)
	)
	(fn gcd[a b] 
	  (if (= b 0)
	    a
	    (recur b (mod a b))
	))
)

; Problem 67
; Write a function which returns the first x number of prime numbers.
(problem[_]
	(list
		(= (_ 2) [2 3])
		(= (_ 5) [2 3 5 7 11])
		(= (last (_ 100)) 541)
	)
	(fn [x]
		(take x 
	  		(for [r (drop 2 (range)) :when (= (some #(= (mod r %) 0) (range 2 r)) nil)] r))
	)
)

; Problem 68
; Solution: [7 6 5 4 3]

; Problem 69
; Write a function which takes a function f and a variable number of maps. 
; Your function should return a map that consists of the rest of the maps conj-ed onto the 
; first. If a key occurs in more than one map, the mapping(s) from the 
; latter (left-to-right) should be combined with the mapping in the result by 
; calling (f val-in-result val-in-latter)
; Forbidden: merge-with
(problem[_]
	(list
		(= (_ * {:a 2, :b 3, :c 4} {:a 2} {:b 2} {:c 5})
   			{:a 4, :b 6, :c 20})
		(= (_ - {1 10, 2 20} {1 3, 2 10, 3 15})
   			{1 7, 2 10, 3 15})
		(= (_ concat {:a [3], :b [6]} {:a [4 5], :c [8 9]} {:b [7]})
   			{:a [3 4 5], :b [6 7], :c [8 9]})
	)
	(fn [op & maps]
	  (reduce (fn [l r] 
	    (reduce (fn [l k]
	      (let [lv (get l k)]
	        (if (nil? lv)
	          (assoc l k (r k))
	          (assoc l k (op lv (r k)))
	    ))) l (keys r))) maps
	))
)

; Problem 70
; Write a function that splits a sentence up into a sorted list of words.
; Capitalization should not affect sort order and punctuation should be ignored.
(problem[_]
	(list
		(= (_  "Have a nice day.")
   			["a" "day" "Have" "nice"])
		(= (_  "Clojure is a fun language!")
   			["a" "Clojure" "fun" "is" "language"])
		(= (_  "Fools fall for foolish follies.")
   			["fall" "follies" "foolish" "Fools" "for"])
	)
	(fn [x] 
	  (sort #(.compareTo (.toLowerCase %1) (.toLowerCase %2))
	    (map #(clojure.string/replace %1 #"\.|\!" "")
	    (clojure.string/split x #"\s+")))
	)
)

; Problem 71
; Solution: last

; Problem 72
; Solution: apply +

; Problem 73
; TODO: there has to be a general way to get the diagonal of a matrix....
; A tic-tac-toe board is represented by a two dimensional vector. 
; X is represented by :x, O is represented by :o, and empty is represented by :e. 
; A player wins by placing three Xs or three Os in a horizontal, vertical, or diagonal row. 
; Write a function which analyzes a tic-tac-toe board and returns :x if X has won, 
; :o if O has won, and nil if neither player has won.
(problem[_]
	(list
		(= nil (_ [[:e :e :e]
            [:e :e :e]
            [:e :e :e]]))
		(= :x (_ [[:x :e :o]
           [:x :e :e]
           [:x :e :o]]))
		(= :o (_ [[:e :x :e]
           [:o :o :o]
           [:x :e :x]]))
		(= nil (_ [[:x :e :o]
            [:x :x :e]
            [:o :x :o]]))
		(= :x (_ [[:x :e :e]
           [:o :x :e]
           [:o :e :x]]))
		(= :o (_ [[:x :e :o]
           [:x :o :e]
           [:o :e :x]]))
		(= nil (_ [[:x :o :x]
            [:x :o :x]
            [:o :x :o]]))
	)
	(fn [board] 
	  (let [won?
	    (fn [s] 
	      (not (empty?
	        (filter #(= (list s s s) %) 
	          (concat board (vec (apply map vector board)) 
	          (list (vector (get-in board [0 0]) (get-in board [1 1]) (get-in board [2 2])))
	          (list (vector (get-in board [0 2]) (get-in board [1 1]) (get-in board [2 0])))
	      ))))
	    )]
	    (cond
	      (won? :x) :x
	      (won? :o) :o
	    )
	))
)

; Problem 74
; Given a string of comma separated integers, 
; write a function which returns a new comma separated string that only
; contains the numbers which are perfect squares.
(problem[_]
	(list
		(= (_ "4,5,6,7,8,9") "4,9")
		(= (_ "15,16,25,36,37") "16,25,36")
	)
	(fn [s] 
	  (clojure.string/join "," (filter
	    #(= (- (Math/sqrt (Integer/parseInt %)) (int (Math/sqrt (Integer/parseInt %)))) 0.0)
	    (clojure.string/split s #",")))
	)
)

; Problem 75
; Two numbers are coprime if their greatest common divisor equals 1. 
; Euler's totient function f(x) is defined as the number of positive integers less 
; than x which are coprime to x. The special case f(1) equals 1. 
; Write a function which calculates Euler's totient function.
(problem[_]
	(list
		(= (_ 1) 1)
		(= (_ 10) (count '(1 3 7 9)) 4)
		(= (_ 40) 16)
		(= (_ 99) 60)
	)
	(fn [x]
		(let [gcd 
		    (fn [a b] 
		      (if (= b 0)
			      a
			      (recur b (mod a b))
			  ))]
		  (count (for [n (range x) :when (= (gcd n x) 1)] n))
	))
)

; Problem 76
; Solution: [1 3 5 7 9 11]

; Problem 77
; Write a function which finds all the anagrams in a vector of words. 
; A word x is an anagram of word y if all the letters in x can be rearranged in a
; different order to form y. 
; Your function should return a set of sets, 
; where each sub-set is a group of words which are anagrams of each other. 
; Each sub-set should have at least two words. 
; Words without any anagrams should not be included in the result.
(problem[_]
	(list
		(= (_ ["meat" "mat" "team" "mate" "eat"])
   			#{#{"meat" "team" "mate"}})
		(= (_ ["veer" "lake" "item" "kale" "mite" "ever"])
   			#{#{"veer" "ever"} #{"lake" "kale"} #{"mite" "item"}})
	)
	(fn [x] (set (map set (filter #(> (count %) 1) (vals (group-by frequencies x))))))
)

; Problem 78
; Reimplement the function described in "Intro to Trampoline".
; Forbidden: trampoline
(problem[_]
	(list
		(= (letfn [(triple [x] #(sub-two (* 3 x)))
	          (sub-two [x] #(stop?(- x 2)))
	          (stop? [x] (if (> x 50) x #(triple x)))]
	    	(_ triple 2))
	  	82)
	  	(= (letfn [(my-even? [x] (if (zero? x) true #(my-odd? (dec x))))
		         (my-odd? [x] (if (zero? x) false #(my-even? (dec x))))]
		    (map (partial _ my-even?) (range 6)))
		  [true false true false true false])
	)
	(fn tramp
	  ([x & args]
	    (tramp (apply x args))
	  )
	  ([x]
	    (if (fn? x)
	      (recur (x))
	      x
	  ))
	)
)

; Probelm 79
; Write a function which calculates the sum of the minimal path through a triangle. 
; The triangle is represented as a collection of vectors. 
; The path should start at the top of the triangle and move to an adjacent number on the next row until the bottom of the triangle is reached.
(problem[_]
	(list
		(= 7 (_ '([1]
          [2 4]
         [5 1 4]
        [2 3 4 5]))) ; 1->2->1->3
        (= 20 (_ '([3]
           [2 4]
          [1 9 3]
         [9 9 2 4]
        [4 6 6 7 8]
       [5 7 3 5 1 4]))) ; 3->4->3->2->7->1
	)
	(fn [triangle]
	  (let [mkpaths (fn mkpaths [triangle vert tpath]
			(if (nil? (get-in triangle [(+ (first vert) 1) (last vert)]))
				[(conj tpath (get-in triangle vert))]
				(concat (mkpaths triangle [(+ (first vert) 1) (last vert)] (conj tpath (get-in triangle vert)))
						(mkpaths triangle [(+ (first vert) 1) (+ (last vert) 1)] (conj tpath (get-in triangle vert))))
			))]
			(first (sort (map #(apply + %) (mkpaths (vec triangle) [0 0] []))))
		)
	)
)

; #(= (apply + (for [x (drop 1 (range %)) :when (= (mod % x) 0)] x)) %)

; multi-mapping
; (assoc map key (conj (get map key []) val))

; Problem 80
; A number is "perfect" if the sum of its divisors equal the number itself. 
; 6 is a perfect number because 1+2+3=6. Write a function which returns true for perfect numbers and false otherwise.
(problem[_]
	(list
		(= (_ 6) true)
		(= (_ 7) false)
		(= (_ 496) true)
		(= (_ 500) false)
		(= (_ 8128) true)
	)
	(fn [n] (= (apply + (for [x (drop 1 (range n)) :when (= (mod n x) 0)] x)) n))
)