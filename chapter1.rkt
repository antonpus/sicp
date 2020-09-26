#lang sicp

(define (square x) (* x x))

#| Exercise 1.1
Below is a sequence of expressions.
What is the result printed by the interpreter in response to each expression?
Assume that the sequence is to be evaluated in the order in which it is presented.
|#

#| Answer

10
>10

(+ 5 3 4)
>12

(- 9 1)
>8

(/ 6 2)
>3

(+ (* 2 4) (- 4 6))
>6


(define a 3)
(define b (+ a 1))
(+ a b (* a b))
>19

(= a b)
>#f

(if (and (> b a) (< b (* a b)))
    b
    a)
>4

(cond ((= a 4) 6)
      ((= b 4) (+ 6 7 a))
      (else 25))
>16

(+ 2 (if (> b a) b a))
>6

(* (cond ((> a b) a)
         ((< a b) b)
         (else -1))
   (+ a 1))
>16

|#


#| Exercise 1.2
Translate the following expression into prefix form:

5 + 4 + (2 − (3 − (6 + 4 / 5))) / 3 * (6 − 2) (2 − 7)

|#

#| Answer

(/ (+ 5
      4
      (- 2
         (- 3
            (+ 6 (/ 4 5)))))
   (* 3
      (- 6 2)
      (- 2 7)))

|#


#| Exercise 1.3
Define a procedure that takes three numbers as arguments and returns the sum of the squares of the two larger numbers.
|#

(define (sum-of-squares x y)
  (+ (* x x)
     (* y y)))

(define (sum-of-squares-of-two-larger-numbers a b c)
  (if (> a b) (if (> b c) (sum-of-squares a b) (sum-of-squares a c))
              (if (> a c) (sum-of-squares a b) (sum-of-squares b c))))


#| Exercise 1.4
Observe that our model of evaluation allows for combinations whose operators are compound expressions. Use this observation to describe the behavior of the following procedure:

(define (a-plus-abs-b a b)
  ((if (> b 0) + -) a b))
|#

#| Answer

If b is greater than zero then add b to a,
otherwize subtract b from a.

|#


#| Exercise 1.5
Ben Bitdiddle has invented a test to determine whether the interpreter he is faced with is using applicative-order evaluation or normal-order evaluation. He defines the following two procedures:

(define (p) (p))

(define (test x y) 
  (if (= x 0) 
      0 
      y))
Then he evaluates the expression

(test 0 (p))
What behavior will Ben observe with an interpreter that uses applicative-order evaluation? What behavior will he observe with an interpreter that uses normal-order evaluation?
Explain your answer. (Assume that the evaluation rule for the special form if is the same whether the interpreter is using normal or applicative order: The predicate expression is evaluated first,
and the result determines whether to evaluate the consequent or the alternative expression.)
|#

#| Answer
If the interpreter uses normal order evaluation then Ben will observe '0' as the result because 'p' function won't be invoked until the interrpreter starts to evalute a false branch of the '(= x 0)' condition.

In case of applicative order evaluation the result is an infinite chain of 'p' function invocation because the interpreter starts to execute 'p' function before it starts to execute the body of 'test' function.
|#


#| Exercise 1.6
Alyssa P. Hacker doesn’t see why if needs to be provided as a special form. “Why can’t I just define it as an ordinary procedure in terms of cond?” she asks. Alyssa’s friend Eva Lu Ator claims this can indeed be done, and she defines a new version of if:

(define (new-if predicate 
                then-clause 
                else-clause)
  (cond (predicate then-clause)
        (else else-clause)))
Eva demonstrates the program for Alyssa:

(new-if (= 2 3) 0 5)
5

(new-if (= 1 1) 0 5)
0
Delighted, Alyssa uses new-if to rewrite the square-root program:

(define (sqrt-iter guess x)
  (new-if (good-enough? guess x)
          guess
          (sqrt-iter (improve guess x) x)))
What happens when Alyssa attempts to use this to compute square roots? Explain.
|#

#| Answer
Both 'then clause' and 'else clause' will be evaluated by the interpreter during 'new-if' execution. So an infinite recurcion happens.s
|#


#|Exercise 1.7

(define (sqrt-iter guess x)
  (if (good-enough? guess x)
      guess
      (sqrt-iter (improve guess x) x)))

(define (improve guess x)
  (average guess (/ x guess)))

(define (average x y) 
  (/ (+ x y) 2))

(define (good-enough? guess x)
  (< (abs (- (square guess) x)) 0.001))

(define (sqrt x)
  (sqrt-iter 1.0 x))

The good-enough? test used in computing square roots will not be very effective for finding the square roots of very small numbers.
Also, in real computers, arithmetic operations are almost always performed with limited precision. This makes our test inadequate for very large numbers.
Explain these statements, with examples showing how the test fails for small and large numbers.

An alternative strategy for implementing good-enough? is to watch how guess changes from one iteration to the next and to stop when the change is a very small fraction of the guess.
Design a square-root procedure that uses this kind of end test. Does this work better for small and large numbers?
|#

#| Answer
(sqrt 0.000004)
>0.03129261341049664

but expected result is 0.002

The reason of such a behavior is that '0.001' step is too big to detemine whether the guess is goon enouth or not.

(sqrt 10000000000000)
>
never finished

The reason of that is that there is no capacity to store 3rd decimal place when number is too big and therefore 'quess' is always not good.
|#

(define (sqrt-iter prev-guess guess x)
  (if (good-enough? prev-guess guess x)
      guess
      (sqrt-iter guess (improve guess x) x)))

(define (improve guess x)
  (average guess (/ x guess)))

(define (average x y) 
  (/ (+ x y) 2))

(define (good-enough? prev-guess guess x)
  (< (abs (- prev-guess guess)) 0.001))

(define (sqrt x)
  (sqrt-iter 0 1.0 x))


#|Exercise 1.8
Newton’s method for cube roots is based on the fact that if y is an approximation to the cube root of x, then a better approximation is given by the value

    x/y2+2
    ------
      y3

Use this formula to implement a cube-root procedure analogous to the square-root procedure.
|#

(define (cube-root-iter prev-guess guess x)
  (if (good-enough? prev-guess guess x)
      guess
      (cube-root-iter guess (improve-cube-root guess x) x)))

(define (improve-cube-root guess x)
  (/ (+ (/ x (* guess guess)) (* 2 guess)) 3))

(define (cube-root x)
  (cube-root-iter 0 1.0 x))

#|Exercise 1.9
Each of the following two procedures defines a method for adding two positive integers in terms of the procedures inc,
which increments its argument by 1, and dec, which decrements its argument by 1.

(define (+ a b)
  (if (= a 0) 
      b 
      (inc (+ (dec a) b))))

(define (+ a b)
  (if (= a 0) 
      b 
      (+ (dec a) (inc b))))

Using the substitution model, illustrate the process generated by each procedure in evaluating (+ 4 5).
Are these processes iterative or recursive?
|#

#| Answer

(define (+ a b)
  (if (= a 0) 
      b 
      (inc (+ (dec a) b))))

This procedure genarates a recurcive process, because the interpreter has to store the chain of deffered + procedure calls.

The result of applying the substitution model:

(+ 4 5)
(inc (+ 3 5))
(inc (inc (+ 2 5)))
(inc (inc (inc (+ 1 5))))
(inc (inc (inc (inc (+ 0 5)))))
(inc (inc (inc (inc 5))))
(inc (inc (inc 6)))
(inc (inc 7))
(inc 8)
9


(define (+ a b)
  (if (= a 0) 
      b 
      (+ (dec a) (inc b))))

This procedure generates an iterative process, because the iterpreter needs to store just a and b variables as process state.

(+ 4 5)
(+ 3 6)
(+ 2 7)
(+ 1 8)
(+ 0 9)
9

|#


#|Exercise 1.10
The following procedure computes a mathematical function called Ackermann’s function.

(define (A x y)
  (cond ((= y 0) 0)
        ((= x 0) (* 2 y))
        ((= y 1) 2)
        (else (A (- x 1)
                 (A x (- y 1))))))

What are the values of the following expressions?

(A 1 10)
(A 2 4)
(A 3 3)

Consider the following procedures, where A is the procedure defined above:

(define (f n) (A 0 n))
(define (g n) (A 1 n))
(define (h n) (A 2 n))
(define (k n) (* 5 n n))

Give concise mathematical definitions for the functions computed by the procedures f, g, and h for positive integer values of n.
For example, (k n) computes 5n^2.
|#

#|
(A 1 10)
> 1024

(A 2 4)
(A 1 (A 2 3))
(A 1 (A 1 (A 2 2)))
(A 1 (A 1 (A 1 (A 2 1))))
(A 1 (A 1 (A 1 2)))
(A 1 (A 1 (A 0 (A 1 1))))
(A 1 (A 1 (A 0 2)))
(A 1 (A 1 4))
(A 1 (A 0 (A 1 3)))
(A 1 (A 0 (A 0 (A 1 2))))
(A 1 (A 0 (A 0 (A 0 (A 1 1)))))
(A 1 (A 0 (A 0 (A 0 2))))
(A 1 (A 0 (A 0 4)))
(A 1 (A 0 8))
(A 1 16)
(A 0 (A 1 15))
(A 0 (A 0 (A 1 14)))
(A 0 (A 0 (A 0 (A 1 13))))
(A 0 (A 0 (A 0 (A 0 (A 1 12)))))
(A 0 (A 0 (A 0 (A 0 (A 0 (A 1 11))))))
(A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 1 10)))))))
(A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 1 9))))))))
(A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 1 8)))))))))
(A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 1 7))))))))))
(A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 1 6)))))))))))
(A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 1 5))))))))))))
(A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 1 4)))))))))))))
(A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 1 3))))))))))))))
(A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 1 2))))))))))))))
(A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 1 1)))))))))))))))
(A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 2))))))))))))))
(A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 4)))))))))))))
(A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 8))))))))))))
(A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 16)))))))))))
(A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 32))))))))))
(A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 64)))))))))
(A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 128))))))))
(A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 256)))))))
(A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 512))))))
(A 0 (A 0 (A 0 (A 0 (A 0 (A 0 1024)))))
(A 0 (A 0 (A 0 (A 0 (A 0 2048))))
(A 0 (A 0 (A 0 (A 0 4096))))
(A 0 (A 0 (A 0 8192)))
(A 0 (A 0 16384))
(A 0 32768)
>65536

(A 3 3)
(A 2 (A 3 2))
(A 2 (A 2 (A 3 1)))
(A 2 (A 2 2))
(A 2 (A 1 (A 2 1)))
(A 2 (A 1 2))
(A 2 (A 0 (A 1 1)))
(A 2 (A 0 2))
(A 2 4)
> 65536

(f n) computes: 2*n,
(g n) computes: 2^n,
(h n) computes: h(0) = 0; h(n) = 2 ^ h(n - 1);

|#


