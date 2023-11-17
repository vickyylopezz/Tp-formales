(ns tp-formales.core-test
  (:require [clojure.test :refer :all]
            [tp-formales.core :refer :all]))

(deftest verificar-parentesis-test
  (testing "Test de la cantidad de parentesis"
    (is (= 1 (verificar-parentesis "(hola 'mundo")))
    (is (= 2 (verificar-parentesis "((hola 'mundo")))
    (is (= -1 (verificar-parentesis "(hola '(mundo)))")))
    (is (= -1 (verificar-parentesis "(hola '(mundo) () 6) 7)")))
    (is (= -1 (verificar-parentesis "(hola '(mundo) () 6) 7) 9)")))
    (is (= 0 (verificar-parentesis "(hola '(mundo) )")))
    (is (= 0 (verificar-parentesis "hola mundo")))
    ))

(deftest error?-test
  (testing "Test de si una lista tiene `;ERROR:` o `;WARNING:` como primer elemento"
    (is (not (error? (list (symbol ";ERROR:") 'mal 'hecho))))
    (is (error? (list 'mal 'hecho)))
    (is (not (error? (list (symbol ";WARNING:") 'mal 'hecho))))
    (is (error? (list '3 '(5) '6)))
  ))

(deftest fnc-sumar-test
  (testing "Test de suma de elementos"
    (is (= 0 (fnc-sumar ())))
    (is (= 3 (fnc-sumar '(3))))
    (is (= 7 (fnc-sumar '(3 4))))
    (is (= 12 (fnc-sumar '(3 4 5))))
    (is (= 18 (fnc-sumar '(3 4 5 6))))
    (is (= (symbol ";ERROR: +: Wrong type in arg1 A") (fnc-sumar '(A 4 5 6))))
    (is (= (symbol ";ERROR: +: Wrong type in arg2 A") (fnc-sumar '(3 A 5 6))))
    (is (= (symbol ";ERROR: +: Wrong type in arg3 A") (fnc-sumar '(3 4 A 6))))
    ))

(deftest fnc-restar-test
  (testing "Test de resta de elementos"
    (is (= ";ERROR: -: Wrong number of args given" (fnc-restar ())))
    (is (= -3 (fnc-restar '(3))))
    (is (= -1 (fnc-restar '(3 4))))
    (is (= -6 (fnc-restar '(3 4 5))))
    (is (= -12 (fnc-restar '(3 4 5 6))))
    (is (= (symbol ";ERROR: -: Wrong type in arg1 A") (fnc-restar '(A 4 5 6))))
    (is (= (symbol ";ERROR: -: Wrong type in arg2 A") (fnc-restar '(3 A 5 6))))
    (is (= (symbol ";ERROR: -: Wrong type in arg3 A") (fnc-restar '(3 4 A 6))))
    )
  )

(deftest fnc-menor-test
  (testing "Test de orden estrictamente creciente de elementos"
    (is (= (symbol "#t") (fnc-menor ())))
    (is (= (symbol "#t") (fnc-menor '(1))))
    (is (= (symbol "#t") (fnc-menor '(1 2))))
    (is (= (symbol "#t") (fnc-menor '(1 2 3))))
    (is (= (symbol "#t") (fnc-menor '(1 2 3 4))))
    (is (= (symbol "#f") (fnc-menor '(1 2 2 4))))
    (is (= (symbol "#f") (fnc-menor '(1 2 1 4)))) 
    (is (= (symbol "#f") (fnc-menor '(4 1 3 2))))
    (is (= (symbol ";ERROR: <: Wrong type in arg1 A") (fnc-menor '(A 1 2 4))))
    (is (= (symbol ";ERROR: <: Wrong type in arg2 A") (fnc-menor '(1 A 1 4))))
    (is (= (symbol ";ERROR: <: Wrong type in arg3 A" )(fnc-menor '(1 2 A 4))))
    ))

(deftest fnc-mayor-test
  (testing "Test de orden estrictamente decreciente de elementos"
    (is (= (symbol "#t") (fnc-mayor ())))
    (is (= (symbol "#t") (fnc-mayor '(1))))
    (is (= (symbol "#t") (fnc-mayor '(2 1))))
    (is (= (symbol "#t") (fnc-mayor '(3 2 1))))
    (is (= (symbol "#t") (fnc-mayor '(4 3 2 1))))
    (is (= (symbol "#f") (fnc-mayor '(4 2 2 1))))
    (is (= (symbol "#f") (fnc-mayor '(4 2 1 4))))
    (is (= (symbol ";ERROR: >: Wrong type in arg1 A") (fnc-mayor '(A 3 2 1))))
    (is (= (symbol ";ERROR: >: Wrong type in arg2 A") (fnc-mayor '(3 A 2 1))))
    (is (= (symbol ";ERROR: >: Wrong type in arg3 A") (fnc-mayor '(3 2 A 1))))
    ))

(deftest fnc-mayor-o-igual-test
  (testing "Test de orden estrictamente decreciente de elementos"
    (is (= (symbol "#t") (fnc-mayor-o-igual ())))
    (is (= (symbol "#t") (fnc-mayor-o-igual '(1))))
    (is (= (symbol "#t") (fnc-mayor-o-igual '(2 1))))
    (is (= (symbol "#t") (fnc-mayor-o-igual '(3 2 1))))
    (is (= (symbol "#t") (fnc-mayor-o-igual '(4 3 2 1))))
    (is (= (symbol "#t") (fnc-mayor-o-igual '(4 2 2 1))))
    (is (= (symbol "#f") (fnc-mayor-o-igual '(4 2 1 4))))
    (is (= (symbol ";ERROR: >=: Wrong type in arg1 A") (fnc-mayor-o-igual '(A 3 2 1))))
    (is (= (symbol ";ERROR: >=: Wrong type in arg2 A") (fnc-mayor-o-igual '(3 A 2 1))))
    (is (= (symbol ";ERROR: >=: Wrong type in arg3 A") (fnc-mayor-o-igual '(3 2 A 1))))))

(deftest fnc-equal?-test
  (testing "Test de orden estrictamente decreciente de elementos"
    (is (= (symbol "#t") (fnc-equal? ())))
    (is (= (symbol "#t") (fnc-equal? '(A))))
    (is (= (symbol "#t") (fnc-equal? '(A a))))
    (is (= (symbol "#t") (fnc-equal? '(A a A))))
    (is (= (symbol "#t") (fnc-equal? '(A a A a))))
    (is (= (symbol "#f") (fnc-equal? '(A a A B))))
    (is (= (symbol "#t") (fnc-equal? '(1 1 1 1))))
    (is (= (symbol "#f") (fnc-equal? '(1 1 2 1))))
    ))

(deftest fnc-append-test
  (testing "Test de fusionar listas"
    (is (= '(1 2 3 4 5 6 7) (fnc-append '((1 2) (3) (4 5) (6 7))))) 
    (is (= '(1 2 4 (3) 4 5 6 7) (fnc-append '((1 2) (4 (3)) (4 5) (6 7)))))
    (is (= (symbol ";ERROR: append: Wrong type in arg 3") (fnc-append '((1 2) 3 (4 5) (6 7)))))
    (is (= (symbol ";ERROR: append: Wrong type in arg A") (fnc-append '((1 2) A (4 5) (6 7)))))
    ))
