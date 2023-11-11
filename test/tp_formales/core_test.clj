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
    (is (= ";ERROR: +: Wrong type in arg1 A" (fnc-sumar '(A 4 5 6)))) 
    (is (= ";ERROR: +: Wrong type in arg2 A" (fnc-sumar '(3 A 5 6))))
    (is (= ";ERROR: +: Wrong type in arg3 A" (fnc-sumar '(3 4 A 6))))

    ))
