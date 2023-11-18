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
    (is (= (list (symbol ";ERROR: +: Wrong type in arg1 A")) (fnc-sumar '(A 4 5 6))))
    (is (= (list (symbol ";ERROR: +: Wrong type in arg2 A")) (fnc-sumar '(3 A 5 6))))
    (is (= (list (symbol ";ERROR: +: Wrong type in arg3 A")) (fnc-sumar '(3 4 A 6))))
    ))

(deftest fnc-restar-test
  (testing "Test de resta de elementos"
    (is (= ";ERROR: -: Wrong number of args given" (fnc-restar ())))
    (is (= -3 (fnc-restar '(3))))
    (is (= -1 (fnc-restar '(3 4))))
    (is (= -6 (fnc-restar '(3 4 5))))
    (is (= -12 (fnc-restar '(3 4 5 6))))
    (is (= (list (symbol ";ERROR: -: Wrong type in arg1 A")) (fnc-restar '(A 4 5 6))))
    (is (= (list (symbol ";ERROR: -: Wrong type in arg2 A")) (fnc-restar '(3 A 5 6))))
    (is (= (list (symbol ";ERROR: -: Wrong type in arg3 A")) (fnc-restar '(3 4 A 6))))
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
    (is (= (list (symbol ";ERROR: <: Wrong type in arg1 A")) (fnc-menor '(A 1 2 4))))
    (is (= (list (symbol ";ERROR: <: Wrong type in arg2 A")) (fnc-menor '(1 A 1 4))))
    (is (= (list (symbol ";ERROR: <: Wrong type in arg3 A" ))(fnc-menor '(1 2 A 4))))
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
    (is (= (list (symbol ";ERROR: >: Wrong type in arg1 A")) (fnc-mayor '(A 3 2 1))))
    (is (= (list (symbol ";ERROR: >: Wrong type in arg2 A")) (fnc-mayor '(3 A 2 1))))
    (is (= (list (symbol ";ERROR: >: Wrong type in arg3 A")) (fnc-mayor '(3 2 A 1))))
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
    (is (= (list (symbol ";ERROR: >=: Wrong type in arg1 A")) (fnc-mayor-o-igual '(A 3 2 1))))
    (is (= (list (symbol ";ERROR: >=: Wrong type in arg2 A")) (fnc-mayor-o-igual '(3 A 2 1))))
    (is (= (list (symbol ";ERROR: >=: Wrong type in arg3 A")) (fnc-mayor-o-igual '(3 2 A 1))))))

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
    (is (= (list (symbol ";ERROR: append: Wrong type in arg 3")) (fnc-append '((1 2) 3 (4 5) (6 7)))))
    (is (= (list (symbol ";ERROR: append: Wrong type in arg A")) (fnc-append '((1 2) A (4 5) (6 7)))))
    ))

(deftest fnc-proteger-bool-en-str-test
  (testing "Test de reemplazar #f y #t por %f y %t en una cadena"
    (is (= "(or %f %t)" (proteger-bool-en-str "(or #f #t)"))) 
    (is (= "(and (or %f %t) %t)" (proteger-bool-en-str "(and (or #f #t) #t)"))) 
    (is (= "#" (proteger-bool-en-str "#")))
    (is (= "(es %t)" (proteger-bool-en-str "(es #t)"))) 
    (is (= "(es %f)" (proteger-bool-en-str "(es #f)")))
    (is (= "" (proteger-bool-en-str ""))) 
    (is (= "(and (or %F %t) %T)" (proteger-bool-en-str "(and (or #F #t) #T)"))) 
    ) 
  )

(deftest fnc-restaurar-bool-test
  (testing "Test de reemplazar %f y %t por #f y #t en una cadena"
    (is (= (list (symbol "(and (or #F #f #t #T) #T)")) (restaurar-bool (read-string (proteger-bool-en-str "(and (or #F #f #t #T) #T)")))))
    (is (= (list (symbol "(and (or #F #f #t #T) #T)")) (restaurar-bool (read-string "(and (or %F %f %t %T) %T)"))))
   ))

(deftest fnc-actualizar-amb-test
  (testing "Test de actualizar un ambiente dado con una clave (nombre de la variable o funcion) y su valor dados."
    (is (= '(a 1 b 2 c 3 d 4) (actualizar-amb '(a 1 b 2 c 3) 'd 4)))
    (is (= '(a 1 b 4 c 3) (actualizar-amb '(a 1 b 2 c 3) 'b 4))) 
    (is (= '(b 4 a 1 c 3) (actualizar-amb '(b 2 a 1 c 3) 'b 4))) 
    (is (= '(a 1 b 2 c 3) (actualizar-amb '(a 1 b 2 c 3) 'b (list (symbol ";ERROR:") 'mal 'hecho)))) 
    (is (= '(b 7) (actualizar-amb () 'b 7)))
  ))

(deftest fnc-buscar-test
  (testing "Test de buscar una clave en un ambiente " 
    (is (= 3 (buscar 'c '(a 1 b 2 c 3 d 4 e 5))))
    (is (= (list (symbol ";ERROR: unbound variable: f")) (buscar 'f '(a 1 b 2 c 3 d 4 e 5)))) 
    (is (= (list (symbol ";ERROR: unbound variable: z")) (buscar 'z '(a 1 b 2 c 3 d 4 e 5))))
    ))

(deftest fnc-evaluar-escalar-test
  (testing "Test de evaluar una expresion escalar"
    (is (= '(32 (x 6 y 11 z "hola")) (evaluar-escalar 32 '(x 6 y 11 z "hola"))) )
    (is (= '("chau" (x 6 y 11 z "hola")) (evaluar-escalar "chau" '(x 6 y 11 z "hola"))))
    (is (= '(11 (x 6 y 11 z "hola")) (evaluar-escalar 'y '(x 6 y 11 z "hola"))))
    (is (= '("hola" (x 6 y 11 z "hola")) (evaluar-escalar 'z '(x 6 y 11 z "hola"))))
    (is (= (list (list (symbol ";ERROR: unbound variable: n")) '(x 6 y 11 z "hola")) (evaluar-escalar 'n '(x 6 y 11 z "hola"))))
    ))
