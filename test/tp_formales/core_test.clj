(ns tp-formales.core-test
  (:require [clojure.test :refer :all]
            [tp-formales.core :refer :all]))

(deftest leer-entrada-test
  (testing "Lectura de entrada"
    (is (= "Hola" (with-in-str "Hola" (leer-entrada))))
    (is (= "123" (with-in-str "123" (leer-entrada))))
    (is (= "(Hola mundo)" (with-in-str "(Hola\nmundo)" (leer-entrada))))
    (is (= "( muchos( parentesis( en lineas) ))" (with-in-str "(\nmuchos(\nparentesis(\nen lineas)\n))" (leer-entrada))))))

(deftest verificar-parentesis-test
  (testing "Test de la cantidad de parentesis"
    (is (= 1 (verificar-parentesis "(hola 'mundo")))
    (is (= 2 (verificar-parentesis "((hola 'mundo")))
    (is (= -1 (verificar-parentesis "(hola '(mundo)))")))
    (is (= -1 (verificar-parentesis "(hola '(mundo) () 6) 7)")))
    (is (= -1 (verificar-parentesis "(hola '(mundo) () 6) 7) 9)")))
    (is (= 0 (verificar-parentesis "(hola '(mundo) )")))
    (is (= 0 (verificar-parentesis "hola mundo")))))

(deftest error?-test
  (testing "Test de si una lista tiene `;ERROR:` o `;WARNING:` como primer elemento"
    (is (error? (list (symbol ";ERROR:") 'mal 'hecho)))
    (is (not (error? (list 'mal 'hecho))))
    (is (error? (list (symbol ";WARNING:") 'mal 'hecho)))
    (is (not (error? (list '3 '(5) '6))))))

(deftest fnc-equal?-test
  (testing "Test de orden estrictamente decreciente de elementos"
    (is (= (generar-mensaje-error :wrong-number-args 'equal?) (fnc-equal? ())))
    (is (= (generar-mensaje-error :wrong-number-args 'equal?) (fnc-equal? '(A))))
    (is (= (symbol "#t") (fnc-equal? '(A A))))
    (is (= (symbol "#f") (fnc-equal? '(A a))))
    (is (= (generar-mensaje-error :wrong-number-args 'equal?) (fnc-equal? '(A A A))))
    (is (= (symbol "#t") (fnc-equal? '((1 1) (1 1)))))
    (is (= (symbol "#f") (fnc-equal? '((1 1) (2 1)))))))

(deftest fnc-append-test
  (testing "Test de fusionar listas"
    (is (= '(1 2 3 4 5 6 7) (fnc-append '((1 2) (3) (4 5) (6 7)))))
    (is (= '(1 2 4 (3) 4 (9) 5 6 7) (fnc-append '((1 2) (4 (3)) (4 (9) 5) (6 7)))))
    (is (= (generar-mensaje-error :wrong-type-arg 'append 3) (fnc-append '((1 2) 3 (4 5) (6 7)))))
    (is (= (generar-mensaje-error :wrong-type-arg 'append 'A) (fnc-append '((1 2) A (4 5) (6 7)))))))

(deftest proteger-bool-en-str-test
  (testing "Test de reemplazar #f y #t por %f y %t en una cadena"
    (is (= "(or %f %t)" (proteger-bool-en-str "(or #f #t)")))
    (is (= "(and (or %f %t) %t)" (proteger-bool-en-str "(and (or #f #t) #t)")))
    (is (= "#" (proteger-bool-en-str "#")))
    (is (= "(es %t)" (proteger-bool-en-str "(es #t)")))
    (is (= "(es %f)" (proteger-bool-en-str "(es #f)")))
    (is (= "" (proteger-bool-en-str "")))
    (is (= "(and (or %F %t) %T)" (proteger-bool-en-str "(and (or #F #t) #T)")))))

(deftest restaurar-bool-test
  (testing "Test de reemplazar %f y %t por #f y #t en una cadena"
    (is (= (list (symbol "and") (list (symbol "or") (symbol "#F") (symbol "#f") (symbol "#t") (symbol "#T")) (symbol "#T")) (restaurar-bool (read-string (proteger-bool-en-str "(and (or #F #f #t #T) #T)")))))
    (is (= (list (symbol "and") (list (symbol "or") (symbol "#F") (symbol "#f") (symbol "#t") (symbol "#T")) (symbol "#T")) (restaurar-bool (read-string "(and (or %F %f %t %T) %T)"))))))

(deftest fnc-sumar-test
  (testing "Test de suma de elementos"
    (is (= 0 (fnc-sumar ())))
    (is (= 3 (fnc-sumar '(3))))
    (is (= 7 (fnc-sumar '(3 4))))
    (is (= 12 (fnc-sumar '(3 4 5))))
    (is (= 18 (fnc-sumar '(3 4 5 6))))
    (is (= (generar-mensaje-error :wrong-type-arg1 '+ 'A) (fnc-sumar '(A 4 5 6))))
    (is (= (generar-mensaje-error :wrong-type-arg2 '+ 'A) (fnc-sumar '(3 A 5 6))))
    (is (= (generar-mensaje-error :wrong-type-arg2 '+ 'A) (fnc-sumar '(3 4 A 6))))))

(deftest fnc-restar-test
  (testing "Test de resta de elementos"
    (is (= (generar-mensaje-error :wrong-number-args '-) (fnc-restar ())))
    (is (= -3 (fnc-restar '(3))))
    (is (= -1 (fnc-restar '(3 4))))
    (is (= -6 (fnc-restar '(3 4 5))))
    (is (= -12 (fnc-restar '(3 4 5 6))))
    (is (= (generar-mensaje-error :wrong-type-arg1 '- 'A) (fnc-restar '(A 4 5 6))))
    (is (= (generar-mensaje-error :wrong-type-arg2 '- 'A) (fnc-restar '(3 A 5 6))))
    (is (= (generar-mensaje-error :wrong-type-arg2 '- 'A) (fnc-restar '(3 4 A 6))))))

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
    (is (= (generar-mensaje-error :wrong-type-arg1 '< 'A) (fnc-menor '(A 1 2 4))))
    (is (= (generar-mensaje-error :wrong-type-arg2 '< 'A) (fnc-menor '(1 A 1 4))))
    (is (= (generar-mensaje-error :wrong-type-arg2 '< 'A) (fnc-menor '(1 2 A 4))))))

(deftest fnc-mayor-test
  (testing "Test de orden estrictamente decreciente de elementos"
    (is (= (symbol "#t") (fnc-mayor ())))
    (is (= (symbol "#t") (fnc-mayor '(1))))
    (is (= (symbol "#t") (fnc-mayor '(2 1))))
    (is (= (symbol "#t") (fnc-mayor '(3 2 1))))
    (is (= (symbol "#t") (fnc-mayor '(4 3 2 1))))
    (is (= (symbol "#f") (fnc-mayor '(4 2 2 1))))
    (is (= (symbol "#f") (fnc-mayor '(4 2 1 4))))
    (is (= (generar-mensaje-error :wrong-type-arg1 '> 'A) (fnc-mayor '(A 3 2 1))))
    (is (= (generar-mensaje-error :wrong-type-arg2 '> 'A) (fnc-mayor '(3 A 2 1))))
    (is (= (generar-mensaje-error :wrong-type-arg2 '> 'A) (fnc-mayor '(3 2 A 1))))))

(deftest fnc-mayor-o-igual-test
  (testing "Test de orden estrictamente decreciente de elementos"
    (is (= (symbol "#t") (fnc-mayor-o-igual ())))
    (is (= (symbol "#t") (fnc-mayor-o-igual '(1))))
    (is (= (symbol "#t") (fnc-mayor-o-igual '(2 1))))
    (is (= (symbol "#t") (fnc-mayor-o-igual '(3 2 1))))
    (is (= (symbol "#t") (fnc-mayor-o-igual '(4 3 2 1))))
    (is (= (symbol "#t") (fnc-mayor-o-igual '(4 2 2 1))))
    (is (= (symbol "#f") (fnc-mayor-o-igual '(4 2 1 4))))
    (is (= (generar-mensaje-error :wrong-type-arg1 '>= 'A) (fnc-mayor-o-igual '(A 3 2 1))))
    (is (= (generar-mensaje-error :wrong-type-arg2 '>= 'A) (fnc-mayor-o-igual '(3 A 2 1))))
    (is (= (generar-mensaje-error :wrong-type-arg2 '>= 'A) (fnc-mayor-o-igual '(3 2 A 1))))))

(deftest fnc-actualizar-amb-test
  (testing "Test de actualizar un ambiente dado con una clave (nombre de la variable o funcion) y su valor dados."
    (is (= '(a 1 b 2 c 3 d 4) (actualizar-amb '(a 1 b 2 c 3) 'd 4)))
    (is (= '(a 1 b 4 c 3) (actualizar-amb '(a 1 b 2 c 3) 'b 4)))
    (is (= '(b 4 a 1 c 3) (actualizar-amb '(b 2 a 1 c 3) 'b 4)))
    (is (= '(a 1 b 2 c 3) (actualizar-amb '(a 1 b 2 c 3) 'b (list (symbol ";ERROR:") 'mal 'hecho))))
    (is (= '(b 7) (actualizar-amb () 'b 7)))))

(deftest fnc-buscar-test
  (testing "Test de buscar una clave en un ambiente "
    (is (= 3 (buscar 'c '(a 1 b 2 c 3 d 4 e 5))))
    (is (= (generar-mensaje-error :unbound-variable 'f) (buscar 'f '(a 1 b 2 c 3 d 4 e 5))))
    (is (= (generar-mensaje-error :unbound-variable 'z) (buscar 'z '(a 1 b 2 c 3 d 4 e 5))))))

(deftest evaluar-escalar-test
  (testing "Test de evaluar una expresion escalar"
    (is (= '(32 (x 6 y 11 z "hola")) (evaluar-escalar 32 '(x 6 y 11 z "hola"))))
    (is (= '("chau" (x 6 y 11 z "hola")) (evaluar-escalar "chau" '(x 6 y 11 z "hola"))))
    (is (= '(11 (x 6 y 11 z "hola")) (evaluar-escalar 'y '(x 6 y 11 z "hola"))))
    (is (= '("hola" (x 6 y 11 z "hola")) (evaluar-escalar 'z '(x 6 y 11 z "hola"))))
    (is (= (list (generar-mensaje-error :unbound-variable 'n) '(x 6 y 11 z "hola")) (evaluar-escalar 'n '(x 6 y 11 z "hola"))))))

(deftest evaluar-define-test
  (testing "Test de evaluar una expresion `define`"
    (is (= (list (symbol "#<void>") '(x 2)) (evaluar-define '(define x 2) '(x 1))))
    (is (= (list (symbol "#<void>") '(x 1 f (lambda (x) (+ x 1)))) (evaluar-define '(define (f x) (+ x 1)) '(x 1))))
    (is (= (list (generar-mensaje-error :missing-or-extra 'define '(define)) '(x 1)) (evaluar-define '(define) '(x 1))))
    (is (= (list (generar-mensaje-error :missing-or-extra 'define '(define x)) '(x 1)) (evaluar-define '(define x) '(x 1))))
    (is (= (list (generar-mensaje-error :missing-or-extra 'define '(define x 2 3)) '(x 1)) (evaluar-define '(define x 2 3) '(x 1))))
    (is (= (list (generar-mensaje-error :missing-or-extra 'define '(define ())) '(x 1)) (evaluar-define '(define ()) '(x 1))))
    (is (= (list (generar-mensaje-error :bad-variable 'define '(define () 2)) '(x 1)) (evaluar-define '(define () 2) '(x 1))))
    (is (= (list (generar-mensaje-error :bad-variable 'define '(define 2 x)) '(x 1)) (evaluar-define '(define 2 x) '(x 1))))))

(deftest evaluar-if-test
  (testing "Test de evaluar una expresion `if`"
    (is (= '(2 (n 7)) (evaluar-if '(if 1 2) '(n 7))))
    (is (= '(7 (n 7)) (evaluar-if '(if 1 n) '(n 7))))
    (is (= '(7 (n 7)) (evaluar-if '(if 1 n 8) '(n 7))))
    (is (= (list (symbol "#<void>") (list 'n 7 (symbol "#f") (symbol "#f"))) (evaluar-if (list 'if (symbol "#f") 'n) (list 'n 7 (symbol "#f") (symbol "#f")))))
    (is (= (list 8 (list 'n 7 (symbol "#f") (symbol "#f"))) (evaluar-if (list 'if (symbol "#f") 'n 8) (list 'n 7 (symbol "#f") (symbol "#f")))))
    (is (= (list (symbol "#<void>") (list 'n 9 (symbol "#f") (symbol "#f"))) (evaluar-if (list 'if (symbol "#f") 'n '(set! n 9)) (list 'n 7 (symbol "#f") (symbol "#f")))))
    (is (= (list (generar-mensaje-error :missing-or-extra 'if '(if)) '(n 7)) (evaluar-if '(if) '(n 7))))
    (is (= (list (generar-mensaje-error :missing-or-extra 'if '(if 1)) '(n 7)) (evaluar-if '(if 1) '(n 7))))))

(deftest evaluar-or-test
  (testing "Test de evaluar una expresion or"
    (is (= (list (symbol "#f") (list (symbol "#f") (symbol "#f") (symbol "#t") (symbol "#t"))) (evaluar-or (list 'or) (list (symbol "#f") (symbol "#f") (symbol "#t") (symbol "#t")))))
    (is (= (list (symbol "#t") (list (symbol "#f") (symbol "#f") (symbol "#t") (symbol "#t"))) (evaluar-or (list 'or (symbol "#t")) (list (symbol "#f") (symbol "#f") (symbol "#t") (symbol "#t")))))
    (is (= (list 7 (list (symbol "#f") (symbol "#f") (symbol "#t") (symbol "#t"))) (evaluar-or (list 'or 7) (list (symbol "#f") (symbol "#f") (symbol "#t") (symbol "#t")))))
    (is (= (list 5 (list (symbol "#f") (symbol "#f") (symbol "#t") (symbol "#t"))) (evaluar-or (list 'or (symbol "#f") 5) (list (symbol "#f") (symbol "#f") (symbol "#t") (symbol "#t")))))
    (is (= (list (symbol "#f") (list (symbol "#f") (symbol "#f") (symbol "#t") (symbol "#t"))) (evaluar-or (list 'or (symbol "#f")) (list (symbol "#f") (symbol "#f") (symbol "#t") (symbol "#t")))))))

(deftest evaluar-set-test
  (testing "Test de evaluar una expresion set!"
    (is (= (list (symbol "#<void>") '(x 1)) (evaluar-set! '(set! x 1) '(x 0))))
    (is (= (list (generar-mensaje-error :unbound-variable 'x) '()) (evaluar-set! '(set! x 1) '())))
    (is (= (list (generar-mensaje-error :missing-or-extra 'set! '(set! x)) '(x 0)) (evaluar-set! '(set! x) '(x 0))))
    (is (= (list (generar-mensaje-error :missing-or-extra 'set! '(set! x 1 2)) '(x 0)) (evaluar-set! '(set! x 1 2) '(x 0))))
    (is (= (list (generar-mensaje-error :bad-variable 'set! 1) '(x 0)) (evaluar-set! '(set! 1 2) '(x 0))))))

(deftest chequear_cadena-test
  (testing "Test de chequear que una lista sea solo de numeros. Y aplicar la funcion"
    (is (= 6 (chequear_cadena (map-indexed list '(1 2 3)) "+" sumar '(1 2 3))))
    (is (= (generar-mensaje-error :wrong-type-arg1 '+ 'A) (chequear_cadena (map-indexed list '(A 2 3)) "+" sumar '(A 2 3))))
    (is (= (generar-mensaje-error :wrong-type-arg2 '+ 'B) (chequear_cadena (map-indexed list '(1 2 B)) "+" sumar '(1 2 B))))
    (is (= -4 (chequear_cadena (map-indexed list '(1 2 3)) "-" restar '(1 2 3))))
    (is (= (generar-mensaje-error :wrong-type-arg1 '- 'C) (chequear_cadena (map-indexed list '(C 2 3)) "-" restar '(C 2 3))))
    (is (= (generar-mensaje-error :wrong-type-arg2 '- 'D) (chequear_cadena (map-indexed list '(1 2 3 D)) "-" restar '(1 2 3 D))))
    (is (= (symbol "#t") (chequear_cadena (map-indexed list '(1 2 3 4)) "<" menor '(1 2 3 4) true)))
    (is (= (symbol "#f") (chequear_cadena (map-indexed list '(1 2 3 4)) "<" menor '(1 2 1 4) true)))
    (is (= (symbol "#f") (chequear_cadena (map-indexed list '(1 2 1 4)) ">" mayor '(1 2 1 4) true)))
    (is (= (symbol "#t") (chequear_cadena (map-indexed list '(4 2 1)) ">" mayor '(4 2 1) true)))
    (is (= (symbol "#f") (chequear_cadena (map-indexed list '(1 2 1 4)) ">=" mayor-o-igual '(1 2 1 4) true)))
    (is (= (symbol "#t") (chequear_cadena (map-indexed list '(4 2 2 1)) ">=" mayor-o-igual '(4 2 2 1) true)))))

(deftest as_map-test
  (testing "Test de convertir una lista en un mapa"
    (is (= '{x 1, y 2, z 3} (as_map '(x 1 y 2 z 3))))))

(deftest es-parentesis-test
  (testing "Test de contar parentesis"
    (is (= 0 (es_parentesis (partition 1 "(hola)") 0)))
    (is (= 1 (es_parentesis (partition 1 "(hola") 0)))
    (is (= -1 (es_parentesis (partition 1 "hola)") 0)))))

(deftest as_list-test
  (testing "Test de convertir una lista en un mapa"
    (is (=  '(x 1 y 2 z 3) (as_list '{x 1, y 2, z 3})))))

(deftest transponer-test
  (testing "Test de transponer una lista"
    (is (=  '((x y z) (1 2 3)) (transponer '((x 1) (y 2) (z 3)))))))

(deftest actualizar-test
  (testing "Test de actualizar un ambiente"
    (is (=  '(x 5 y 2 z 3) (actualizar '(x 1 y 2 z 3) 'x 5)))
    (is (=  '(x 1 y 2 z 3 v 5) (actualizar '(x 1 y 2 z 3) 'v 5)))))


(deftest buscar_clave-test
  (testing "Test de buscar una clave en un ambiente "
    (is (= 2 (buscar_clave '{x 1, y 2, z 3} 'y)))
    (is (= (generar-mensaje-error :unbound-variable 'e) (buscar_clave '{x 1, y 2, z 3} 'e)))))

(deftest restaurar-test
  (testing "Test de restaurar un elemento"
    (is (= 2 (restaurar 2)))
    (is (= (symbol "#t") (restaurar (symbol "%t"))))
    (is (= (symbol "#f") (restaurar (symbol "%f"))))
    (is (= (symbol "#T") (restaurar (symbol "%T"))))
    (is (= (symbol "#F") (restaurar (symbol "%F"))))
    (is (= 'x (restaurar 'x)))))

(deftest append-test
  (testing "Test de concatenar un elemento a una lista"
    (is (= '(x 1 y 2) (append '(x 1) '((y 2))))) 
    (is (= (generar-mensaje-error :wrong-type-arg 'append 'A) (append '(x 1) '(A (y 2)))))

    )
  )

(deftest equal-test
  (testing "Test de si los dos primeros elementos de una lista son iguales"
    (is (= (symbol "#f") (equal '(x 1)))) 
    (is (= (symbol "#t") (equal '(1 1 3 2))))
))

(deftest armar_lambda-test
  (testing "Test de si los dos primeros elementos de una lista son iguales"
    (is (= (list 'f (concat (list 'lambda '(x)) '(+ x 1))) (armar_lambda '(f x) '(+ x 1))))
    (is (= (list 'sumar (concat (list 'lambda '(a b)) '(+ a b))) (armar_lambda '(sumar a b) '(+ a b))))
    ))
