# Interprete de Racket

Trabajo practico de la materia Lenguajes Formales - 75.14 / 95.48 de la Facultad de Ingenieria de la UBA del 2C2023.
Consiste en la implementancion de un interprete de Racket en Clojure.
Se completó el archivo "core.clj" que contiene todas las expresiones para poder ejecutar el interprete.

## Usage
Levantar clojure con el siguiente comando:

    $ java -jar clojure-1.8.0.jar

Luego cargar el archivo donde se encuentra el interprete con:

    (load-file "src/tp_formales/core.clj")

Acceder al name space con:

    (ns tp-formales.core)

Acceder al repl con:

    (repl)

## Ejecutar archivo "demo.rkt"

Una vez dentro del repl ejecutar:

    (enter! "demo.rkt")

## Ejecutar archivo "jarras.rkt"

Una vez dentro del repl ejecutar:

    (enter! "jarras.rkt")

### Ejemplo:

Ejecutar:

    (breadth-first bc)

Luego ingresar el estado inicial:

    (0 0)

Luego ingresar el estado final: 

    (0 4)

Salida esperada:

    user=> (load-file "src/tp_formales/core.clj")
    true
    user=> (ns tp-formales.core)
    nil
    tp-formales.core=> (repl)
    Interprete de Racket en Clojure
    Trabajo Practico de 75.14/95.48 - Lenguajes Formales 2023
    > (enter! "jarras.rkt")
    "jarras.rkt">     (breadth-first bc)
    Ingrese el estado inicial:     (0 0)
    Ingrese el estado   final:     (0 4)
    Exito !!!
    Prof ....... 12
    Solucion ... ((0 0) (5 0) (0 5) (5 5) (2 8) (2 0) (0 2) (5 2) (0 7) (5 7) (4 8) (4 0) (0 4))
    #t

## Ejecutar tests

Los test unitarios se pueden ejecutar con el comando:

    $ lein test


## License

Copyright © 2023 FIXME

This program and the accompanying materials are made available under the
terms of the Eclipse Public License 2.0 which is available at
http://www.eclipse.org/legal/epl-2.0.

This Source Code may also be made available under the following Secondary
Licenses when the conditions for such availability set forth in the Eclipse
Public License, v. 2.0 are satisfied: GNU General Public License as published by
the Free Software Foundation, either version 2 of the License, or (at your
option) any later version, with the GNU Classpath Exception which is available
at https://www.gnu.org/software/classpath/license.html.
