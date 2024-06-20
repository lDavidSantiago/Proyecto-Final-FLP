#lang eopl

;Especificación Léxica

(define lexica
'((white-sp
   (whitespace) skip)
  (comment
   ("$" (arbno (not #\newline))) skip)
  (identificador
   (letter (arbno (or letter digit "?"))) symbol)
  (digitoDecimal
   (digit (arbno digit)) number)
  (digitoDecimal
   ("-" digit (arbno digit)) number)
  (digitoBinario
   ("b" (or "0" "1") (arbno (or "0" "1"))) string)
  (digitoBinario
   ("-" "b" (or "0" "1") (arbno (or "0" "1"))) string)
  (digitoOctal
   ("0x" (or "0" "1" "2" "3" "4" "5" "6" "7")(arbno (or "0" "1" "2" "3" "4" "5" "6" "7"))) string)
  (digitoOctal
   ("-" "0x" (or "0" "1" "2" "3" "4" "5" "6" "7") (arbno (or "0" "1" "2" "3" "4" "5" "6" "7"))) string)
  (digitoHexadecimal
   ("hx" (or "0" "1" "2" "3" "4" "5" "6" "7" "8" "9" "A" "B" "C" "D" "E" "F") (arbno (or "0" "1" "2" "3" "4" "5" "6" "7" "8" "9" "A" "B" "C" "D" "E" "F"))) string)
  (digitoHexadecimal
   ("-" "hx" (or "0" "1" "2" "3" "4" "5" "6" "7" "8" "9" "A" "B" "C" "D" "E" "F") (arbno (or "0" "1" "2" "3" "4" "5" "6" "7" "8" "9" "A" "B" "C" "D" "E" "F"))) string)
  (flotante
   (digit (arbno digit) "." digit (arbno digit)) number)
  (flotante
   ("-" digit (arbno digit) "." digit (arbno digit)) number)
  ))

;Especificación Gramática

(define gramatica
  '((program (expresion) a-program)
    
    ;;Numero-exp
    (expresion (numero-exp) num-exp)  
    (numero-exp (digitoDecimal) decimal-num)
    (numero-exp (digitoBinario) bin-num)
    (numero-exp (digitoOctal) octal-num)
    (numero-exp (digitoHexadecimal) hex-num)
    (numero-exp (flotante) flotante-num)

    ;; Listas
    (expresion ("list" "(" (separated-list expresion ",") ")") lista-exp)
    (expresion ("cons" "(" expresion expresion ")") cons-exp)
    (expresion ("empty") empty-list-exp)

    ;; Primitivas numéricas
    (primitive ("+") sum-prim)
    (primitive ("-") minus-prim)
    (primitive ("*") mult-prim)
    (primitive ("mod") mod-prim)
    (primitive ("pow") elevar-prim)
    (primitive ("<") menor-prim)
    (primitive (">") mayor-prim)
    (primitive ("<=") menorigual-prim)
    (primitive (">=") mayorigual-prim)
    (primitive ("!=") diferente-prim)
    (primitive ("==") igual-prim)

    ;; Expresiones aritméticas
    (expresion ("(" expresion primitive expresion ")") prim-num-exp)
    


    ;; Identificadores y cadenas
    (expresion (identificador) var-exp)
    (expresion ("\"" identificador (arbno identificador) "\"") cadena-exp)
    (expresion ("true") true-exp)
    (expresion ("false") false-exp)

    
    ;; Primitivas listas
    (expresion (primitivaListas "(" expresion ")") prim-list-exp)
    (primitivaListas ("first") first-primList)
    (primitivaListas ("rest") rest-primList)
    (primitivaListas ("empty?") empty-primList)

    ;; Funciones y llamadas
    (expresion ("func" "(" (separated-list identificador ",") ")" expresion) func-exp)
    (expresion ("call" expresion "(" (separated-list expresion ",") ")") app-exp)
    
    
    ;; Ligaduras modificables
    (expresion ("var" (arbno identificador "=" expresion) "in" expresion) lvar-exp)
    (expresion ("let" (arbno identificador "=" expresion) "in" expresion) let-exp)

    ;; Bloques de expresiones
    (expresion ("begin" expresion (arbno ";" expresion) "end") begin-exp)
    (expresion ("set" identificador "=" expresion) set-exp)

    ;; Estructuras
    ;(struct-decl ("struct" identificador "{" (arbno identificador) "}") struct-exp)
    ;; Instanciación y uso de estructuras
    (expresion ("new" identificador "(" (separated-list expresion ",") ")") new-struct-exp)
    (expresion ("get" expresion "." identificador) get-struct-exp)
    (expresion ("set-struct" expresion "." identificador "=" expresion) set-struct-exp)

    ;; Primitivas de cadenas
    (expresion (primitivaCadena "(" (separated-list expresion ",") ")") prim-cad-exp)
    (primitivaCadena ("concat") concat-primCad)
    (primitivaCadena ("string-length") length-primCad)
    (primitivaCadena ("elementAt") index-primCad)

    ;; Iteradores
    (expresion ("for" identificador "from" expresion "until" expresion "by" expresion "do" expresion) for-exp)


    ;; Arrays y primitivas de arrays
    (expresion ("array" "(" (separated-list expresion ",") ")") array-exp)
    (primitivaArray ("length") length-primArr)
    (primitivaArray ("index") index-primArr)
    (primitivaArray ("slice") slice-primArr)
    (primitivaArray ("setlist") setlist-primArr)
    (expresion (primitivaArray "(" (separated-list expresion ",") ")") prim-array-exp)

    ;; Reconocimiento de patrones
    (expresion ("match" expresion "{" (arbno regular-exp "=>" expresion) "}") match-exp)
    ;; Expresiones regulares
    (regular-exp (identificador "::" identificador) list-match-exp)
    (regular-exp ("numero" "(" identificador ")") num-match-exp)
    (regular-exp ("cadena" "(" identificador ")") cad-match-exp)
    (regular-exp ("boolean" "(" identificador ")") bool-match-exp)
    (regular-exp ("array" "(" (separated-list identificador ",") ")") array-match-exp)
    (regular-exp ("empty") empty-match-exp)
    (regular-exp ("default") default-match-exp)

    ;; Primitivas booleanas
    (primitivaBooleana ("and") and-prim)
    (primitivaBooleana ("or") or-prim)
    (primitivaBooleana ("xor") xor-prim)
    (primitivaBooleana ("not") not-prim)

    
    ;; Expresión literal
    ;;(expresion (number) lit-exp)
    ))

(sllgen:make-define-datatypes lexica gramatica)

(define show-the-datatypes
  (lambda () (sllgen:list-define-datatypes lexica gramatica)))

;*******************************
;Parser, Scanner, Interfaz

;El FrontEnd (Análisis léxico (scanner) y sintáctico (parser) integrados)

(define scan&parse
  (sllgen:make-string-parser lexica gramatica))

;El Analizador Léxico (Scanner)

(define just-scan
  (sllgen:make-string-scanner lexica gramatica))

;El Interpretador (FrontEnd + Evaluación + señal para lectura )

(define interpretador
  (sllgen:make-rep-loop  "--> "
    (lambda (pgm) (init-program  pgm)) 
    (sllgen:make-stream-parser 
      lexica
      gramatica)))

;*******************************
;El Interprete


; evaluar un programa completo en el contexto de un ambiente específico

(define init-program
  (lambda (pgm)
    (cases program pgm
      (a-program (body)
                 (determinar-expresion body (empty-env))))))


; toma una expresión y un ambiente como entrada y evalúa la expresión en el contexto de ese ambiente
(define determinar-expresion
  (lambda (exp env)
    (cases expresion exp
      (num-exp ; Caso para expresiones numéricas
       (tipo_numero)
       (cases 
           numero-exp 
         tipo_numero
         (decimal-num (valor) valor)
         (bin-num (valor) valor)
         (octal-num (valor) valor)
         (hex-num (valor) valor)
         (flotante-num (valor) valor)
         )
       )
      
      (var-exp (identificador) ; Caso para variables
       (aplicar-ambiente env identificador))
      (true-exp () ; Caso para el valor verdadero
       #T)
      (false-exp () ; Caso para el valor falso
       #F)

      (func-exp 
       (identificadores cuerpo)
       (closure identificadores cuerpo env)
       )

      (app-exp 
       (operador operandos)
       (let 
           (
            (procedimiento (determinar-expresion operador env))
            (argumentos (evaluar-operandos operandos env))
            )
         (if 
          (procval? procedimiento)
          (aplicar-procedimiento procedimiento argumentos env) ; Aplicar el procedimiento si es válido
          (eopl:error 'determinar-expresion
                      "Intento de un no procedimiento." procedimiento) ; Error si el procedimiento no es válido
          )
         )
       )
      (set-exp ; Caso para asignaciones de variables
       (id nuevo_valor)
       (let 
           (
            (valor_evaluado (determinar-expresion nuevo_valor env)) ; Evaluar el nuevo valor
            )
         (modificar-ligadura env id valor_evaluado) ; Modificar la variable en el ambiente
         'void-exp ; Retornar 'void-exp' después de la asignación
         )
       )
      ;;Primitiva numeros 
      (prim-num-exp ; Caso para operaciones primitivas con números
       (exp1 prim exp2)
       (let  ; Evaluar las expresiones
           (
            (exp_evaluada1 (determinar-expresion exp1 env))
            (exp_evaluada2 (determinar-expresion exp2 env)) 
            )
         (aplicar-primitiva prim exp_evaluada1 exp_evaluada2) ; Aplicar la operación primitiva
         )
       )
      ;;Primitiva Cadena
      (prim-cad-exp ; Caso para operaciones primitivas con cadenas
       (prim operandos)
       (let 
           ((argumentos (evaluar-operandos operandos env)) ; Evaluar los operandos
            )
         (aplicar-primitiva-cadena prim argumentos) ; Aplicar la operación primitiva
         )
       )

      (begin-exp ; Caso para la expresión 'begin'
        (exp lista_exps) 
        (let loop 
          (
           (acumulador (determinar-expresion exp env)) ; Evaluar la primera expresión
           (lista_exps lista_exps)
           )
          (if 
           (null? lista_exps) 
           acumulador ; Si no hay más expresiones, retornar el acumulador
           (loop 
            (determinar-expresion (car lista_exps) env) ; Evaluar la siguiente expresión
            (cdr lista_exps)
            )
           )
          )
        )
      (let-exp ; Caso para la expresión 'let'
       (identificadores operandos cuerpo)
       (let 
           (
            (argumentos_evaluados (evaluar-operandos operandos env)) ; Evaluar los operandos
            )
         (determinar-expresion cuerpo
                               (extend-env identificadores argumentos_evaluados env)) ; Evaluar el cuerpo en un ambiente extendido
         )
       )

      (lvar-exp ; Caso para la expresión 'lvar'
       (identificadores operandos cuerpo)
       (let 
           (
            (argumentos_evaluados (evaluar-operandos operandos (extend-modificable-env identificadores (list->vector operandos) env))) ; Evaluar los operandos
            )
         (determinar-expresion cuerpo
                               (extend-modificable-env identificadores (list->vector argumentos_evaluados) env)) ; Evaluar el cuerpo en un ambiente extendido
         )
       )

      (cadena-exp ; Caso para la expresión 'cadena'
       (id lista_identificadores)
       (letrec
           [
            (construir_cadena ; Función auxiliar para construir la cadena
             (lambda (lista_ids)
               (cond
                 [(null? lista_ids) ""]
                 [else (string-append " " (symbol->string (car lista_ids)) (construir_cadena (cdr lista_ids)))] ; Concatenar el identificador actual con el resto de la cadena
                 )
               )
             )
            ]
         (string-append (symbol->string id) (construir_cadena lista_identificadores)) ; Construir la cadena completa
         )
       )
      ;;Listas
      (lista-exp ; Caso para la expresión 'lista'
       (lista_exp)
       (evaluar-operandos lista_exp env) ; Evaluar los operandos de la lista
       )

      (cons-exp ; Caso para la expresión 'cons'
       (exp1 exp2)
       (cons 
        (evaluar-operando exp1 env) 
        (evaluar-operando exp2 env) 
        )
       )
      
      ;;Lista primitiva
      (prim-list-exp ; Caso para operaciones primitivas con listas
       (prim expresion)
       (let 
           (
            (argumento (evaluar-operando expresion env)) ; Evaluar el operando
            )
         (aplicar-lista prim argumento) ; Aplicar la operación primitiva
         )
       )
      
      ;;Bucle for
      (for-exp ; Caso para la expresión 'for'
       (var expresion-inicio expresion-final expresion-suma cuerpo_exp)
       (let 
           (
            (inicio
             (determinar-expresion expresion-inicio env)
             ) ; Evaluar la expresión de inicio
            (fin
             (determinar-expresion expresion-final env)
             ) ; Evaluar la expresión de fin
            (suma
             (determinar-expresion expresion-suma env)
             ) ; Evaluar la expresión de suma
            )
         (bucle-for cuerpo_exp var inicio suma fin env) ; Ejecutar el bucle 'for'  
         )
       )
      
      ;;Arrays
      (array-exp ; Caso para la expresión 'array'
       (lista) 
       (list->vector (evaluar-operandos lista env)) ; Convertir la lista evaluada a un vector
       )

      (prim-array-exp ; Caso para operaciones primitivas con arrays
       (primitiva lista_args)
       (primitiva-vector primitiva (evaluar-operandos lista_args env)) ; Aplicar la operación primitiva
       )

      (empty-list-exp ; Caso para la expresión 'empty-list'
       () 
       '() ; Retornar una lista vacía
       )

      (match-exp ; Caso para la expresión 'match' que busca si los casos coinciden
       (exp_var lista_casos lista_exp)
       (let 
           (
            (valor_evaluado (determinar-expresion exp_var env)) ; Evaluar la expresión variable
            )
         (coincidir-valor valor_evaluado lista_casos lista_exp env) ; Buscar una coincidencia en los casos
         )
       )

      (new-struct-exp ; Caso para la expresión 'new-struct'
       (identificador lista_atributos)
       0 
       )

      (get-struct-exp ; Caso para la expresión 'get-struct'
       (estructura atributo)
       0 
       )

      (set-struct-exp ; Caso para la expresión 'set-struct'
       (estructura_var atributo nuevo_valor)
       0 
       )
    )
  )
)


; funciones auxiliares en general

; lista de operandos 
(define evaluar-operandos
  (lambda (operandos env)
    (map (lambda (x) (evaluar-operando x env)) operandos); Evaluar cada operando en el ambiente dado
    )
  )

(define evaluar-operando
  (lambda (operando env)
    (determinar-expresion operando env); Evaluar el operando en el ambiente dado
    )
  )

; Modificacion de Strings
(define eliminar-caracter
  (lambda (cadena caracter)
    (letrec
        [
         (remover-caracter 
          (lambda (cadena caracter)
            (cond
              [(null? cadena) '()] ; Si la cadena está vacía, retornar una lista vacía
              [else 
               (if (char=? caracter (car cadena); Si el caracter actual es igual al caracter a eliminar
                           ) 
                   (remover-caracter (cdr cadena) caracter) ; Continuar con el resto de la cadena sin añadir el caracter actual
                   (cons (car cadena)
                         (remover-caracter (cdr cadena) caracter); Añadir el caracter actual y continuar con el resto de la cadena
                         )
                   ) 
               ]
              )
            )
          )
         ]
      (list->string (remover-caracter (string->list cadena) caracter)) ; Convertir la cadena a una lista, eliminar el caracter y convertir de nuevo a cadena
      )
    )
  )

(define reemplazar-caracter
  (lambda (cadena caracter nuevo_caracter)
    (letrec 
        [
         (reemplazar-car 
          (lambda (cadena caracter nuevo_caracter)
            (cond
              [(null? cadena) '()] ; Si la cadena está vacía, retornar una lista vacía
              [else 
               (if (char=? caracter (car cadena)) ; Si el caracter actual es igual al caracter a reemplazar
                   (cons nuevo_caracter
                         (reemplazar-car (cdr cadena) caracter nuevo_caracter); Añade el nuevo caracter y continuar con el resto de la cadena
                         ) 
                   (cons (car cadena) (
                                       reemplazar-car (cdr cadena) caracter nuevo_caracter); Añade el caracter actual y continuar con el resto de la cadena
                         )
                   ) 
               ]
              )
            )
          )
         ]
      (list->string (reemplazar-car (string->list cadena) caracter nuevo_caracter)) ; Convertir la cadena a una lista, reemplazar el caracter y convertir de nuevo a cadena
      )
    )
  )



; Aplicación de operaciones según la base del número
(define condicionales-bases
  (lambda (operacion arg1 arg2 convertir)
    (cond
      [(string? arg1) ; Si el argumento es una cadena
       (cond 
         [(or (equal? (string-ref arg1 0) #\b)  ; Si el primer caracter es 'b' (base 2)
              (and (equal? (string-ref arg1 1) #\b)  ; O si el segundo caracter es 'b' y el primero es '-' (base 2 negativa)
                   (equal? (string-ref arg1 0) #\-)))
          (convertir ; con la intencion de poder realizar operaciones en algunos tipos de datos
           (operacion 
            (string->number (eliminar-caracter arg1 #\b) 2)  ; Convertir la cadena a número en base 2
            (string->number (eliminar-caracter arg2 #\b) 2)
            )
           2
           )
          ]
         [(or (equal? (string-ref arg1 0) #\h)  ; Si el primer caracter es 'h' (base 16)
              (and (equal? (string-ref arg1 1) #\h)  ; O si el segundo caracter es 'h' y el primero es '-' (base 16 negativa)
                   (equal? (string-ref arg1 0) #\-)))
          (convertir 
           (operacion 
            (string->number (reemplazar-caracter arg1 #\h #\#) 16)  ; Convertir la cadena a número en base 16
            (string->number (reemplazar-caracter arg2 #\h #\#) 16)
            )
           16
           )
          ]
         [(or (equal? (string-ref arg1 1) #\x)  ; Si el segundo caracter es 'x' (base 8)
              (and (equal? (string-ref arg1 2) #\x)  ; O si el tercer caracter es 'x' y el primero es '-' (base 8 negativa)
                   (equal? (string-ref arg1 0) #\-)
                   )
              )
          (convertir 
           (operacion 
            (string->number (eliminar-caracter arg1 #\x) 8)  ; Convertir la cadena a número en base 8
            (string->number (eliminar-caracter arg2 #\x) 8)
            )
           8
           )
          ]
         [else (display "Error de formato")] ; Si no se cumple ninguna de las condiciones anteriores, mostrar un error
         )
       ]
      [else (operacion arg1 arg2)] ; Si el argumento no es una cadena, aplicar la operación directamente
      )
    )
  )

(define retornar-booleano
  (lambda (salida base) salida) ; Retorna el valor de salida sin cambios
  )

; Conversión de número a cadena según la base
(define convertir-numero-a-base
  (lambda (numero base)
    (let* ((es-negativo? (< numero 0)) ; Verificar si el número es negativo
           (valor-absoluto-numero (abs numero))) ; Obtener el valor absoluto del número
      (case base
        [(2) (if es-negativo? 
                 (string-append "-" "b" (number->string valor-absoluto-numero 2)) ; Si es negativo, añadir '-' y 'b' al inicio
                 (string-append "b" (number->string valor-absoluto-numero 2))) ; Si no es negativo, añadir 'b' al inicio
        ]
        [(8) (if es-negativo? 
                 (string-append "-" "0x" (number->string valor-absoluto-numero 8)) ; Si es negativo, añadir '-' y '0x' al inicio
                 (string-append "0x" (number->string valor-absoluto-numero 8))) ; Si no es negativo, añadir '0x' al inicio
        ]
        [(16) (if es-negativo? 
                  (string-append "-" "hx" (number->string valor-absoluto-numero 16)) ; Si es negativo, añadir '-' y 'hx' al inicio
                  (string-append "hx" (number->string valor-absoluto-numero 16))) ; Si no es negativo, añadir 'hx' al inicio
        ]
        [else (eopl:error "Base no soportada")] ; Si la base no es 2, 8 o 16, mostrar un error
        )
      )
    )
  )

(define aplicar-primitiva
  (lambda (primitiva arg1 arg2)
    ;;Comparaciones
    (cases primitive primitiva
      (sum-prim () (condicionales-bases + arg1 arg2
                                        convertir-numero-a-base) ; suma
                )
      (minus-prim () (condicionales-bases - arg1 arg2
                                          convertir-numero-a-base) ; resta
                  )
      (mult-prim () (condicionales-bases * arg1 arg2
                                         convertir-numero-a-base) ; multiplicación
                 )
      (mayor-prim () (condicionales-bases > arg1 arg2
                                          retornar-booleano) ; mayor que
                  )
      (menor-prim () (condicionales-bases < arg1 arg2
                                          retornar-booleano) ; menor que
                  )
      (menorigual-prim () (condicionales-bases <= arg1 arg2
                                               retornar-booleano) ; menor o igual que
                       )
      (mayorigual-prim () (condicionales-bases >= arg1 arg2
                                               retornar-booleano) ; mayor o igual que
                       )
      (diferente-prim () (not
                          (equal? arg1 arg2) ; desigualdad
                          )
                      )
      (igual-prim () (equal? arg1 arg2) ; igualdad
                  )
      ;;Operaciones
      (mod-prim () (condicionales-bases remainder arg1 arg2
                                        convertir-numero-a-base) ; módulo
                )
      (elevar-prim () (condicionales-bases expt arg1 arg2
                                           convertir-numero-a-base) ; potencia
                   )
      )
    )
  )


; toma una primitiva y una lista de argumentos y aplica la operación correspondiente a la primitiva
(define aplicar-primitiva-cadena
  (lambda (primitiva args)
    (cases primitivaCadena primitiva
      (length-primCad () (string-length (car args))) ; Aplicar longitud de cadena
      (index-primCad ()  (string  (string-ref (car args) (cadr args)))) ; Aplicar indexación de cadena
      (concat-primCad () 
                      (letrec
                          [
                           (concatenar
                            (lambda (lista_cadenas)
                              (cond
                                [(null? lista_cadenas) ""] ; Si la lista está vacía, retornar cadena vacía
                                [else (string-append (car lista_cadenas) (concatenar (cdr lista_cadenas)))] ; De lo contrario, concatenar la cabeza de la lista con el resultado de concatenar el resto de la lista
                                )
                              )
                            )
                           ]
                        (concatenar args)  ; Aplicar concatenación de cadenas
                        )
                      )
      )
    )
  )

;metodos de listas
; aplicar-primitiva lista
(define aplicar-lista
  (lambda (primitiva arg)
    (let ((operacion (cases primitivaListas primitiva
                       (first-primList () car)
                       (rest-primList () cdr)
                       (empty-primList () null?))))
      (operacion arg)
      )
    )
  )

;vectores
(define obtener-subvector
  (lambda (vector inicio fin)
    (if (= inicio fin)
        (list (vector-ref vector inicio)
              )
        (cons (vector-ref vector inicio) (obtener-subvector vector (+ inicio 1) fin)
              )
        )
    )
  )

(define primitiva-vector
  (lambda (primitiva arg)
    (let ((primer-arg (car arg))
          (segundo-arg (cadr arg))
          (tercer-arg (caddr arg)))
      (cases primitivaArray primitiva
        (length-primArr () (vector-length primer-arg))
        (slice-primArr () (list->vector (obtener-subvector primer-arg segundo-arg tercer-arg)))
        (index-primArr () (vector-ref primer-arg segundo-arg))
        (setlist-primArr () 
                         (vector-set! primer-arg segundo-arg tercer-arg)
                         primer-arg
                         )   
        )
      )
    )
  )


; Se define el bucle for
(define bucle-for
  (lambda (expresion-cuerpo variable indice incremento fin entorno)
    (if (< indice fin) ; Comprueba si el índice es menor que el valor final
        (begin
          (determinar-expresion expresion-cuerpo (extend-env (list variable) (list indice) entorno)) ; Evalúa la expresión del cuerpo con la variable establecida en el valor del índice
          (bucle-for expresion-cuerpo variable (+ indice incremento) incremento fin entorno)) ; Llama recursivamente a bucle-for con el índice incrementado
        'void-exp ; Si el índice no es menor que el valor final, retorna 'void-exp'
        )
    )
  )
; Entra una lista de variables pero si esta no es suficiente, la última toma el valor del resto del vector
(define establecer-vector-a-variables
  ; Si la lista de variables es más corta que el vector de valores, la última variable en la lista se asigna al resto del vector
  (lambda (lis_ids vect acc)
    (cond
      [(null? (cdr lis_ids))
       (cons
        (obtener-subvector vect acc (- (vector-length vect) 1)) '())] ; Si solo queda una variable en la lista, se le asigna el resto del vector
      [else
       (cons
        (vector-ref vect acc) (establecer-vector-a-variables (cdr lis_ids) vect (+ acc 1)))]
      ; De lo contrario, se asigna el valor actual del vector a la variable actual y se llama recursivamente a la función con el siguiente índice
      )
    )
  )

;evalua si coinciden
(define coincidir-valor 
  (lambda (valor primit-coincidencia expresion_coincidencia env)
    (cases regular-exp (car primit-coincidencia)
      (empty-match-exp () 
                       ; si el valor es nulo, evalúa la primera expresión de coincidencia en el entorno actual
                       ; sino, llama recursivamente a la función con el resto de las primitivas y expresiones de coincidencia
                       (if (null? valor)
                           (determinar-expresion (car expresion_coincidencia) env)
                           (coincidir-valor valor (cdr primit-coincidencia) (cdr expresion_coincidencia) env)
                           )
                       )
      (list-match-exp (cabeza cola) 
                      ; si el valor es una lista, evalúa la primera expresión de coincidencia en un entorno extendido con la cabeza y la cola de la lista
                      ; sino, llama recursivamente a la función con el resto de las primitivas y expresiones de coincidencia
                      (if (list? valor)
                          (determinar-expresion (car expresion_coincidencia) 
                                                (extend-env (cons cabeza (cons cola '())) 
                                                            (list (car valor) (cdr valor)) env))
                          (coincidir-valor valor (cdr primit-coincidencia) (cdr expresion_coincidencia) env)
                          )
                      )
      (num-match-exp (ids) 
                     ; si el valor es un número o una cadena que representa un número binario, hexadecimal o decimal, evalúa la primera expresión de coincidencia en un entorno extendido con el valor
                     ; sino, llama recursivamente a la función con el resto de las primitivas y expresiones de coincidencia
                     (if (or (number? valor)
                             (and (equal? (string-ref valor 0) #\b)
                                  (or (equal? (string-ref valor 1) #\b) 
                                      (equal? (string-ref valor 0) #\-)))
                             (and (equal? (string-ref valor 0) #\h)
                                  (or (equal? (string-ref valor 1) #\h) 
                                      (equal? (string-ref valor 0) #\-)))
                             (and (equal? (string-ref valor 0) #\0)
                                  (or (equal? (string-ref valor 1) #\x) 
                                      (equal? (string-ref valor 0) #\-))))
                         (determinar-expresion (car expresion_coincidencia) 
                                               (extend-env (list ids) (list valor) env))
                         (coincidir-valor valor (cdr primit-coincidencia) (cdr expresion_coincidencia) env)
                         )
                     )
      (cad-match-exp (ids) 
                     ; Si el valor es una cadena que representa un número binario, hexadecimal o decimal, llama recursivamente a la función con el resto de las primitivas y expresiones de coincidencia
                     ; sino evalúa la primera expresión de coincidencia en un entorno extendido con el valor
                     (if (or (and (equal? (string-ref valor 0) #\b)
                                  (or (equal? (string-ref valor 1) #\0) 
                                      (equal? (string-ref valor 1) #\1))
                                  )
                             (and (equal? (string-ref valor 0) #\-)
                                  (equal? (string-ref valor 1) #\b)
                                  (or (equal? (string-ref valor 2) #\0) 
                                      (equal? (string-ref valor 2) #\1))
                                  )
                             (and (equal? (string-ref valor 0) #\h)
                                  (equal? (string-ref valor 1) #\x)
                                  )
                             (and (equal? (string-ref valor 0) #\-)
                                  (equal? (string-ref valor 1) #\h)
                                  (equal? (string-ref valor 2) #\x)
                                  )
                             (and (equal? (string-ref valor 0) #\0)
                                  (equal? (string-ref valor 1) #\x)
                                  )
                             (and (equal? (string-ref valor 0) #\-)
                                  (equal? (string-ref valor 1) #\0)
                                  (equal? (string-ref valor 2) #\x))
                             )
                         (coincidir-valor valor (cdr primit-coincidencia) (cdr expresion_coincidencia) env)
                         (determinar-expresion (car expresion_coincidencia) 
                                               (extend-env (list ids) (list valor) env))
                         )
                     )
      (bool-match-exp (ids) 
                      ; Si el valor es un booleano, evalúa la primera expresión de coincidencia en un entorno extendido con el valor
                      ; sino llama recursivamente a la función con el resto de las primitivas y expresiones de coincidencia
                      (if (boolean? valor)
                          (determinar-expresion (car expresion_coincidencia) 
                                                (extend-env (list ids) (list valor)
                                                            env)
                                                )
                          (coincidir-valor valor (cdr primit-coincidencia) (cdr expresion_coincidencia)
                                           env)
                          )
                      )
      (array-match-exp (ids) 
        ; Si el valor es un vector, evalúa la primera expresión de coincidencia en un entorno extendido con los valores del vector
        ; sino llama recursivamente a la función con el resto de las primitivas y expresiones de coincidencia
        (cond
          [(vector? valor)
           (determinar-expresion (car expresion_coincidencia) 
                           (extend-env ids
                                       (establecer-vector-a-variables ids valor 0)
                                       env)
                           )]
          [else
           (coincidir-valor valor (cdr primit-coincidencia)
                            (cdr expresion_coincidencia)
                            env)]
        )
      )
      (default-match-exp () 
        ; Evalúa la primera expresión de coincidencia en el entorno actual
        (determinar-expresion (car expresion_coincidencia)
                              env)
      )
    )
  )
)

;Procedimientos
(define-datatype procval procval?
  (closure
   (identificadores (list-of symbol?); Lista de identificadores
                    ) 
   (cuerpos expresion?) ; Cuerpo del procedimiento
   (env environment?); Entorno de evaluación
   )
  ) 


;evalua el cuerpo de un procedimiento en un ambiente extendido
(define aplicar-procedimiento
  (lambda (proc args envI) ; Toma un procedimiento, argumentos y un entorno inicial
    (cases procval proc ; comprueba si el procedimiento es de tipo 'procval'
      (closure (identificadores cuerpo env) ; si es una clausura, extrae los identificadores, el cuerpo y el entorno
        (cases environment env ; comprueba el tipo de entorno
          (extend-modificable-env (lista-identificadores lista-valores next-env) 
            ; Si es modificable, extiende el entorno y evalúa el cuerpo
            (determinar-expresion cuerpo (extend-env identificadores args envI))
          )
          (else 
            ; Si no es modificable, extiende el entorno y evalúa el cuerpo
            (determinar-expresion cuerpo (extend-env identificadores args envI))
          )
        )
      )
    )
  )
)


;Ambientes



(define-datatype environment environment?
  (empty-env) ; Constructor para un entorno vacío
  (extend-env ; Constructor para un entorno extendido
   (syms (list-of symbol?); Lista de símbolos
         ) 
   (valores
    (list-of scheme-value?); Lista de valores
    ) 
   (env environment?) ; Entorno existente
  )
  (extend-modificable-env ; Constructor para un entorno extendido modificable
   (syms
    (list-of symbol?); Lista de símbolos
    ) 
   (valores vector?) ; Vector de valores
   (env environment?) ; Entorno existente
  )
)

; toma un argumento v y siempre devuelve #t,
(define scheme-value? 
  (lambda (v) #t))


;;Esta función se utiliza para buscar una variable específica en un ambiente dado

(define search-no-modificable-env
  
  (lambda (lista-identificadores lista-valores valor-buscado next-amb)
    (cond
      [(null? lista-identificadores) ; si es la lista de identificadores es vacia, aplica el entorno siguiente al valor buscado.
       (aplicar-ambiente next-amb valor-buscado
                  )
       ]
      ; Si el valor buscado es igual al primer identificador en la lista, devuelve el primer valor en la lista de valores
      [(equal? (car lista-identificadores) valor-buscado
               ) (car lista-valores)
                 ]
      [else (search-no-modificable-env ; Si no, busca en el resto de la lista de identificadores y valores
             (cdr lista-identificadores)(cdr lista-valores)
             valor-buscado next-amb)
            ]
      )  
    ) 
  )

;Busca un valor en un entorno modificable.
(define search-modificable-env
  (lambda
      (lista-identificadores lista-valores valor-buscado next-amb acumulador env )
    (cond
      [(null? lista-identificadores); busca en el siguiente entorno
       (aplicar-ambiente next-amb valor-buscado)
       ]
      [(equal?
        (car lista-identificadores)
        valor-buscado) 
       (if (expresion? (vector-ref lista-valores acumulador); evalúa la expresión en el entorno env. Si no es una expresión, se devuelve el valor tal cual
                       ) 
           (determinar-expresion (vector-ref lista-valores acumulador)
                                 env)
           (vector-ref lista-valores
                       acumulador)
           )
       ]
      ; se busca en el resto de la lista de identificadores y valores, incrementando el acumulador en 1.
      [else (search-modificable-env (cdr lista-identificadores) lista-valores valor-buscado next-amb (+ acumulador 1)
                                    env)
            ]
      )  
    ) 
  )

;función que busca un símbolo en un ambiente
(define aplicar-ambiente
  (lambda
      (env sym)
    (cases environment env
      (empty-env () (eopl:error "No se encontro la variable"
                                sym)
                 )
      (extend-env (lista-identificadores lista-valores next-env)
                  (search-no-modificable-env lista-identificadores lista-valores sym
                                             next-env)
                  )
      (extend-modificable-env (lid lval next-env)
                              (search-modificable-env lid lval sym next-env 0
                                                      env)
                              )
      ) 
    )
  )

; busca el valor asociado con sym en env
(define busqueda
  (lambda (lista-identidicadores lista-valores valor-buscado next-amb)
    (cond
      [(null? lista-identidicadores); Si el entorno es vacío, lanza un error
       (extend-env (aplicar-ambiente next-amb
                                     valor-buscado)
                   )]
      [(equal? (car lista-identidicadores); Si el entorno es extendido no modificable, busca el símbolo en el entorno
               valor-buscado) (car lista-valores)
                              ]
      [else (search-no-modificable-env; Si el entorno es extendido modificable, busca el símbolo en el entorno
             (cdr lista-identidicadores) (cdr lista-valores) valor-buscado
             next-amb)
            ]
      )  
    ) 
  )

; Función que busca un símbolo en un ambiente
(define establecer-env
  (lambda (env sym)
    (cases environment env
      ; Si el entorno es vacío, lanza un error
      (empty-env () (eopl:error "No se encontro la variable"))
      ; Si el entorno es extendido no modificable, busca el símbolo en el entorno
      (extend-env (lista-identificadores lista-valores next-env) 
                  (search-no-modificable-env lista-identificadores lista-valores sym next-env))
      ; En cualquier otro caso, lanza un error
      (else (eopl:error "No se encontro la variable"))
    )
  )
)

; Función que modifica una ligadura en un ambiente
(define modificar-ligadura
  (lambda (env sym val)
    (cases environment env
      ; Si el entorno es extendido modificable, busca el índice del símbolo y modifica el valor en el vector de entorno
      (extend-modificable-env (simbolos vectorEntorno entornoSiguiente)
                              (letrec
                                  [(buscarIndice
                                    (lambda (listaSimbolos acumulador)
                                      (cond
                                        ; Si la lista de símbolos es nula, lanza un error
                                        [(null? listaSimbolos) (eopl:error "indice no encontrado " sym)]
                                        ; Si el primer símbolo en la lista es igual al símbolo buscado, devuelve el acumulador
                                        [(equal? (car listaSimbolos) sym) acumulador]
                                        ; Si no, busca en el resto de la lista de símbolos, incrementando el acumulador
                                        [else (buscarIndice (cdr listaSimbolos) (+ acumulador 1))]
                                      )
                                    )
                                  )]
                                ; Modifica el valor en el vector de entorno en el índice encontrado
                                (vector-set! vectorEntorno (buscarIndice simbolos 0) val)
                              )
      )
      ; Si el entorno es extendido no modificable, modifica la ligadura en el entorno siguiente
      (extend-env (simbolos lista entornoSiguiente) (modificar-ligadura entornoSiguiente sym val))
      ; En cualquier otro caso, lanza un error
      (else (eopl:error "No se encontró la variable"))
    )
  )
)

;******************************


(show-the-datatypes)

;(define un-programa-dificil (a-program una-expresion-dificil))

;; Descomentar para pruebas, vuelva a comentar para envitar
(interpretador)

#|
No tocar
Exporar funciones
|#
(provide (all-defined-out))
