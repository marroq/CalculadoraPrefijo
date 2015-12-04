;PRCEDIMIENTO PARA MOSTRAR EL COMANDO calculadora >> CADA VEZ QUE SE VA A INGRESAR UN DATO
(define (comando)
	(display "calculadora >> ")
)
;PROCEDIMIENTO PARA MOSTRAR EL COMANDO respuesta>> CADA VEZ QUE SE MUESTRE UN RESULTADO
(define (respuesta)
	(display "respuesta>> ")
)
;PROCEDIMIENTO PRINCIPAL QUE TIENE DEFINIDO LAS INSTRUCCIONES DE LA CALCULADORA
(define (calcScheme)
	(newline)
	;MENSAJE DE BIENVENIDA Y DATOS DEL PROGRAMADOR	
	(display "            BIENVENIDO!! a SchemeCalc") (newline)	
	(display "	Programador: Douglas Figueroa") (newline)
	(display "  Carnet: 13000530") (newline)
	(display "  Seccion: BN") (newline)
	(display "	Que su uso sea satisfactorio") (newline)
	;AQUI SE LLAMA LA FUNCION COMANDO PARA QUE INICIE DESPLEGANDO calculadora>> 
	(comando)
	;DEFINICION DEL CICLO QUE HARA QUE LA CALCULADORA REGRESE AL PUNTO DE INGRESAR DATOS CADA VEZ QUE MUESTRE UN RESULTADO
	(define (ciclo)
		;INGRESO DEL STRING QUE CONTIENE LA OPERACION
		(define dato (read-line))
		;COMANDO QUE REALIZA LA SALIDA DEL PROGRAMA
		(cond
			((string=? dato "quit")
			(display "Saliendo...") (newline)
			(display "Que el uso de esta calculadora haya sido de su agrado")
			(quit))
		)
		;CONTEO DE PARENTESIS ABIERTOS ")" Y PARENTESIS CERRADOS ")"
		(define parentesis1 (string-search-all "(" dato))
		(define parentesis2 (string-search-all ")" dato))
		(define parentesisA (length parentesis1))
		(define parentesisB (length parentesis2))
		;SI LOS PARENTESIS CERRADOS Y ABIERTOS NO SON IGUALES LA CALCULADORA NO DEJARA OPERAR DEVOLVERA
		;UN MENSAJE DE EXPRESIÓN INVALIDA Y RETORNARA PARA INGRESAR UN NUEVO VALOR
		(if (not (equal? parentesisA parentesisB))
			(begin
				(respuesta)
				(display "ERROR! Invalid Expression") (newline)
				(respuesta)
				(display "Los paréntesis no fueron cerrados correctamente") (newline)
				(comando)
				(ciclo)
			)
		)
		;DEFINICION DE VARIALBES PARA LA UBICACION DE LOS PARENTESIS, SIGNOS, ESPACIOS Y NUMEROS
		(define pCerrado (string-search-forward ")" dato))
		(define pAbierto (- (string-search-backward "(" (substring dato 0 pCerrado)) 1))
		(define Esp2 (- (string-search-backward " " (substring dato 0 pCerrado)) 1))
		(define Esp1 (- (string-search-backward " " (substring dato 0 Esp2)) 1))
		(define n1 (substring dato (+ pAbierto 1) Esp1))
		(define n2 (substring dato (+ Esp1 1) Esp2))
		(define signo (substring dato (+ Esp2 1) pCerrado))
		;CONVERSION DEL STRING A UN NUMERO
		(define num1 (string->number n1))
		(define num2 (string->number n2))
		;CONDICION QUE EVALUA QUE LOS DATOS INGRESADOS SEAN NUMEROS, EVALUA SI LOS ESPACIOS ENTRE NUMEROS
		;Y SIGNOS SON CORRECTOS  SI NO LO SON MOSTRARA EL RESPECTIVO
		;MENSAJE DE ERROR Y MOSTRARA EL COMANDO calculadora>> PARA INGRESAR UNA NUEVA OPERACION
		(if (or (not (number? num1)) (not (number? num2)))
			(begin
				(respuesta)
				(display "ERROR! Invalid Expression") (newline)
				(comando)
				(ciclo)
			)
		)
		;CONDICIÓN QUE EVALUA SI UNA DIVISÓN ES DENTRO DE "0" MOSTRARA EL RESPECTIVO MENSAJE DE ERROR
		;Y RETORNARA PARA INGRESAR UNA NUEVA OPERACION
		(if (or (and (= num2 0) (string=? signo "/")) (and (= num2 0) (string=? signo "div")) (and (= num2 0) (string=? signo "%")))
			(begin
				(respuesta)
				(display "ERROR! Division by zero") (newline)
				(respuesta)
				(display "No puede realizar división dentro de 0") (newline)
				(comando)
				(ciclo)
			)
		)
		;DEFINICION DE LAS OPERACIONES VALIDAS PARA LA CALCULADORA CON LAS VARIABLES QUE YA FUERON 
		;CONVERTIDAS DE STRING A NUMERO
		(define suma (+ num1 num2))
		(define resta (- num1 num2))
		(define multi (* num1 num2))
		(define division (/ num1 num2))
		(define cociente (quotient num1 num2))
		(define residuo (remainder num1 num2))
		(if (equal? parentesisA parentesisB)
			(cond
				;SI EL SIGNO QUE ENCUENTRA ES UN "+" MUESTRA EL COMANDO respuesta>> DESPLIEGA EL RESULTADO
				;DE LA SUMA MUESTRA EL COMANDO calculadora>> Y REGRESA AL CICLO
				((string=? signo "+")
					(respuesta)
					(display suma) (newline)
					(comando)
					(ciclo)
				)
				;SI EL SIGNO QUE ENCUENTRA ES UN "-" MUESTRA EL COMANDO respuesta>> DESPLIEGA EL RESULTADO
				;DE LA RESTA MUESTRA EL COMANDO calculadora>> Y REGRESA AL CICLO
				((string=? signo "-")
					(respuesta)
					(display resta) (newline)
					(comando)
					(ciclo)
				)
				;SI EL SIGNO QUE ENCUENTRA ES UN "*" MUESTRA EL COMANDO respuesta>> DESPLIEGA EL RESULTADO
				;DE LA MULTIPLICACION MUESTRA EL COMANDO calculadora>> Y REGRESA AL CICLO
				((string=? signo "*")
					(respuesta)
					(display multi) (newline)
					(comando)
					(ciclo)
				)
				;SI EL SIGNO QUE ENCUENTRA ES UN "/" MUESTRA EL COMANDO respuesta>> DESPLIEGA EL RESULTADO
				;DE LA DIVISION MUESTRA EL COMANDO calculadora>> Y REGRESA AL CICLO
				((string=? signo "/")
					(respuesta)
					(display division) (newline)
					(comando)
					(ciclo)
				)
				;SI EL SIGNO QUE ENCUENTRA ES UN "div" MUESTRA EL COMANDO respuesta>> DESPLIEGA EL RESULTADO
				;DEL COCIENTE DE LA DIVISION MUESTRA EL COMANDO calculadora>> Y REGRESA AL CICLO
				((string=? signo "div")
					(respuesta)
					(display cociente) (newline)
					(comando)
					(ciclo))
				;SI EL SIGNO QUE ENCUENTRA ES UN "%" MUESTRA EL COMANDO respuesta>> DESPLIEGA EL RESULTADO
				;DEL RESIDUO DE LA DIVISION MUESTRA EL COMANDO calculadora>> Y REGRESA AL CICLO
				((string=? signo "%")
					(respuesta)
					(display residuo) (newline)
					(comando)
					(ciclo))
			)
		)
	)
	(ciclo)
)
(calcScheme)