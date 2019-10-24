#lang racket


;;Libreria de Cartas para graficar
(require games/cards)
(provide (all-defined-out))


;;Deck definido para barajar las cartas
(define Deck  '(Ks Qs Js 10s 09s 08s 07s 06s 05s 04s 03s 02s As
                Kh Qh Jh 10h 09h 08h 07h 06h 05h 04h 03h 02h Ah
                Kd Qd Jd 10d 09d 08d 07d 06d 05d 04d 03d 02d Ad
                Kc Qc Jc 10c 09c 08c 07c 06c 05c 04c 03c 02c Ac))


;;Función Principal
(define (bCEj X)
  (cond ((= (lenList X) 3) (aux1bCEj 3 Deck))
        ((= (lenList X) 2) (aux1bCEj 2 Deck))
        ((= (lenList X) 1) (aux1bCEj 1 Deck))
        (else "Cantidad de jugadores no permitida")))


;;Funcion Auxiliar 1 Principal, la cual construye la lista de jugadores con sus respectivas cartas
;;R1 (Numero Random 1), R2 (Numero Random 2)
(define (aux1bCEj X deck)
  (juego (aux2bCEj X (random 52) (random 52) 52 deck)))


;;Funcion Juego
(define (juego deck)
  (cond ((= (lenList deck) 5) deck)
        ((= (lenList deck) 4) (append (list (car deck)) (list (cadr deck)) '(()) (list (caddr deck)) (list (cadddr deck))))
        ((= (lenList deck) 3) (append (list (car deck)) '(()) '(()) (list (cadr deck)) (list (caddr deck))))
        (else "Sin Jugadores")))
  #|(cond ((= (lenList deck) 5) (IniciarJuego deck))
        ((= (lenList deck) 4) (IniciarJuego (append (list (car deck)) (list (cadr deck)) '(()) (list (caddr deck)) (list (cadddr deck)))))
        ((= (lenList deck) 3) (IniciarJuego (append (list (car deck)) '(()) '(()) (list (cadr deck)) (list (caddr deck)))))
        (else "Sin Jugadores")))|#
        

;;Funcion Iniciar Juego
(define (IniciarJuego deck)
  (cond ((null? deck) "Error Deck Vacio")
        (else "Error"))
  #|(display (retornarCartas 1 deck))
  (display (retornarCartas 2 deck))
  (display (retornarCartas 3 deck))
  (display (retornarCartas 4 deck))|#
  (display "\n")
  (recorrerDeck deck))
        


;;Funcion Auxiliar 2 Principal, la cual construye la lista de jugadores con sus respectivas cartas
(define (aux2bCEj X R1 R2 lenDeck deck)
  (cond ((= X -1) (list deck))
        ;;Compara si las dos cartas son iguales
        ((= R1 R2) (aux2bCEj X (random lenDeck) (random lenDeck) lenDeck deck))
    ;;retorna una lista con los jugadores y el diler
    (else (cons (list (cartasAleatorias deck R1 0) (cartasAleatorias deck R2 0))
                              (aux2bCEj (- X 1) (random (- lenDeck 2)) (random (- lenDeck 2)) (- lenDeck 2) (modificarDeck R1 R2 0 0 deck))))))


;;Funcion para modificar el deck eliminando 2 cartas y que no se repitan las cartas
(define (modificarDeck pos pos2 cont cont2 deck)
  (cond ((null? deck) '())
        ((and (= pos cont) (= pos2 cont2)) (cdr deck))
        ((= pos cont) (modificarDeck pos pos2 (+ cont 1) (+ cont2 1) (cdr deck)))
        ((= pos2 cont) (modificarDeck pos pos2 (+ cont 1) (+ cont2 1) (cdr deck)))
        (else (cons (car deck) (modificarDeck pos pos2 (+ cont 1) (+ cont2 1) (cdr deck))))))


;;Funcion para modificar el deck eliminando 1 carta
(define (modificarDeck2 pos cont deck)
  (cond ((null? deck) '())
        ((= pos cont) (cdr deck))
        (else (cons (car deck) (modificarDeck2 pos (+ cont 1) (cdr deck))))))


;;Función Carta Aleatoria
(define (cartasAleatorias deck numRandom contRandom)
  (cond
    ((null? deck) '())
    ((= numRandom contRandom)(car deck)) 
    (else (cartasAleatorias (cdr deck) numRandom (+ 1 contRandom)))))


;;Funcion len de una Lista
(define (lenList lista)
  (length lista))

;;Funcion Recorrer Deck Para saber si quieren cartas
(define (recorrerDeck jugador deck funcion)
  (auxRecorrerDeck jugador (random (- (lenList (car (cddddr deck))) 1)) deck funcion))

        
;;Funcion auxiliar Recorrer Deck Para saber si quieren cartas
(define (auxRecorrerDeck jugador randomC deck funcion)
  (cond ((null? (cdddr deck)) "Ya no hay cartas")
        ((= jugador 1) (if (equal? funcion "Solicitar Carta")
                           (cons (PedirCarta 1 randomC deck) (cdr deck))
                           (#f)))
        ((= jugador 2) (if (equal? funcion "Solicitar Carta")
                           (append (list(car deck)) (list(PedirCarta 2 randomC deck)) (cddr deck))
                           (#f)))
        ((= jugador 3) (if (equal? funcion "Solicitar Carta")
                           (append (list(car deck)) (list(cadr deck)) (list(PedirCarta 3 randomC deck)) (cdddr deck))
                           (#f)))
        ((= jugador 4) (if (equal? funcion "Solicitar Carta")
                           (append (list(car deck)) (list(cadr deck)) (list(caddr deck)) (list(PedirCarta 4 randomC deck)) (cddddr deck))
                           (#f)))
        
        (else deck)))
        

;;Solicitar carta a jugador
(define (soliCarta j deck)
  (cond ((= j 1)(print 'Jugador_1_Solicita_Carta?: )
                (read ))
        ((and (= j 2) (not (null? (cadr deck))))(print 'Jugador_2_Solicita_Carta?: )
                (read ))
        ((and (= j 3) (not (null? (caddr deck))))(print 'Jugador_3_Solicita_Carta?: )
                (read ))
        ((and (= j 4) (not (null? (cadddr deck))))(print 'Crupier_Solicita_Carta?: )
                (read ))
        (else '())))


;;Funcion para solicitar una carta
(define (PedirCarta Jugador randomC deck)
  (aux1PedirCarta Jugador randomC  deck))


;;Funcion auxiliar 1 para solicitar una carta verifica la lista del jugador antes de agregar carta
(define (aux1PedirCarta Jugador numRandom deck)
  (cond ((null? deck) '())
        ((>= numRandom (lenList (car (cddddr deck)))) "Error el random sobrepasa el limite")
        ((> Jugador 5) "Error El Jugador No Existe")
        ((= Jugador 1) (if (equal? (verificar21 (car deck) 1) #t) 
                           (aux2PedirCarta Jugador numRandom deck)
                           (car deck)))
        
        ((= Jugador 2) (if (equal? (verificar21 (car (cdr deck)) 2) #t) 
                           (aux2PedirCarta Jugador numRandom deck)
                           (car (cdr deck))))
        
        ((= Jugador 3) (if (equal? (verificar21 (car (cddr deck)) 3) #t) 
                           (aux2PedirCarta Jugador numRandom deck)
                           (car (cddr deck))))
    
        ((= Jugador 4) (if (equal? (verificar21 (car (cdddr deck)) 4) #t) 
                           (aux2PedirCarta Jugador numRandom deck)
                           (car (cdddr deck))))
        (else "Error")))


;;Funcion auxiliar 2 para solicitar una carta para un jugador con carta agregada X
(define (aux2PedirCarta Jugador numRandom deck)
  (cond ((null? deck) '())
        ((>= numRandom (lenList (car (cddddr deck)))) "Error el random sobrepasa el limite")
        ((> Jugador 5) "Error El Jugador No Existe")
        ((= Jugador 1) (if (equal? (verificar21 (append (car deck) (list (cartasAleatorias (car (cddddr deck)) numRandom 0))) 1) #t) 
                           (append (list (append (car deck) (list (cartasAleatorias (car (cddddr deck)) numRandom 0))))
                                   (list (cadr deck) (caddr deck) (cadddr deck) (modificarDeck2 numRandom 0 (car (cddddr deck)))))
                           (append (car deck) (list (cartasAleatorias (car (cddddr deck)) numRandom 0)))))
                       
        ((= Jugador 2) (if (equal? (verificar21 (append (car (cdr deck)) (list (cartasAleatorias (car (cddddr deck)) numRandom 0))) 2) #t) 
                           (append (list (car deck))
                                   (list (append (car (cdr deck)) (list (cartasAleatorias (car (cddddr deck)) numRandom 0))))
                                   (list (caddr deck) (cadddr deck) (modificarDeck2 numRandom 0 (car (cddddr deck)))))
                           (append (car (cdr deck)) (list (cartasAleatorias (car (cddddr deck)) numRandom 0)))))        
        
        ((= Jugador 3) (if (equal? (verificar21 (append (car (cddr deck)) (list (cartasAleatorias (car (cddddr deck)) numRandom 0))) 3) #t) 
                           (append (list (car deck) (cadr deck))
                                   (list (append (car (cddr deck)) (list (cartasAleatorias (car (cddddr deck)) numRandom 0))))
                                   (list (cadddr deck) (modificarDeck2 numRandom 0 (car (cddddr deck)))))
                           (append (car (cddr deck)) (list (cartasAleatorias (car (cddddr deck)) numRandom 0)))))
    
        ((= Jugador 4) (if (equal? (verificar21 (append (car (cdddr deck)) (list (cartasAleatorias (car (cddddr deck)) numRandom 0))) 4) #t) 
                           (recorrerDeck 4 (append (list (car deck) (cadr deck) (caddr deck))
                                                   (list (append (car (cdddr deck)) (list (cartasAleatorias (car (cddddr deck)) numRandom 0))))
                                                   (list (modificarDeck2 numRandom 0 (car (cddddr deck))))) "Solicitar Carta")
                           (append (car (cdddr deck)) (list (cartasAleatorias (car (cddddr deck)) numRandom 0)))))
        (else "Error")))


;;Funcion Para realizar una comparacion entre el Deck del GUI con Deck de la logica
(define (comparacion carta deckR deckGUI)
  (auxComparacion carta 0 Deck deckGUI))


;;Funcion auxiliar para realizar una comparacion
(define (auxComparacion carta cont deck deckGUI)
  (cond ((null? deck) '())
        ((equal? carta (car deck)) (FuncionDeckGUI cont deckGUI))
        (else (auxComparacion carta (+ 1 cont) (cdr deck) deckGUI))))


;;Funcion que grafica la carta del deck
(define (FuncionDeckGUI carta deckGUI)
  (cond ((= carta 0) (car deckGUI))
        (else (FuncionDeckGUI (- carta 1) (cdr deckGUI)))))


;;Funcion que verifica que la suma sea 21 o menor
(define (verificar21 listaJugador J)
  (auxVerificar21 listaJugador J 0))


(define (auxVerificar21 listaJugador J suma)
  (cond ((null? listaJugador) (verificarSuma J suma))
        ;;Compara si la carta es un A para sumar 1 o 11
        ((equal? (substring (symbol->string  (car listaJugador)) 0 1) "A") (auxVerificar21 (cdr listaJugador) J (+ suma 11)))
        ;;Compara si la carta es un K para sumar 10
        ((equal? (substring (symbol->string  (car listaJugador)) 0 1) "K") (auxVerificar21 (cdr listaJugador) J (+ suma 10)))
        ;;Compara si la carta es un Q para sumar 10
        ((equal? (substring (symbol->string  (car listaJugador)) 0 1) "Q") (auxVerificar21 (cdr listaJugador) J (+ suma 10)))
        ;;Compara si la carta es un J para sumar 10
        ((equal? (substring (symbol->string  (car listaJugador)) 0 1) "J") (auxVerificar21 (cdr listaJugador) J (+ suma 10)))
        (else (auxVerificar21 (cdr listaJugador) J (+ suma (string->number (substring (symbol->string  (car listaJugador)) 0 2)))))))


;;Verifica si la suma es 21 para declarar BlackJack o Fail o #t
(define (verificarSuma J suma)
  (cond ((= J 4) (if (>= suma 17) "Crupier" #t))
        ((= suma 21) "BlackJack")
        ((> suma 21) "Fail")
        (else #t)))

;;Funcion que muestra los resultados
(define (resultados deck)
  (list (string-append "Jugador 1 con un total de puntos de: " (number->string(auxResultados (car deck)))) (string-append "Jugador 2 con un total de puntos de: " (number->string(auxResultados (cadr deck))))
        (string-append "Jugador 3 con un total de puntos de: " (number->string(auxResultados (caddr deck)))) (string-append "Crupier con un total de puntos de: " (number->string(auxResultados (cadddr deck))))))

;;Axiliar resultados
(define (auxResultados listaJugador)
  (cond ((null? listaJugador) 0)
        ;;Compara si la carta es un A para sumar 1 o 11
        ((equal? (substring (symbol->string  (car listaJugador)) 0 1) "A") (+ 11(auxResultados (cdr listaJugador))))
        ;;Compara si la carta es un K para sumar 10
        ((equal? (substring (symbol->string  (car listaJugador)) 0 1) "K") (+ 10 (auxResultados (cdr listaJugador))))
        ;;Compara si la carta es un Q para sumar 10
        ((equal? (substring (symbol->string  (car listaJugador)) 0 1) "Q") (+ 10 (auxResultados (cdr listaJugador))))
        ;;Compara si la carta es un J para sumar 10
        ((equal? (substring (symbol->string  (car listaJugador)) 0 1) "J") (+ 10 (auxResultados (cdr listaJugador))))
        (else (+ (string->number (substring (symbol->string  (car listaJugador)) 0 2)) (auxResultados (cdr listaJugador))))))



;;Funcion retorna las Cartas
(define (retornarCartas Jugador deck deckGUI)
  (cond ((null? deck) "Ya no hay cartas")
        ((= Jugador 1) (graficarCartas (car deck) (car(cddddr deck)) deckGUI))
        ((and (= Jugador 2) (not (null? (cadr deck)))) (graficarCartas (cadr deck) (car(cddddr deck)) deckGUI))
        ((and (= Jugador 3) (not (null? (caddr deck)))) (graficarCartas (caddr deck) (car(cddddr deck)) deckGUI))
        ((and (= Jugador 4) (not (null? (cadddr deck)))) (graficarCartas (cadddr deck) (car(cddddr deck)) deckGUI))
        (else '())))


;;Funcion graficar las Cartas
(define (graficarCartas lista deckR deckGUI)
  (cond ((null? lista) '())
        (else (append (list (comparacion (car lista) deckR deckGUI)) (graficarCartas (cdr lista) deckR deckGUI)))))
  

;(verificar21 '(10h 02s))
;(string->number (substring (symbol->string  '9h) 0 1))
;(comparacion 'Ac)
;(PedirCarta 1 (bCEj  '("J" "K" "L")) '())
;(bCEj '("J" "k"))

