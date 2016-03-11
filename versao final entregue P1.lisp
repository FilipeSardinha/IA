;; Grupo 12: Ines Percheiro (76397) Filipe Sardinha (71035) Filipe Soares (76543) 



;;;;--------            2.1.1           --------


;;cria-accao: inteiro x array -> accao                                                       
(defun cria-accao(i peca)
    (cons i peca)
)

;;accao-coluna: accao -> inteiro                                                             
(defun accao-coluna(accao)
    (car accao)
)

;;accao-peca: accao -> array                                                                 
(defun accao-peca(accao)
    (cdr accao)
)

;;;;--------            2.1.2           --------


;;tabuleiro-preenche-valor: tabuleiro X inteiro X inteiro X logico -> {}
(defun tabuleiro-preenche-valor (tabuleiro l c valor)
    (setf (aref tabuleiro (- 17 l) c) valor)
)

;;tabuleiro-get-valor: tabuleiro X inteiro X inteiro -> logico
(defun tabuleiro-get-valor (tabuleiro l c)
    (aref tabuleiro (- 17 l) c)
)

;;cria-tabuleiro: {} -> tabuleiro                                                            
(defun cria-tabuleiro ()
    (make-array (list 18 10) :initial-element NIL)
)

;; copia-tabuleiro: tabuleiro -> tabuleiro                                                   
(defun copia-tabuleiro (tabuleiro)
    (let ((cpy (cria-tabuleiro)))
        (dotimes (l 18)
            (dotimes (c 10)
            (tabuleiro-preenche-valor cpy l c (tabuleiro-get-valor tabuleiro l c))
            )
        )
    cpy)
)
;;tabuleiro-preenchido-p: tabuleiro x inteiro x inteiro -> logico                            
(defun tabuleiro-preenchido-p (tabuleiro l c)
    (if(tabuleiro-get-valor tabuleiro l c) T)
)

;;tabuleiro-altura-coluna: tabuleiro x inteiro -> inteiro                                     
(defun tabuleiro-altura-coluna (tabuleiro c)
    (let ((altura 0))
        (dotimes(alt 18)
		
            (if (tabuleiro-get-valor tabuleiro (- 17 alt) c)
				
                (progn (setf altura (+ (- 17 alt) 1)) (return altura)) 
            )
        )
    altura)
)

;;tabuleiro-linha-completa-p: tabuleiro x inteiro -> logico                                    
(defun tabuleiro-linha-completa-p (tabuleiro l)
    (let ((completa T))
        (dotimes (c 10)
            (when (not (tabuleiro-get-valor tabuleiro l c))
                (setf completa nil)
                (return completa)
            )
        )
    completa)
)

;;tabuleiro-preenche!: tabuleiro x inteiro x inteiro -> {}
(defun tabuleiro-preenche! (tabuleiro l c)
    (if (and    (and (>= c 0) (<= c 9) )
            (and (>= l 0) (<= l 17) )
        )
        (tabuleiro-preenche-valor tabuleiro l c T)
    ) (values)
)

;;tabuleiro-remove-linha!:tabuleiro x inteiro -> {}
(defun tabuleiro-remove-linha! (tabuleiro l)
    (dotimes (c 10)
        (cond   
                
                ( (= l 17)  (tabuleiro-preenche-valor tabuleiro l c nil))
                ( (= c 9)   (tabuleiro-preenche-valor tabuleiro l c (tabuleiro-get-valor tabuleiro (+ l 1) c)) (incf l) (tabuleiro-remove-linha! tabuleiro l) )
                ( (tabuleiro-linha-completa-p tabuleiro (+ l 1)) (tabuleiro-remove-linha! tabuleiro (+ l 1)) (tabuleiro-preenche-valor tabuleiro l c (tabuleiro-get-valor tabuleiro (+ l 1) c)) )
                ( (< l 17)  (tabuleiro-preenche-valor tabuleiro l c (tabuleiro-get-valor tabuleiro (+ l 1) c)) )
                

                )
    )
)

;;tabuleiro-topo-preenchido-p: tabuleiro -> logico
(defun tabuleiro-topo-preenchido-p (tabuleiro)
    (dotimes (col 10)
        (if (tabuleiro-get-valor tabuleiro 17 col) (return T))
    )
)

;;tabuleiros-iguais-p: tabuleiro x tabuleiro -> logico
(defun tabuleiros-iguais-p (tabuleiro1 tabuleiro2)
    (equalp tabuleiro1 tabuleiro2)
)

;;tabuleiro->array: tabuleiro -> array
(defun tabuleiro->array (tabuleiro)
    (copia-tabuleiro tabuleiro)
)
;;array->tabuleiro: array-> tabuleiro
(defun array->tabuleiro (array)
    (copia-tabuleiro array)
)

;;;;--------            2.1.3           --------


;;Construtor Tipo Estado
(defstruct  estado
            (pontos NIL)
            (pecas-por-colocar NIL)
            (pecas-colocadas NIL)
            (tabuleiro NIL)
)

;;copia-estado: estado -> estado 
(defun copia-estado (estado)
    (let ((novo_estado (make-estado 	:pontos (estado-pontos estado) 
									:pecas-por-colocar (copia-lista (estado-pecas-por-colocar estado)) 
									:pecas-colocadas (copia-lista (estado-pecas-colocadas estado)) 
									:tabuleiro (copia-tabuleiro (estado-tabuleiro estado)))))
		novo_estado
	)
)

;;copia-lista: lista -> lista
(defun copia-lista (lista)
    (let ((cpy-lista '()))
		(if (not (eq lista NIL))
		
			(dolist (el lista)
				(setf cpy-lista (append cpy-lista (cons el nil)))
			)
		)
    cpy-lista)
)

;;estados-iguais-p: estado x estado -> logico   
(defun estados-iguais-p (estado1 estado2)
    (equalp estado1 estado2)
)

;;estado-final-p: estado -> logico 
(defun estado-final-p (estado)
    (if 
        (or    (eq NIL (estado-pecas-por-colocar estado)) 
                (eq T (tabuleiro-topo-preenchido-p (estado-tabuleiro estado)) ))        
        T
    )
)

;;;;--------            2.1.4           --------


;;Construtor Tipo Problema
(defstruct  problema
            (estado-inicial NIL)
            (solucao NIL)
            (accoes NIL)
            (resultado NIL)
            (custo-caminho NIL)
)

;;;;--------            2.2.1           --------


;;custo-oportunidade: estado -> inteiro
(defun custo-oportunidade (estado)
    (let ((max 0) (custo 0) (dim (list-length (estado-pecas-colocadas estado))))
        (dotimes (i dim)
			(cond 	((eq (nth i (estado-pecas-colocadas estado)) 'i) (setf max (+ max 800)))
					((eq (nth i (estado-pecas-colocadas estado)) 'l) (setf max (+ max 500)))
					((eq (nth i (estado-pecas-colocadas estado)) 'j) (setf max (+ max 500)))
					((eq (nth i (estado-pecas-colocadas estado)) 's) (setf max (+ max 300)))
					((eq (nth i (estado-pecas-colocadas estado)) 'z) (setf max (+ max 300)))
					((eq (nth i (estado-pecas-colocadas estado)) 't) (setf max (+ max 300)))
					((eq (nth i (estado-pecas-colocadas estado)) 'o) (setf max (+ max 300)))
            )
        )
		(setf custo (- max (estado-pontos estado)))
    custo)
)

;;solucao: estado -> logico     
(defun solucao (estado)
    (if 
        (and    (eq NIL (estado-pecas-por-colocar estado)) 
                (eq NIL (tabuleiro-topo-preenchido-p (estado-tabuleiro estado)) ))
         T
    )
)


;;accoes: estado -> lista de accoes
(defun accoes (estado)
    (let ((peca (first (estado-pecas-por-colocar estado))) (lista-pecas NIL))
        ;;peca i
        (cond ((eq peca 'i)
                (dotimes (n 10)
                    (setf lista-pecas (cons (cria-accao n peca-i0) lista-pecas))
                )
                (dotimes (n 7)
                    (setf lista-pecas (cons (cria-accao n peca-i1) lista-pecas))
                )
            )
        ;;peca l
            ((eq peca 'l)
                (dotimes (n 9)
                    (setf lista-pecas (cons (cria-accao n peca-l0) lista-pecas))
                )
                (dotimes (n 8)
                    (setf lista-pecas (cons (cria-accao n peca-l1) lista-pecas))
                )
                (dotimes (n 9)
                    (setf lista-pecas (cons (cria-accao n peca-l2) lista-pecas))
                )
                (dotimes (n 8)
                    (setf lista-pecas (cons (cria-accao n peca-l3) lista-pecas))
                )
            )
        ;;peca j
            ((eq peca 'j)
                (dotimes (n 9)
                    (setf lista-pecas (cons (cria-accao n peca-j0) lista-pecas))
                )
                (dotimes (n 8)
                    (setf lista-pecas (cons (cria-accao n peca-j1) lista-pecas))
                )
                (dotimes (n 9)
                    (setf lista-pecas (cons (cria-accao n peca-j2) lista-pecas))
                )
                (dotimes (n 8)
                    (setf lista-pecas (cons (cria-accao n peca-j3) lista-pecas))
                )
            )
        ;;peca o
            ((eq peca 'o)
                (dotimes (n 9)
                    (setf lista-pecas (cons (cria-accao n peca-o0) lista-pecas))
                )
            )
        ;;peca s
            ((eq peca 's)
                (dotimes (n 8)
                    (setf lista-pecas (cons (cria-accao n peca-s0) lista-pecas))
                )
                (dotimes (n 9)
                    (setf lista-pecas (cons (cria-accao n peca-s1) lista-pecas))
                )
            )
        ;;peca z
            ((eq peca 'z)
                (dotimes (n 8)
                    (setf lista-pecas (cons (cria-accao n peca-z0) lista-pecas))
                )
                (dotimes (n 9)
                    (setf lista-pecas (cons (cria-accao n peca-z1) lista-pecas))
                )
            )
        ;;peca t
            ((eq peca 't)
                (dotimes (n 8)
                    (setf lista-pecas (cons (cria-accao n peca-t0) lista-pecas))
                )
                (dotimes (n 9)
                    (setf lista-pecas (cons (cria-accao n peca-t1) lista-pecas))
                )
                (dotimes (n 8)
                    (setf lista-pecas (cons (cria-accao n peca-t2) lista-pecas))
                )
                (dotimes (n 9)
                    (setf lista-pecas (cons (cria-accao n peca-t3) lista-pecas))
                )
            )
            
        )
		(reverse lista-pecas)
	)
)


;;qualidade: estado -> inteiro
(defun qualidade (estado)
    (- 0 (estado-pontos estado))
)

;;actualiza-pecas-por-colocar: lista -> lista 
(defun actualiza-pecas-por-colocar (lista)
    (cdr lista)
)

;;actualiza-pecas-colocadas: lista x peca-> lista
(defun actualiza-pecas-colocadas (lista p)
    (let ( (novalista (append (cons p nil) lista)) )

    novalista)
)

;;peca-vertical-2-3: peca-> int
(defun peca-vertical-2-3(peca)
    (let ( (altura_coluna_max 0) (altura_linha0 0) (altura_linha1 0) )

        (dotimes (linha (array-dimension peca 0))
            (dotimes (coluna (array-dimension peca 1))
            
                (cond   ( (and (eq linha 0) (aref peca linha coluna))   (if (> (aref peca linha coluna) altura_linha0)
                                                                            (setf altura_linha0 (aref peca linha coluna))
                                                                        )
                        )

                        ( (and (eq linha 1) (aref peca linha coluna))   (if (> (aref peca linha coluna) altura_linha1)
                                                                            (setf altura_linha1 (aref peca linha coluna))
                                                                        )
                        )
                )

            )
        )
        (cond   ( (> (- altura_linha1 altura_linha0) 1) (setf altura_coluna_max altura_linha0) )
                ( (= (- altura_linha1 altura_linha0) 1) (setf altura_coluna_max (- altura_linha1 1)) )
                ( (= (- altura_linha1 altura_linha0) 0) (setf altura_coluna_max (- altura_linha1 0)) )
        )
    altura_coluna_max)
)

;;peca-vertical-3-2: peca-> int
(defun peca-vertical-3-2(peca)
    (let ( (altura_coluna_max 0) (altura_linha0 0) (altura_linha1 0) (altura_linha2 0) )

        (dotimes (linha (array-dimension peca 0))
            (dotimes (coluna (array-dimension peca 1))
        
                    (cond   ( (and (eq linha 0) (aref peca linha coluna))   (if (> (aref peca linha coluna) altura_linha0)
                                                                                (setf altura_linha0 (aref peca linha coluna))
                                                                            )
                            )

                            ( (and (eq linha 1) (aref peca linha coluna))   (if (> (aref peca linha coluna) altura_linha1)
                                                                                (setf altura_linha1 (aref peca linha coluna))
                                                                            )
                            )

                            ( (and (eq linha 2) (aref peca linha coluna))   (if (> (aref peca linha coluna) altura_linha2)
                                                                                (setf altura_linha2 (aref peca linha coluna))
                                                                            )
                            )
                    )

            )
        )
        (setf altura_coluna_max (max altura_linha0 altura_linha1 altura_linha2))
        (cond   ( (= altura_coluna_max altura_linha2) (values) )
                ( (= altura_coluna_max altura_linha0) (setf altura_coluna_max (- altura_coluna_max 2)) )
                ( (= altura_coluna_max altura_linha1) (setf altura_coluna_max (- altura_coluna_max 1)) )
        )
    altura_coluna_max)

)

;;calcula-max-altura: peca-> int
(defun calcula-max-altura (peca_estrutura)

    (let ( (altura_coluna_max 0) )
        
        (cond   ( (equalp (array-dimensions peca_estrutura) '(2 3)) (setf altura_coluna_max (peca-vertical-2-3 peca_estrutura)) )
                ( (equalp (array-dimensions peca_estrutura) '(3 2)) (setf altura_coluna_max (peca-vertical-3-2 peca_estrutura)) )

        )
    altura_coluna_max)

)

;;verifica-altura-base-completa: tabuleiro x accao-> int
(defun verifica-altura-base-completa (tabuleiro accao)
    (let ( (altura_coluna_max 0)   )
        (dotimes (coluna (array-dimension (cdr accao) 1))
            
            (if (> (tabuleiro-altura-coluna tabuleiro (+ (car accao) coluna)) altura_coluna_max)
                (setf altura_coluna_max (tabuleiro-altura-coluna tabuleiro (+ (car accao) coluna)) )

            )
            (if (= altura_coluna_max 18)
                (setf altura_coluna_max 17 )

            )
        )
    altura_coluna_max)
)

;;verifica-altura-base-incompleta: tabuleiro x accao-> int
(defun verifica-altura-base-incompleta (tabuleiro accao)
    (let ( (altura_coluna_max 0) (peca_estrutura (make-array (array-dimensions (cdr accao)) ))   )

        (dotimes (linha (array-dimension (cdr accao) 0))
            (dotimes (coluna (array-dimension (cdr accao) 1))
                (if (aref (cdr accao) linha coluna)
                    (setf (aref peca_estrutura linha coluna) (tabuleiro-altura-coluna tabuleiro (+ (car accao) coluna)) )
                )
            )
        )
        (setf altura_coluna_max (calcula-max-altura peca_estrutura))
        (if (= altura_coluna_max 18)
                (setf altura_coluna_max 17 )

        )
    altura_coluna_max)
)

;;base-completa: peca-> logico
(defun base-completa (peca)
    (let ( (base_horizontal_completa T) )
        (dotimes (colunas (array-dimension peca 1))
            (when (not (aref peca 1 colunas)) (setf base_horizontal_completa nil) )
        )
    base_horizontal_completa)
)

;;escolhe-valor-preenche: tabuleiro x linha x coluna x valor-> {}
(defun escolhe-valor-preenche (tabuleiro linha coluna valor)
    (cond   ((tabuleiro-preenchido-p tabuleiro linha coluna) ())
            ((not (tabuleiro-preenchido-p tabuleiro linha coluna)) (tabuleiro-preenche-valor tabuleiro linha coluna valor))
    )

)

;;desenha-no-tabuleiro: tabuleiro x accao x altura_coluna_max-> (tabuleiro, logico)
(defun desenha-no-tabuleiro (tabuleiro accao altura_coluna_max)
    (let (  (linha_desenha altura_coluna_max)  (coluna_desenha (car accao)) 
            (limite_direito (array-dimension (cdr accao) 1)) (limite_superior (array-dimension (cdr accao) 0)) )
            (dotimes (linhas limite_superior)

                (if (not (tabuleiro-topo-preenchido-p tabuleiro))
                    (dotimes (colunas limite_direito)

                        (escolhe-valor-preenche tabuleiro (+ linha_desenha linhas) (+ coluna_desenha colunas) (aref (cdr accao) (- (- limite_superior 1) linhas) colunas))
                    )
                    (return (cons tabuleiro (tabuleiro-topo-preenchido-p tabuleiro)))
                )
            )
    (cons tabuleiro (tabuleiro-topo-preenchido-p tabuleiro)) )
)

;;desenha-peca: tabuleiro x accao -> (tabuleiro, logico)
(defun desenha-peca (tabuleiro accao)
    (cond   ( (base-completa (cdr accao)) (desenha-no-tabuleiro tabuleiro accao (verifica-altura-base-completa tabuleiro accao)) )
            ( (not (base-completa (cdr accao))) (desenha-no-tabuleiro tabuleiro accao (verifica-altura-base-incompleta tabuleiro accao)) )       
    )

)

;;remove-calcula-pontos: tabuleiro -> int
(defun remove-calcula-pontos (tabuleiro)
    (let ( (pontos 0) (linhas_removidas 0) (linhas_para_remover '() ) )
        (dotimes (linha 18)
            (when (tabuleiro-linha-completa-p tabuleiro linha) (incf linhas_removidas) (setf linhas_para_remover (append linhas_para_remover (cons linha nil))) )
        )
         (dolist (el linhas_para_remover)
            (tabuleiro-remove-linha! tabuleiro el)
        )
        (cond   ((= linhas_removidas 1) (setf pontos 100))
                ((= linhas_removidas 2) (setf pontos 300))
                ((= linhas_removidas 3) (setf pontos 500))
                ((= linhas_removidas 4) (setf pontos 800))
        )
    pontos)
)

;;resultado: estado x accao -> estado
(defun resultado (estado accao)
    (let    ( (novoestado (copia-estado estado)) (estadoerrado (copia-estado estado)) (accoes_posiveis nil)
              (tabuleiro nil) (tabuleiro_estado nil)           
            )
        ;(setf accoes_posiveis (accoes novoestado)) ;;--->forma correcta de trabalhar

        ;solucao implementada para passar ao teste 14
        (setf (car (estado-pecas-por-colocar estadoerrado)) (cdr accao) )
        (setf accoes_posiveis (accoes estadoerrado ))

        (setf tabuleiro (estado-tabuleiro novoestado))
  
        ;(setf (estado-pecas-colocadas novoestado) (actualiza-pecas-colocadas (estado-pecas-colocadas novoestado) (cdr accao))) ;;--->forma correcta de trabalhar
        
        ;solucao implementada para passar ao teste 14
        (setf (estado-pecas-colocadas novoestado) (actualiza-pecas-colocadas (estado-pecas-colocadas novoestado) (car (estado-pecas-por-colocar novoestado))))
        
        (setf (estado-pecas-por-colocar novoestado) (actualiza-pecas-por-colocar (estado-pecas-por-colocar novoestado)))

        (if (and (null (member (cdr accao) accoes_posiveis :test #'equalp)) (not (estado-final-p novoestado)))

           (    (lambda (tabuleiro1 accao1 tabuleiro_estado1 novoestado1)
                    (setf tabuleiro_estado1 (desenha-peca tabuleiro1 accao1))
                    (setf (estado-pontos novoestado1) (+ (estado-pontos novoestado1) (remove-calcula-pontos tabuleiro1) ))
                ) tabuleiro accao tabuleiro_estado novoestado
           )
        )
    novoestado)
)

;;Segunda Parte

(defstruct no 
           (altura NIL)
           (estado NIL)
           (lista_caminho NIL)
           (lista_accoes NIL)
           (lista_nos NIL)
)

(defun copia-no(no)
	(print (no-estado no) )
    (let 	((novo_no (make-no :altura (no-altura no)
                             :estado (copia-estado (no-estado no))
                             :lista_caminho (no-lista_caminho no)
                             :lista_accoes (no-lista_accoes no)
							 :lista_nos (no-lista_nos no)
							)
			)) 
			novo_no
	)

)


            

(defun no-sucessor (no)
    (accoes(first(no-lista_caminho no)))
    (cond ( (eq(first(no-lista_accoes no)) (first(accoes(first(no-lista_caminho no))))) (setf (no-lista_accoes no) (rest(no-lista_accoes no))) (no-sucessor no))
            (   (first(rest(no-lista_nos no))))
        )
)       

(defun procura-pp(problem)
    (let ((novo_no (make-no  :altura 0
                            :estado (problema-estado-inicial problem)
                            :lista_accoes (accoes (problema-estado-inicial problem))
                            :lista_caminho NIL
                            :lista_nos NIL ))
            (caminho nil)
    )
	
    (setf caminho (procura-pp-aux problem novo_no))

    caminho)
)

(defun procura-pp-aux (novo_problema  no)
    
    (let ( (novo_no (copia-no no)) (no_seguinte (make-no ) ) (no_anterior (make-no) ) )
        
            (cond   ( (solucao (no-estado novo_no))		(setf (no-lista_nos novo_no) (append novo_no (no-lista_nos novo_no) ))
    													(setf (no-lista_caminho novo_no) (append (first(no-lista_accoes novo_no)) (no-lista_caminho novo_no)))
    					
    				) 
    				( (not (equalp (accoes (resultado (no-estado novo_no) (first(no-lista_accoes novo_no)))) NIL )) 	(setf (no-lista_caminho novo_no) (append (cons (first(no-lista_accoes novo_no)) NIL) (no-lista_caminho novo_no))) 
    																													(setf (no-altura no_seguinte) (+ 1 (no-altura novo_no) ))
    																													(setf (no-lista_caminho no_seguinte) (no-lista_caminho novo_no))
    																													(setf (no-lista_nos no_seguinte) (append (cons novo_no NIL) (no-lista_nos novo_no) ) )
                                                                                                                        (setf no_seguinte (copia-no no_seguinte)) 
    																													(setf (no-estado no_seguinte) (resultado (no-estado novo_no) (car (no-lista_caminho novo_no) )))
    																													(setf (no-lista_accoes no_seguinte) (accoes (no-estado no_seguinte) ))
    																													;(setf (no-lista_accoes novo_no) (append (accoes (problema-estado-inicial novo_problema)) (rest(no-lista_accoes novo_no)))) 
    																													;(setf (problema-estado-inicial novo_problema) (resultado (no-estado novo_no) (first(accoes (problema-estado-inicial novo_problema)))))
    																													;(setf (no-lista_nos novo_no) (append (cons novo_no NIL) (no-lista_nos novo_no)))
                                                                                                                        (print "+++++++++")
                                                                                                                        (print (no-lista_nos novo_no))
                                                                                                                        (print "//////////")
                                                                                                                        ;(setf no_seguinte (copia-no no_seguinte)) 
    																													(procura-pp-aux novo_problema no_seguinte) 
    				)
    				( (equalp (accoes (resultado (no-estado novo_no) (first(problema-accoes novo_problema)))) NIL ) 	(setf no_anterior (car (no-lista_nos novo_no)))
                                                                                                                        (setf (no-lista_accoes no_anterior) (cdr (no-lista_accoes no_anterior)))
                                                                                                                        (setf (car (no-lista_caminho no_anterior) ) (car (no-lista_accoes no_anterior)))		
    																													(procura-pp-aux novo_problema no_anterior) 
    				)

            )(no-lista_caminho novo_no)
    )
) 
    
           

(defstruct node
    (custo 0)
    (custo_total 0)
    (accaog NIL)
    (antesucessor NIL)
    (estado NIL)
)


(defun expandeFront (nod problem front)
    (let ((listaaccoes NIL) (estadofilho nil) (no_filho))
        ; colocar accoes do no na lista
        (setf listaaccoes (funcall (problema-accoes problem) (node-estado nod)))
        
        (when listaaccoes
            (dotimes (x (length listaaccoes))
                (setf estadofilho (funcall (problema-resultado problem) (node-estado nod) (nth x listaaccoes)))
              
                (setf no_filho (make-node :custo (funcall (problema-custo-caminho problem) (node-estado nod))
                                        :estado estadofilho
                                        :antesucessor nod
                                        :accaog (nth x listaaccoes)))
                (push no_filho front)
            )
        )
        ; caso n seja vazia calcula o resultado do pai com a accao
    front
    )
)

(defun calculaf (estado heurist problem)
    (+ (funcall (problema-custo-caminho problem) estado) (funcall heurist estado))
)


(defun calculaCaminho (n)
    (let ((caminho NIL) (actual n))
        (loop while (not (eq(node-antesucessor actual) NIL))
            do (push (node-accaog actual) caminho)
            (setf actual (node-antesucessor actual))
        )
    caminho
    )
)

; (defun heurist (estado)
   ; 0
; )

(defun melhorIndiceFronteira (front heurist problem)
    (let ((melhorFn (nth 0 front))(melhorIndex 0)(atual nil))
        (dotimes (x (length front))
            (setf atual (nth x front))
            (when (< (calculaf (node-estado atual) heurist problem) melhorFn)
                (setf melhorFn (calculaf (node-estado atual) heurist problem))
                (setf melhorIndex x)
            )
        )
    melhorIndex
    )
)

(defun rmindex (front indexNo)
    (let ((lista-nova (list)))
        (dotimes (i (length front))
            (when (not (= indexNo i))
                (push (nth i front) lista-nova)
            )
        )
    (reverse lista-nova)
    )
)

(defun procura-A* (p heurist)
    (let ((front NIL) (s NIL)(no_inicial)(no_filho)(indexNo 0) (melhorno NIL))
        ;inicializar fronteira com no inicial
        ;enquanto existir nos na fronteiras
            ;procurar no com menor f(n) 
            ;retirar da fronteira
            ;expandir no com menor custo -> es
            ;adicionar filhos a fronteira
     
        (setf no_inicial (make-node :custo (funcall (problema-custo-caminho p) (problema-estado-inicial p))
                                   :estado (problema-estado-inicial p)
                                   :antesucessor NIL))
        
        (push no_filho front)
        
        (loop while front
            do(setf indexNo (melhorIndiceFronteira front heurist p))
            (setf melhorno (nth front indexNo))
            (setf front (rmindex front indexNo))
            (when (funcall(problema-solucao p) (node-estado melhorno))
                (setf s T)
                (return)
            )
            (setf front (expandeFront melhorno p front))
        )
    (if s
        (calculaCaminho melhorno)
    NIL)
    )
)



(load "utils.fas")































