;; jeu de toc
;; désolé les francophobes mais ici on joue

;;; =======================================
;;; SECTION VT102
;;; =======================================

;;; calcule le code d'effacement de l'ecran
(define (vt102-efface-ecran)
   "\x1b;[2J")

;;; calcule le code d'effacement de l'ecran
(define (vt102-efface-fin)
   "\x1b;[J")

;;; calcule lr code d'effacement de l'ecran
(define (vt102-efface-ligne)
   "\x1b;[2K")

;;; calcule le code vt102 de positionnement en ligne colonne
(define (vt102-va ligne colonne)
   (string-append
      "\x1b;["
      (number->string ligne)
      ";"
      (number->string colonne)
      "H"))

;;; calcule le code de gestion des couleurs
(define (vt102-couleur . spec)
   (let ()

      (define couleurs '(   
            (noir    . 0)
            (rouge   . 1)
            (vert    . 2)
            (jaune   . 3)
            (bleu    . 4)
            (magenta . 5)
            (cyan    . 6)
            (blanc   . 7)
            (défaut  . 9)))

      (define domaines '(
            (fond    . 40)
            (face    . 30)
            (normal  . 30)
            (brille  . 90)))

      (letrec*
            ((et (lambda (item fin)
                  (if fin
                     (cons item (cons ";" fin))
                     (cons item (cons "m" ())))))
             (examine-couleur (lambda (fond liste suite)
                  (cond
                     ((null? liste) (suite #f))
                     ((assq (car liste) couleurs) =>
                           (lambda (p)
                              (examine-domaine (cdr liste)
                                 (lambda (fin)
                                    (suite
                                       (et
                                          (number->string (+ fond (cdr p)))
                                          fin))))))
                     (else (examine-domaine (cdr liste) suite)))))
             (examine-domaine (lambda (liste suite)
                  (cond
                     ((null? liste) (suite #f))
                     ((assq (car liste) domaines) =>
                           (lambda (p)
                              (examine-couleur (cdr p) (cdr liste) suite)))
                     (else (examine-couleur 30 liste suite))))))
         (examine-domaine spec
            (lambda (value)
               (if value
                  (apply string-append (cons "\x1b;[" value))
                  "\x1b;[0m"))))))

;;; =======================================
;;; SECTION JEU DE CARTE
;;; =======================================

;;; définition enseigne des cartes
(define-record-type <enseigne>
   (crée-enseigne index symbole nom couleur)
   enseigne?
   (index     index_enseigne)
   (symbole   symbole-enseigne)
   (nom       nom-enseigne)
   (couleur   couleur-enseigne))

;;; les enseignes et valeurs des cartes standards françaises
(define enseignes
   (vector
      (crée-enseigne 0 "♣" "trèfle"  'noir)
      (crée-enseigne 1 "♦" "carreau" 'rouge)
      (crée-enseigne 2 "♥" "cœur"    'rouge)
      (crée-enseigne 3 "♠" "pique"   'noir)))

;;; définition valeurs des cartes
(define-record-type <valeur>
   (crée-valeur index symbole nom)
   valeur?
   (index     index-valeur)
   (symbole   symbole-valeur)
   (nom       nom-valeur))

;;; crée un jeu de 32 cartes standard
(define valeurs-32
   (vector
      (crée-valeur  6 "7" "7")
      (crée-valeur  7 "8" "8")
      (crée-valeur  8 "9" "9")
      (crée-valeur  9 "X" "10")
      (crée-valeur 10 "V" "Valet")
      (crée-valeur 11 "D" "Dame")
      (crée-valeur 12 "R" "Roi")
      (crée-valeur  0 "A" "As")))

;;; crée un jeu de 52 cartes standard
(define valeurs-52
   (vector
      (crée-valeur  0 "A" "As")
      (crée-valeur  1 "2" "2")
      (crée-valeur  2 "3" "3")
      (crée-valeur  3 "4" "4")
      (crée-valeur  4 "5" "5")
      (crée-valeur  5 "6" "6")
      (crée-valeur  6 "7" "7")
      (crée-valeur  7 "8" "8")
      (crée-valeur  8 "9" "9")
      (crée-valeur  9 "X" "10")
      (crée-valeur 10 "V" "Valet")
      (crée-valeur 11 "D" "Dame")
      (crée-valeur 12 "R" "Roi")))

;;; définition d'une carte
(define-record-type <carte>
   (crée-carte index face valeur enseigne)
   carte?
   (index     index-carte)
   (face      face-carte)
   (valeur    valeur-carte)
   (enseigne  enseigne-carte))

;;; crée un paquet de cartes sur la base des enseignes et des valeurs
;;; le troisième argument, optionel est le faceur de la carte qui
;;; calcule le texte représentant la face de la carte
;;; c'est une procédure (<valeur> <enseigne>) -> <string>
(define (crée-jeu-cartes enseignes valeurs . rest)
   (let* ((faceur (if (and (pair? rest) (procedure? (car rest)))
                     (car rest)
                     (lambda (valeur enseigne)
                        (string-append
                           (symbole-valeur valeur)
                           (symbole-enseigne enseigne)))))
          (nens   (vector-length enseignes))
          (nval   (vector-length valeurs))
          (J      (make-vector (* nens nval))))
      (do ((iens 0 (+ iens 1)))
         ((= iens nens))
         (do ((ival 0 (+ ival 1)))
            ((= ival nval))
            (let ((idx (+ ival (* iens nval)))
                  (ens (vector-ref enseignes iens))
                  (val (vector-ref valeurs ival)))
               (vector-set!
                  J
                  idx
                  (crée-carte idx (faceur val ens) val ens)))))
      J))

;;; fabrique la face d'une carte sur fond blanc
(define (faceur-carte-blanc valeur enseigne)
   (string-append
         (vt102-couleur 'fond 'blanc 'face 'noir)
         (symbole-valeur valeur)
         (vt102-couleur 'face (couleur-enseigne enseigne))
         (symbole-enseigne enseigne)
         (vt102-couleur 'fond 'défaut 'face 'défaut)))

;;; fabrique la face d'une carte sur fond courant
(define (faceur-carte-simple valeur enseigne)
   (let* ((rouge?  (eq? 'rouge (couleur-enseigne enseigne)))
          (couleur (if rouge? 'rouge 'blanc)))
      (string-append
            (symbole-valeur valeur)
            (vt102-couleur 'face couleur)
            (symbole-enseigne enseigne)
            (vt102-couleur))))

;;; fabrique un nouveau jeu de 52 cartes
(define (jeu-52-cartes . rest)
   (crée-jeu-cartes
      enseignes
      valeurs-52
      (if (null? rest)
         faceur-carte-blanc
         (car rest))))

;;; fabrique un nouveau jeu de 32 cartes
(define (jeu-32-cartes . rest)
   (crée-jeu-cartes
      enseignes
      valeurs-32
      (if (null? rest)
         faceur-carte-simple
         (car rest))))

;;; alea
(define alea
   (let ((rr (open-binary-input-file "/dev/random")))
      (lambda (compte)
         (remainder
            (let lp ((x (read-u8 rr)) (n (quotient compte 256)))
               (if (zero? n)
                  x
                  (lp (+ (* x 256) (read-u8 rr)) (quotient n 256))))
            compte))))
               
;;; mélange du paquet
(define (mélange-paquet paquet)
   (let* ((P  (if (vector? paquet)
                  (vector-copy paquet)
                  (list->vector paquet)))
          (le (vector-length P)))
      (do ((i (- le 1) (- i 1)))
         ((zero? i))
         (let* ((j (alea (+ i 1)))
                (a (vector-ref P i))
                (b (vector-ref P j)))
            (vector-set! P i b)
            (vector-set! P j a)))
      P))

;;; coupe du paquet
(define (coupe-paquet paquet)
   (let* ((P  (if (vector? paquet) paquet (list->vector paquet)))
          (le  (vector-length P))
          (R   (make-vector le))
          (le7 (floor-quotient le 7))
          (mil (- le le7 le7))
          (pi  (+ le7 (alea mil))))
      (vector-copy! R 0         P pi)
      (vector-copy! R (- le pi) P 0  pi)
      R))

;;; affiche un paquet de cartes
(define (affiche-paquet paquet)
   (let ((P  (if (vector? paquet) paquet (list->vector paquet))))
      (do ((i 0 (+ i 1)))
         ((= i (vector-length P)))
            (display (face-carte (vector-ref P i)))
            (display (if (= (+ i 1) (vector-length P)) "\n" " ")))))

;;; =======================================
;;; SECTION GÉOMÉTRIE DU JEU DE TOC
;;; =======================================
;   · ⬤ ❶ ❷ ❸ ❹ ⚬
; 
;                       16
;              · · · · ·            
;              ·       ·            
;      · ·     ·   ·   ·     · ·    
;      · ·     ·   ·   ·     · ·    
;              ·   ·   ·            
; 0            ·   ·   ·            
;  · · · · · · ·       · · · · · · ·
;  ·                               ·
;  ·   · · · · ·       · · · · ·   ·
;  ·                               ·
;  · · · · · · ·   ·   · · · · · · ·
;              ·   ·   ·            32
;              ·   ·   ·            
;      · ·     ·   ·   ·     · ·    
;      · ·     ·   ·   ·     · ·    
;              ·       ·            
;              · · · · ·            
;            48

;;; déclaration des unions
(define-record-type <union>
   (crée-union index camps)
   union?
   (index   index-union)
   (camps   camps-union))

;;; déclaration des camps
(define-record-type <camp>
   (crée-camp index union couleur pions main champ stalles entrée)
   camp?
   (index   index-camp)
   (union   union-camp)
   (couleur couleur-camp)
   (pions   pions-camp)
   (main    main-camp donne-main-camp!)
   (champ   champ-camp)
   (stalles stalles-camp)
   (entrée  entrée-camp))

;;; déclaration des pions
(define-record-type <pion>
   (crée-pion index camp face lieu)
   pion?
   (index index-pion)
   (camp  camp-pion)
   (face  face-pion)
   (lieu  lieu-pion met-lieu-pion!))

;;; déclaration des lieux
(define-record-type <lieu>
   (crée-lieu index type vtpos face après avant pion pieu? ligne colonne)
   lieu?
   (index   index-lieu)
   (type    type-lieu)
   (vtpos   vtpos-lieu)
   (face    face-lieu)
   (après   après-lieu met-après-lieu!)
   (avant   avant-lieu met-avant-lieu!)
   (pion    pion-lieu  met-pion-lieu!)
   (pieu?   pieu?-lieu met-pieu?-lieu!)
   (ligne   ligne-lieu)
   (colonne colonne-lieu))

;;; construction de la géométrie du jeu de toc
;;; 4 camps et une piste
(define-values (unions camps piste)
   (let ()
      ;; les unions
      (define unions (vector
                        (crée-union 0 (make-vector 2))
                        (crée-union 1 (make-vector 2))))

      ;; les camps
      (define camps (make-vector 4))

      ;; la piste
      (define piste (make-vector 64))

      ;; symbole des lieux
      (define symbole-lieu  "⚬") ; "⬤")

      ;; symboles des pions
      (define symbole-pions "❶❷❸❹")

      ;; un lieu normal
      (define lieu-gris (string-append
                           (vt102-couleur 'face 'blanc)
                           symbole-lieu))

      ;; calcule la coordonée ligne colonne
      ;; (multiplie les colonnes par 2)
      (define (coo col lig col0 lig0)
         (cons
            (+ 3 col0 col0 col col)
            (+ 2 lig0 lig)))

      ;; fabrique un lieu
      (define (fait-lieu idx type face coo)
         (let* ((lig (cdr coo))
                (col (car coo))
                (vtc (vt102-va lig col)))
            (crée-lieu idx type vtc face #f #f #f #f lig col)))

      ;; ajoute un lieu
      (define (add-lieu-to to idx type face coo)
               (vector-set! to idx (fait-lieu idx type face coo)))

      ;; iterateur sur 0 .. n-1
      (define (iter n proc)
         (do ((i 0 (+ i 1)))
            ((= i n))
               (proc i)))

      ;; construction du jeu
      (for-each
         (lambda (index couleur trf)
            (define base (* 16 index))
            (define champ (make-vector 4))
            (define stalles (make-vector 4))
            (define pions (make-vector 4))
            (define vtc (vt102-couleur 'face couleur))
            (define lieu-camp (string-append vtc symbole-lieu))
            (define union (vector-ref unions (modulo index 2)))

            ; lieux de la piste
            (iter 6
               (lambda (x)
                  (add-lieu-to
                     piste
                     (+ base x)
                     'piste
                     (if (zero? x) lieu-camp lieu-gris)
                     (trf x 6))))
            (iter 6
               (lambda (x)
                  (add-lieu-to
                     piste
                     (+ base x 6)
                     'piste
                     lieu-gris
                     (trf 6 (- 6 x)))))
            (iter 4
               (lambda (x)
                  (add-lieu-to
                     piste
                     (+ base x 12)
                     'piste
                     lieu-gris
                     (trf (+ x 6) 0))))

            ; lieux du camp
            (iter 4
               (lambda (x)
                  (add-lieu-to stalles x 'stalle lieu-camp (trf (+ x 2) 8))
                  (add-lieu-to champ x 'champ lieu-camp
                     (trf
                        (+ 2 (modulo x 2))
                        (+ 2 (abs (floor/ (- x 1) 2))) ))))

            ; succesions dans la stalle
            (iter 3
               (lambda (x)
                  (met-après-lieu!
                     (vector-ref stalles x)
                     (vector-ref stalles (+ x 1)))))

            ; le camp
            (let ((camp (crée-camp
                     index union couleur pions #f
                     champ stalles (vector-ref piste base))))
               (vector-set! camps index camp)
               (vector-set! (camps-union union) (quotient index 2) camp)

               ; les pions du camp
               (iter 4
                  (lambda (x)
                     (let* ((symb (substring symbole-pions x (+ x 1)))
                            (face (string-append vtc symb))
                            (lieu (vector-ref champ x))
                            (pion (crée-pion x camp face lieu)))
                        (vector-set! pions x pion)
                        (met-pion-lieu! lieu pion)))))
         )
         ; les donées: index couleur transformation
         '(0 1 2 3)
         '(rouge bleu jaune vert)
         (list
            (lambda (col lig) (coo col     lig      0  0))
            (lambda (col lig) (coo (- lig) col     16  0))
            (lambda (col lig) (coo (- col) (- lig) 16 16))
            (lambda (col lig) (coo lig     (- col)  0 16))))

      ;; succession des lieux
      (iter 64
         (lambda (x)
            (let ((après (vector-ref piste x))
                  (avant (vector-ref piste (if (zero? x) 63 (- x 1)))))
               (met-après-lieu! avant après)
               (met-avant-lieu! après avant))))

      ;; construction des camps
      (values unions camps piste)))

;;; raz du jeu
(define (raz)
   (vector-for-each
      (lambda (camp)
         (vector-for-each
            (lambda (pion)
               (met-pion-lieu! (lieu-pion pion) #f))
            (pions-camp camp))
         (vector-for-each
            (lambda (pion)
               (let ((lieu (vector-ref (champ-camp camp) (index-pion pion))))
                  (met-lieu-pion! pion lieu)
                  (met-pion-lieu! lieu pion)))
            (pions-camp camp))
         (met-pieu?-lieu! (entrée-camp camp) #f))
      camps))

;;; montre un lieu
(define (montre-lieu lieu)
   (display (vtpos-lieu lieu))
   (let ((pion (pion-lieu lieu)))
      (display (if pion (face-pion pion) (face-lieu lieu)))))

;;; montre un pion
(define (montre-pion pion)
   (display (vtpos-lieu (lieu-pion pion)))
   (display (face-pion pion))
   (display (vtpos-lieu lieu))
   (display (face-lieu lieu)))

;;; montre la situation: piste, champs, stalles, pions
(define (montre-situation)
   (vector-for-each montre-lieu piste)
   (vector-for-each
         (lambda (camp)
            (vector-for-each montre-lieu (champ-camp camp))
            (vector-for-each montre-lieu (stalles-camp camp)))
         camps))

;;; =======================================
;;; SECTION CARTES ET ACTIONS DU JEU DE TOC
;;; =======================================

;;; l'action prend le pion du lieu 'avant' et le place
;;; au lieu 'après'
;;; si 'après' est occupé par un pion alors 'prise' doit
;;; indiquer le lieu d'arrivée du pion pris
(define-record-type <action>
   (crée-action avant après prise pieu)
   action?
   (avant   avant-action)
   (après   après-action)
   (prise   prise-action)
   (pieu    pieu-action))

;;; compares a and b for equality
(define (action-equal? a b)
   (and
      (eq? (avant-action a) (avant-action b))
      (eq? (après-action a) (après-action b))
      (eq? (prise-action a) (prise-action b))
      (eq? (pieu-action a) (pieu-action b))))

;;; déplace le pion du lieu avant au lieu après
;;; le lieu après ne doit pas avoir de pion
(define (déplace-pion avant après)
   (let ((pion (pion-lieu avant)))
      (met-pion-lieu! avant #f)
      (met-pion-lieu! après pion)
      (met-lieu-pion! pion après)))

;;; déplace le pion du lieu avant au lieu après
;;; le pion lieu situé au lieu après est envoyé
;;; dans le lieu prise
(define (prend-pion avant après prise)
   (let ((pion (pion-lieu après)))
      (déplace-pion avant après)
      (met-pion-lieu! prise pion)
      (met-lieu-pion! pion prise)))

;;; joue l'action
(define (joue-une-action action)
   (let ((avant   (avant-action action))
         (après   (après-action action))
         (prise   (prise-action action)))
      (case (pieu-action action)
         ((retire) (met-pieu?-lieu! avant #f))
         ((met)    (met-pieu?-lieu! après #t)))
      (if prise
         (prend-pion avant après prise)
         (déplace-pion avant après))))

;;; joue une ou des actions
(define (joue-action action)
   (if (list? action)
      (for-each joue-une-action action)
      (joue-une-action action)))

;;; déjoue l'action
(define (déjoue-une-action action)
   (let ((avant   (avant-action action))
         (après   (après-action action))
         (prise   (prise-action action)))
      (case (pieu-action action)
         ((retire) (met-pieu?-lieu! avant #t))
         ((met)    (met-pieu?-lieu! après #f)))
      (if prise
         (prend-pion prise après avant)
         (déplace-pion après avant))))

;;; déjoue une ou des actions
(define (déjoue-action action)
   (if (list? action)
      (for-each déjoue-une-action action)
      (déjoue-une-action action)))

;;; montre les élément modifiés par l'action
;;; à appeler après chaque action pour rétablir
;;; l'affichage
(define (montre-une-action action)
   (let ((avant   (avant-action action))
         (après   (après-action action))
         (prise   (prise-action action)))
      (montre-lieu avant)
      (montre-lieu après)
      (if prise
         (montre-lieu prise))))

;;; montre une ou des actions
(define (montre-action action)
   (if (list? action)
      (for-each montre-une-action action)
      (montre-une-action action)))

;;; affiche le contenu d'une action
(define (affiche-une-action action)
   (let ((avant   (avant-action action))
         (après   (après-action action))
         (prise   (prise-action action))
         (pieu    (pieu-action  action)))
      (display "[ACTION ")
      (display (type-lieu avant))
      (display (index-lieu avant))
      (display " -> ")
      (display (type-lieu après))
      (display (index-lieu après))
      (when pieu
         (display " ")
         (display pieu))
      (display "]")))

;;; affiche une ou des actions
(define (affiche-action action)
   (if (list? action)
      (for-each affiche-une-action action)
      (affiche-une-action action)))

;;; crée un déversoir
(define (crée-versoir)
   (let ((liste ()))
      (lambda item
         (let encore ((val item))
            (if (pair? val)
               (begin
                  (set! liste (cons (car val) liste))
                  (encore (cdr val)))
               liste)))))

;;; cherche la prochaine place libre dans le champ du pion
(define (place-dans-champ pion)
   (let ((champ (champ-camp (camp-pion pion))))
      (let suivant ((i 3))
         (let ((lieu (vector-ref champ i)))
            (if (pion-lieu lieu)
               (suivant (- i 1))
               lieu)))))

;;; crée l'action de déplacement du pion situé
;;; au lieu avant vers le lieu après en gérant
;;; l'aspect pieu selon ce qui est donné et l'aspect
;;; prise automatiquement
(define (crée-action-prise pieu avant après)
   (let ((pion (pion-lieu après)))
      (crée-action avant après (and pion (place-dans-champ pion)) pieu)))

;;; crée l'action de déplacement du pion situé
;;; au lieu avant vers le lieu après en gérant
;;; les aspects pieu et prise automatiquement
(define (crée-action-retire avant après)
   (crée-action-prise (and (pieu?-lieu avant) 'retire) avant après))

;;; crée l'action de déplacement du pion situé
;;; au lieu avant (un champ) vers le lieu après (entrée du camp du pion)
;;; en gérant l'aspect prise automatiquement
(define (crée-action-entre avant après)
   (crée-action-prise 'met avant après))

;;; crée l'action d'échange du pion situé
;;; au lieu avant avec le pion situé au lieu après
(define (crée-action-echange avant après)
   (crée-action avant après avant (and (pieu?-lieu avant) 'retire)))

;;; cherche les déplacements "standards" pour un pion
(define (verse-pion-normal pion camp nombre versoir)
   (let ((avant (lieu-pion pion))
         (entrée (entrée-camp camp)))
      (let scan ((après   avant)
                 (curseur 0))
         (if (= curseur nombre)
            (let ((pion-après (pion-lieu après)))
               (unless (and pion-après (eq? camp (camp-pion pion-après)))
                  (versoir (crée-action-retire avant après))))
            (let ((prochain-lieu   (après-lieu après)))
               (when (eq? après entrée)
                  (unless (zero? curseur)
                     (let ((stalle0 (vector-ref (stalles-camp camp) 0)))
                        (unless (pion-lieu stalle0)
                           (scan stalle0 (+ curseur 1))))))
               (if prochain-lieu
                  (unless
                     (or
                        (pieu?-lieu prochain-lieu)
                        (and
                           (pion-lieu prochain-lieu)
                           (eq? (type-lieu prochain-lieu) 'stalle)))
                     (scan prochain-lieu (+ curseur 1)))))))))

;;; cherche les déplacements "standards" pour un camp
(define (verse-camp-normal camp nombre versoir)
   (vector-for-each
      (lambda (pion)
         (verse-pion-normal pion camp nombre versoir))
      (pions-camp camp)))

;;; cherche les déplacements type 7 pour un camp
(define (verse-camp-éclaté camp nombre versoir)

   ;; les pions du camp
   (define pions (pions-camp camp))

   ;; l'entrée des stalles
   (define entrée (entrée-camp camp))

   ;; la première stalle
   (define première-stalle (vector-ref (stalles-camp camp) 0))

   ;; les séquences de coups trouvés
   (define trouvés ())

   ;; une séquence de coups est-elle déjà trouvée?
   (define (même-coups? coups autre-coups)
      (and
         (= (length coups) (length autre-coups))
         (let encore ((iter coups))
            (or
               (null? iter)
               (and
                  (member (car iter) autre-coups action-equal?)
                  (encore (cdr iter)))))))

   ;; une séquence de coups est-elle déjà trouvée?
   (define (déjà-trouvé? coups)
      (let encore ((iter trouvés))
         (and
            (pair? iter)
            (or
               (même-coups? coups (car iter))
               (encore (cdr iter))))))

   ;; ajoute une séquence de coups
   (define (ajoute-coups coups)
      (unless (déjà-trouvé? coups)
         (set! trouvés (cons coups trouvés))
         (versoir (reverse coups))))

   ;; évalue si on peut poser le pion de avant à après
   (define (bouge-après avant après pion curseur exclus coups)
      (unless (pieu?-lieu après)
         (let* ((pion-après  (pion-lieu après))
                (action      (crée-action-retire avant après))
                (exclus-après (cons pion exclus))
                (coups-après (cons action coups)))
            ;; y a-t-il un pion à la destination?
            (if pion-après
               ;; oui et si ce n'est pas un pion à soi...
               (unless (eq? camp (camp-pion pion-après))
                  ;; prend le pion puis joue soit le pion soit les autres pions
                  (joue-action action)
                  (avance après après pion curseur exclus coups-après #t)
                  (vector-for-each
                     (lambda (autre-pion)
                        (bouge-pion autre-pion curseur exclus-après coups-après))
                     pions)
                  (déjoue-action action))
               ;; non, la destination est libre
               (begin
                  ;; vaplus loin ou va ici et joue les autres pions
                  (avance avant après pion curseur exclus coups #t)
                  (joue-action action)
                  (vector-for-each
                     (lambda (autre-pion)
                        (bouge-pion autre-pion curseur exclus-après coups-après))
                     pions)
                  (déjoue-action action))))))


   ;; examine les coups possible pour un pion parti de avant et étant
   ;; au lieu actuel
   (define (avance avant lieu pion curseur exclus coups non-initial?)
      ;; Est-il possible d'avancer?
      (if (= curseur nombre)
         ;; non, joue les coups mémorisés
         (ajoute-coups
            (if (eq? avant lieu)
               coups
               (cons (crée-action-retire avant lieu) coups)))
         ;; oui il faut avancer
         (let ((après (après-lieu lieu)))
            ;; peut-il entrer dans les stalles
            (if (and (eq? lieu entrée) non-initial?)
               (bouge-après avant première-stalle pion (+ curseur 1) exclus coups))
            ;; avancer si possible
            (if après
               (bouge-après avant après pion (+ curseur 1) exclus coups)))))

   ;; bouge le pion si c'est possible
   (define (bouge-pion pion curseur exclus coups)
      (unless (memq pion exclus)
         (unless (= curseur nombre)
            (let ((avant (lieu-pion pion)))
               (unless (eq? 'champ (type-lieu avant))
                  (avance avant avant pion curseur exclus coups #f))))))

   ;; pour chaque pion initialement
   (define (initial pion)
      (bouge-pion pion 0 () ()))

   ;; applique à tous les pions
   (vector-for-each initial pions))


;;; cherche les déplacements "rebours" pour un pion
(define (verse-pion-rebours pion nombre versoir)
   (let ((avant (lieu-pion pion)))
      (let scan ((après avant)
                 (dcpt nombre))
         (if (zero? dcpt)
            (versoir (crée-action-retire avant après))
            (let ((lieu (avant-lieu après))
                  (dec  (- dcpt 1)))
               (if lieu
                  (unless (pieu?-lieu lieu)
                     (scan lieu dec))))))))

;;; cherche les déplacements "rebours" pour un camp
(define (verse-camp-rebours camp nombre versoir)
   (vector-for-each
      (lambda (pion)
         (verse-pion-rebours pion nombre versoir))
      (pions-camp camp)))

;;; cherche les déplacements type valet pour un camp
(define (verse-camp-échange camp versoir)
   (vector-for-each
      (lambda (pion)
         (let ((lieu (lieu-pion pion)))
            (when (eq? (type-lieu (lieu-pion pion)) 'piste)
               (vector-for-each
                  (lambda (autre-camp)
                     (unless (eq? camp autre-camp)
                        (vector-for-each
                           (lambda (autre-pion)
                              (let ((autre-lieu (lieu-pion autre-pion)))
                                 (if (eq? (type-lieu autre-lieu) 'piste)
                                    (unless (pieu?-lieu autre-lieu)
                                       (versoir (crée-action-echange lieu autre-lieu))))))
                           (pions-camp autre-camp))
                     (pions-camp autre-camp)))
                  camps))))
      (pions-camp camp)))

;;; cherche les déplacements type As ou Roi pour un camp
(define (verse-camp-sortie camp nombre versoir)
   (let* ((entrée      (entrée-camp camp))
          (champ       (champ-camp camp))
          (pion-entrée (pion-lieu entrée)))
      (unless (and pion-entrée (eq? camp (camp-pion pion-entrée)))
         (let suivant ((i 0))
            (when (< i 4)
               (let ((lieu (vector-ref champ i)))
                  (if (pion-lieu lieu)
                     (versoir (crée-action-entre lieu entrée))
                     (suivant (+ i 1))))))))
   (verse-camp-normal camp nombre versoir))

;;; tableau des versoir par carte
(define versoir-de-carte
   (vector
      #| As    |# (lambda (camp versoir) (verse-camp-sortie  camp  1 versoir))
      #| 2     |# (lambda (camp versoir) (verse-camp-normal  camp  2 versoir))
      #| 3     |# (lambda (camp versoir) (verse-camp-normal  camp  3 versoir))
      #| 4     |# (lambda (camp versoir) (verse-camp-rebours camp  4 versoir))
      #| 5     |# (lambda (camp versoir) (verse-camp-normal  camp  5 versoir))
      #| 6     |# (lambda (camp versoir) (verse-camp-normal  camp  6 versoir))
      #| 7     |# (lambda (camp versoir) (verse-camp-éclaté  camp  7 versoir))
      #| 8     |# (lambda (camp versoir) (verse-camp-normal  camp  8 versoir))
      #| 9     |# (lambda (camp versoir) (verse-camp-normal  camp  9 versoir))
      #| 10    |# (lambda (camp versoir) (verse-camp-normal  camp 10 versoir))
      #| Valet |# (lambda (camp versoir) (verse-camp-échange camp    versoir))
      #| Dame  |# (lambda (camp versoir) (verse-camp-normal  camp 12 versoir))
      #| Roi   |# (lambda (camp versoir) (verse-camp-sortie  camp 13 versoir))))

;;; =======================================
;;; SECTION JEU DE CARTE
;;; =======================================

(define (affiche-situation)
   (display (vt102-couleur 'normal 'blanc 'fond 'noir))
   (display (vt102-efface-ecran))
   (montre-situation))

(define (affiche-camp camp)
   (display (vt102-couleur 'normal 'blanc 'fond 'noir))
   (display (vt102-va 20 1))
   (display (vt102-efface-fin))
   (display (vt102-couleur 'normal 'noir 'fond (couleur-camp camp)))
   (display (couleur-camp camp))
   (display (vt102-couleur 'normal 'blanc 'fond 'noir))
   (newline))

(define (dialogue-choix liste afficheur auto)
   (let ((long (length liste)))
      (if (and (<= long 1) auto)
         0
         (let aff ((idx  0)
                   (tete liste))
            (if (< idx long)
               (begin
                  (display idx)
                  (display " ")
                  (afficheur (car tete))
                  (newline)
                  (aff (+ idx 1) (cdr tete)))
               (begin
                  (display "Ton choix? ")
                  (let lire ((v (read)))
                     (if (and (number? v) (< -1 v long))
                        v
                        (begin
                           (display "? ")
                           (lire (read)))))))))))
            
(define (dialogue-carte cartes)
   (let ((idx (dialogue-choix
                  cartes
                  (lambda (carte) (display (face-carte carte)))
                  #t)))
      (let boucle ((it 0)
                   (re (list (list-ref cartes idx)))
                   (li cartes))
         (if (null? li)
            (reverse re)
            (boucle
               (+ it 1)
               (if (= it idx) re (cons (car li) re))
               (cdr li))))))

(define (dialogue-camp camp)
   (let ((main (main-camp camp)))
      (and 
         main
         (begin
            (affiche-situation)
            (affiche-camp camp)
            (let* ((choix   (dialogue-carte main))
                   (carte   (car choix))
                   (reste   (cdr choix))
                   (versoir (crée-versoir))
                   (valeur  (valeur-carte carte))
                   (index   (index-valeur valeur))
                   (verseur (vector-ref versoir-de-carte index)))
               (verseur camp versoir)
               (let ((actions (versoir)))
                  (affiche-camp camp)
                  (display "carte ")
                  (display (face-carte carte))
                  (newline)
                  (if (pair? actions)
                     (let* ((idx (dialogue-choix actions affiche-action #t))
                            (act (list-ref actions idx)))
                        (joue-action act)
                        (montre-action act))))
               (set! défausse (cons carte défausse))
               (donne-main-camp! camp (and (pair? reste) reste)))
            #t))))


;;; création du paquet initial

(define paquet   (vector->list (mélange-paquet (jeu-52-cartes))))
(define défausse ())

(define (distribue compte)
   (unless (zero? compte)
      (vector-for-each
         (lambda (camp)
            (donne-main-camp! camp (cons (car paquet) (or (main-camp camp) ())))
            (set! paquet (cdr paquet)))
         camps)
      (distribue (- compte 1))))

(display (vt102-efface-ecran))
(let boucle ((idx-camp 0))
   (let ((camp    (vector-ref camps idx-camp)))
      (if (dialogue-camp camp)
         (boucle (modulo (+ idx-camp 1) 4))
         (case (length paquet)
               ((52 36)
                  ; AFR choix intelligent
                  (distribue (+ 4 (modulo (index-valeur (valeur-carte (car paquet))) 2)))
                  (boucle idx-camp))
               ((20)
                  (distribue 5)
                  (boucle idx-camp))
               ((32 16)
                  (distribue 4)
                  (boucle idx-camp))
               ((0)
                  (set! paquet (vector->list (coupe-paquet (reverse défausse))))
                  (set! défausse ())
                  (boucle (modulo (+ idx-camp 1) 4)))))))
   

;;; vim: noai ts=3 sw=3 expandtab

