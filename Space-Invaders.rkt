#lang racket

(require 2htdp/image)
(require 2htdp/universe)
(require lang/posn)
(require racket/list)


(define-values
  (scn_w scn_h rows cols e_images e_posns b_images b_posns bullets e_w e_h gap dir CD cd CD_b cd_b p_w p_h p_spd smooth key_r key_l key_d key_u fire? b_count b_spd b_w b_h)
  (values 400 600 4 6 empty empty empty empty empty 25 15 1.3 "right" .5 .5 0.2 0 30 30 10 4 0 0 0 0 #f 0 20 3 7))

(define-values
  (enemy_count p_x p_xto p_y p_yto)
  (values (* rows cols) (/ scn_w 2) (/ scn_w 2) (- scn_h 100) (- scn_h 100)))

(define scn (rectangle scn_w scn_h 209 "black"))
(define enemy (rectangle e_w e_h 255 "white"))
(define player_image (rectangle p_w p_h 255 "white"))
(define bullet (rectangle b_w b_h 255 "white"))

(set! e_posns (let loop_j ((j 0))
                (if (< j rows) (let loop_i ((i 0))
                                 (if (< i cols) (append (list (make-posn (+ (* i e_w gap) e_w) (+ e_h (* j e_h gap)))) (loop_i (add1 i))) (loop_j (add1 j)))
                                 ) empty)
                ))

(set! e_images (let loop_j ((j 0))
                 (if (< j rows) (let loop_i ((i 0))
                                  (if (< i cols) (append (list enemy) (loop_i (add1 i))) (loop_j (add1 j)))
                                  ) empty)
                 ))

(define (find exp what lst)
  (let ((num (what (first lst))))
    (let loop ((i 1))
      (if (< i (length lst)) (if (exp (what (list-ref lst i)) num) (begin
                                                                     (set! num (what (list-ref lst i)))
                                                                     (loop (add1 i))
                                                                     ) (loop (add1 i))) num)
      )
    )
  )

(define (shift lst what amount)
  (let ((l empty))
    (let loop ((i 0))
      (if (< i (length lst)) (append (list (make-posn (+ (if (string=? what "x") amount 0) (posn-x (list-ref lst i))) (+ (if (string=? what "y") amount 0)(posn-y (list-ref lst i))))) (loop (add1 i))) empty)
      )
    )
  )

(define (outofscreen l n)
  (if (or (< (posn-x (list-ref l n)) 0) (> (posn-x (list-ref l n)) scn_w) (< (posn-y (list-ref l n)) 0) (> (posn-y (list-ref l n)) scn_h)) #t #f)
  )

(define (destroy l n)
  (append (take l n) (rest (drop l n)))
  )

(define (check_hit x1 y1 x2 y2 w h)
  (if
   (and (< (- x2 (/ w 2) b_w) x1 (+ x2 (/ w 2) b_w)) (< (- y2 (/ h 2) b_h) y1 (+ y2 (/ h 2) b_h)))
   #t #f)
  )

;; -------------------------------------------------------------- RENDER ------------

(define (render ws)
  (place-images
   (append
    e_images
    b_images
    (list
     player_image
     ))
   (append
    e_posns
    b_posns
    (list
     (make-posn p_x p_y)
     ))
   scn
   )
  )
;; -------------------------------------------------------------- KEY HANDLER -------
(define (key-handler ws ke)
  (cond
    ((key=? ke "right") (set! key_r 1))
    ((key=? ke "left") (set! key_l 1))
    ((key=? ke "up") (set! key_u 1))
    ((key=? ke "down") (set! key_d 1))
    ((key=? ke " ") (set! fire? #t))
    (else ws)
    )
  )


;; -------------------------------------------------------------- RELEASE HANDLER ---
(define (release-handler ws ke)
  (cond
    ((key=? ke "right") (set! key_r 0))
    ((key=? ke "left") (set! key_l 0))
    ((key=? ke "up") (set! key_u 0))
    ((key=? ke "down") (set! key_d 0))
    ((key=? ke " ") (set!-values (fire? cd_b) (values #f 0)))
    (else ws)
    )
  )

;; -------------------------------------------------------------- MOUSE HANDLER -----

(define (mouse-handler ws x y me)
  ws
  )

;; -------------------------------------------------------------- STEP --------------

(define (step ws)
  (begin

    ; -------------------------------------- Bullets --------------------------------

    (if fire? (if (< cd_b 0) (begin (set! b_count (add1 b_count)) (set! cd_b CD_b)) (set! cd_b (- cd_b (/ 1.0 28.0)))) void)


    (let loop ((i 0))
      (if (and (< i b_count) (not (= b_count (length bullets)))) (begin
                                                                   (set! bullets (append bullets (list (list p_x p_y))))
                                                                   (loop (add1 i))
                                                                   ) void)
      )

    (if (not (= b_count (length b_images)))
        (set! b_images
              (let ((b empty))
                (let loop ((i 0))
                  (if (< i b_count) (begin (set! b (append b (list bullet))) (loop (add1 i))) b)
                  )
                )
              )
        void)

    (if (not (= b_count (length b_posns)))
        (set! b_posns
              (let ((b empty))
                (let loop ((i 0))
                  (if (< i b_count) (begin (set! b (append b (list (make-posn (list-ref (list-ref bullets i) 0) (list-ref (list-ref bullets i) 1))))) (loop (add1 i))) b)
                  )
                )
              )
        void)

    (let loop ((i 0))     
      (if (< i b_count)
          (begin
            (set! bullets (list-set bullets i (list-set (list-ref bullets i) 1 (- (list-ref (list-ref bullets i) 1) b_spd))))
            (loop (add1 i))
            )
          void)
      )


    (let loop ((i 0))
      (if (< i b_count)
          (begin
            (set! b_posns (list-set b_posns i (make-posn (posn-x (list-ref b_posns i))
                                                         (- (posn-y (list-ref b_posns i)) b_spd))))
            (loop (add1 i))
            )
          void)
      )

    (if (> b_count 0) (begin
                             (if (outofscreen b_posns 0)
                                 (begin
                                   (set! b_count (sub1 b_count))
                                   (set! b_images (destroy b_images 0))
                                   (set! b_posns (destroy b_posns 0))
                                   (set! bullets (destroy bullets 0))
                                   )
                                 void)
                             )
        void)
    
    ; -------------------------------------- Checking if bullet hits the enemy ------

    (let loop_e ((i 0))
      (if (< i enemy_count)
          (begin
            (let loop_b ((j 0))
              (if (< j b_count)
                  (if (check_hit
                       (list-ref (list-ref bullets j) 0)
                       (list-ref (list-ref bullets j) 1)
                       (posn-x (list-ref e_posns i))
                       (posn-y (list-ref e_posns i)) e_w e_h)
                      (begin
                        (set! b_count (sub1 b_count))
                        (set! b_images (destroy b_images j))
                        (set! b_posns (destroy b_posns j))
                        (set! bullets (destroy bullets j))
                        
                        (set! e_images (destroy e_images i))
                        (set! e_posns (destroy e_posns i))
                        (set! enemy_count (sub1 enemy_count))
                        ) (loop_b (add1 j)))
                  (loop_e (add1 i)))
              )
            ) void)
      )



    ; -------------------------------------- Enemy Movements: -----------------------

    (if (< cd 0) (begin
                   (if (string=? dir "right")
                       (if (< (find > posn-x e_posns) (- scn_w e_w e_w)) (set! e_posns (shift e_posns "x" (* e_w 1.1))) (set!-values (dir e_posns) (values "left" (shift e_posns "y" (* e_h 1.1)))) )
                       (if (string=? dir "left")
                           (if (> (find < posn-x e_posns) (+ e_w e_w)) (set! e_posns (shift e_posns "x" (* -1.1 e_w))) (set!-values (dir e_posns) (values "right" (shift e_posns "y" (* e_h 1.1)))) )
                           void
                           ))
                   (set! cd CD))
        (set! cd (- cd (/ 1.0 28.0))))
    ; -------------------------------------- Player Movements: -----------------------

    (set! p_xto (if
                 (<= (/ p_w 2) (+ p_xto (* p_spd (- key_r key_l)))  (- scn_w (/ p_w 2)))
                 (+ p_xto (* p_spd (- key_r key_l)))
                 p_xto))
    (set! p_x (+ p_x (/ (- p_xto p_x) smooth)))

    (set! p_yto (if
                 (<= (/ p_h 2) (+ p_yto (* p_spd (- key_d key_u)))  (- scn_h (/ p_h 2)))
                 (+ p_yto (* p_spd (- key_d key_u)))
                 p_yto))
    (set! p_y (+ p_y (/ (- p_yto p_y) smooth)))

    )
  )

;; -------------------------------------------------------------- GAME START --------

(big-bang 0
          (on-tick step)
          (on-mouse mouse-handler)
          (on-draw render)
          (on-release release-handler)
          (on-key key-handler)
          )