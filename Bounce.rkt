#lang racket

(require 2htdp/image)
(require 2htdp/universe)
(require racket/list)
(require lang/posn)
(require racket/math)
(require picturing-programs)


(define-values (scn_w scn_h) (values 400 600))
(define-values
  (pu_block_color shield ball_mode block_rows block_cols b_r p_spd p_w p_h p_curve block_w block_h block_curve p_x p_y p_xto p_dir smooth l_key r_key block_images block_positions pt_list pt_images pt_positions ball_count_images ball_count_positions powerup_list powerup_positions powerup_images fr_list fr_images fr_positions CD)
  (values "magenta" #f "normal" 13 8 7 20 70 15 3 45 25 2 (/ scn_w 2) (- scn_h 70) (/ scn_w 2) 0 4 0 0 empty empty empty empty empty empty empty empty empty empty empty empty empty 3))

(define-values
  (countdown ball_count block_count score b_spd b_xspd b_yspd b_x b_y b_dir_v b_dir_h angle)
  (values 3 3 (* block_cols block_rows) 0 15 0 0 (/ scn_w 2) (- scn_h 70 b_r (/ p_h 2)) 1 0 45)
  )

(define-struct pt (x y life dir spd))

(define powerups (list
                  "+ball"
                  "fireball"
                  "shield"
                  empty
                  empty
                  empty
                  empty
                  empty
                  ))

(define scn (empty-scene scn_w scn_h))
(define background (rectangle scn_w scn_h 209 "black"))
(define player_image (polygon (list
                               (make-posn p_curve 0)
                               (make-posn (- p_w p_curve) 0)
                               (make-posn p_w p_curve)
                               (make-posn p_w (- p_h p_curve))
                               (make-posn (- p_w p_curve) p_h)
                               (make-posn p_curve p_h)
                               (make-posn 0 (- p_h p_curve))
                               (make-posn 0 p_curve)
                               )
                              255 "white"
                              ))


(define fire_player_image (polygon (list
                               (make-posn p_curve 0)
                               (make-posn (- p_w p_curve) 0)
                               (make-posn p_w p_curve)
                               (make-posn p_w (- p_h p_curve))
                               (make-posn (- p_w p_curve) p_h)
                               (make-posn p_curve p_h)
                               (make-posn 0 (- p_h p_curve))
                               (make-posn 0 p_curve)
                               )
                              255 "white"
                              ))

(define ball_image (circle b_r "solid" "white"))
(define fire_ball_image (overlay (circle (+ b_r 15) 5 "orange") (circle (+ b_r 10) 10 "orange") (circle (+ b_r 5) 20 "orange") (circle (- b_r 1) 100 "white") (circle (- b_r 2) "solid" "gold") (circle b_r "solid" "darkorange") (circle (+ b_r 1) "solid" "darkred")))
(define +ball_image (beside (overlay/align "center" "center" (rectangle 2 8 255 "white") (rectangle 8 2 255 "white")) (circle b_r "solid" "DeepPink")))
(define shield_pu_image (overlay (rectangle 13 2 100 "white") (rectangle 15 4 255 "MediumSlateBlue")))
(define shield_image (overlay (rectangle (- scn_w 5) 4 100 "white") (rectangle scn_w 6 255 "mediumslateblue")))


(define regular_block_image (polygon (list
                                      (make-posn block_curve 0)
                                      (make-posn (- block_w block_curve) 0)
                                      (make-posn block_w block_curve)
                                      (make-posn block_w (- block_h block_curve))
                                      (make-posn (- block_w block_curve) block_h)
                                      (make-posn block_curve block_h)
                                      (make-posn 0 (- block_h block_curve))
                                      (make-posn 0 block_curve)
                                      )
                                     255 "white"
                                     ))

(define powerup_block_image (polygon (list
                                      (make-posn block_curve 0)
                                      (make-posn (- block_w block_curve) 0)
                                      (make-posn block_w block_curve)
                                      (make-posn block_w (- block_h block_curve))
                                      (make-posn (- block_w block_curve) block_h)
                                      (make-posn block_curve block_h)
                                      (make-posn 0 (- block_h block_curve))
                                      (make-posn 0 block_curve)
                                      )
                                     255 pu_block_color
                                     ))


;-------------------------------- (block_list (list IMAGE X Y DURABILITY POINTS POWERUP))

(define (degtorad a) (degrees->radians a))

(define block_list
  (let ((lst empty))
    (let loop_j ((j 0))
      (if (< j block_rows)
          (let loop_i ((i 0))
            (let ((pu (list-ref powerups (random (length powerups)))))
              (if (< i block_cols) (append (list (list (if (empty? pu) regular_block_image powerup_block_image) (+ (* 0.75 block_w) (* i block_w 1.05)) (+ (* 1 block_h) (* j 1.05 block_h)) 1 1 pu)) (loop_i (add1 i))) (loop_j (add1 j)))
              )) lst)
      )))

(define (check-hit x1 y1 x2 y2 w h)
  (if
   (and (< (- x2 (/ w 2) b_r) x1 (+ x2 (/ w 2) b_r)) (< (- y2 (/ h 2) b_r) y1 (+ y2 (/ h 2) b_r)))
   #t #f)
  )

(define (destroy l n)
  (append (take l n)  (rest (drop l n)))
  )

(define (add-particle x y n)
  (let ((lst pt_list))
    (let loop ((i 0))
      (if (< i n) (let ((ang (random 360)) (spd (random 1 5)) (life (random 5 15))) (append (loop (add1 i)) (list (list x y life ang (* spd (cos (degtorad ang))) (* spd (sin (degtorad ang)))))))
          lst)
      )
    )
  )

(define (add-fire x y n)
  (let ((lst fr_list))
    (let loop ((i 0))
      (if (< i n) (let ((ang (random 360)) (spd (* 3 (random 2))) (life (random 5))) (append (loop (add1 i)) (list (list x y life ang (* spd (cos (degtorad ang))) (* spd (sin (degtorad ang)))))))
          lst)
      )
    )
  )

(define (add-powerup x y type)
  (append powerup_list (list (list x y type)))
  )

(define (decrease_life l pos_l)
  (if (empty? l) l (append (list (list (posn-x (first pos_l)) (posn-y (first pos_l)) (- (list-ref (first l) 2) 0.7) (list-ref (first l) 3) (list-ref (first l) 4) (list-ref (first l) 5))) (decrease_life (rest l) (rest pos_l))))
  )

(define (decrease_yspd l)
  (if (empty? l) l (append (list (list (list-ref (first l) 0) (list-ref (first l) 1) (list-ref (first l) 2) (list-ref (first l) 3) (list-ref (first l) 4) (- (list-ref (first l) 5) 0.2))) (decrease_yspd (rest l))))
  )

(define (decrease_y l pos_l)
  (if (empty? l) l (append (list (list (posn-x (first pos_l)) (posn-y (first pos_l)) (list-ref (first l) 2))) (decrease_y (rest l) (rest pos_l))))
  )

(define (increase_yspd l)
  (if (empty? l) l (append (list (list (list-ref (first l) 0) (list-ref (first l) 1) (list-ref (first l) 2) (list-ref (first l) 3) (list-ref (first l) 4) (+ (list-ref (first l) 5) 0.6))) (increase_yspd (rest l))))
  )

(define (move_pt l pt_l)
  (if (empty? l) l (append (list (make-posn (+ (list-ref (first pt_l) 4) (posn-x (first l))) (- (posn-y (first l)) (list-ref (first pt_l) 5) ))) (move_pt (rest l) (rest pt_l))))
  )

(define (move_powerups l)
  (if (empty? l) l (append (list (make-posn (posn-x (first l)) (+ (posn-y (first l)) 5))) (move_powerups (rest l))))
  )

(define (damage l i a)
  (append (take l i) (list (list (list-ref (list-ref l i) 0) (list-ref (list-ref l i) 1) (list-ref (list-ref l i) 2) (- (list-ref (list-ref l i) 3) a) (list-ref (list-ref l i) 4) (list-ref (list-ref l i) 5))) (rest (drop l i)))
  )

(define (render ws)
  (place-images
   (append
    (list
     player_image
     (if (string=? ball_mode "normal") ball_image fire_ball_image)
     shield_image
     )
    powerup_images
    ball_count_images
    pt_images
    fr_images
    block_images
    )
   (append
    (list
     (make-posn p_x p_y)
     (make-posn b_x b_y)
     (make-posn (/ scn_w 2) (- scn_h 50))
     )
    powerup_positions
    ball_count_positions
    pt_positions
    fr_positions
    block_positions)
   (place-image background (/ scn_w 2) (/ scn_h 2) scn)
   )
  )

(define (key-handler ws ke)
  (cond
    ((key=? ke "right") (set! r_key 1))
    ((key=? ke "left") (set! l_key 1))
    )
  )

(define (release-handler ws ke)
  (cond
    ((key=? ke "right") (set! r_key 0))
    ((key=? ke "left") (set! l_key 0))
    )
  )

(define (mouse-handler ws x y me)
  ws
  )

(define (step ws)
  (begin
    
    ;Checking collisions between ball and blocks
    (let loop_j ((j 0))
      (if (< j block_rows)
          (let loop_i ((i 0))
            (if (and (< i block_cols) (< (+ i (* j block_cols)) (length block_list)))
                (let ((index (+ i (* j block_cols))))
                  (let ((block_x (list-ref (list-ref block_list index) 1))
                        (block_y (list-ref (list-ref block_list index) 2)))
                
                    (if (check-hit b_x b_y block_x block_y block_w block_h) ; If ball hits the block
                        (begin
                          (set! block_list (damage block_list index 1))
                          (if (string=? "normal" ball_mode)
                              (cond
                                ((< (+ 1 (- block_x (/ block_w 2))) b_x (- (+ block_x (/ block_w 2)) 1))
                                 (set! b_dir_v (* -1 b_dir_v))
                                 )
                                ((< (- 1 (+ block_y (/ block_h 2))) b_y (+ (- block_y (/ block_h 2)) 1))
                                 (set! b_dir_h (* -1 b_dir_h))
                                 )
                                (else (cond
                                        ((and (< b_x block_x) (< b_y block_y)) (set!-values (b_dir_h b_dir_v) (values -1 1))) ; iI
                                        ((and (> b_x block_x) (< b_y block_y)) (set!-values (b_dir_h b_dir_v) (values 1 1))) ; Iİ
                                        ((and (< b_x block_x) (> b_y block_y)) (set!-values (b_dir_h b_dir_v) (values -1 -1))) ; !I
                                        ((and (> b_x block_x) (> b_y block_y)) (set!-values (b_dir_h b_dir_v) (values 1 -1))) ; I!
                                        ))
                                ) void)
                          (if (<= (list-ref (list-ref block_list index) 3) 0)
                              (begin
                               
                                (if (not (empty? (list-ref (list-ref block_list index) 5)))
                                    (begin
                                      (set! powerup_list (add-powerup block_x block_y (list-ref (list-ref block_list index) 5)))
                                      (set! powerup_images
                                            (let ((lst empty))
                                              (let loop ((i 0))
                                                (if (< i (length powerup_list)) (append (list (cond
                                                                                                ((string=? "+ball" (list-ref (list-ref powerup_list i) 2)) +ball_image)
                                                                                                ((string=? "shield" (list-ref (list-ref powerup_list i) 2)) shield_pu_image)
                                                                                                ((string=? "fireball" (list-ref (list-ref powerup_list i) 2)) fire_ball_image)
                                                                                                )) (loop (add1 i))) lst)
                                                )
                                              )
                                            )

                                      (set! powerup_positions
                                            (let ((lst empty))
                                              (let loop ((i 0))
                                                (if (< i (length powerup_list)) (append (list (make-posn (list-ref (list-ref powerup_list i) 0) (list-ref (list-ref powerup_list i) 1))) (loop (add1 i))) lst)
                                                )
                                              )
                                            ))
                                    void
                                    )

                                (begin
                                  (set! pt_list (add-particle block_x block_y 10))
                                  (set! pt_images
                                        (let ((lst pt_images))
                                          (let loop ((i 0))
                                            (if (< i 10) (append (loop (add1 i)) (list (rotate (random 360) (rectangle 5 5 255  (if (color=? (get-pixel-color 10 10 (list-ref (list-ref block_list index) 0)) "white") "white" pu_block_color)))) ) lst)
                                            )
                                          )
                                        )

                                  (set! pt_positions
                                        (let ((lst empty))
                                          (let loop ((i 0))
                                            (if (< i (length pt_list)) (append (list (make-posn (list-ref (list-ref pt_list i) 0) (list-ref (list-ref pt_list i) 1))) (loop (add1 i))) lst)
                                            )
                                          )
                                        ))
                          
                                (set! block_count (sub1 block_count))
                                (set! score (add1 score))
                                (set! block_list (destroy block_list index))
                          
                          
                                
                                )
                              void))
                        (loop_i (add1 i)))
                    )
                  )
                (loop_j (add1 j))
                )
            ) void)
      )

    ;Checking the collision between ball and the player

    (if (check-hit b_x b_y p_x p_y p_w p_h)
        (cond
          ((< (+ 0 (- p_x (/ p_w 2))) b_x (- (+ p_x (/ p_w 2)) 0))
           (set! b_dir_v (* -1 b_dir_v))
           )
          ((< (- 0 (+ p_y (/ p_h 2))) b_y (+ (- p_y (/ p_h 2)) 0))
           (set! b_dir_h (* -1 b_dir_h))
           )
          (else (cond
                  ((and (< b_x p_x) (< b_y p_y)) (set!-values (b_dir_h b_dir_v) (values -1 1))) ; iI
                  ((and (> b_x p_x) (< b_y p_y)) (set!-values (b_dir_h b_dir_v) (values 1 1))) ; Iİ
                  ((and (< b_x p_x) (> b_y p_y)) (set!-values (b_dir_h b_dir_v) (values -1 -1))) ; !I
                  ((and (> b_x p_x) (> b_y p_y)) (set!-values (b_dir_h b_dir_v) (values 1 -1))) ; I!
                  ))
          ) void)

    ;Checking the collisions between powerups and the player
    (let loop ((i 0))
      (if (< i (length powerup_list)) (if (check-hit (posn-x (list-ref powerup_positions i)) (posn-y (list-ref powerup_positions i)) p_x p_y p_w p_h)
                                          (cond
                                            ((string=? (list-ref (list-ref powerup_list i) 2) "+ball") (begin
                                                                                                         (set! ball_count (add1 ball_count))
                                                                                                         (set! powerup_images (destroy powerup_images i))
                                                                                                         (set! powerup_positions (destroy powerup_positions i))
                                                                                                         (set! powerup_list (destroy powerup_list i))
                                                                                                         ))
                                            ((string=? (list-ref (list-ref powerup_list i) 2) "shield") (begin
                                                                                                          (set! shield #t)
                                                                                                          (set! powerup_images (destroy powerup_images i))
                                                                                                          (set! powerup_positions (destroy powerup_positions i))
                                                                                                          (set! powerup_list (destroy powerup_list i))
                                                                                                          ))
                                            ((string=? (list-ref (list-ref powerup_list i) 2) "fireball") (begin
                                                                                                            (set! ball_mode "fire")
                                                                                                            (set! powerup_images (destroy powerup_images i))
                                                                                                            (set! powerup_positions (destroy powerup_positions i))
                                                                                                            (set! powerup_list (destroy powerup_list i))
                                                                                                            ))
                                            ) (loop (add1 i))) void)
      )
    
    ;Stopping the ball from moving when it is out of screen

    (if (or (> b_y (+ b_r scn_h)) (< b_y (* -1 b_r))) (if (= ball_count 1)
                                                          (set!-values
                                                           (shield ball_mode ball_count block_count b_x b_y b_dir_v b_dir_h p_x p_y p_xto block_images block_positions pt_list pt_images pt_positions ball_count_images ball_count_positions block_list powerup_list powerup_images powerup_positions)
                                                           (values #f "normal" 3 (* block_cols block_rows) (/ scn_w 2) (- scn_h 70 b_r (/ p_h 2)) 1 1 (/ scn_w 2) (- scn_h 70) (/ scn_w 2) empty empty empty empty empty empty empty
                                                                   (let ((lst empty))
                                                                     (let loop_j ((j 0))
                                                                       (if (< j block_rows)
                                                                           (let loop_i ((i 0))
                                                                             (let ((pu (list-ref powerups (random (length powerups)))))
                                                                               (if (< i block_cols) (append (list (list (if (empty? pu) regular_block_image powerup_block_image) (+ (* 0.75 block_w) (* i block_w 1.05)) (+ (* 1 block_h) (* j 1.05 block_h)) 1 1 pu)) (loop_i (add1 i))) (loop_j (add1 j)))
                                                                             )) lst)
                                                                       )) empty empty empty
                                                                   ))
                                                          (set!-values
                                                           (shield ball_mode ball_count b_x b_y b_dir_v b_dir_h p_x p_y p_xto powerup_list powerup_images powerup_positions)
                                                           (values #f "normal" (sub1 ball_count) (/ scn_w 2) (- scn_h 70 b_r (/ p_h 2)) 1 1 (/ scn_w 2) (- scn_h 70) (/ scn_w 2) empty empty empty)))void)
    
    ;Updating the components of ball's speed according the angle
    
    (set!-values (b_xspd b_yspd) (values (* b_spd (cos (degtorad angle))) (* b_spd (sin (degtorad angle)))))
    
    ;Player Movement:
    
    (set! p_dir (- r_key l_key))
    (set! p_xto (if
                 (< (/ p_w 2) (+ p_xto (* p_spd p_dir))  (- scn_w (/ p_w 2)))
                 (+ p_xto (* p_spd p_dir))
                 p_xto))
    (set! p_x (+ p_x (/ (- p_xto p_x) smooth)))

    ;Ball Movement:
    
    ;Horizontal Ball Collisions:

    (if (= b_dir_h 1) ; If the ball is moving rightwards
        (if (> b_x (- scn_w b_r)) ; If it hits the right wall of the screen
            (set! b_dir_h -1)
            (set! b_x (+ b_x b_xspd))
            )
        (if (< b_x b_r) ; If it hits the left wall of the screen
            (set! b_dir_h 1)
            (set! b_x (- b_x b_xspd))
            ))
    
    ;Vertical Ball Collisions:

    (if (= b_dir_v -1) ; If the ball is moving downwards
        (if (and shield (> b_y (- scn_h 50 b_r))) ; If it hits the top of the screen
            (set!-values (b_dir_v shield) (values 1 #f))
            (set! b_y (+ b_y b_yspd))
            )
        (if (< b_y (* 2 b_r)) ; If it hits the top of the screen
            (set! b_dir_v -1)
            (set! b_y (- b_y b_yspd))
            ))

    (set! block_images
          (let ((lst empty))
            (let loop ((i 0))
              (if (< i block_count) (append (list (list-ref (list-ref block_list i) 0)) (loop (add1 i))) lst)
              )))

    (set! block_positions
          (let ((lst empty))
            (let loop ((i 0))
              (if (< i block_count) (append (list (make-posn (list-ref (list-ref block_list i) 1) (list-ref (list-ref block_list i) 2))) (loop (add1 i))) lst)
              )))

    (set! ball_count_images
          (let ((lst empty))
            (let loop ((i 0))
              (if (< i ball_count) (append (list (circle 7 255 "white")) (loop (add1 i))) lst)
              )))

    (set! ball_count_positions
          (let ((lst empty))
            (let loop ((i 0))
              (if (< i ball_count) (append (list (make-posn (+ 20 (* i 1.1 14)) (- scn_h 20))) (loop (add1 i))) lst)
              )))

    (set! pt_list (decrease_life pt_list pt_positions))
    (set! pt_list (decrease_yspd pt_list))
    (set! pt_positions (move_pt pt_positions pt_list))

    (set! fr_list (decrease_life fr_list fr_positions))
    (set! fr_list (increase_yspd fr_list))
    (set! fr_positions (move_pt fr_positions fr_list))

    (set! powerup_positions (move_powerups powerup_positions))
    (set! powerup_list (decrease_y powerup_list powerup_positions))

    (let loop ((i 0))
      (if (< i (length pt_list)) (if (< (list-ref (list-ref pt_list i) 2) 0) (begin
                                                                               (set! pt_images (destroy pt_images i))
                                                                               (set! pt_positions (destroy pt_positions i))
                                                                               (set! pt_list (destroy pt_list i))
                                                                               )(loop (add1 i)))
          void)
      )

    (let loop ((i 0))
      (if (< i (length fr_list)) (if (< (list-ref (list-ref fr_list i) 2) 0) (begin
                                                                               (set! fr_images (destroy fr_images i))
                                                                               (set! fr_positions (destroy fr_positions i))
                                                                               (set! fr_list (destroy fr_list i))
                                                                               )(loop (add1 i)))
          void)
      )

    (if (and (string=? ball_mode "fire") (< countdown 0)) (begin
                          (set! fr_list (add-fire b_x b_y 4))
                          (set! fr_images
                                (let ((lst empty))
                                  (let loop ((i 0))
                                    (if (< i (length fr_list)) (append (list (rectangle 2 2 255 "orange")) (loop (add1 i))) lst)
                                    )
                                  )
                                )

                          (set! fr_positions
                                (let ((lst empty))
                                  (let loop ((i 0))
                                    (if (< i (length fr_list)) (append (list (make-posn (list-ref (list-ref fr_list i) 0) (list-ref (list-ref fr_list i) 1))) (loop (add1 i))) lst)
                                    )
                                  )
                                )
                          (set! countdown CD)
                          )
        (set! countdown (- countdown 1.5)))

    (if (> (length fr_list) 15) (begin
                                  (set! fr_images (take fr_images 15))
                                  (set! fr_positions (take fr_positions 15))
                                  (set! fr_list (take fr_list 15))
                                  ) void)

    (if (> (length pt_list) 30) (begin
                                  (set! pt_images (take pt_images 30))
                                  (set! pt_positions (take pt_positions 30))
                                  (set! pt_list (take pt_list 30))
                                  ) void)

    (set! shield_image (if shield (overlay (rectangle (- scn_w 5) 4 100 "white") (rectangle scn_w 6 255 "mediumslateblue")) (circle 0 0 "black")))
    
    )
  )

(big-bang 0
          (on-tick step)
          (on-mouse mouse-handler)
          (on-draw render)
          (on-release release-handler)
          (on-key key-handler)
          )