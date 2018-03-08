#lang racket

(require 2htdp/image)
(require 2htdp/universe)
(require lang/posn)
(require racket/list)

(define-values
  (rows cols g_w g_h grids grid_images grid_posns game? m_color rev_color fg_color bg_color mine_count n_list grid_values rev_list shown_list)
  (values 20 20 30 30 empty empty empty #t (color 250 50 100 255) (color 0 0 0 180) "white" "black" 80 empty empty empty empty))


(define-values
  (scn_w scn_h)
  (values (* g_w cols) (* g_h rows)))


(define scn (rectangle scn_w scn_h 209 bg_color))
(define grid_image (overlay
                    (rectangle g_w g_h "outline" fg_color)
                    (rectangle g_w g_h 209 bg_color)
                    (rectangle g_w g_h 255 fg_color)
                    ))
(define m_image
  (overlay
   (circle (/ g_w 6) "solid" m_color)
   (let loop ((i 0))
     (if (< i 8) (place-image (circle (/ g_w 30) 255 m_color) (+ (/ g_w 5) (* (/ g_w 6) (cos (* i (/ pi 4))))) (+ (/ g_h 5) (* (/ g_h 6) (sin (* i (/ pi 4))))) (loop (add1 i))) (rectangle (/ (* g_w 2) 5) (/ (* g_h 2) 5) 0 fg_color))
     )
   (rectangle g_w g_h "outline" fg_color))
  )

(define (grid_image_value v)
  (overlay
   (text/font (if (number? v) (if (= v 0) "" (number->string v)) "") (round (/ g_w 2.5)) fg_color "Gill Sans" 'swiss 'normal 'bold #f)
   (rectangle g_w g_h "outline" fg_color)
   (rectangle g_w g_h "solid" rev_color)
   (rectangle g_w g_h 255 "white"))
  )

(define (index i j)
  (+ i (* j cols))
  )

(define (sum l)
  (if (empty? l) 0 (+ (first l) (sum (rest l))))
  )

(define (exists? n l)
  (let loop ((i 0))
    (if (< i (length l)) (if (equal? (list-ref l i) n) #true (loop (add1 i))) #f)    
    )
  )

(define (init_grids)
  (set! grids (let loop_j ((j 0))
                (if (< j rows) (let loop_i ((i 0))
                                 (if (< i cols) (let ((index (+ i (* j cols))))
                                                  (append (list (list (cond
                                                                        ((equal? (list-ref grid_values index) "m") (if game? grid_image m_image))
                                                                        (else (if (or (exists? (list i j) shown_list) (exists? (list i j) rev_list))
                                                                                  (grid_image_value (list-ref grid_values index))
                                                                                  grid_image
                                                                                  ))
                                                                        ) (+ (* i g_w) (/ g_w 2)) (+ (* j g_h) (/ g_h 2)))) (loop_i (add1 i)))
                                                  ) (loop_j (add1 j)))
                                 ) empty)
                )))

  
(define (init_grid_images)
  (set! grid_images (let loop ((i 0))
                      (if (< i (length grids)) (append (list (list-ref (list-ref grids i) 0)) (loop (add1 i))) empty)
                      )))
(define (init_grid_posns)
  (set! grid_posns (let loop ((i 0))
                     (if (< i (length grids)) (append (list (make-posn (list-ref (list-ref grids i) 1) (list-ref (list-ref grids i) 2))) (loop (add1 i))) empty)
                     ))
  )


(define (init_game)
  (begin
    (set!-values (rev_list shown_list game?) (values empty empty #t))
    (set! grid_values
          (let loop ((i 0))
            (if (< i (* cols rows)) (append (list 0) (loop (add1 i))) empty)
            ))

    (let loop ((i 0))
      (let ((num (- (random 1 (+ 1 (* cols rows))) 1)))
        (if (< i mine_count) (if (equal? (list-ref grid_values num) "m") (loop i) (begin(set! grid_values (list-set grid_values num "m")) (loop (add1 i)))) void)
        )
      )

    (set! n_list
          (let loop ((i 0))
            (if (< i (* cols rows)) (append (list (list 0 0 0 0 0 0 0 0)) (loop (add1 i))) empty)
            ))

    (let loop_j ((j 0))
      (if (< j rows) (let loop_i ((i 0))
                       (if (< i cols) (begin
                                        (if (not (equal? (list-ref grid_values (index i j)) "m"))
                                            (begin
                                              (if (and (>= (- i 1) 0)          (>= (- j 1) 0))          (set! n_list (list-set n_list (index i j) (list-set (list-ref n_list (index i j)) 0 (if (equal? (list-ref grid_values (index (- i 1) (- j 1))) "m") 1 0)))) void)
                                              (if                              (>= (- j 1) 0)           (set! n_list (list-set n_list (index i j) (list-set (list-ref n_list (index i j)) 1 (if (equal? (list-ref grid_values (index    i    (- j 1))) "m") 1 0)))) void)
                                              (if (and (<= (+ i 1) (- cols 1)) (>= (- j 1) 0))          (set! n_list (list-set n_list (index i j) (list-set (list-ref n_list (index i j)) 2 (if (equal? (list-ref grid_values (index (+ i 1) (- j 1))) "m") 1 0)))) void)
                                              (if      (<= (+ i 1) (- cols 1))                          (set! n_list (list-set n_list (index i j) (list-set (list-ref n_list (index i j)) 3 (if (equal? (list-ref grid_values (index (+ i 1)    j   )) "m") 1 0)))) void)
                                              (if (and (<= (+ i 1) (- cols 1)) (<= (+ j 1) (- rows 1))) (set! n_list (list-set n_list (index i j) (list-set (list-ref n_list (index i j)) 4 (if (equal? (list-ref grid_values (index (+ i 1) (+ j 1))) "m") 1 0)))) void)
                                              (if                              (<= (+ j 1) (- rows 1))  (set! n_list (list-set n_list (index i j) (list-set (list-ref n_list (index i j)) 5 (if (equal? (list-ref grid_values (index    i    (+ j 1))) "m") 1 0)))) void)
                                              (if (and (>= (- i 1) 0)          (<= (+ j 1) (- rows 1))) (set! n_list (list-set n_list (index i j) (list-set (list-ref n_list (index i j)) 6 (if (equal? (list-ref grid_values (index (- i 1) (+ j 1))) "m") 1 0)))) void)
                                              (if      (>= (- i 1) 0)                                   (set! n_list (list-set n_list (index i j) (list-set (list-ref n_list (index i j)) 7 (if (equal? (list-ref grid_values (index (- i 1)    j   )) "m") 1 0)))) void)
                                              (set! grid_values (list-set grid_values (index i j) (sum (list-ref n_list (index i j)))))) void)
                                        (loop_i (add1 i))
                                        ) (loop_j (add1 j)))
                       ) void)
      )
    (init_grids)
    (init_grid_images)
    (init_grid_posns)
    
    ))
(define (find-index x y)
  (list (floor (/ x g_w)) (floor (/ y g_h)))
  )



(init_game)


(define (check-4-sides i j)
  (begin
    (if (not (exists? (list i j) rev_list)) (set! rev_list (append (list (list i j)) rev_list)) void)
    (if (>= (- i 1) 0) (if (and (equal? (list-ref grid_values (index (- i 1) j)) 0) (not (exists? (list (- i 1) j) rev_list)))
                           (check-4-sides (- i 1) j)
                           void) void)
    (if (>= (- j 1) 0) (if (and (equal? (list-ref grid_values (index i (- j 1))) 0) (not (exists? (list i (- j 1)) rev_list)))
                           (check-4-sides i (- j 1))
                           void) void)
    (if (<= (+ i 1) (- cols 1)) (if (and (equal? (list-ref grid_values (index (+ i 1) j)) 0) (not (exists? (list (+ i 1) j) rev_list)))
                                    (check-4-sides (+ i 1) j)
                                    void) void)
    (if (<= (+ j 1) (- rows 1)) (if (and (equal? (list-ref grid_values (index i (+ j 1))) 0) (not (exists? (list i (+ j 1)) rev_list)))
                                    (check-4-sides i ( + j 1))
                                    void) void)
    
    )
  )

(define (reveal i j)
  (let ((v (list-ref grid_values (index i j))))
    (if (equal? v "m")
        (set! game? #f)
        
        (if (not (equal? v 0))
            (if (not (exists? (list i j) shown_list)) (set! shown_list (append (list (list i j)) shown_list)) void)
            (check-4-sides i j))
        )
    )
  )

(define (show_n)
  (let loop ((n 0))
    (if (< n (length rev_list))
        (let ((i (list-ref (list-ref rev_list n) 0))
              (j (list-ref (list-ref rev_list n) 1)))
          (begin
            (if (and (>= (- i 1) 0)          (>= (- j 1) 0))          (if (and (not (equal? (list-ref grid_values (index (- i 1) (- j 1))) 0)) (not (exists? (list (- i 1) (- j 1)) shown_list))) (set! shown_list (append (list (list (- i 1) (- j 1))) shown_list)) void) void)
            (if                              (>= (- j 1) 0)           (if (and (not (equal? (list-ref grid_values (index    i    (- j 1))) 0)) (not (exists? (list    i    (- j 1)) shown_list))) (set! shown_list (append (list (list    i    (- j 1))) shown_list)) void) void)
            (if (and (<= (+ i 1) (- cols 1)) (>= (- j 1) 0))          (if (and (not (equal? (list-ref grid_values (index (+ i 1) (- j 1))) 0)) (not (exists? (list (+ i 1) (- j 1)) shown_list))) (set! shown_list (append (list (list (+ i 1) (- j 1))) shown_list)) void) void)
            (if      (<= (+ i 1) (- cols 1))                          (if (and (not (equal? (list-ref grid_values (index (+ i 1)    j   )) 0)) (not (exists? (list (+ i 1)    j   ) shown_list))) (set! shown_list (append (list (list (+ i 1)    j   )) shown_list)) void) void)
            (if (and (<= (+ i 1) (- cols 1)) (<= (+ j 1) (- rows 1))) (if (and (not (equal? (list-ref grid_values (index (+ i 1) (+ j 1))) 0)) (not (exists? (list (+ i 1) (+ j 1)) shown_list))) (set! shown_list (append (list (list (+ i 1) (+ j 1))) shown_list)) void) void)
            (if                              (<= (+ j 1) (- rows 1))  (if (and (not (equal? (list-ref grid_values (index    i    (+ j 1))) 0)) (not (exists? (list    i    (+ j 1)) shown_list))) (set! shown_list (append (list (list    i    (+ j 1))) shown_list)) void) void)
            (if (and (>= (- i 1) 0)          (<= (+ j 1) (- rows 1))) (if (and (not (equal? (list-ref grid_values (index (- i 1) (+ j 1))) 0)) (not (exists? (list (- i 1) (+ j 1)) shown_list))) (set! shown_list (append (list (list (- i 1) (+ j 1))) shown_list)) void) void)
            (if      (>= (- i 1) 0)                                   (if (and (not (equal? (list-ref grid_values (index (- i 1)    j   )) 0)) (not (exists? (list (- i 1)    j   ) shown_list))) (set! shown_list (append (list (list (- i 1)    j   )) shown_list)) void) void)
            (loop (add1 n))
            ))
        void)
    )
  )



(define (render ws)
  (place-images
   (append
    (list
     
     )
    grid_images)
   (append
    (list

     )
    grid_posns)
   scn)
  )

(define (key-handler ws ke)
  (if (and (key=? "r" ke) (not game?)) (init_game) void))
  

(define (release-handler ws ke)
  ws
  )

(define (mouse-handler ws x y me)
  (let ((i (list-ref (find-index x y) 0))
        (j (list-ref (find-index x y) 1)))
    (if (and game? (string=? me "button-down") (< 0 x scn_w) (< 0 y scn_h))
        (begin
          (reveal i j)
          (show_n)
          (init_grids)
          (init_grid_images)
          ) void)
    )
  )

(define (step ws)
  ws
  )

(big-bang 0
          (on-tick step)
          (on-mouse mouse-handler)
          (on-draw render)
          (on-release release-handler)
          (on-key key-handler)
          )