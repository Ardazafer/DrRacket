#lang racket

#|

Recursive Backtracking Algorithm:

    1-Make the initial cell the current cell and mark it as visited

    2-While there are unvisited cells

        1-If the current cell has any neighbours which have not been visited

            1-Choose randomly one of the unvisited neighbours
            2-Push the current cell to the stack
            3-Remove the wall between the current cell and the chosen cell
            4-Make the chosen cell the current cell and mark it as visited

        2-Else if stack is not empty

            1-Pop a cell from the stack
            2-Make it the current cell


|#

(require 2htdp/image)
(require 2htdp/universe)
(require lang/posn)


(define-values (w h scl bg_color) (values 600 600 20 (color 0 0 0 180)))
(define-values (cols rows) (values (/ w scl) (/ h scl)))
(define scn (rectangle w h "solid" bg_color))
(define index_list empty)
(define visited_list empty)
(define stack empty)
(define wall_list empty)
(define neighbours_visited_list '(5 5 5 5))
(define len (* cols rows))
(define current 0)
(define next 0)

(let loop_indexes ((i 0)) (cond
                            ((= i len) void)
                            (else (begin
                                    (set! index_list (append index_list (list i)))
                                    (loop_indexes (add1 i))
                                    ))
                            ))

(let loop_visited ((i 0)) (cond
                            ((= i len) void)
                            (else (begin
                                    (set! visited_list (append visited_list (list 0)))
                                    (loop_visited (add1 i))
                                    ))
                            ))

(let loop_walls ((i 0)) (cond
                          ((= i len) void)
                          (else (begin
                                  (set! wall_list (append wall_list (list (list 1 1 1 1))))
                                  (loop_walls (add1 i))
                                  ))
                          ))

(define (exists? n l)
  (let loop ((i 0))
    (if (< i (length l)) (if (= (list-ref l i) n) #true (loop (add1 i))) #f)    
    )
  )


(define (remove_walls c n)
  (begin
    (set! wall_list (list-set wall_list c (list-set (list-ref wall_list c) n 0)))
    (set! wall_list (list-set wall_list (cond
                                          ((= next 0) (- current cols))
                                          ((= next 1) (+ current 1))
                                          ((= next 2) (+ current cols))
                                          ((= next 3) (- current 1))
                                          (else void)
                                          ) (list-set (list-ref wall_list (cond
                                                                            ((= next 0) (- current cols))
                                                                            ((= next 1) (+ current 1))
                                                                            ((= next 2) (+ current cols))
                                                                            ((= next 3) (- current 1))
                                                                            (else void))) (modulo (+ n 2) 4) 0)))
    )
  )

(define (pick_next)
  (let loop ((x (random 4)))
    (if (exists? 0 neighbours_visited_list) (if (= (list-ref neighbours_visited_list x) 0) x (loop (random 4))) current))
  )

(define (mark_visited pos)
  (set! visited_list (list-set visited_list pos 1))
  )
(mark_visited current)

(define (show c r)
  (cond
    ((= r 0) scn)
    (else (let loop_r ((j 0)) (cond
                                ((= j r) scn)
                                (else (let loop_c ((i 0)) (cond
                                                            ((= i c) (loop_r (add1 j)))
                                                            (else (begin

                                                                    (if (= (list-ref index_list (+ i (* j rows))) current) (begin
                                                                                                                             (if (>= (- i 1) 0)        (set! neighbours_visited_list (list-set neighbours_visited_list 3 (list-ref visited_list (+ (- i 1) (* j rows))))) (set! neighbours_visited_list (list-set neighbours_visited_list 3 -1)))
                                                                                                                             (if (<= (+ i 1) (- c 1))  (set! neighbours_visited_list (list-set neighbours_visited_list 1 (list-ref visited_list (+ (+ i 1) (* j rows))))) (set! neighbours_visited_list (list-set neighbours_visited_list 1 -1)))
                                                                                                                             (if (>= (- j 1) 0)        (set! neighbours_visited_list (list-set neighbours_visited_list 0 (list-ref visited_list (+ i (* (- j 1) rows))))) (set! neighbours_visited_list (list-set neighbours_visited_list 0 -1)))
                                                                                                                             (if (<= (+ j 1) (- r 1))  (set! neighbours_visited_list (list-set neighbours_visited_list 2 (list-ref visited_list (+ i (* (+ j 1) rows))))) (set! neighbours_visited_list (list-set neighbours_visited_list 2 -1)))
                                                                                                                             ) void)

                                                                    (place-images

                                                                   
                                                                     (list
                                                                    
                                                                      (rectangle scl 2 "solid" (color 255 255 255 (if (= (list-ref (list-ref wall_list (+ i (* j rows))) 0) 1) 255 0))) ; TOP
                                                                      (rectangle scl 2 "solid" (color 255 255 255 (if (= (list-ref (list-ref wall_list (+ i (* j rows))) 2) 1) 255 0))) ; BOTTOM
                                                                      (rectangle 2 scl "solid" (color 255 255 255 (if (= (list-ref (list-ref wall_list (+ i (* j rows))) 3) 1) 255 0))) ; LEFT
                                                                      (rectangle 2 scl "solid" (color 255 255 255 (if (= (list-ref (list-ref wall_list (+ i (* j rows))) 1) 1) 255 0))) ; RIGHT
                                                                      (rectangle scl scl "solid" (if (= (list-ref index_list (+ i (* j rows))) current) (color 200 0 200 255) (color 180 0 150 (if (= (list-ref visited_list (+ i (* j rows))) 1) 255 0))))

                                                                      )


                                                                     (list
                                                                    
                                                                      (make-posn (+ (/ scl 2) (* i scl))  (* j scl))               ; TOP
                                                                      (make-posn (+ (/ scl 2) (* i scl))  (+ scl (* j scl)))       ; BOTTOM
                                                                      (make-posn (* i scl)                (+ (/ scl 2) (* j scl))) ; LEFT
                                                                      (make-posn (+ scl (* i scl))        (+ (/ scl 2) (* j scl))) ; RIGHT
                                                                      (make-posn (+ (/ scl 2) (* i scl))  (+ (/ scl 2) (* j scl)))

                                                                      ) (loop_c (add1 i)))))
                                                            )))
                                ))
          )))

(define (render ws)
  (show cols rows)
  )

(define (step ws)
  (if (exists? 0 visited_list) (let loop ((x 0)) (if
                                                  (exists? 0 neighbours_visited_list)
                                                  (begin
                                            
                                                    (set! next (pick_next))

                                                    (set! stack (append stack (list current)))

                                                    (remove_walls current next)
                                            
                                                    (cond
                                                      ((= next 0) (set! current (- current cols)))
                                                      ((= next 1) (set! current (+ current 1)))
                                                      ((= next 2) (set! current (+ current cols)))
                                                      ((= next 3) (set! current (- current 1)))
                                                      (else void)
                                                      )
                                                    (mark_visited current)
                                                    loop
                                                    )
                                                  (begin
                                                    (set! current (last stack))
                                                    (set! stack (drop-right stack 1))
                                                    )))
      void)
  )

(big-bang 0
          (on-tick step )
          (on-draw render)
          )