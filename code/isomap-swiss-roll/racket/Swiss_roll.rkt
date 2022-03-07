#lang racket
;(random-seed 2)
(require plot)
(struct ThreeDsPoint ([x #:mutable] [y #:mutable] [z #:mutable] [edges #:mutable] [distance_l #:mutable] ))

;use python gaussian distribution mean 0 sd 1
(define g1 (vector 0.69367316  0.45887631 -0.27201727 -0.50100811 -0.55366984  0.38389635
 -0.7189839  -0.07661105 -1.45008544 -1.29588942 ))
(define g11 (vector -0.58082813 -0.41185415  1.50283252  1.66644126  0.00545085  1.04662115
 -1.88699253  0.56400277 -1.37043874 -1.50664345 ))

(define g2 (vector -0.27157859  0.49989246 -0.03840236 -1.60477629 -1.45861074  0.45220839
 -1.23513928 -0.00335019  0.69709912  0.93882459 ))
(define g22 (vector 1.11434269 -0.86925474  0.92143418 -1.69024199  1.25249645  0.5624198
  0.53800368  0.80776239  0.57904594  0.42310416))

(define g3 (vector 0.87974584 -0.63712157  1.23389162 -0.0768133   1.21875351 -1.00988432
  0.50673822  1.65785988  1.74605434  0.17694 ))
(define g33 (vector -0.09347606 -1.9029108  -1.93904418 -1.11373776 -0.07953666 -0.00429755
 -0.0761124  -0.80764769  0.1571795   1.87273461 ))

(define g4 (vector -1.7318661   1.69693952  1.32026145 -0.57885347 -1.83259047 -0.48937073
 -1.16560047 -1.83621524 -0.62312556 -0.08068919))
(define g44 (vector 0.8281635   0.07978825  0.44895481  0.98963405  0.42269139 -0.78388047
 -1.69610369 -0.29465518 -0.6721648   1.61940052))

(define (randGaussian_7.5 v)
(+ v 7.5))
(define (randGaussian_12.5 v)
(+ v 12.5))

(define (cosTransform v)
(* v(cos v)))

(define (sinTransform v)
(* v(sin v)))


; centers = [7.5 7.5; 7.5 12.5; 12.5 7.5; 12.5 12.5]
(define g1_x7.5 (vector-map randGaussian_7.5 g1 ))
(define g1_y7.5 (vector-map randGaussian_7.5 g11 ))

(define g2_x7.5 (vector-map randGaussian_7.5 g2 ))
(define g2_y12.5 (vector-map randGaussian_12.5 g22 ))

(define g3_x12.5 (vector-map randGaussian_12.5 g3 ))
(define g3_y7.5 (vector-map randGaussian_7.5 g33 ))

(define g4_x12.5 (vector-map randGaussian_12.5 g4 ))
(define g4_y12.5 (vector-map randGaussian_12.5 g44 ))


(parameterize ([plot-width    150]
                 [plot-height   150]
                 [plot-x-label  #f]
                 [plot-y-label  #f])
   
    (list (plot (points (map vector (vector->list g1_x7.5) (vector->list g1_y7.5))
                        #:x-min 0 #:x-max 20
                        #:y-min 0 #:y-max 20))
          (plot (points (map vector (vector->list g2_x7.5) (vector->list g2_y12.5))
                        #:x-min 0 #:x-max 20
                        #:y-min 0 #:y-max 20))
          (plot (points (map vector (vector->list g3_x12.5) (vector->list g3_y7.5))
                        #:x-min 0 #:x-max 20
                        #:y-min 0 #:y-max 20))
           (plot (points (map vector (vector->list g4_x12.5) (vector->list g4_y12.5))
                        #:x-min 0 #:x-max 20
                        #:y-min 0 #:y-max 20))))

(define g1_x7.5_cos (vector-map cosTransform g1_x7.5))
(define g1_x7.5_sin (vector-map sinTransform g1_x7.5))
(define g2_x7.5_cos (vector-map cosTransform g2_x7.5))
(define g2_x7.5_sin (vector-map sinTransform g2_x7.5))
(define g3_x12.5_cos (vector-map cosTransform g3_x12.5 ))
(define g3_x12.5_sin (vector-map sinTransform g3_x12.5 ))
(define g4_x12.5_cos (vector-map cosTransform g4_x12.5 ))
(define g4_x12.5_sin (vector-map sinTransform g4_x12.5 ))


(plot3d (points3d (map vector (vector->list g1_x7.5_cos) (vector->list g1_y7.5) (vector->list g1_x7.5_sin))
                  #:sym 'dot #:color 'red)     #:altitude 25)
     
(plot3d (points3d (map vector (vector->list g2_x7.5_cos) (vector->list g2_y12.5) (vector->list g2_x7.5_sin))
                  #:sym 'dot #:color 'blue) #:altitude 25)

(plot3d (points3d (map vector (vector->list g3_x12.5_cos) (vector->list g3_y7.5) (vector->list g3_x12.5_sin))
                  #:sym 'dot #:color 'green) #:altitude 25)

(plot3d (points3d (map vector (vector->list g4_x12.5_cos) (vector->list g4_y12.5) (vector->list g4_x12.5_sin))
                  #:sym 'dot #:color 'black) #:altitude 25)


(define group1 (ThreeDsPoint  g1_x7.5_cos  g1_y7.5  g1_x7.5_sin [] []))
(define group2 (ThreeDsPoint  g2_x7.5_cos  g2_y12.5  g2_x7.5_sin [] []))
(define group3 (ThreeDsPoint  g3_x12.5_cos  g3_y7.5  g3_x12.5_sin  [] []))
(define group4 (ThreeDsPoint   g4_x12.5_cos  g4_y12.5  g4_x12.5_sin  [] []))
(define point-list (list group1 group2 group3 group4))

;define our own way to calculate Euclidean distance
(define (dist a b)
  sqrt(+(expt((-(ThreeDsPoint-x a)(ThreeDsPoint-x b)), 2)
         expt((-(ThreeDsPoint-y a)(ThreeDsPoint-y b)), 2)
         expt((-(ThreeDsPoint-z a)(ThreeDsPoint-z b)), 2))))

;if we generated the graph with k(5) nearest neighbour such that each ThreeDsPoint only have edge to
; its nearest neighbour in terms of Euclidean distance
;then we can proferm dijkstra to calculate geodesic distance
;which allow us to have a 4N*4N matrix of each points' geodesic distance


;code referenced from https://github.com/xinchaosong/shortest-path-problem-racket/blob/master/dijkstra.rktfrom 
(define (dijkstra source target graph)
  ; point-list is a list storing all points in the input graph g

  ; returns the index of a point in point-list
  (define (point-index point)
    (index-of point-list point))
  
  (cond
    ; if source or target is invalid, returns #false
    [(not (and (member source point-list) (member target point-list))) #false]
    ; otherwise solves the problem
    [else
     (let (;in the beginning distance from starting point to starting point is 0
           ;and from starting point to destination is unknown
           [dist (for/vector ([i point-list]) (if (symbol=? i source) 0 +inf.0))]
           ; previous point in optimal path from starting point to starting point is starting point itself
           [prev (for/vector ([i point-list]) (if (symbol=? i source) source 'UNDEFINED))]
           ; q-bool is a [Vector-of Boolean] that shows which points have not been processed
           ; #true means that the corresponding point has not been processed and vice versa
           [q-bool (make-vector (length point-list) #true)]
           ; u is the point point currently processed
           [u source])

       ; processes every point in point-list 
       ; or all points have been processed
       (for ([n point-list]
             #:break (symbol=? u target))
           
         ; point with the least distance will be processed first
         (let* ([min-dist (argmin values (for/list ([i dist] [j q-bool] #:when j) i))]
                [min-index (vector-member min-dist dist)])
           (set! u (list-ref point-list min-index)))
           
         ; marks the point u as processed
         (vector-set! q-bool (point-index u) #false)
           
         ; relaxes edges for each neighbor v of u
         (for ([i graph])             
           (let* ([v (caddr i)]
                  [weight (cadr i)]
                  [alt (+ (vector-ref dist (point-index u)) weight)])
             ; a shorter path to v has been found
             (when (and (symbol=? u (car i))
                        (vector-ref q-bool (point-index v))
                        (> (vector-ref dist (point-index v)) alt))
               ; updates the data of dist and prev
               (vector-set! dist (point-index v) alt)
               (vector-set! prev (point-index v) u)))))
         
       ; reads the shortest path from source to target           
       (let* ([u target]
              [prev-u (vector-ref prev (point-index u))])
         ; returns false if there is no valid previous point found
         (cond [(symbol=? prev-u 'UNDEFINED) #false] 
               [else
                ; constructs the shortest path with a stack
                (for/fold ([tail '()])
                          ([i prev]
                           ; pushes the source onto the stack at the end of the loop
                           #:final (symbol=? prev-u u))
                  ; pushes the vertex onto the stack and traverse from target to source
                  (begin0 (cons u tail)
                          (set! u prev-u)
                          (set! prev-u (vector-ref prev (point-index u)))))])))]))


; assume we have one 4N*4N matrix of geodesic_distance 
(require srfi/42ref)
(require (planet wmfarr/plt-linalg:1:13/matrix))
(require math/array)

(provide eigensystem)

(define m
  (matrix geodesic_distance ))

(define-values (eigenvector_a eigenvector_b) (eigensystem m))

(define projection-matrix
(list*->array (map (lambda (x)) (array->list* eigenvector_a eigenvector_b))))


;Summary of the steps that should be implement for Isomap
;1. Calculate the Euclidean distance between each points
;create a 4N*4N matrix as a reference
;2. Create a graph in which a point has edges to k(for example 5) nearest neighbours, each edge has cost as the the Euclidean distance
;then use Djikstra’s Shortest Path algorithm to find the shortest path between each points
;3. Create a new  geodesic distance matrix for reference
;4. Find the top two biggest eigenvectors of the matrix.
;5. Use it as the new x and y axis to plot the points

;it is very hard for Racket to have a 2D array that can store values and then refer to each value using
;two numbers as coordinations to access it .
;it is very hard for Racket to implement a graph data structure
;it is very hard to calculate geodesic distance following Djikstra’s Shortest Path algorithm
;it is very hard for Racket to calculate eigenvectors of the matrix.
;it is thus very hard to recover a 2D graph based on the new axis.
