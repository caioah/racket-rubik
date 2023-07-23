#lang racket
(define (transpose lst) (apply map list lst))
(define (fxy->xyz f x y max)
  (case f
    [(0) (list x (- max y) 0)] ;front
    [(3) (list (- max x) (- max y) max)] ;back
    [(1) (list 0 (- max y) (- max x))] ;left
    [(4) (list max (- max y) x)] ;right
    [(2) (list x max (- max y))] ;up
    [(5) (list x 0 y)])) ;down
#|
corners: (0 0 0), (max 0 0), (0 max 0), (max max 0), (0 0 max), (0 max max), (max 0 max), (max max max)
middle: (0 1..max-1 0), (1..max-1 0 0), (1..max-1 max 0), (1..max-1 1..max-1 0), (0 0 1..max-1), (0 max 1..max-1), (max 0 1..max-1), (max max 1..max-1), (0 1..max-1 max), (1..max-1 0 max), (1..max-1 max max), (max 1..max-1 max)
center: (1..max-1 1..max-1 0), (0 1..max-1 1..max-1), (1..max-1 0 1..max-1), (1..max-1 max 1..max-1), (max 1..max-1 1..max-1), (1..max-1 1..max-1 max)
axis: (1..max-1 1..max-1 1..max-1)
|#
(define faces '(front left up back right down))
(define (xyz->piece x y z max)
  (append (cond [(= x 0) '(left)] [(= x max) '(right)] [else null])
          (cond [(= y max) '(up)] [(= y 0) '(down)] [else null])
          (cond [(= z 0) '(front)] [(= z max) '(back)] [else null])))
(define (make-cube k)
  (build-list k (λ (z) (build-list k (λ (y) (build-list k (λ (x) (xyz->piece x y z (sub1 k)))))))))
(define (cube-ref cube x y z)
  (list-ref (list-ref (list-ref cube z) y) x))
(define (cube-set cube x y z v)
  (list-update cube z (λ (xy) (list-update xy y (λ (lst) (list-set lst x v))))))

(define (get-face-pieces cube f)
  (define len (length cube))
  (for/list ([y (in-range len)])
    (for/list ([x (in-range len)])
      (apply cube-ref cube (fxy->xyz f x y (sub1 len))))))
(define (set-face-pieces cube f m)
  (define len (length cube))
  (for/fold ([res cube]) ([y (in-range len)] #:when #t [x (in-range len)])
    (apply cube-set res (append (fxy->xyz f x y (sub1 len)) (list (list-ref (list-ref m y) x))))))
(define (face-rotl cube f [k 1])
  (cond [(= (modulo k 4) 0) cube]
        [(= (modulo k 4) 2)
         (set-face-pieces cube f (reverse (map reverse (get-face-pieces cube f))))]
        [else
         (face-rotl (set-face-pieces cube f (map (λ (row) (map (λ (p) (piece-rot f p)) row)) (reverse (transpose (get-face-pieces cube f))))) f (sub1 k))]))
(define (piece-rot f p)
  (cond [(= (modulo f 3) 2) (reverse p)]
        [(< (length p) 3) p]
        [(= (modulo f 3) 1) (cons (first p) (reverse (cdr p)))]
        [else (append (reverse (take p 2)) (list (last p)))]))
(define (face-rotr cube f [k 1])
  (face-rotl cube f (- 4 (modulo k 4))))
(define (side-ref cube f x y)
  (let* ([max (sub1 (length cube))]
         [p (apply cube-ref cube (fxy->xyz f x y max))])
    (cond [(= 0 (modulo f 3)) (last p)]
          [(or (= 1 (modulo f 3)) (= (length p) 1)) (first p)]
          [(and (= (length p) 2) (or (= y 0) (= y max))) (first p)]
          [else (second p)])))
(define (get-face cube f)
  (map (λ (y) (map (λ (x) (side-ref cube f x y)) (range (length cube)))) (range (length cube))))
(define (invert-move str)
   (match str
     [(regexp #px"^([FBLRUD])([i']?)(2?)(.*)" (list _ f i d rem))
      (if (string=? i "") (string-append (invert-move rem) f "i" d)
          (string-append (invert-move rem) f d))]
     [(regexp #px"^([fblrud])([i']?)(2?)(.*)" (list _ f i d rem))
      (if (string=? i "") (string-append (invert-move rem) (string-upcase f) d)
          (string-append (invert-move rem) (string-upcase f) "i" d))]
     [(regexp #px"^\\s*$" (list _)) ""]
     [else (error "invalid move" str)]))
(define (parallel-move? a b)
  (or (and (regexp-match #px"^[Ff]" a)
           (regexp-match #px"^[Bb]" b))
      (and (regexp-match #px"^[Ff]" b)
           (regexp-match #px"^[Bb]" a))
      (and (regexp-match #px"^[Ll]" a)
           (regexp-match #px"^[Rr]" b))
      (and (regexp-match #px"^[Ll]" b)
           (regexp-match #px"^[Rr]" a))
      (and (regexp-match #px"^[Uu]" a)
           (regexp-match #px"^[Dd]" b))
      (and (regexp-match #px"^[Uu]" b)
           (regexp-match #px"^[Dd]" a))))
(define (move-append a b)
  (cond [(string=? (string-trim b) "") a]
        [(regexp-match #px"^(.*)([FBLRUDfblrud]2)\\2(.*)$" b)
         => (λ (lst) (move-append a (string-append (second lst) (fourth lst))))]
        [else
         (let ([l (regexp-match #px"(.*)([FBLRUDfblrud][i']?2?)\\s*$" a)]
               [r (regexp-match #px"^\\s*([FBLRUDfblrud][i']?2?)(.*)$" b)])
           (cond [(not r) (error "invalid move")]
                 [(and l (string=? (third l) (invert-move (second r))))
                  (move-append (second l) (third r))]
                 [(and l (parallel-move? (third l) (second r)))
                  (if (string=? (string-trim (third r)) "")
                      (string-append a (second r))
                      (let ([r2 (regexp-match #px"^\\s*([FBLRUDfblrud][i']?2?)(.*)$" (third r))])
                        (cond [(not r2) (error "invalid move")]
                              [(string=? (third l) (invert-move (second r2)))
                               (move-append (string-append (second l) (second r)) (third r2))]
                              [else (move-append (string-append a (second r)) (third r))])))]
                 [else (move-append (string-append a (second r)) (third r))]))]))
(define (move-cube cube str)
  (match str
    [(regexp #px"^([FBLRUD])([i']?)(2?)(.*)" (list _ f i d rem))
     (let ([n (index-of '("F" "L" "U" "B" "R" "D") f)])
       (move-cube (cond [(and (string=? i "") (string=? d "")) (face-rotr cube n)]
             [(not (string=? d "")) (face-rotl cube n 2)]
             [else (face-rotl cube n)]) rem))]
    [(regexp #px"^([fblrud])([i']?)(2?)(.*)" (list _ f i d rem))
     (let ([n (index-of '("f" "l" "u" "b" "r" "d") f)])
       (move-cube (cond [(and (string=? i "") (string=? d "")) (face-rotl cube n)]
             [(not (string=? d "")) (face-rotl cube n 2)]
             [else (face-rotr cube n)]) rem))]
    [(regexp #px"^\\s*$") cube]
    [else (error "invalid move" str cube)]))

(require racket/draw)
(define cube-colors (make-parameter '("green" "orange" "white" "blue" "red" "yellow")))
(define (render-face cube f sz)
  (let ([lst (get-face cube f)]
        [bmp (make-object bitmap% (* sz (length cube)) (* sz (length cube)))])
    ((λ (dc)
       (for ([y (in-range 0 (* sz (length cube)) sz)] [row (in-list lst)])
         (for ([x (in-range 0 (* sz (length cube)) sz)] [p (in-list row)])
           (send dc set-brush (list-ref (cube-colors) (index-of faces p eq?)) 'solid)
           (send dc draw-rectangle x y sz sz))) bmp) (send bmp make-dc))))
(define (render-cube cube sz)
  (let* ([fsz (* sz (length cube))]
         [bmp (make-object bitmap% (* 4 fsz) (* 3 fsz))])
    ((λ (dc)
       (send dc draw-bitmap (render-face cube 2 sz) fsz 0)
       (for ([i (in-list '(1 0 4 3))] [k (in-naturals)])
         (send dc draw-bitmap (render-face cube i sz) (* k fsz) fsz))
       (send dc draw-bitmap (render-face cube 5 sz) fsz (* 2 fsz)) bmp)
     (send bmp make-dc))))

(define (draw-move cube str sz)
  (render-cube (move-cube cube str) sz))

(require pict3d pict3d/universe)
(define (quadxy sz p [z 0] [b? #t] [col (rgba "white")])
  (let ([m (/ sz 15)])
    (quad (vertex (pos+ p (dir 0 0 z)) #:color col) (vertex (pos+ p (dir (- sz m) 0 z)) #:color col)
                   (vertex (pos+ p (dir (- sz m) (- sz m) z)) #:color col)
                   (vertex (pos+ p (dir 0 (- sz m) z)) #:color col) #:back? b?)))
(define (quadxz sz p [y 0] [b? #f] [col (rgba "white")])
  (let ([m (/ sz 15)])
    (quad (vertex (pos+ p (dir 0 y 0)) #:color col) (vertex (pos+ p (dir (- sz m) y 0)) #:color col)
                   (vertex (pos+ p (dir (- sz m) y (- sz m))) #:color col)
                   (vertex (pos+ p (dir 0 y (- sz m))) #:color col) #:back? b?)))
(define (quadyz sz p [x 0] [b? #t] [col (rgba "white")])
  (let ([m (/ sz 15)])
    (quad (vertex (pos+ p (dir x 0 0)) #:color col) (vertex (pos+ p (dir x (- sz m) 0)) #:color col)
                   (vertex (pos+ p (dir x (- sz m) (- sz m))) #:color col)
                   (vertex (pos+ p (dir x 0 (- sz m))) #:color col) #:back? b?)))
;#|
(define (cube3d-face f cube sz)
  (let* ([fc (get-face cube f)]
         [face-color (λ (x y) (rgba (list-ref (cube-colors)
                                              (index-of faces (list-ref (list-ref fc x) y) eq?))))])
    (case (list-ref faces f)
      [(back) (apply combine (for/list ([y (in-range 3)] #:when #t [x (in-range 3)])
                            (quadyz 1/4 (pos+ origin (dir-scale (dir 0 x y) sz)) 0 #t
                                    (face-color (- 2 y) (- 2 x)))))]
      [(front) (apply combine (for/list ([y (in-range 3)] #:when #t [x (in-range 3)])
                            (quadyz 1/4 (pos+ origin (dir-scale (dir 3 x y) sz)) 0 #f
                                    (face-color (- 2 y) x))))]
      [(up) (apply combine (for/list ([y (in-range 3)] #:when #t [x (in-range 3)])
                            (quadxy 1/4 (pos+ origin (dir-scale (dir x y 3) sz)) 0 #f
                                    (face-color x y))))]
      [(down) (apply combine (for/list ([y (in-range 3)] #:when #t [x (in-range 3)])
                            (quadxy 1/4 (pos+ origin (dir-scale (dir x y 0) sz)) 0 #t
                                    (face-color (- 2 x) y))))]
      [(right) (apply combine (for/list ([y (in-range 3)] #:when #t [x (in-range 3)])
                            (quadxz 1/4 (pos+ origin (dir-scale (dir x 3 y) sz)) 0 #t
                                    (face-color (- 2 y) (- 2 x)))))]
      [(left) (apply combine (for/list ([y (in-range 3)] #:when #t [x (in-range 3)])
                            (quadxz 1/4 (pos+ origin (dir-scale (dir x 0 y) sz)) 0 #f
                                    (face-color (- 2 y) x))))]
      [else (error "invalid face index")])))
(define (get-cube3d cube sz)
  (apply combine (build-list 6 (λ (f) (cube3d-face f cube sz)))))
;|#

(define lights+camera
  (combine (light (pos 0 1 2) (emitted "white"))
           (light (pos 0 -1 -2) (emitted "yellow"))
           (basis 'camera (point-at (pos 1 1 0) origin))))
(define ref 0)
(define (on-draw s n t)
  (when (not (= (modulo (exact-floor (/ t 10000)) (hash-count patterns)) ref))
    (set! ref (modulo (exact-floor (/ t 10000)) (hash-count patterns)))
    (reset!)
    (do-move (hash-ref patterns (list-ref (hash-keys patterns) ref))))
  (combine (rotate-z (rotate-y (rotate-x (move (get-cube3d (unbox s) 1/4) (dir -3/8 -3/8 -3/8))
                                         (/ t 16))
                               (/ t 32))
                     (/ t 64))
           lights+camera))

(define patterns
  '#hash((Anaconda . "LUBiUiRLiBRiFBiDRDiFi")
  (BlackMamba . "RDLFiRLiDRiUDiBUiRiDi")
  (Center-Edge-Corner . "UiR2L2F2B2UiRLFBiUF2D2R2L2F2U2F2UiF2")
  (Checkerboard . "FB2RiD2BRUDiRLiDiFiR2DF2Bi")
  (Checkerboard2 . "U2D2F2B2L2R2")
  (Checkerboard3 . "FBBRiDDBRUDiRLiDiFiRRDFFBi")
  (Checkerboard6 . "RiDiFiDLFU2BiLUDiRiDiLFL2UFi")
  (Checkerboard6-alt . "R2L2UBL2DiFB2RLiFiBRDF2LiUi")
  (ChristmansCross . "UFBiL2U2L2FiBU2L2U")
  (Cross . "UFBiL2U2L2FiBU2L2U")
  (Cross2 . "R2LiDF2RiDiRiLUiDRDB2RiUD2")
  (CubeInCube . "UiLiUiFiR2BiRFUB2UBiLUiFURFi")
  (EdgeHexagon2 . "UB2UiFiUiDLiD2LUDiFDiLLB2Di")
  (EdgeHexagon3 . "DLiURiBiRBU2DBDiBiLUDi")
  (EdgeHexagon3-alt . "FLBULF2B2RiF2B2UiBiLiFi")
  (ExchangedChickenFeet . "FLiDiBiLFUFiDiFL2BiRiUL2DiF")
  (ExchangedDuckFeet . "UFR2FiDiRUB2U2FiR2FDB2RBi")
  (ExchangedPeaks . "FU2LFLiBLUBiRiLiURiDiFiBR2")
  (ExchangedPeaks-alt . "F2R2DR2UDF2DiRiDiFL2FiDRUi")
  (ExchangedRings . "BiUiBiLiDBUD2BULDiLiUiL2D")
  (ExchangedRings-alt . "FUDiLiB2LUiDFUR2L2UiL2F2")
  (FBoa . "RUiR2U2FD2R2UiDiRDiFi")
  (FRattlesnake . "U2DiL2DBUBiRiL2U2FUiFR")
  (FacingCheckerboards . "U2F2U2F2B2U2F2D2")
  (FourSpot . "F2B2UDiR2L2UDi")
  (FourTwistedPeaks . "UiDBRiFRBiLiFiBLFRiBiRFiUiD")
  (GreenMamba . "RDRFRiFiBDRiUiBiUD2")
  (MBoa . "FDRiUDR2D2FiU2R2URi")
  (MRattlesnake . "RiFiUFiU2RL2BUiBiDiL2U2D")
  (OppositeCorners . "RLU2F2D2F2RLF2D2B2D2")
  (PlummersCross . "R2LiDF2RiDiRiLUiDRDB2RiUD2")
  (PonsAsinorum . "F2B2R2L2U2D2")
  (Python . "F2RiBiURiLFiLFiBDiRBL2")
  (RonsCube2 . "FDiFiRDFiRiDRDLiFLDRiFDi")
  (RonsCube2-alt . "L2D2LiD2B2L2B2LiD2L2B2LiB2")
  (SixSpot . "UDiRLiFBiUDi")
  (SixT . "F2R2U2FiBD2L2FB")
  (SixTwoOne . "UB2D2LBiLiUiLiBD2B2")
  (Speedsolving.com . "RiLiU2F2D2F2RLB2U2B2U2")
  (Spiral . "LiBiDURUiRiD2R2DLDiLiRiFU")
  (SquareInTheMiddle . "RLiUDiFiBRLi")
  (Stripes . "FUFRL2BDiRD2LDiBR2LFUF")
  (Superflip . "UR2FBRB2RU2LB2RUiDiR2FRiLB2U2F2")
  (Tablecloth . "RLU2FiU2D2R2L2FiD2F2DR2L2F2B2DB2L2")
  (Tetris . "LRFBUiDiLiRi")
  (TomParks . "LUF2RLiU2BiUDB2LFBiRiLFiR")
  (TwistedChickenFeet . "FLiDFiUiBUFUiFRiF2LUiRiD2")
  (TwistedCube . "FLFUiRUF2L2UiLiBDiBiL2U")
  (TwistedDuckFeet . "FRiBRUFiLiFiU2LiUiD2BDiFBiU2")
  (TwistedRings . "FDFiD2LiBiULDRULiFiULU2")
  (Twister . "FRiULFiLiFUiRULiUiLFi")
  (TwoTwistedPeaks . "FBiUFUFULBL2BiUFiLULiB")
  (TwoTwistedPeaks-alt . "FD2BRBiLiFDiL2F2RFiRiF2LiFi")
  (Wire . "RLFBRLFBRLFBR2B2L2R2B2L2")
  (Zig-Zag . "R2L2F2B2UF2B2U2F2B2U")))

(define current-cube (box (make-cube 3)))
(define current-move (box ""))
(define current-solution (box ""))
(define (do-move str)
  (set-box! current-move (move-append (unbox current-move) str))
  (set-box! current-solution (move-append (unbox current-solution) (invert-move str)))
  (set-box! current-cube (move-cube (unbox current-cube) str)))
(define (reset!)
  (set-box! current-move "")
  (set-box! current-solution "")
  (set-box! current-cube (make-cube 3)))

  
(define t (thread (λ () (big-bang3d current-cube #:on-draw on-draw))))