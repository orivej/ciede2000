
(defpackage #:ciede2000
  (:use #:cl #:let-plus)
  (:export #:ciede2000))

(in-package #:ciede2000)

(defun sq (x)
  (* x x))

(defun avg (&rest xs)
  (if (null xs)
      0
      (/ (reduce #'+ xs)
         (length xs))))

(defun /1+/ (x y)
  (if (zerop y) 0 (/ (1+ (/ x y)))))

(defun mod-range (x min max)
  (let ((range (- max min)))
    (+ min (mod (- x min) range))))

(defun rad360 (x)
  (* x (/ 180 pi)))
(defun 360rad (x)
  (* x (/ pi 180)))

(defun atan360 (y &optional x)
  (mod-range (rad360 (atan y x))
             0 360))
(defun sin360 (y)
  (sin (360rad y)))
(defun cos360 (y)
  (cos (360rad y)))

(defun ciede2000 (L1 a1 b1 L2 a2 b2 &optional (kL 1) (kC 1) (kH 1))
  ;; http://www.ece.rochester.edu/~gsharma/ciede2000/
  (let+ (;; 1. Calculate C~i, h~i
         ((&flet C*iab (ai bi)
            (sqrt (+ (sq ai) (sq bi)))))
         (C*1ab (C*iab a1 b1))
         (C*2ab (C*iab a2 b2))
         (C*ab (avg C*1ab C*2ab))
         (G (* 1/2 (- 1 (sqrt (/1+/ (expt 25 7) (expt C*ab 7))))))
         ((&flet a~i (ai)
            (* (1+ G) ai)))
         (a~1 (a~i a1))
         (a~2 (a~i a2))
         ((&flet C~i (a~i bi)
            (sqrt (+ (sq a~i) (sq bi)))))
         (C~1 (C~i a~1 b1))
         (C~2 (C~i a~2 b2))
         ((&flet h~i (a~i bi)
            (if (= 0 a~i bi) 0 (atan360 bi a~i))))
         (h~1 (h~i a~1 b1))
         (h~2 (h~i a~2 b2))
         ;; 2. Calculate dL~, dC~, dH~
         (dL~ (- L2 L1))
         (dC~ (- C~2 C~1))
         (dh~ (if (zerop (* C~1 C~2)) 0 (mod-range (- h~2 h~1) -180 180)))
         (dH~ (* 2 (sqrt (* C~1 C~2)) (sin360 (/ dh~ 2))))
         ;;
         (L_ (avg L1 L2))
         (C_ (avg C~1 C~2))
         (h_ (cond
               ((zerop (* C~1 C~2))
                (+ h~1 h~2))
               ((<= -180 (- h~1 h~2) 180)
                (avg h~1 h~2))
               ((< (+ h~1 h~2) 360)
                (+ (avg h~1 h~2) 180))
               (t
                (- (avg h~1 h~2) 180))))
         (T= (+ 1 (* -0.17 (cos360 (- h_ 30)))
                (* 0.24 (cos360 (* 2 h_)))
                (* 0.32 (cos360 (+ (* 3 h_) 6)))
                (* -0.20 (cos360 (- (* 4 h_) 63)))))
         (dF (* 30 (exp (- (sq (/ (- h_ 275) 25))))))
         (RC (* 2 (sqrt (/1+/ (expt 25 7) (expt C_ 7)))))
         (SL (1+ (/ (* 0.015 (sq (- L_ 50)))
                    (sqrt (+ 20 (sq (- L_ 50)))))))
         (SC (1+ (* 0.045 C_)))
         (SH (1+ (* 0.015 C_ T=)))
         (RT (* -1 (sin360 (* 2 dF)) RC)))
    (sqrt (+ (sq (/ dL~ kL SL))
              (sq (/ dC~ kC SC))
              (sq (/ dH~ kH SH))
              (* RT
                 (/ dC~ kC SC)
                 (/ dH~ kH SH))))))
