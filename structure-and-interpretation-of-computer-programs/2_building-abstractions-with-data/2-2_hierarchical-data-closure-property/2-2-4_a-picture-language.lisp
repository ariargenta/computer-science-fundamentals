;; Turning batteries ON
(ql:quickload :vecto)

;; APU fire test
(defpackage #:vecto-examples
    (:use #:cl #:vecto))

(in-package #:vecto-examples)

;; APU master switch ON
(defun radiant-lambda (file)
    (with-canvas (:width 512 :height 512)
        (let ((font (get-font "times.ttf"))
              (step (/ pi 7)))
            (set-font font 40)
            (translate 45 45)
            (draw-centered-string 0 -10 #(#x3BB))
            (set-rgb-stroke 1 0 0)
            (centered-circle-path 0 0 35)
            (stroke)
            (set-rgba-stroke 0 0 1.0 0.5)
            (set-line-width 4)
            (dotimes (i 14)
                (with-graphics-state
                    (rotate (* i step))
                    (move-to 30 0)
                    (line-to 40 0)
                    (stroke)))
            (save-png file))))

(defun star-clipping (file)
    (with-canvas (:width 200 :height 200)
        (let ((size 100)
              (angle 0)
              (step (* 2 (/ (* pi 2) 5))))
            (translate size size)
            (move-to 0 size)
            (dotimes (i 5)
                (setf angle (+ angle step))
                (line-to (* (sin angle) size)
                         (* (cos angle) size)))
            (even-odd-clip-path)
            (end-path-no-op)
            (flet ((circle (distance)
                           (set-rgba-fill distance 0 0 (- 1.0 distance))
                           (centered-circle-path 0 0 (* size distance))
                           (fill-path)))
                (loop for i downfrom 1.0 by 0.05
                          repeat 20 do
                          (circle i)))
            (save-png file))))

(defun feedlike-icon (file)
    (with-canvas (:width 100 :height 100)
        (set-rgb-fill 1.0 0.65 0.3)
        (rounded-rectangle 0 0 100 100 10 10)
        (fill-path)
        (set-rgb-fill 1.0 1.0 1.0)
        (centered-circle-path 20 20 10)
        (fill-path)
        (flet ((quarter-circle (x y radius)
                               (move-to (+ x radius) y)
                               (arc x y radius 0 (/ pi 2))))
            (set-rgb-stroke 1.0 1.0 1.0)
            (set-line-width 15)
            (quarter-circle 20 20 30)
            (stroke)
            (quarter-circle 20 20 60)
            (stroke))
        (rounded-rectangle 5 5 90 90 7 7)
        (set-gradient-fill 50 90
                           1.0 1.0 1.0 0.7
                           50 20
                           1.0 1.0 1.0 0.0)
        (set-line-width 2)
        (set-rgba-stroke 1.0 1.0 1.0 0.1)
        (fill-and-stroke)
        (save-png file)))

;; Starting APU
(defun flipped-pairs (painter)
    (let ((combine4 (square-of-four identity flip-vert
                                    identity flip-vert)))
        (combine4 painter)))

(defun right-split (painter n)
    (if (= n 0)
        painter
        (let ((smaller (right-split painter (- n 1))))
            (beside painter (below smaller smaller)))))

(defun up-split (painter n)
    (if (= n 0)
        painter
        (let ((smaller (right-split painter (- n 1))))
            (below painter (beside smaller smaller)))))

(defun split (procedure1 procedure2)
    )

(defun corner-split (painter n)
    (if (= n 0)
        painter
        (let ((up (up-split painter (- n 1)))
              (right (right-split painter (- n 1))))
            (let ((top-left (beside up up))
                  (bottom-right (below right right))
                  (corner (corner-split painter (- n 1))))
                (beside (below painter top-left)
                        (below bottom-right corner))))))

(defun square-limit (painter n)
    (let ((combine4 (square-of-four flip-horiz identity
                                    rotate180 flip-vert)))
        (combine4 (corner-split painter n))))

;; Higher-order operations
(defun square-of-four (tl tr bl br)
    (lambda (painter)
        (let ((top (beside (funcall tl painter) (funcall tr painter)))
              (bottom (beside (funcall bl painter) (funcall br painter))))
            (below bottom top))))

;; Frames
(defun frame-coord-map (frame)
    (lambda (v)
        (add-vect
         (origin-frame frame)
         (add-vect (scale-vect (xcor-vect v) (edge1-frame frame))
                   (scale-vect (ycor-vect v) (edge2-frame frame))))))

;; Painters
(defun segments->painter (segment-list)
    (lambda (frame)
        (let ((m (frame-coord-map frame)))
            (for-each
             (lambda (segment)
                 (draw-line
                  (funcall m (start-segment segment))
                  (funcall m (end-segment segment))))
             segment-list))))

(defun transform-painter (painter origin corner1 corner2)
    (lambda (frame)
        (let ((m (frame-coord-map frame)))
            (let ((new-origin (m origin)))
                (painter (make-frame
                          new-origin
                          (sub-vect (m corner1) new-origin)
                          (sub-vect (m corner2) new-origin)))))))

(defun flip-vert (painter)
    (transform-painter painter
                       (make-vect 0.0 1.0)      ; new origin
                       (make-vect 1.0 1.0)      ; new end of edge1
                       (make-vect 0.0 0.0)))    ; new end of edge2

(defun shrink-to-upper-right (painter)
    (transform-painter
     painter (make-vect 0.5 0.5)
     (make-vect 1.0 0.5) (make-vect 0.5 1.0)))

(defun rotate90 (painter)
    (transform-painter painter
                       (make-vect 1.0 0.0)
                       (make-vect 1.0 1.0)
                       (make-vect 0.0 0.0)))

(defun squash-inwards (painter)
    (transform-painter painter
                       (make-vect 0.0 0.0)
                       (make-vect 0.65 0.35)
                       (make-vect 0.35 0.65)))

(defun beside (painter1 painter2)
    (let ((split-point (make-vect 0.5 0.0)))
        (let ((paint-left
               (transform-painter
                painter1
                (make-vect 0.0 0.0)
                split-point
                (make-vect 0.0 1.0)))
              (paint-right
               (transform-painter
                painter2
                split-point
                (make-vect 1.0 0.0)
                (make-vect 0.5 1.0))))
            (lambda (frame)
                (funcall paint-left frame)
                (funcall paint-right frame)))))

(defun +vect (v1 v2)
    (make-vect (+ (xcor v1) (xcor v2))
               (+ (ycor v1) (ycor v2))))

(defun scale-vect (vect factor)
    (make-vect (* factor (xcor vect))
               (* factor (ycor vect))))

(defun -vect (v1 v2)
    (+vect v1 (scale-vect v2 -1)))

(defun rotate-vect (v angle)
    (let ((c (cos angle))
          (s (sin angle)))
        (make-vect (- (* c (xcor v))
                      (* s (ycor v)))
                   (+ (* c (ycor v))
                      (* s (xcor v))))))

(defun make-picture (seglist)
    (lambda (rect)
        (for-each
         (lambda (segment)
             (let ((b (start-segment segment))
                   (e (end-segment segment)))
                 (draw-line rect
                            (xcor b)
                            (ycor b)
                            (xcor e)
                            (ycor e))))
         seglist)))

(defun together (pict1 pict2)
    (lambda (rect)
        (pict1 rect)
        (pict2 rect)))

(defun above (pict1 pict2 a)
    (rotate270
     (beside
      (rotate90 pict1)
      (rotate90 pict2)
      a)))

(defun flip (pict)
    (lambda (rect)
        (pict (make-rectangle
               (+vect (origin rect) (horiz rect))
               (scale-vect (horiz rect) -1)
               (vert rect)))))

(defun up-push (pict n)
    (if (= n 0)
        pict
        (above (up-push pict (- n 1))
               pict
               0.25)))

(defun corner-push (pict n)
    (if (= n 0)
        pict
        (above
         (beside
          (up-push pict n)
          (corner-push pict (- n 1))
          0.75)
         (beside
          pict
          (right-push pict (- n 1))
          0.75)
         0.25)))