#-quicklisp
(let ((quicklisp-init (merge-pathnames "quicklisp/setup.lisp"
                                       (user-homedir-pathname))))
    (when (probe-file quicklisp-init)
          (load quicklisp-init)))

(ql:quickload :vecto)

(defpackage #:a-picture-language
    (:use #:cl #:vecto))

(in-package #:a-picture-language)

(defun make-vect (x y)
    (cons x y))

(defun xcor-vect (v)
    (car v))

(defun ycor-vect (v)
    (cdr v))

(defun add-vect (v1 v2)
    (make-vect (+ (xcor-vect v1)
                  (xcor-vect v2))
               (+ (ycor-vect v1)
                  (ycor-vect v2))))

(defun sub-vect (v1 v2)
    (make-vect (- (xcor-vect v1)
                  (xcor-vect v2))
               (- (ycor-vect v1)
                  (ycor-vect v2))))

(defun scale-vect (s v)
    (make-vect (* s (xcor-vect v))
               (* s (ycor-vect v))))

(defun make-segment (start end)
    (cons start end))

(defun start-segment (segment)
    (car segment))

(defun end-segment (segment)
    (cdr segment))

(defun make-frame (origin edge1 edge2)
    (list origin edge1 edge2))

(defun origin-frame (f)
    (car f))

(defun edge1-frame (f)
    (cadr f))

(defun edge2-frame (f)
    (caddr f))

(defun frame-coord-map (frame)
    (lambda (v)
        (add-vect
         (origin-frame frame)
         (add-vect (scale-vect (xcor-vect v)
                               (edge1-frame frame))
                   (scale-vect (ycor-vect v)
                               (edge2-frame frame))))))

(defun segments->painter (segment-list)
    (lambda (frame)
        (let ((coord-map (frame-coord-map frame)))
            (dolist (segment segment-list)
                (let ((start (funcall coord-map (start-segment segment)))
                      (end (funcall coord-map (end-segment segment))))
                    (vecto:move-to (xcor-vect start) (ycor-vect start))
                    (vecto:line-to (xcor-vect end) (ycor-vect end))
                    (vecto:stroke))))))

(defun transform-painter (painter origin corner1 corner2)
    (lambda (frame)
        (let ((m (frame-coord-map frame)))
            (let ((new-origin (funcall m origin)))
                (funcall painter
                 (make-frame new-origin
                             (sub-vect (funcall m corner1) new-origin)
                             (sub-vect (funcall m corner2) new-origin)))))))

(defun beside (painter1 painter2)
    (let ((split-point (make-vect 0.5 0.0)))
        (let ((paint-left
               (transform-painter painter1
                                  (make-vect 0.0 0.0)
                                  split-point
                                  (make-vect 0.0 1.0)))
              (paint-right
               (transform-painter painter2
                                  split-point
                                  (make-vect 1.0 0.0)
                                  (make-vect 0.5 1.0))))
            (lambda (frame)
                (funcall paint-left frame)
                (funcall paint-right frame)))))

(defun flip-vert (painter)
    (transform-painter painter
                       (make-vect 0.0 1.0)
                       (make-vect 1.0 1.0)
                       (make-vect 0.0 0.0)))

(defun flip-horiz (painter)
    (transform-painter painter
                       (make-vect 1.0 0.0)
                       (make-vect 0.0 0.0)
                       (make-vect 1.0 1.0)))

(defparameter outline-frame-painter
              (segments->painter
               (list
                (make-segment (make-vect 0 0)
                              (make-vect 0 1))
                (make-segment (make-vect 0 1)
                              (make-vect 1 1))
                (make-segment (make-vect 1 1)
                              (make-vect 1 0))
                (make-segment (make-vect 1 0)
                              (make-vect 0 0)))))

(defparameter x-painter
              (segments->painter
               (list
                (make-segment (make-vect 0 0)
                              (make-vect 1 1))
                (make-segment (make-vect 0 1)
                              (make-vect 1 0)))))

(defparameter diamond-painter
              (segments->painter
               (list
                (make-segment (make-vect 0 0.5)
                              (make-vect 0.5 0))
                (make-segment (make-vect 0.5 0)
                              (make-vect 1 0.5))
                (make-segment (make-vect 1 0.5)
                              (make-vect 0.5 1))
                (make-segment (make-vect 0.5 1)
                              (make-vect 0 0.5)))))

(defparameter wave-painter
              (segments->painter
               (list
                (make-segment (make-vect 0.5 0.4)
                              (make-vect 0.6 0))
                (make-segment (make-vect 0.5 0.4)
                              (make-vect 0.4 0))
                (make-segment (make-vect 0.3 0)
                              (make-vect 0.35 0.4))
                (make-segment (make-vect 0.35 0.4)
                              (make-vect 0.3 0.7))
                (make-segment (make-vect 0.3 0.7)
                              (make-vect 0.2 0.6))
                (make-segment (make-vect 0.2 0.6)
                              (make-vect 0 0.8))
                (make-segment (make-vect 0 0.9)
                              (make-vect 0.2 0.7))
                (make-segment (make-vect 0.2 0.7)
                              (make-vect 0.3 0.75))
                (make-segment (make-vect 0.3 0.75)
                              (make-vect 0.4 0.75))
                (make-segment (make-vect 0.4 0.75)
                              (make-vect 0.35 0.9))
                (make-segment (make-vect 0.35 0.9)
                              (make-vect 0.4 1))
                (make-segment (make-vect 0.5 1)
                              (make-vect 0.55 0.9))
                (make-segment (make-vect 0.55 0.9)
                              (make-vect 0.5 0.75))
                (make-segment (make-vect 0.5 0.75)
                              (make-vect 0.6 0.75))
                (make-segment (make-vect 0.6 0.75)
                              (make-vect 1 0.45))
                (make-segment (make-vect 1 0.3)
                              (make-vect 0.6 0.5))
                (make-segment (make-vect 0.6 0.5)
                              (make-vect 0.7 0)))))

(defun rotate-180 (painter)
    (flip-vert painter))

(defun rotate-270 (painter)
    (transform-painter painter
                       (make-vect 1.0 0.0)
                       (make-vect 1.0 1.0)
                       (make-vect 0.0 0.0)))

(defun below (painter1 painter2)
    (let ((split-point (make-vect 0.0 0.5)))
        (let ((paint-bottom
               (transform-painter painter1
                                  (make-vect 0.0 0.0)
                                  (make-vect 1.0 0.0)
                                  split-point))
              (paint-top
               (transform-painter painter2
                                  split-point
                                  (make-vect 1.0 0.5)
                                  (make-vect 0.0 1.0))))
            (lambda (frame)
                (funcall paint-bottom frame)
                (funcall paint-top frame)))))

(defun right-split (painter n)
    (if (= n 0)
        painter
        (let ((smaller (right-split painter (- n 1))))
            (beside painter (below smaller smaller)))))

(defun up-split (painter n)
    (if (= n 0)
        painter
        (let ((smaller (up-split painter (- n 1))))
            (below painter (beside smaller smaller)))))

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
    (let ((quarter (corner-split painter n)))
        (let ((half (beside (flip-horiz quarter) quarter)))
            (below (flip-vert half) half))))

(defun below-transformations (painter1 painter2)
    (flip-horiz (flip-vert (rotate-270 (beside (rotate-270 painter1) (rotate-270 painter2))))))

(defparameter unit-frame (make-frame (make-vect 0 500) (make-vect 500 0) (make-vect 0 -500)))

(defun render (painter filename &optional (size 512))
    (let ((frame (make-frame (make-vect 0 0)
                             (make-vect size 0)
                             (make-vect 0 size))))
        (vecto:with-canvas (:width size :height size)
            (vecto:set-rgb-fill 1 1 1)
            (vecto:rectangle 0 0 size size)
            (vecto:fill-path)
            (vecto:set-rgb-stroke 0 0 0)
            (vecto:set-line-width 1)
            (funcall painter frame)
            (vecto:save-png filename))))

(render (square-limit wave-painter 4) "wave.png")