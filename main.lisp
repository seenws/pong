(load "~/quicklisp/setup.lisp")
(ql:quickload "sdl2")

(defconstant +window-width+ 640)
(defconstant +window-height+ 480)

(defconstant +paddle-width+ 15)
(defconstant +paddle-height+ 100)
(defconstant +ball-size+ 16)

(defstruct game
  (ball-x 312)
  (ball-y 232)
  (ball-velocity-x 4)
  (ball-velocity-y 3)

  (left-paddle-x 30)
  (left-paddle-y 180)
  (right-paddle-x 595)
  (right-paddle-y 180)

  (left-score 0)
  (right-score 0))

(defun handle-input (game)
  (when (sdl2:keyboard-state-p :scancode-w)
    (decf (game-left-paddle-y game) 5))

  (when (sdl2:keyboard-state-p :scancode-s)
    (incf (game-left-paddle-y game) 5))

  (when (sdl2:keyboard-state-p :scancode-a)
    (decf (game-right-paddle-y game) 5))

  (when (sdl2:keyboard-state-p :scancode-d)
    (incf (game-right-paddle-y game) 5))

  (setf (game-left-paddle-y game)
        (max 0
             (min (game-left-paddle-y game)
                  (- +window-height+ +paddle-height+))))

  (setf (game-right-paddle-y game)
        (max 0
             (min (game-right-paddle-y game)
                  (- +window-height+ +paddle-height+)))))

(defun reset-ball (game direction)
  (setf (game-ball-x game)
        (/ (- +window-width+ +ball-size+) 2)
        (game-ball-y game)
        (/ (- +window-height+ +ball-size+) 2)
        (game-ball-velocity-x game)
        (* direction 4)
        (game-ball-velocity-y game)
        
        (if (minusp (game-ball-velocity-y game)) -3 3)))

(defun gamestep (game)
  (incf (game-ball-x game)
        (game-ball-velocity-x game))

  (incf (game-ball-y game)
        (game-ball-velocity-y game))

  (when (or (<= (game-ball-y game) 0)
            (>= (game-ball-y game)
                (- +window-height+ +ball-size+)))
    (setf (game-ball-velocity-y game)
          (- (game-ball-velocity-y game))))

  ;; left paddle collision
  (when (and (< (game-ball-velocity-x game) 0)
           (<= (game-ball-x game)
               (+ (game-left-paddle-x game) +paddle-width+))
           (>= (+ (game-ball-x game) +ball-size+)
               (game-left-paddle-x game))
           (>= (+ (game-ball-y game) +ball-size+)
               (game-left-paddle-y game))
           (<= (game-ball-y game)
               (+ (game-left-paddle-y game)
                  +paddle-height+)))
  (setf (game-ball-velocity-x game)
        (abs (game-ball-velocity-x game))))

  ;; right paddle collision
  (when (and (> (game-ball-velocity-x game) 0)
           (<= (game-ball-x game)
               (+ (game-right-paddle-x game) +paddle-width+))
           (>= (+ (game-ball-x game) +ball-size+)
               (game-right-paddle-x game))
           (>= (+ (game-ball-y game) +ball-size+)
               (game-right-paddle-y game))
           (<= (game-ball-y game)
               (+ (game-right-paddle-y game)
                  +paddle-height+)))
  (setf (game-ball-velocity-x game)
        (- (abs (game-ball-velocity-x game)))))

  (when (< (+ (game-ball-x game) +ball-size+) 0)
    (incf (game-right-score game))
    (reset-ball game -1))

  (when (> (game-ball-x game) +window-width+)
    (incf (game-left-score game))
    (reset-ball game 1)))

;; I could import sdl2-ttf and draw the text as a texture, but writing the score as a segmented number
;; looks nicer imo
(defun draw-segment (renderer segment x y)
  (let ((rect
          (case segment
            (:a (sdl2:make-rect x y 12 3))
            (:b (sdl2:make-rect (+ x 9) (+ y 3) 3 9))
            (:c (sdl2:make-rect (+ x 9) (+ y 15) 3 9))
            (:d (sdl2:make-rect x (+ y 24) 12 3))
            (:e (sdl2:make-rect x (+ y 15) 3 9))
            (:f (sdl2:make-rect x (+ y 3) 3 9))
            (:g (sdl2:make-rect x (+ y 12) 12 3)))))

    (sdl2:render-fill-rect renderer rect)))

(defun draw-digit (renderer digit x y)
  (let ((segments
          (case digit
            (0 '(:a :b :c :d :e :f))
            (1 '(:b :c))
            (2 '(:a :b :g :e :d))
            (3 '(:a :b :g :c :d))
            (4 '(:f :g :b :c))
            (5 '(:a :f :g :c :d))
            (6 '(:a :f :g :e :c :d))
            (7 '(:a :b :c))
            (8 '(:a :b :c :d :e :f :g))
            (9 '(:a :b :c :d :f :g)))))
    
    (dolist (segment segments)
      (draw-segment renderer segment x y))))

(defun draw-score (renderer score x y)
  (loop for char across (write-to-string score)
        for offset from 0
        for digit = (digit-char-p char)
        do (draw-digit renderer digit (+ x (* offset 16)) y)))

(defun render (renderer game)
  (sdl2:set-render-draw-color renderer 30 30 40 255)
  (sdl2:render-clear renderer)

  ;; dotted center line
  (sdl2:set-render-draw-color renderer 100 100 115 255)
  (loop for y from 10 below +window-height+ by 20
        do (sdl2:render-fill-rect
            renderer
            (sdl2:make-rect 319 y 2 10)))

  ;; score display
  (sdl2:set-render-draw-color renderer 220 220 230 255)
  (draw-score renderer (game-left-score game) 280 30)
  (draw-score renderer (game-right-score game) 348 30)

  (sdl2:set-render-draw-color renderer 80 180 255 255)

  ;; left paddle
  (sdl2:render-fill-rect
    renderer
    (sdl2:make-rect
      (game-left-paddle-x game)
      (game-left-paddle-y game)
      
      +paddle-width+
      +paddle-height+))

  ;; right paddle
  (sdl2:render-fill-rect
    renderer
    (sdl2:make-rect
      (game-right-paddle-x game)
      (game-right-paddle-y game)

      +paddle-width+
      +paddle-height+))

  ;; ball
  (sdl2:render-fill-rect
    renderer
    (sdl2:make-rect
     (game-ball-x game)
     (game-ball-y game)

     +ball-size+
     +ball-size+))

  (sdl2:render-present renderer))

(defun main ()
  (let ((game (make-game)))
    (sdl2:with-init (:video)
      (sdl2:with-window (window
                         :title "Pong in CL"
                         :w +window-width+
                         :h +window-height+
                         :flags '(:shown))
        (sdl2:with-renderer (renderer window)
          (sdl2:with-event-loop (:method :poll)
            (:quit () t)
            (:idle ()
              (handle-input game)
              (gamestep game)
              (render renderer game)
              (sdl2:delay 16))))))))

(main)
