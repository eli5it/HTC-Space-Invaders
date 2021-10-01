;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-abbr-reader.ss" "lang")((modname eli-space-invaders) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require 2htdp/universe)
(require 2htdp/image)

;; Space Invaders


;; Constants:

(define WIDTH  300)
(define HEIGHT 500)
(define CTR-X (/ WIDTH 2))
(define CTR-Y (/ HEIGHT 2))
(define GENERATION-RANGE (- WIDTH 1))
(define SAFE-TURNAROUND (- GENERATION-RANGE 11)) 
(define INVADER-X-SPEED 1.5)  ;speeds (not velocities) in pixels per tick
(define INVADER-Y-SPEED 1.5)
(define TANK-SPEED 3.5)
(define MISSILE-SPEED 10)
(define MISSILE-Y-START (- HEIGHT 19.5))
(define GAME-OVER-TEXT (text "You lost the game....
                              get gud son" 20 "indigo"))
(define TEXT-BOX (rectangle WIDTH (/ HEIGHT 2) "outline" "white"))
(define OVER-MESSAGE (put-image GAME-OVER-TEXT CTR-X (/ HEIGHT 4) TEXT-BOX))
(define HIT-RANGE 10)

(define INVADE-RATE 18)
(define INVADER-SAFE-Y 15.5)
(define INVADER-SAFE-X 12.5)

(define BACKGROUND (empty-scene WIDTH HEIGHT))

(define INVADER
  (overlay/xy (ellipse 10 15 "outline" "blue")              ;cockpit cover
              -5 6
              (ellipse 20 10 "solid"   "blue")))            ;saucer

(define TANK
  (overlay/xy (overlay (ellipse 28 8 "solid" "black")       ;tread center
                       (ellipse 30 10 "solid" "green"))     ;tread outline
              5 -14
              (above (rectangle 5 10 "solid" "black")       ;gun
                     (rectangle 20 10 "solid" "black"))))   ;main body

(define TANK-HEIGHT/2 (/ (image-height TANK) 2))
(define TANK-BOTTOM (- HEIGHT TANK-HEIGHT/2))
(define INVADER-HEIGHT/2 (/ (image-height INVADER) 2))
(define INVADER-ROOM (+ INVADER-HEIGHT/2 (image-height INVADER)))

(define MISSILE (ellipse 5 15 "solid" "red"))

(define INVADER-Y-START (/ (image-height INVADER) 2) )
(define RIGHT-EDGE (- WIDTH (/ (image-width INVADER) 2)))
(define LEFT-EDGE  (/ (image-width INVADER) 2))

(define LEFT-EDGE-INVADER (/ (image-width INVADER) 2))
(define RIGHT-EDGE-INVADER (- WIDTH (/ (image-width INVADER) 2)))
(define INVADER-BOTTOM (- HEIGHT INVADER-HEIGHT/2))

(define Y-HIT-RANGE (- HEIGHT (+ INVADER-HEIGHT/2 TANK-HEIGHT/2)))
(define X-HIT-RANGE 25)
;; Data Definitions:

(define-struct game (invaders missiles tank))
;; Game is (make-game  (listof Invader) (listof Missile) Tank)
;; interp. the current state of a space invaders game
;;         with the current invaders, missiles and tank position

;; Game constants defined below Missile data definition

#;
(define (fn-for-game s)
  (... (fn-for-loinvader (game-invaders s)) ; listofinvaders
       (fn-for-lom (game-missiles s))       ; listofmissiles
       (fn-for-tank (game-tank s))))        ; only one tank



(define-struct tank (x dir))
;; Tank is (make-tank Number Integer[-1, 1])
;; interp. the tank location is x, HEIGHT - TANK-HEIGHT/2 in screen coordinates
;;         the tank moves TANK-SPEED pixels per clock tick left if dir -1, right if dir 1

(define T00 (make-tank (/ WIDTH 2) 0))   ;center with no velocity
(define T0  (make-tank (/ WIDTH 2) 1))   ;center going right
(define T1  (make-tank 50 1))            ;going right
(define T1a  (make-tank 50 -1))     
(define T2  (make-tank 50 -1))           ;going left

#;
(define (fn-for-tank t)
  (... (tank-x t) (tank-dir t)))



(define-struct invader (x y dx))
;; Invader is (make-invader Number Number Number)
;; interp. the invader is at (x, y) in screen coordinates
;;         the invader along x by dx pixels per clock tick

(define I1 (make-invader 150 100 12))           ;not landed, moving right
(define I1b (make-invader 150 100 -12))           ;not landed, moving left
(define I2 (make-invader 150 HEIGHT -10))       ;exactly landed, moving left 
(define I3 (make-invader 150 (+ HEIGHT 10) 10)) ;> landed, moving right
(define I4 (make-invader 100 50 13))             ; not landed, moving right
(define I5 (make-invader (/ WIDTH 2) Y-HIT-RANGE 1)) ; invader that should hit center tank (for testing)
; according to the rules from Wikipedia, you lose the game if the invader reaches the bottom of the screen





(define-struct missile (x y))
;; Missile is (make-missile Number Number)
;; interp. the missile's location is x y in screen coordinates

(define M1 (make-missile 150 300))                       ;not hit U1
(define M2 (make-missile (invader-x I1) (+ (invader-y I1) 10)))  ;exactly hit U1 ; missile has just grazed the invader
(define M3 (make-missile (invader-x I1) (+ (invader-y I1)  5)))  ;> hit U1       ; missile has PENETRATED THE INVADER
(define M4 (make-missile 100 50)) ; missile in same-location as invader
#;
(define (fn-for-missile m)
  (... (missile-x m) (missile-y m)))


(define G00 (make-game empty empty T00))
(define G0 (make-game empty empty T0))
(define G1 (make-game empty empty T1))
(define G2 (make-game (list I1) (list M1) T1))
(define G3 (make-game (list I1 I2) (list M1 M2) T1))
(define G1a (make-game empty empty T1a))


(define (main game-state)
  (big-bang game-state                        ; Game
    (on-tick   new-gamestate)         ; Game -> Game
    (stop-when gameover? last-image)  ; Game -> Boolean
    (to-draw   render-game)           ; Game -> Image
    (on-key    handle-key)))          ; Game KeyEvent -> Game


;; Game -> Game
;; produce the next Gamestate
;; adjust each invader's position by it's x-speed and y-speed
;; adjust tank's position by it's x-speed
;; adjust missile's y-position by it's speed
;; insert new invaders if room
;; Game is (make-game  (listof Invader) (listof Missile) Tank)

;(define (new-gamestate game) game) ; stub

;; examples

(check-random (new-gamestate G0)
              (make-game (list (make-invader (random GENERATION-RANGE) INVADER-HEIGHT/2 1))
                         empty
                         (make-tank (+ (tank-x T0) TANK-SPEED) (tank-dir T0))))



;; template taken from game
(define (new-gamestate s)
  (make-game(advance-invaders (game-invaders s) (game-missiles s))
            (advance-missiles (game-missiles s))
            (advance-tank (game-tank s))))



;; !!!
;; (advance-invaders)
;; First moves invaders currently on screen
;; - advancing their x and y position by INVADER-X-SPEED and INVADER-Y-SPEED
;; - direction depends on invader-dir
;; Then adds newly created invaders to the list
;; with the amount being dependant on INVADE-RATE

;; ListofInvaders -> LostofInvaders

;(define (advance-invaders LOI) LOI) ; stub

;; examples

(check-random (advance-invaders empty empty) (list (make-invader (random GENERATION-RANGE) INVADER-HEIGHT/2 1)))
;(check-random (advance-invaders (list I1)) (list (make-invader (+ INVADER-X-SPEED 150) (+ INVADER-Y-SPEED 100) 12) (make-invader (random GENERATION-RANGE) INVADER-HEIGHT/2 1)))
(check-expect (advance-invaders (list (make-invader 100 INVADER-HEIGHT/2 12)) empty) (list (make-invader (+ INVADER-X-SPEED 100) (+ INVADER-Y-SPEED INVADER-HEIGHT/2) 12)))
;; template (function composition)

(define (advance-invaders LOI LOM)
  (new-invaders  (move-invaders  (delete-invaders LOI LOM))))




;; !!!
;; (move-invaders)
;; LOI -> LOI
;; adjusts the x and y position of each invader in listofinvaders by predefined constants
;; Direction is dependent on (invader-dir)


;(define (move-invaders loi) loi) ; stub


;; examples
(check-expect (move-invaders empty) empty)           ; base case, empty list
(check-expect (move-invaders (list I1)) (list (make-invader (+ INVADER-X-SPEED 150) (+ INVADER-Y-SPEED 100) 12)))
(check-expect (move-invaders (list I1 I1b))
              (list (make-invader (+ INVADER-X-SPEED 150) (+ INVADER-Y-SPEED 100) 12)
                    (make-invader (- 150 INVADER-X-SPEED) (+ 100 INVADER-Y-SPEED) -12))) 
(check-expect (move-invaders (list (make-invader LEFT-EDGE-INVADER 100 -1))) (list (make-invader (+ INVADER-X-SPEED LEFT-EDGE-INVADER) (+ INVADER-Y-SPEED 100) 1 )))

;; (template combined list and from invaders)

(define (move-invaders loi)
  (cond [(empty? loi) empty]
        [else (cons (if (hit-the-edge? (first loi))
                        (change-direction (first loi))
                        (if (> (invader-dx (first loi)) 0)
                            (make-invader (+ (invader-x (first loi)) INVADER-X-SPEED) (+ INVADER-Y-SPEED (invader-y (first loi))) (invader-dx (first loi)))
                            (make-invader (- (invader-x (first loi)) INVADER-X-SPEED) (+ INVADER-Y-SPEED (invader-y (first loi))) (invader-dx (first loi)))
                            ))
                    (move-invaders (rest loi)))]))
                   
               
;; Switch Directions
;; takes an inputted invader and changes it's dx from a negative to a positive or a positive to a negative
;; Invader -> Invader

;(define (change-direction i) I1) ; stub

(check-expect (change-direction I1)(make-invader SAFE-TURNAROUND (+ INVADER-Y-SPEED (invader-y I1)) (- (invader-dx I1))))
(check-expect (change-direction I2) (make-invader (+ (invader-x I2) INVADER-X-SPEED) (+ INVADER-Y-SPEED (invader-y I2)) (- (invader-dx I2))))
(check-expect (change-direction (make-invader LEFT-EDGE-INVADER 100 -1)) (make-invader (+ INVADER-X-SPEED LEFT-EDGE-INVADER) (+ INVADER-Y-SPEED 100) 1 ))
;; function template taken from invader


(define (change-direction invader)
  (if (rightward? invader)
      (make-invader SAFE-TURNAROUND (+ INVADER-Y-SPEED(invader-y invader)) (- (invader-dx invader)))
      (make-invader (+ (invader-x invader) INVADER-X-SPEED) (+ INVADER-Y-SPEED (invader-y invader)) (- (invader-dx invader)))))
                 
;; HittheEdge?
;; !!!
;; returns true if the invader has hit the edge of the screen
;; Invader -> Boolean

;(define (hit-the-edge? i) false) ;stub

;; examples/tests

(check-expect (hit-the-edge? I1) false)
(check-expect (hit-the-edge? (make-invader RIGHT-EDGE-INVADER 100 1)) true)
(check-expect (hit-the-edge? (make-invader LEFT-EDGE-INVADER  100 -1)) true)
(check-expect (hit-the-edge? (make-invader LEFT-EDGE-INVADER 100 -1)) true)
              

;; template copied from space invaders 

(define (hit-the-edge? invader)
  (or (<= (invader-x invader) LEFT-EDGE-INVADER) (>= (invader-x invader) RIGHT-EDGE-INVADER)))


;; Invader -> Boolean
;; returns true if the invaders' dx value is positive
;; returns false otherwise


;(define (rightward? i) true) ; stub

;; examples

(check-expect (rightward? I1) true)   ; examples
(check-expect (rightward? I2) false)  ; examples


;; function definition

(define (rightward? I)
  (if (> (invader-dx I) 0)
      true
      false))


;; New Invaders
;; ListofInvaders -> ListofInvaders
;; returns a new list of invaders based on how many are added as defined by INVADE-RATE

;(define (new-invaders loi) loi) ; stub

;; examples
(check-random (new-invaders empty) (list (make-invader (random GENERATION-RANGE) INVADER-HEIGHT/2 1)))
;(check-random (new-invaders (list I1)) (list I1 (make-invader (random GENERATION-RANGE) INVADER-HEIGHT/2 1))) ; is room for new invader
(check-expect (new-invaders (list (make-invader 100 INVADER-HEIGHT/2 -12))) (list (make-invader 100 INVADER-HEIGHT/2 -12)))
              
(define (new-invaders loi)
  (cond [(empty? loi) (cons (make-invader (random GENERATION-RANGE) INVADER-HEIGHT/2 1) empty)]
        [(make-new? (first loi))
         (cons (first loi)
               (new-invaders (rest loi)))]
        [else loi]))

;; Delete Invaders
;; ListofInvaders listofmissiles -> ListofInvaders
;; returns a new list of invaders with the hit invaders omitted
;(define (delete-invaders loi lom) loi) ; stub
         



;; delete-invaders
;; ListofInvaders ListofMissiles -> Boolean
;; returns true if the invader is not touching a missle, false otherwise

;(define (delete-invaders loi lom) true) ; stub
       
(check-expect (delete-invaders empty empty) empty) ;no invaders and no missiles, defaults to true
(check-expect (delete-invaders (list I1) empty) (list I1)) ; no missiles, therefore invader is safe
(check-expect (delete-invaders (list I4) (list M4)) empty) ; missile has hit invader


 
(define (delete-invaders loi lom)
  (cond  [(empty? loi) empty]
         [else (if (safe? (first loi) lom)
                   (cons (first loi)
                         (delete-invaders (rest loi) lom))
                   (delete-invaders (rest loi) lom))]))

(define (safe? i lom)
  (cond [(empty? lom) true]
        [else (if (or (> (abs (- (invader-x i) (missile-x (first lom)))) INVADER-SAFE-X)
                       (> (abs (- (invader-y i) (missile-y (first lom)))) INVADER-SAFE-Y))
                  (safe? i (rest lom))
                  false)]))
          
         
         
(define (make-new? i)
  (if (>= (invader-y i) (- HEIGHT (* INVADE-RATE INVADER-ROOM)))
      true
      false))
      
          

         




;;; !!! (advance missiles)
;;; changes missile's y position by (-) Missile-Speed

;(define (advance-missiles LOM) LOM) ; stub

;; examples

(check-expect (advance-missiles (list M1)) (list (make-missile 150 (- 300 MISSILE-SPEED))))
(check-expect (advance-missiles empty) empty)
(check-expect (advance-missiles (list M1 (make-missile 100 200))) (list (make-missile 150 (- 300 MISSILE-SPEED)) (make-missile 100 (- 200 MISSILE-SPEED)))) 


(define (advance-missiles lom)
  (cond [(empty? lom) empty]
        [else (cons (make-missile (missile-x (first lom)) (- (missile-y (first lom)) MISSILE-SPEED))
                    (advance-missiles(rest lom)))]))


;;; !!! (advance tank)
;; changes the tank's x position by a pre-defined amount
;; tank  -> tank
;(define (advance-tank t) t) ; stub

(check-expect (advance-tank T00) T00) 
(check-expect (advance-tank T0) (make-tank (+ TANK-SPEED (/ WIDTH 2)) (tank-dir T0)))
(check-expect (advance-tank T2) (make-tank (- 50 TANK-SPEED) (tank-dir T2)))



;;

(define (advance-tank t)
  (cond
    [ (and (> (tank-dir t)  0) (< (tank-x t) RIGHT-EDGE)) (make-tank (+ (tank-x t) TANK-SPEED) (tank-dir t))]
    [ (and (< (tank-dir t)  0) (> (tank-x t) LEFT-EDGE))(make-tank (- (tank-x t) TANK-SPEED) (tank-dir t))]
    [else t]))
     



;; Game -> Image
;; render image associated with current gamestate of world program
;; !!!

;(define (render-game ws) BACKGROUND) ; stub


;; examples

(check-expect (render-game G0) (place-image TANK (tank-x (game-tank G0)) TANK-BOTTOM BACKGROUND))
(check-expect (render-game G1) (place-image TANK (tank-x (game-tank G1)) TANK-BOTTOM BACKGROUND))
(check-expect (render-game G2)
              (place-image MISSILE (missile-x M1) (missile-y M1)
                           (place-image INVADER (invader-x I1) (invader-y I1)
                                        (place-image TANK (tank-x (game-tank G2)) TANK-BOTTOM BACKGROUND))))
;(check-expect (render-game G3) (place-image MISSILE (missile-x M2) (missile-y M2)
;                                            (place-image MISSILE (missile-x M1) (missile-y M1)
;                                                         (place-image INVADER (invader-x I1) (invader-y I1)
;                                                                      (place-image INVADER (invader-x I2) (invader-y I2)
;                                                                                   (place-image TANK (tank-x (game-tank G2)) TANK-BOTTOM BACKGROUND))))))

;; template for function operating on Game

;; right now I'm thinking about iteratively rendering each object one at a time
;; render- game will merely be a composition of all of these separate functions

(define (render-game s)
  (render-invaders (game-invaders s)
                   (render-missiles (game-missiles s)
                                    (render-tank (game-tank s)))))


;; (render tank)
;; tank -> image
;; renders the tank image on the correct place within MTS based on it's x position

;(define (render-tank t) BACKGROUND) ; stub

(check-expect (render-tank T1) (place-image TANK (tank-x T1) TANK-BOTTOM BACKGROUND))

(define (render-tank t)
  (place-image TANK (tank-x t) TANK-BOTTOM  BACKGROUND))


;; (render missiles)
;; LOM IMG -> IMG
;; takes a given background and list of missiles and renders the missiles on the background

;(define (render-missiles lom img) BACKGROUND) ; stub


;; examples

(check-expect (render-missiles empty BACKGROUND) BACKGROUND) ; no missiles to render
(check-expect (render-missiles  (list M1) BACKGROUND) (place-image MISSILE 150 300 BACKGROUND))
(check-expect (render-missiles  (list M1 M2) BACKGROUND) (place-image MISSILE (missile-x M2) (missile-y M2) (place-image MISSILE (missile-x M1) (missile-y M1) BACKGROUND)))

;; template copied from missiles

  
(define (render-missiles lom img)
  (cond [(empty? lom) img]
        [else (place-image MISSILE (missile-x (first lom)) (missile-y (first lom))
                           (render-missiles (rest lom) img))]))




;; (render-invaders)
;; ListofInvaders Image-> Image
;; Takes an inputted listofinvaders and places their respective images on the given image
;; if the ListofInvaders is empty the image is returned unchanged

;(define (render-invaders LOI img) BACKGROUND) ; stub

;; examples

(check-expect (render-invaders empty BACKGROUND) BACKGROUND)
(check-expect (render-invaders (list I1) BACKGROUND) (place-image INVADER (invader-x I1) (invader-y I1) BACKGROUND))
(check-expect (render-invaders (list I1 I2) BACKGROUND) (place-image INVADER (invader-x I2) (invader-y I2)(place-image INVADER (invader-x I1) (invader-y I1) BACKGROUND)))

;; template borrowed from invaders

(define (render-invaders loi img)
  (cond [(empty? loi) img]
        [else (place-image INVADER (invader-x (first loi)) (invader-y (first loi))
                           (render-invaders (rest loi) img))]))
                                                      



         
;; !!!
;; Game -> Boolean
;; produces true if INVADER reaches bottom, or if ship is hit

;(define (gameover? game) false) ; stub

;; examples

(check-expect (gameover? G3) true)  ; alien crosses over
(check-expect (gameover? G0) false) ; start of game

;; template

(define (gameover? game)
  (or(hit-tank? (game-invaders game) (game-tank game))
     (hitbottom? (game-invaders game))))
   
  

;; template copied from game

;(define (fn-for-game s)
;  (... (fn-for-loinvader (game-invaders s))
;       (fn-for-lom (game-missiles s))
;       (fn-for-tank (game-tank s))))


;; ListofInvaders -> Boolean
;; produces true if Invader has touched the ground
;; as in any invader in listofInvaders has a y-position greater than or equal to Height- 1/2 invader-height

;(define (hitbottom? loi) false) ; stub

(check-expect (hitbottom? (list I1 I1b)) false)
(check-expect (hitbottom? (list I3)) true)
(check-expect (hitbottom?  (list I1 I3)) true)
(check-expect (hitbottom? empty) false)

(define (hitbottom? loi)
  (cond [(empty? loi) false]
        [else (if (>= (invader-y (first loi)) INVADER-BOTTOM)
                  true
                  (hitbottom? (rest loi) ))]))

;; ListofInvaders Tank -> Boolean
;; produces true if any invader in ListofInvaders has hit tank
;; as in the invader's X and Y-position is within hit-range of Tank

;(define (hit-tank? loi t) false) ; stub

(check-expect (hit-tank? empty  T0) false)
(check-expect (hit-tank? (list I1) T0) false)
(check-expect (hit-tank? (list I1 I4) T0) false)
(check-expect (hit-tank? (list I5   ) T0)  true)
(check-expect (hit-tank? (list I5 I1) T0)  true)

 
;; template taken from listofinvaders
              
              
(define (hit-tank? loi t)
  (cond [(empty? loi) false]
        [else (if (and (>= (invader-y (first loi)) Y-HIT-RANGE) (<= (abs (- (invader-x (first loi)) (tank-x t)))  X-HIT-RANGE))
                  true
                  (hit-tank? (rest loi) t))]))

;; !!!
;; Last Image
;; Game-> Image
;; takes an inputted game state and renders a new image with OVER-MESSAGE OVERLAYED

(define (last-image g)
  (place-image OVER-MESSAGE CTR-X CTR-Y (render-game g)))

;; !!!
;; On-key
;; Game KeyEvent -> Game
;; takes inputted game state and applies appropriate changes based on keyevents
;; - if left-arrow pressed then move ship left by speed
;; - if right-arrow pressed then move ship right by speed
;; - if space-bar pressed shoot missile

;(define (handle-key ws ke) ws) ; stub

(check-expect (handle-key G1 "r") G1) ; no key pressed
(check-expect (handle-key G1 "left")  (make-game empty empty (make-tank 50 -1)))
(check-expect (handle-key G1a "right") (make-game empty empty (make-tank 50  1)))
(check-expect (handle-key G1 " "     ) (make-game empty (list (make-missile (tank-x T1) MISSILE-Y-START)) T1))
;; template taken from handle-key

(define (handle-key game ke)
  (cond
    [(key=? ke "left")  (change-direction-tank game)]
    [(key=? ke "right") (change-direction-tank game)]
    [(key=? ke " "    ) (shoot-missile game)]
    [else 
     game]))


;; change-direction-tank
;; game -> game
;; takes inputted gamestate and produces new gamestate unchanged, except for tank with opposite dx value
;; i.e negative -> positive or positive -> negative


;(define (change-direction-tank t) t) ; stub


;; examples

(check-expect (change-direction-tank G1) (make-game (game-invaders G1) (game-missiles G1)  (make-tank 50 -1)))
(check-expect (change-direction-tank G1a)(make-game (game-invaders G1a) (game-missiles G1a)  (make-tank 50  1)))

;; template taken from tank

(define (change-direction-tank game)
  (make-game
   (game-invaders game)
   (game-missiles game)
   (make-tank (tank-x (game-tank game) ) (- (tank-dir (game-tank game))))))


;; Game -> Game
;; (shoot missile)
;; creates a new game with an additional missile upon user inputting " " key-press
;; All missiles start with Y value of MISSILE-Y-START and an x-value of whatever the tank's x-value is is
;; (I hope the centers will align correctly

;(define (shoot-missile g) g) ; stub

;; examples

(check-expect (shoot-missile G0) (make-game empty (list (make-missile (tank-x (game-tank G0)) MISSILE-Y-START)) T0))


;; template taken from missile / game





(define (shoot-missile g)
  (make-game
   (game-invaders g)
   (cons 
    (add-missile (game-tank g)) (game-missiles g))
   (game-tank g)))
;
;
(define (add-missile t)
  (make-missile (tank-x t) MISSILE-Y-START))

;(define (add-missile lom)
;  (cond [(empty? lom) empty]
;        [else (cons (