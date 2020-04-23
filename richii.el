;;; richii.el --- Implementation of Richii (Japanese) Mahjong for Emacs. -*- lexical-binding: t; -*-

;; TODO: license here

;;; Commentary:
;; Originally I was using SVG to draw the tiles, but using Unicode makes the tiles more readable,
;; and saves the effort of drawing in SVG, albeit at the cost of less control over positioning.
;; Rather than go for accuracy of a 4 player board I think optimising for 1 player ergonomics
;; is a better route.

;;; Code:
(require 'esxml)

;; TODO: make all these consts defcustoms

(defconst richii/richii-buffer-name "*richii*"
  "The name of the buffer created when starting richii mode.")

(defconst richii/edge-padding 20)

;; 
;; Tile Functions
;; 

(defun richii/make-numeric-tileset (name)
  "Return a mahjong tileset of the symbol NAME."
  (loop
   for i from 1 upto 9
   collect `(,name . ,i)))

(defconst richii/sou-tiles (richii/make-numeric-tileset 'sou))
(defconst richii/pin-tiles (richii/make-numeric-tileset 'pin))
(defconst richii/man-tiles (richii/make-numeric-tileset 'man))

(defconst richii/wind-tiles '((wind . north)
			      (wind . south)
			      (wind . east)
			      (wind . west)))

(defconst richii/dragon-tiles '((dragon . green)
				(dragon . red)
				(dragon . white)))

(defconst richii/tileset
  ;; Splice all five tilesets together, then duplicate twice.
  (let ((l (append richii/sou-tiles
		   richii/pin-tiles
		   richii/man-tiles
		   richii/wind-tiles
		   richii/dragon-tiles)))
    (append l l l l))
  "The full set of richii tiles.
This includes all three sets of simple tiles: Sou, Pin, and Man,
and the honor sets: Winds and Dragons.")

;; Helpers to tidy up sorting etc
;; use '-' syntax to avoid writing soup lmao
(defun richii/sou-p (tile)
  (eq (car tile) 'sou))

(defun richii/pin-p (tile)
  (eq (car tile) 'man))

(defun richii/man-p (tile)
  (eq (car tile) 'pin))

(defun richii/wind-p (tile)
  (eq (car tile) 'wind))

(defun richii/dragon-p (tile)
  (eq (car tile) 'dragon))

(defun richii/kshuffle (list)
  "Perform a Knuth/Fisher-Yates shuffle on LIST.
Taken from https://stackoverflow.com/a/49505968."
  (loop for i from (length list) downto 2
	do (rotatef (elt list (random i))
		    (elt list (1- i))))
  list)

(defun richii/make-hands ()
  "Return a list of 4 shuffled hands (themselves lists).
First a full tileset is made, then shuffled, then split into four."
  (let* ((tileset (richii/kshuffle richii/tileset))
	 (hand-size 13))
    (loop
     for seat in '(east south north west)
     for hand in (seq-partition tileset hand-size)
     collect `(,seat . ,hand))))

(defun richii/sort-hand (hand)
  "Take HAND and sort it according to the usual mahjong rules:
(Row in sort order)
Numerals: Man < Pin < Sou.
Winds: South < East < West < (North)?
Dragons: White < Green < Red"
  ;; In order to implement an easy sort, we'll assign numeric
  ;; values to the above tile categories. Then we can just use <
  (let ((tile-order [man pin sou wind dragon])
	(wind-order [south east west north])
	(dragon-order [white green red]))
    (cl-labels ((dragon-sort (a b)
			     (< (seq-position dragon-order (cdr a))
				(seq-position dragon-order (cdr b))))
		(wind-sort (a b)
			   (< (seq-position wind-order (cdr a))
			      (seq-position wind-order (cdr b))))
		(tile-sort-predicate (a b)
				     (cond
				      ;; Both tiles are same set, compare based on value
				      ((eq (car a) (car b))
				       (cond
					((or (richii/man-p a)
					     (richii/pin-p a)
					     (richii/sou-p a)) (< (cdr a) (cdr b)))
					((richii/wind-p a) (wind-sort a b))
					((richii/dragon-p a) (dragon-sort a b))))
				      ;; Tiles are different sets, use tile order
				      (t (< (seq-position tile-order (car a))
					    (seq-position tile-order (car b)))))))
      (sort hand #'tile-sort-predicate))))

;; 
;; Drawing functions
;;

(defun richii/hand-to-string (hand)
  "Take a hand list HAND and return a unicode string representing it."
  (let ((sou-base #x1F010)
	(pin-base #x1F019)
	(man-base #x1F007)
	(wind-north-id #x1F003)
	(wind-south-id #x1F001)
	(wind-east-id #x1F000)
	(wind-west-id #x1F002)
	(dragon-red-id #x1F004)
	(dragon-green-id #x1F005)
	(dragon-white-id #x1F006))
    (loop for (set . value) in hand
	  concat (string (cond
			  ((eq set 'sou) (+ sou-base value))
			  ((eq set 'pin) (+ pin-base value))
			  ((eq set 'man) (+ man-base value))
			  ((eq set 'wind)
			   (cond
			    ((eq value 'north) wind-north-id)
			    ((eq value 'south) wind-south-id)
			    ((eq value 'east)  wind-east-id)
			    ((eq value 'west)  wind-west-id)))
			  ((eq set 'dragon)
			   (cond
			    ((eq value 'red)   dragon-red-id)
			    ((eq value 'green) dragon-green-id)
			    ((eq value 'white) dragon-white-id))))))))

(defun richii/draw-closed-hand (hand side)
  "Draw a closed hand HAND on SIDE. Called tiles will be shown,
but all others drawn upright and obscured."
  ;; TODO don't use ESNW, use like ABCD and map them to
  ;; appropriate seat winds
  (let ((seat-letter (cond
		      ((eq side 'south) "S")
		      ((eq side 'north) "N")
		      ((eq side 'west)  "W"))))
    (insert seat-letter)
    (insert " ")
    (insert (richii/hand-to-string hand) "\n")))

(defun richii/draw-open-hand (hand)
  "Draw the players hand."
  ;; TODO make these less hardcoded
  (insert "E ")
  (insert (richii/hand-to-string (richii/sort-hand hand)) "\n"))

(defun richii/draw-hands (hands)
  "Draw four HANDS on screen, where HANDS is a list of four hand lists."
  (loop for seat in '(south north west)
	do (richii/draw-closed-hand (cdr (assoc seat hands)) seat))

  ;; TODO defcustom separator char
  (insert "\n" (make-string 15 ?-) "\n\n")
  (richii/draw-open-hand (cdr (assoc 'east hands))))

(defun richii/draw (prevalent-wind hands)
  ;; Write prevalent wind
  ;; Write 
  (richii/draw-hands hands))

(define-derived-mode richii-mode
  special-mode "Richii Mahjong"
  "Richii Mahjong game mode."
  :group 'games)

;; TODO mode keymap
;; left/right - switch highlighted tile
;; enter - discard

;; (define-key 'richii-mode-map (kbd "<left>") nil)
;; (define-key 'richii-mode-map (kbd "<right") nil)
;; (define-key 'richii-mode-map (kbd "<return>") nil)

(defun richii ()
  "Start a game of richii mahjong."
  (interactive)
  ;; Make new buffer/clear restart current
  (switch-to-buffer richii/richii-buffer-name)
  (erase-buffer)
  
  ;; Go richii-mode
  
  ;; Setup hands
  (setq hands (richii/make-hands))
  
  ;; Draw em
  (goto-line 1)
  (richii/draw 'east hands)
  
  ;; Start game
  ;; At end of each turn, redraw hands.
  ;; Player turn, set hand.
  ;; compute AI turns, setting hand.
  ;; 
  )

(provide 'richii)
;;; richii.el ends here
