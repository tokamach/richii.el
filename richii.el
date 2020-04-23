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
  ;; Todo just use index of SNW in list   
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
  (insert (richii/hand-to-string hand) "\n"))

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
