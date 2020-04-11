;;; richii.el --- Implementation of Richii (Japanese) Mahjong for Emacs

;; TODO: license here

;;; Commentary:

;;; Code:
;; (require 'cl-lib)

(defun richii/make-numeric-tileset (name)
  "Return a mahjong tileset of the symbol NAME."
  (loop
   for i from 1 upto 9
   collect `(,name . ,i)))

(defconst richii/sou-tiles (richii/make-numeric-tileset 'sou))
(defconst richii/pin-tiles (richii/make-numeric-tileset 'pin))
(defconst richii/man-tiles (richii/make-numeric-tileset 'man))

(defconst richii/wind-tiles '((wind north)
			      (wind south)
			      (wind east)
			      (wind west)))

(defconst richii/dragon-tiles '((dragon green)
				(dragon red)
				(dragon white)))

(defconst richii/tileset
  ;; Splice all five tilesets together, then duplicate twice.
  (let ((l (append richii/sou-tiles
		   richii/pin-tiles
		   richii/man-tiles
		   richii/wind-tiles
		   richii/dragon-tiles)))
    (append l l l l))
    "Create the full set of richii tiles. This includes all three sets
of simple tiles: Sou, Pin, and Man, and the honor sets: Winds and
Dragons.")

(defun richii/kshuffle (list)
  "Perform a Knuth/Fisher-Yates shuffle on LIST.  Taken from stackoverflow."
  (loop for i from (length list) downto 2
	do (rotatef (elt list (random i))
		    (elt list (1- i))))
  list)

(defun richii ()
  "Start richii mode."
  ;; Make new buffer
  ;; Go richii-mode
  ;; Clear screen
  ;; Setup hands
  ;; Draw em
  ;; Start game
  )

(provide 'richii)
;;; richii.el ends here
