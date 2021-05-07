;;; djvu3.el --- Extend djvu.el

;; Copyright (C) 2021 Free Software Foundation, Inc.


;; Author: D. L. Nicolai <dalanicolaih@gmail.com>
;; Version: 1.0
;; Package-Requires: ((svg "1.1"))
;; Keywords: documents
;; URL: https://github.com/dalanicolai/djvu+


;;; Commentary:

;; This package provides some extensions to djvu.el. In particular it implements
;; display of annotations with convenient keyboard function and fast djvu-occur
;; search functionality, and continue at last viewed page when revisiting a
;; file.

;; DONE check `djvu2.el' for arrow-keybindings in djvu-image-mode map
;; TODO try to make `djvu-image-invert' work
;; DONE update `djvu-mouse-rect-area', `djvu-mouse-text-area-internal',
;;`djvu-mouse-line-area-arrow' and `djvu-mouse-line-area-internal' functions
;; (see `djvu2.el`)
;; DONE add clickable links by adding `:map' keyword to `create-image' as follows:
;; :map '(((rect . ((0 . 0) . (100 . 100))) area4 (:pointer hand)))


(require 'djvu)
(require 'svg)
(require 'tablist)

;;; EXTEND SVG WITH MARKERS (INCL. ARROWHEADS)

(defun svg-marker (svg id width height &optional color reverse)
  "Add a gradient with ID to SVG.
TYPE is `linear' or `radial'.
STOPS is a list of percentage/color pairs."
  (svg--def
   svg
   (apply
    'dom-node
    'marker 
    `((id . ,id)
      (viewBox . "0 0 10 10")
      (refX . 5)
      (refY . 5)
      ,(pcase id
         ("arrow" `(markerWidth . ,width))
         ("dot" `(markerWidth . ,width)))
      ,(pcase id
         ("arrow" `(markerHeight . ,height))
         ("dot" `(markerHeight . ,height)))
      ,(pcase id
         ;; ("arrow" '(orient . auto-start-reverse))))
         ("arrow" (if reverse
                      '(orient . auto)
                    '(orient . auto-start-reverse)))))
    (pcase id
      ("arrow" (list (dom-node 'path `((d . "M 0 0 L 10 5 L 0 10 z")
                                       (fill . ,(or color "black"))))))
      ("dot" (list (dom-node 'circle `((cx . 5)
                                       (cy . 5)
                                       (r . 5)
                                       (fill . ,(or color "black"))))))))))

;;; START OF DJVU-EXTENSION

(defcustom djvu-restore-filename (if dotspacemacs-directory
                                     (concat dotspacemacs-directory ".djvu-view-restore")
                                   ".djvu-view-restore")
  "Filename to save the last known pdf position."
  :group 'djvu
  :type 'string)

(defvar-local djvu-doc-hotspots nil
  "Hotspots (i.e. cliackable links) of current page of a Djvu document.
This is a list.")

;;; Annotations

(defun djvu-annots-listify (doc)
"Create list with annotations.
DOC is a djvu-read-buffer object.
The list is created by wrapping the annotations in the annotation
buffer in a list after converting the color hex-symbols into
strings."
  (interactive)
  (let ((buffer (get-buffer-create "*annot-list*")))
    (with-current-buffer (djvu-ref annot-buf doc)
      (copy-to-buffer "*annot-list*" (point-min) (point-max)))
    (with-current-buffer buffer
      (while (re-search-forward " \\(#[[:alnum:]]+\\)" nil t)
        (replace-match " \"\\1\""))
      (goto-char (point-min))
      (insert "(")
      (goto-char (point-max))
      (insert ")")
      (emacs-lisp-mode)
      (indent-region (point-min) (point-max))
      (read (buffer-string)))))

;; Another fast function for listify (for use when annot-buffer is not available
;; .e.g. in DocView mode)

;; (defun djvu-annots-listify (doc)
;;   "Return list of page's annotations.
;; Code written to be usable in djvu- and in doc-view-mode."
;;   (interactive)
;;   (let ((file doc))
;;     (with-temp-buffer
;;       (shell-command (format "djvused %s -e 'select %s; print-ant'"
;;                              (shell-quote-argument file)
;;                              page)
;;                      (current-buffer))
;;       (goto-char (point-min))
;;       (while (re-search-forward " #\\([[:alnum:]]+\\)" nil t)
;;         (replace-match " \"\\1\""))
;;       (goto-char (point-min))
;;       (insert "(")
;;       (goto-char (point-max))
;;       (insert ")")
;;       (read (buffer-string)))))

(makunbound 'djvu-image-mode-map)
(define-minor-mode djvu-image-mode
  "Image display of current page."
  :lighter "Image"
  :keymap '(([drag-mouse-1]   . djvu-mouse-rect-area)
            ([S-drag-mouse-1] . djvu-mouse-text-area)
            ([C-drag-mouse-1] . djvu-mouse-text-area-pushpin)
            ([drag-mouse-2]   . djvu-mouse-line-area)
            ([S-drag-mouse-2] . djvu-mouse-line-area-horiz)
            ([C-drag-mouse-2] . djvu-mouse-line-area-vert)
            ([C-S-drag-mouse-2] . djvu-mouse-line-area-arrow)
            ;;
            ;; ([down-mouse-1]   . djvu-mouse-drag-track-area)
            ;; ([S-down-mouse-1] . djvu-mouse-drag-track-area)
            ;; ([C-down-mouse-1] . djvu-mouse-drag-track-area)
            ;; ([down-mouse-2]   . (lambda (event) (interactive "e")
            ;;                       (djvu-mouse-drag-track-area event t)))
            ;; ([S-down-mouse-2] . (lambda (event) (interactive "e")
            ;;                       (djvu-mouse-drag-track-area event 'horiz)))
            ;; ([C-down-mouse-2] . (lambda (event) (interactive "e")
            ;;                       (djvu-mouse-drag-track-area event 'vert)))
            ;; ([C-S-down-mouse-2] . (lambda (event) (interactive "e")
            ;;                         (djvu-mouse-drag-track-area event 'arrow)))
            ;; FIXME: The following binding has no effect.  Why??
            ([M-drag-mouse-1] . djvu-mouse-word-area)
            ;; ([M-down-mouse-1] . djvu-mouse-drag-track-area)
            ([drag-mouse-3]   . djvu-mouse-word-area) ; substitute
            ;; ([down-mouse-3]   . djvu-mouse-drag-track-area) ; substitute
            ;;
            ("C-c m" . djvu-invert)
            ("+" . djvu-image-zoom-in)
            ("-" . djvu-image-zoom-out))

  ;; Adopted from `doc-view-mode'
  (image-mode-setup-winprops) ; record current scroll settings
  ;; Don't scroll unless the user specifically asked for it.
  (setq-local auto-hscroll-mode nil)

  (if (and djvu-image-mode
           (not (get-text-property (point-min) 'display)))
      ;; Remember DPOS if we enable `djvu-image-mode'.
      (djvu-set read-pos (let (djvu-image-mode)
                           (djvu-read-dpos))))
  (let ((tmp (and (not djvu-image-mode)
                  (get-text-property (point-min) 'display))))
    (djvu-image)
    ;; Go to DPOS if we disable `djvu-image-mode'.
    (if tmp (djvu-goto-read (djvu-ref read-pos)))))

(defun djvu-event-to-area (event &optional dir)
  "Convert mouse EVENT to Djvu area coordinates."
  (let* ((e-start (event-start event))
         (e-end   (event-end   event))
         (_ (unless (and (posn-image e-start) (posn-image e-end))
              (user-error "Area not over image")))
         (start (posn-object-x-y e-start))
         (end   (posn-object-x-y e-end))
         (x1 (car start)) (y1 (cdr start)) (x2 (car end)) (y2 (cdr end))
         (size (posn-object-width-height e-start))
         (_ (if (equal size '(0 . 0))
                (error "See Emacs bug#18839 (GNU Emacs 24.4)")))
         (width  (/ (float (car (djvu-ref pagesize))) (car size)))
         (height (/ (float (cdr (djvu-ref pagesize))) (cdr size)))
         (area
          (list (round (* (if (memq dir '(vert free arrow))
                              x1 (min x1 x2))
                          width))
                (round (* (- (cdr size) (if (memq dir '(horiz free arrow))
                                            y1 (max y1 y2)))
                          height))
                (round (* (if (memq dir '(vert free arrow))
                              x2 (max x1 x2))
                          width))
                (round (* (- (cdr size) (if  (memq dir '(horiz free arrow))
                                            y2 (min y1 y2)))
                          height)))))
    (djvu-set read-pos (djvu-mean-dpos area))
    area))

(defun djvu-mouse-line-area-arrow (event)
  (interactive "e")
  (djvu-mouse-line-area-internal event 'arrow))

(defun djvu-mouse-line-area-internal (event &optional dir)
  (djvu-with-event-buffer event
    (let* ((line (djvu-event-to-area event dir))
           (color (djvu-interactive-color djvu-color-line))
           (text (read-string (format "(%s) Line: " color)
                              nil nil nil djvu-inherit-input-method)))
      (cond ((eq dir 'horiz)
             (setq line (list (nth 0 line) (nth 1 line)
                              (nth 2 line) (nth 1 line))))
            ((eq dir 'vert)
             (setq line (list (nth 0 line) (nth 1 line)
                              (nth 0 line) (nth 3 line)))))
      (if (eq dir 'arrow)
          (djvu-line-area nil text line nil t djvu-line-width djvu-color-line)
        (djvu-line-area nil text line nil nil djvu-line-width djvu-color-line))
      (djvu-set image nil)
      (djvu-image nil t))))

;; Extended version of `djvu-image' from original djvu.el. Implements display of
;; annotations with svg.el and embedding the ppm image in an svg image. Also
;; adds update flag to allow for immediate update after creating annotation.

(defun djvu-image (&optional isize update match)
  "If `djvu-image-mode' is enabled, display image of current Djvu page.
Otherwise remove the image."
  ;; Strange!  `djvu-image' modifies the buffer (its text properties).
  ;; Nonetheless, we end up with an unmodified buffer.  This holds,
  ;; in particular, for the "bare" calls of `djvu-image' by
  ;; `djvu-image-zoom-in' and `djvu-image-zoom-out'.
  (if (not djvu-image-mode)
      (if (get-text-property (point-min) 'display)
          (let (buffer-read-only)
            (remove-text-properties (point-min) (point-max) '(display nil))))
    ;; Update image if necessary.
      (when (or update (not (eq (djvu-ref page) (car (djvu-ref image))))
                (and isize
                     (not (eq isize (nth 1 (djvu-ref image))))))
        (let ((isize (or isize
                         (nth 1 (djvu-ref image))
                         djvu-image-size))
              (doc djvu-doc)
              (inhibit-quit t))
          (with-temp-buffer
            (set-buffer-multibyte nil)
            (let* ((coding-system-for-read 'raw-text)
                   ;; For a rectangular image, ISIZE does not give us
                   ;; the actual size of the image, but (max width height)
                   ;; will be equal to ISIZE.
                   (status (call-process "ddjvu" nil t nil
                                         (format "-size=%dx%d" isize isize)
                                         "-format=ppm"
                                         (format "-page=%d" (djvu-ref page doc))
                                         (djvu-ref file doc))))
              (unless (zerop status)
                (error "Ddjvu error %s" status))
              ;; (let* ((pre-im (create-image (buffer-substring-no-properties
              ;;                            (point-min) (point-max))
              ;;                           'pbm t))
              ;;        (post-im (djvu-image-invert pre-im))
              ;;        (ppm (create-image post-im
              ;;                           'pbm t))
              (let* ((ppm (create-image (buffer-substring-no-properties
                                         (point-min) (point-max))
                                        'pbm t))
                     (size (image-size ppm t))
                     (scaling-factor (/ isize (float (cdr (djvu-ref pagesize doc)))))
                     (svg (svg-create (car size) (cdr size)))
                     url-data)
                (svg-marker svg "arrow" 8 8 "black" t)
                (svg-embed svg (image-property ppm :data) "image/x-portable-bitmap" t
                           :width (format "%spx" (car size)) :height (format "%spx" (cdr size))
                           :x "0px" :y "0px")

                ;; Draw annotations
                (dolist (annot (djvu-annots-listify doc))
                  (when annot
                    (pcase (car annot)
                      ('background (message "Viewer are color should be %s (not (yet) implemented)" (car annot)))
                      ('zoom (message "Zoom value should be %s (not (yet) implemented)" (car annot)))
                      ('mode (message "Mode value should be %s (not (yet) implemented)" (car annot)))
                      ('align (message "Horizontal annot vertical align should be %s %s (not (yet) implemented)"
                                       (nth 1 annot) (nth 2 annot)))
                      ('maparea
                       (let* ((url (nth 1 annot))
                              (comment (nth 2 annot))
                              (area (if (listp (car (nth 3 annot)))
                                        (nth 3 annot)
                                      (list (nth 3 annot)))))
                         (dolist (a area)
                           (let* ((coords (mapcar (lambda (x) (* x scaling-factor)) (cdr a)))
                                  ;; y coord is defined from bottom up, while svg is top down
                                  ;; therefore we must add ywidth to y0 (hence we assign ywidth before y0)
                                  (xwidth (nth 2 coords))
                                  (ywidth (nth 3 coords))
                                  (x0 (nth 0 coords))
                                  (y0 (- isize (nth 1 coords)))
                                  (x1 (nth 2 coords))
                                  (y1 (- isize (nth 3 coords)))
                                  (options (nthcdr 4 annot))
                                  (svg-command-data (pcase (car a)
                                                      ('rect (cons 'svg-rectangle
                                                                   (list x0 y1 (- x1 x0) (- y0 y1))))
                                                      ;; transformation for area from direct djvused annots
                                                      ;; ('rect (list x0 (- y0 ywidth) xwidth ywidth))
                                                      ('oval (cons 'svg-ellipse
                                                                   (list (/ (+ x0 x1) 2)
                                                                         (/ (+ y0 y1) 2)
                                                                         (/ (- x1 x0) 2)
                                                                         (/ (- y0 y1) 2))))
                                                      ;; ('poly 'svg-polygon)
                                                      ('text (cons 'svg-text
                                                                   (list comment
                                                                         :x x0
                                                                         :y y0
                                                                         :font-size (- y0 y1))))
                                                      ('line (cons 'svg-line (list x0 y0 x1 y1))))))
                             (apply (car svg-command-data)
                                    svg
                                    (append
                                     (cdr svg-command-data)
                                     (if-let (x (car (alist-get 'opacity options)))
                                         (list :opacity (/ x 100.0))
                                       (pcase (car a)
                                         ((or 'text 'line) (list :opacity 1.0))
                                         (_ (list :opacity 0.3))))
                                     (when-let (x (car (alist-get 'hilite options)))
                                       (list :fill-color (car (rassoc (format "%s" x) djvu-color-alist))))
                                     (when-let (x (car (alist-get 'width options)))
                                       (list :stroke-width x))
                                     (if-let (x (car (alist-get 'lineclr options)))
                                         (list :stroke-color (car (rassoc (format "%s" x) djvu-color-alist)))
                                       (when (equal (car a) 'line)
                                         (list :stroke-color "black")))
                                     (when-let (x (car (alist-get 'textclr options)))
                                       (list :fill x))
                                     (when (assoc 'arrow options)
                                       (list :marker-end "url(#arrow)"))
                                     ))
                             (when (not (= (length url) 0))
                               (push (list (cons 'rect
                                                 (cons (cons (truncate x0) (truncate y1))
                                                       (cons (truncate x1) (truncate y0))))
                                           (intern (mapconcat 'number-to-string
                                                              (mapcar 'truncate (list x0 y1 x1 y0)) "-"))
                                           (list 'pointer 'hand 'help-echo url))
                                     url-data))
                             ))))
                      )))
                (when match
                  (let* ((coords (mapcar (lambda (x) (* x scaling-factor)) match))
                         (x0 (nth 0 coords))
                         (y0 (- isize (nth 1 coords)))
                         (x1 (nth 2 coords))
                         (y1 (- isize (nth 3 coords))))
                    (apply 'svg-rectangle svg (append
                                               (list x0 y1 (- x1 x0) (- y0 y1))
                                               (list :fill-color "green")
                                               (list :opacity 0.5)))))
                ;; (let* ((url (nth 1 annot))
                ;;        (comment (nth 2 annot))
                ;;        (area (mapcar (lambda (x) (* x scaling-factor))  (cdr (nth 3 annot))))
                ;;        ;; area y coord is defined from bottom up, while svg is top down
                ;;        ;; therefore we must add ywidth to y0 (hence we assign ywidth before y0)
                ;;        (xwidth (nth 2 area))
                ;;        (ywidth (nth 3 area))
                ;;        (x0 (nth 0 area))
                ;;        (y0 (- isize (+ (nth 1 area) ywidth)))
                ;;        (options (nthcdr 4 annot)))
                ;;   (svg-rectangle svg x0 y0 xwidth ywidth :fill "red" :opacity "0.5" :stroke "red")))))
                (djvu-set image
                          (append (list (djvu-ref page doc) isize)
                                  ;; Images are lists
                                  (svg-image svg
                                             :map url-data
                                             ;; :map '(((rect . ((0 . 0) . (100 . 100))) area4 (:pointer hand)))
                                             ))
                          doc)
                (djvu-set hotspots url-data doc)
                (djvu-ref hotspots doc)
                )))))
    ;; Display image.
    (let ((hscroll (window-hscroll))
          buffer-read-only)
      (if (= (point-min) (point-max)) (insert " "))
      (put-text-property (point-min) (point-max)
                         'display (nthcdr 2 (djvu-ref image)))
      (set-window-hscroll (selected-window) hscroll))
    (dolist (x (djvu-ref hotspots))
      (local-set-key
       (vector (nth 1 x) 'mouse-1)
       (lambda (event)
         (interactive "@e")
         (let ((hs-list (djvu-ref hotspots)))
           (while (not (eq (posn-area (nth 1 event)) (nth 1 (car hs-list))))
             (setq hs-list (cdr hs-list)))
           (djvu-goto-page (string-to-number (substring (plist-get (nth 2 (car hs-list)) 'help-echo) 1)))))))))

;; Shorter verion of `djvu-image-rect' of original function in djvu.el. Possibly
;; cut off to much.

;; THIS FUNCTION IS NOT USED IN DJVU3
(defun djvu-image-rect (&optional event line)
  (print "This message from `djvu-image-rect' can be neglected. How to unbind in djvu.el defined keymap?"))
;;   "For PPM image specified via EVENT mark rectangle by inverting bits."
;;   ;; FIXME: Can the following be implemented more efficiently in the
;;   ;; image display code?  Could this be useful for other packages, too?
;;   (if event
;;       (let* ((e-start (event-start event))
;;              (e-end   (event-end   event))
;;              (_ (unless (and (posn-image e-start) (posn-image e-end))
;;                   (user-error "Area not over image")))
;;              (start (posn-object-x-y e-start))
;;              (end   (posn-object-x-y e-end))
;;              (x1 (if line (car start)
;;                    (min (car start) (car end))))
;;              (y1 (if line (cdr start)
;;                    (min (cdr start) (cdr end))))
;;              (x2 (if line (car end)
;;                    (max (car start) (car end))))
;;              (y2 (if line (cdr end)
;;                    (max (cdr start) (cdr end))))
;;              (image (copy-sequence (nth 6 (djvu-ref image))))
;;              ))))
    ;;          ;; (_ (unless (string-match "\\`P6\n\\([0-9]+\\) +\\([0-9]+\\)\n\\([0-9]+\\)\n" image)
    ;;          ;;      (error "Not a PPM image")))
    ;;          (width (djvu-match-number 1 image))
    ;;          ; (height (djvu-match-number 2 image))
    ;;          (depth (djvu-match-number 3 image))
    ;;          (i0 (match-end 0))
    ;;          (old-image (get-text-property (point-min) 'display)))
    ;;     (unless (= depth 255)
    ;;       (error "Cannot handle depth %d" depth))
    ;;     (cl-flet ((invert (i imax)
    ;;                       (while (< i imax)
    ;;                         ;; Invert bits
    ;;                         (aset image i (- 255 (aref image i)))
    ;;                         (setq i (1+ i)))))
    ;;       (if (not line)
    ;;           (while (< y1 y2)
    ;;             ;; i = i0 + 3 * (y * width + x)
    ;;             (let ((i (+ i0 (* 3 (+ x1 (* width y1))))))
    ;;               (invert i (+ i (* 3 (- x2 x1)))))
    ;;             (setq y1 (1+ y1)))
    ;;         (cond ((eq line 'horiz) (setq y2 y1))
    ;;               ((eq line 'vert)  (setq x2 x1)))
    ;;         (if (< (abs (- x2 x1)) (abs (- y2 y1)))
    ;;             (let ((dx (/ (- x2 x1) (float (- y2 y1))))
    ;;                   (y y1) (step (cl-signum (- y2 y1))))
    ;;               (while (/= y y2)
    ;;                 ;; x = (y - y1) * dx + x1
    ;;                 (let ((i (+ i0 (* 3 (+ (* y width) x1
    ;;                                        (round (* (- y y1) dx)))))))
    ;;                   (invert i (+ i 3)))
    ;;                 (setq y (+ y step))))
    ;;           (let ((dy (/ (- y2 y1) (float (- x2 x1))))
    ;;                 (x x1) (step (cl-signum (- x2 x1))))
    ;;             (while (/= x x2)
    ;;               ;; y = (x - x1) * dy + y1
    ;;               (let ((i (+ i0 (* 3 (+ x (* (+ y1 (round (* (- x x1) dy)))
    ;;                                           width))))))
    ;;                 (invert i (+ i 3)))
    ;;               (setq x (+ x step)))))))
    ;;     (with-silent-modifications
    ;;       (put-text-property
    ;;        (point-min) (point-max) 'display
    ;;        (create-image image 'pbm t)))
    ;;     (image-flush old-image))
    ;; ;; Restore unmodified image
    ;; (let ((old-image (get-text-property (point-min) 'display)))
    ;;   (with-silent-modifications
    ;;     (put-text-property (point-min) (point-max)
    ;;                        'display (nthcdr 2 (djvu-ref image))))
    ;;   (image-flush old-image))))


;;; Keyboard annotation functionality

(defun djvu-kb-annot-get-matches (pattern)
"Create list with position of matches.
The buffer is searched for PATTERN. The list contains starting
position for keyboard annotation regions."
  (if (search-forward pattern nil t)
      (cons (point) (djvu-kb-annot-get-matches pattern))))

(defun djvu-annot-get-ranges (patt1 patt2)
  "Create alist with starting en ending positions for annotations.
The buffer is searched for the strings PATT1 and PATT2 as
starting and ending positions"
        (let ((start-points (progn (goto-char (point-min))
                                   (djvu-kb-annot-get-matches patt1))))
          (mapcar (lambda (x)
                    (goto-char x)
                    (cons x (djvu-kb-annot-get-matches patt2))) start-points)))

(defun djvu-annot-get-words (beg end &optional last)
"Get words in subregion of annotation.
With LAST is nil/non-nil, the first/last (partial) word is
replaced with the complete word at point BEG/END."
  (let ((word-list (split-string (buffer-substring-no-properties beg end)))
        (full-word (save-excursion
                     (goto-char (if last
                                    end
                                  beg))
                     (thing-at-point 'word t))))
  (when last
    (setq word-list (nreverse word-list)))
  (setq word-list (cons full-word (cdr word-list)))
  (when last
    (setq word-list (nreverse word-list)))
  (string-join word-list " ")))


(defun ivy-annot-collection (ranges)
  "Print start and end strings of annotation.
This function is an adaptation of `djvu-rect-region'. The
function can be used to provide better info in completion frameworks."
    (mapcan (lambda (start-point-list)
              (mapcar (lambda (end)
                        (print end)
                        (let ((beg (1- (car start-point-list))))
                          (unless (get-text-property beg 'word)
                            (user-error "Start position `%s' not a word" beg))
                          (unless (get-text-property (1- end) 'word)
                            (user-error "End position `%s' not a word" end))
                          (let ((lines (djvu-region-count beg end 'line))
                                (paras (djvu-region-count beg end 'para))
                                (regions (djvu-region-count beg end 'region))
                                (columns (djvu-region-count beg end 'column))
                                areas)
                            (unless (and (>= 1 paras) (>= 1 regions) (>= 1 columns))
                              (user-error "Region spans multiple paragraphs"))

                            (if (eq 1 lines)
                                (let ((first-words (djvu-annot-get-words beg end)))
                                  (setq regions (list first-words
                                                      beg end)))

                              (if (eq 2 lines)
                                  (let* ((l1e (djvu-property-end (1+ beg) 'line))
                                         (l2b (djvu-property-beg (1- end) 'line))
                                         (c1 (djvu-scan-zone beg (djvu-property-end (1+ beg) 'line) 'word))
                                         (c2 (djvu-scan-zone (djvu-property-beg (1- end) 'line) end 'word)))
                                    ;; If BEG is beginning of first line, both lines share same left margin.
                                    (if (and (= beg (djvu-property-beg beg 'line))
                                             (djvu-areas-justify t c1 c2))
                                        (djvu-justify-areas 'min 0 c1 c2))
                                    ;; If END is end of second line, both lines share same right margin.
                                    (if (and (= end (djvu-property-end end 'line))
                                             (djvu-areas-justify nil c2 c1))
                                        (djvu-justify-areas 'max 2 c1 c2))
                                    (if (<= (aref c1 0) (aref c2 2))
                                        ;; Lower bound of upper box and upper bound of lower box coincide.
                                        (let ((tmp (/ (+ (aref c1 1) (aref c2 3)) 2)))
                                          (aset c1 1 tmp) (aset c2 3 tmp)))
                                    (let ((first-words (djvu-annot-get-words beg l1e))
                                          (last-words (djvu-annot-get-words l2b end t)))
                                      (list (concat first-words
                                                    " ... "
                                                    last-words)
                                            beg end)))
                                ;; 3 lines
                                (let* ((l1e (djvu-property-end (1+ beg) 'line))
                                       (l2b (djvu-property-beg (1- end) 'line))
                                       (c1  (djvu-scan-zone beg l1e 'word))
                                       (ci  (djvu-scan-zone (1+ l1e) (1- l2b) 'line))
                                       (c2  (djvu-scan-zone l2b end 'word)))
                                  ;; If BEG is beginning of first line, all lines share same left margin.
                                  (cond ((and (= beg (djvu-property-beg beg 'line))
                                              (djvu-areas-justify t c1 ci c2))
                                         (djvu-justify-areas 'min 0 c1 ci c2))
                                        ((djvu-areas-justify t ci c2)
                                         (djvu-justify-areas 'min 0 ci c2)))
                                  ;; If END is end of last line, all lines share same right margin.
                                  (cond ((and (= end (djvu-property-end end 'line))
                                              (djvu-areas-justify nil c2 ci c1))
                                         (djvu-justify-areas 'max 2 c1 ci c2))
                                        ((djvu-areas-justify nil c1 ci)
                                         (djvu-justify-areas 'max 2 c1 ci)))
                                  (let ((tmp1 (/ (+ (aref c1 1) (aref ci 3)) 2))
                                        (tmp2 (/ (+ (aref ci 1) (aref c2 3)) 2)))
                                    ;; Lower bound of upper boxes and upper bound of lower boxes coincide.
                                    (aset c1 1 tmp1) (aset ci 3 tmp1)
                                    (aset ci 1 tmp2) (aset c2 3 tmp2))
                                  (let ((first-words (djvu-annot-get-words beg l1e))
                                        (last-words (djvu-annot-get-words l2b end t)))
                                    (list (concat first-words
                                                  " ... "
                                                  last-words)
                                          beg end)
                                                        ;; (list c1 ci c2)))
                                  )))))))
                      (cdr start-point-list)))
            ranges)
  )

(defun djvu-keyboard-annot (patt1 patt2)
"Djvu keyboard annotation command.
Highlight a region starting with PATT1 and anding with PATT2 in
djvu buffer. If multiple regions get matched then select correct
one using completion framework."
  (interactive "sFrom (start pattern): \nsTo (end pattern is pattern in last word): ")
  (let* ((ranges (djvu-annot-get-ranges patt1 patt2))
         (collection (ivy-annot-collection ranges))
         (region (alist-get (ivy-read "Select region: " collection) collection nil nil 'string=)))
    (djvu-rect-region (car region) (cadr region) "nil" "nil" "yellow" "50"))
  (when djvu-image-mode
    (djvu-image djvu-image-size t)))


;;; Djvu 0ccur
(defun djvu-assert-djvu-buffer ()
  (unless (equal major-mode 'djvu-read-mode)
    (error "Buffer is not in DJView mode")))

(defun djvu-sexp-line-to-string (line-sexp)
  (mapconcat (lambda (x) (car (nthcdr 5 x))) (nthcdr 5 line-sexp) " "))

;; (defun djvu-occur-tablist ()
;;   (let ((pattern (read-string "List lines matching: "))
;;         tablist
;;         (file (djvu-ref file)))
;;     (dotimes (x (djvu-ref pagemax))
;;     ;; (dotimes (x 9)
;;       (let ((page (+ x 1)))
;;         (with-temp-buffer
;;           (insert (shell-command-to-string
;;                    (format "djvused %s -e 'select %s; print-txt'"
;;                            (shell-quote-argument file)
;;                            page
;;                            pattern)))
;;           (goto-char (point-min))
;;           (while (search-forward-regexp (format "  (word .*%s.*\")" pattern) nil t)
;;             (let ((word-sexp (read (match-string 0)))
;;                   (line-sexp (read (thing-at-point 'list))))
;;               (setq tablist (append
;;                              tablist
;;                              (list (list
;;                                     nil
;;                                     (vector
;;                                      (format "%s" page)
;;                                      (let* ((text (djvu-sexp-line-to-string line-sexp))
;;                                             (start (string-match pattern text))
;;                                             (end (match-end 0)))
;;                                        (add-face-text-property
;;                                         start
;;                                         end
;;                                         'match
;;                                         nil
;;                                         text)
;;                                        text)))))))))))
;;     tablist))

(defun djvu-occur-exhaust-words (pattern word-list parent-list)
  (let (results
        (word (car word-list)))
    (while word
      (if (string-match pattern (nth 5 word))
          (setq results (cons (list word parent-list) results)))
      (setq word-list (cdr word-list))
      (setq word (car word-list)))
    results))

(defun djvu-occur-exhaust-lines (pattern line-list)
  (let (results
        (line (car line-list)))
    (while line
      (if (stringp (nth 5 line))
          (when (string-match pattern (nth 5 line))
            (setq results (cons line results)))
        (setq results
              (append (djvu-occur-exhaust-words pattern (nthcdr 5 line) line)
                      results)))
      (setq line-list (cdr line-list))
      (setq line (car line-list)))
    results))

(defun djvu-occur-exhaust-paras (pattern para-list)
  (let (results
        (para (car para-list)))
    (while para
      (if (stringp (nth 5 para))
          (when (string-match pattern (nth 5 para))
            (setq results (cons para results)))
        (if (equal (car (nth 5 para)) 'line)
            (setq results
                  (append (djvu-occur-exhaust-lines pattern (nthcdr 5 para))
                          results))
          (setq results
                (append (djvu-occur-exhaust-words pattern (nthcdr 5 para) para)
                        results))))
      (setq para-list (cdr para-list))
      (setq para (car para-list)))
    results))

(defun djvu-occur-exhaust-columns (pattern column-list)
  (let (results
        (column (car column-list)))
    (while column
      (setq results (append (djvu-occur-exhaust-paras pattern (nthcdr 5 column))
                                 results))
      (setq column-list (cdr column-list))
      (setq column (car column-list)))
    results))

(defun djvu-set-text ()
  (read
   (concat "("
           (shell-command-to-string
            "djvutxt -detail=word 'The Art of Experimental Physics - Preston, Daryl W_.djvu'")
           ")")))

;; (setq djvu-text-pages
;;       (read
;;        (concat "("
;;                (shell-command-to-string
;;                 "djvutxt -detail=word 'The Art of Experimental Physics - Preston, Daryl W_.djvu'")
;;                ")")))

;; (defun djvu-occur-extract-pages-text ()
;;   (let ((i 0)
;;         text-pages
;;         (djvu-text-pages (djvu-set-text)))
;;     (dolist (column-list djvu-text-pages text-pages)
;;       (let* ((contents (nth 5 column-list))
;;              (results (cond ((stringp contents)
;;                                   contents)
;;                                  ((equal (car contents) 'line)
;;                                   (djvu-occur-exhaust-lines (nthcdr 5 column-list)))
;;                                  ((equal (car contents) 'column)
;;                                   (djvu-occur-exhaust-columns (nthcdr 5 column-list))))))
;;         (when results
;;           (setq text-pages (cons (cons i results) text-pages)))
;;         (setq i (1+ i))))))

(defun djvu-occur-tablist ()
  (interactive)
  (let ((pattern (read-string "List lines matching: "))
        tablist
        (i 0)
        text-pages
        (djvu-text-pages (read
                          (concat "("
                                  (shell-command-to-string
                                   (format 
                                    "djvutxt -detail=word '%s'" (djvu-ref file)))
                                  ")"))))
    (dolist (column-list djvu-text-pages text-pages)
      (let* ((contents (nth 5 column-list))
             (results (cond ((stringp contents)
                                  contents)
                                 ((equal (car contents) 'line)
                                  (djvu-occur-exhaust-lines pattern (nthcdr 5 column-list)))
                                 ((equal (car contents) 'column)
                                  (djvu-occur-exhaust-columns pattern (nthcdr 5 column-list))))))
        (when results
          (setq text-pages (append
                            (mapcar (lambda (x) (cons i x)) results)
                            text-pages)))
        (setq i (1+ i))))
              (dolist (x (nreverse text-pages))
                (let ((page (+ (car x) 1))
                      (word-sexp (nth 1 x))
                      (line-sexp (nth 2 x)))
                  (setq tablist (append
                                 tablist
                                 (list (list
                                        `(:page ,page :edges ,(butlast (cdr word-sexp)))
                                        (vector
                                         (format "%s" page)
                                         (let* ((text (djvu-sexp-line-to-string line-sexp))
                                                (start (string-match pattern text))
                                                (end (match-end 0)))
                                           (add-face-text-property
                                            start
                                            end
                                            'match
                                            nil
                                            text)
                                           text))))))))
    tablist))

(defvar djvu-occur-mode-map
  (let ((kmap (make-sparse-keymap)))
    (set-keymap-parent kmap tablist-mode-map)
    (define-key kmap (kbd "RET") 'tablist-find-entry)
    (define-key kmap (kbd "C-o") 'tablist-find-entry)
    ;;     (define-key kmap (kbd "SPC") 'djvu-occur-view-occurrence)
    ;;     (define-key kmap (kbd "C-c C-f") 'next-error-follow-minor-mode)
    ;;     (define-key kmap (kbd "g") 'djvu-occur-revert-buffer-with-args)
    ;;     (define-key kmap (kbd "K") 'djvu-occur-abort-search)
    ;;     (define-key kmap (kbd "D") 'djvu-occur-tablist-do-delete)
    ;;     (define-key kmap (kbd "x") 'djvu-occur-tablist-do-flagged-delete)
    ;;     (define-key kmap (kbd "A") 'djvu-occur-tablist-gather-documents)
    kmap)
  "The keymap used for `djvu-occur-buffer-mode'.")

(define-derived-mode djvu-occur-mode
  tablist-mode "DJVUOccur"
  "Major mode for browsing djvu search result"
  (setq-local tabulated-list-format [("page" 10 nil) ("text" 80 nil)])
  (setq-local tablist-operations-function
              (lambda (op &rest _)
                (cl-case op
                  (supported-operations '(find-entry))
                  (find-entry (let ((item (tabulated-list-get-id)))
                                (pop-to-buffer target-buffer)
                                (djvu-goto-page (plist-get item :page) nil (plist-get item :edges)))))))
  (tabulated-list-init-header))

(defun djvu-occur ()
  (interactive)
  (djvu-assert-djvu-buffer)
  (let ((djvu-tablist (djvu-occur-tablist))
        (doc-buffer (current-buffer)))
    (pop-to-buffer "djvu-occur")
    (djvu-occur-mode)
    (setq-local target-buffer doc-buffer)
    (setq-local tabulated-list-entries djvu-tablist)
    (tabulated-list-print)))


;;; Invert toggle from djvu2.el
(defun djvu-toggle-invert ()
  (interactive)
  (setq djvu-invert (if djvu-invert
                        nil
                      t)))


;;; Djvu imenu

(defun djvu-imenu-create-index ()
  (with-current-buffer (djvu-ref bookmarks-buf djvu-doc)
    (goto-char (point-max))
    (let (alist)
      (while (re-search-backward "\"#p*\\([0-9]+\\).*\"" nil t)
        (let ((pagenumber (string-to-number (match-string-no-properties 1))))
          (re-search-backward "(\"\\(.+\\)\"")
          (push (cons (match-string-no-properties 1) pagenumber) alist)))
      alist)))

(add-hook 'djvu-read-mode-hook (lambda () (setq imenu-create-index-function 'djvu-imenu-create-index)))


;;; djvu-restore

(defun djvu-restore ()
  (when (member major-mode '(djvu-read-mode djvu-script-mode djvu-outline-mode))
    (let ((page (djvu-restore-get-page)))
      (when page (djvu-goto-page page)))))

(defun djvu-restore-save ()
  (when (member major-mode '(djvu-read-mode djvu-script-mode djvu-outline-mode))
    (djvu-restore-set-page (djvu-ref page))))

(defun djvu-restore-get-alist ()
  (when (file-exists-p djvu-restore-filename)
    (with-temp-buffer
      (insert-file-contents-literally
       djvu-restore-filename)
      (read (buffer-string)))))

(defun djvu-restore-get-page ()
  "Return restore page."
  (let* ((alist (djvu-restore-get-alist)))
    (cdr (assoc (djvu-ref file) alist))))

(defun djvu-restore-set-page (page)
  "Save restore PAGE."
  (let ((alist (djvu-restore-get-alist)))
    (setf (alist-get (djvu-ref file) alist nil nil 'equal) page)
    (with-temp-file djvu-restore-filename
      (insert (let (print-length) (prin1-to-string alist))))))

(defun djvu-kill-doc-all ()
  "Kill all buffers visiting `djvu-doc' except for the current buffer.
This function is added to `kill-buffer-hook' of all buffers visiting `djvu-doc'
so that killing the current buffer kills all buffers visiting `djvu-doc'."
  (djvu-restore-save)
  (unless djvu-in-kill-doc
    (let ((djvu-in-kill-doc t)
          buffers)
      ;; Sometimes we choke on broken djvu files so that many things
      ;; do not work anymore the way they should.  At least, we want to
      ;; be able to kill the relevant buffers.  So do not bail out here.
      (condition-case nil
          (let ((doc djvu-doc))
            (setq buffers (djvu-buffers doc))
            (unless (memq nil (mapcar 'buffer-live-p buffers))
              (djvu-save doc t))
            (djvu-kill-view doc t))
        (error nil))
      ;; A function in `kill-buffer-hook' should not kill the buffer
      ;; for which we called this hook in the first place, so that
      ;; other functions in this hook can do their job, too.
      (mapc 'kill-buffer (delq (current-buffer) buffers)))))

(defun djvu-find-file (file &optional page view noselect noconfirm)
  "Read and edit Djvu FILE on PAGE.  Return Read buffer.
If VIEW is non-nil start external viewer.
If NOSELECT is non-nil visit FILE, but do not make it current.
If NOCONFIRM is non-nil don't ask for confirmation when reverting buffer
from file."
  (interactive (djvu-read-file-name))
  (unless page (setq page 1))
  (setq file (expand-file-name file))
  ;; Djvu mode needs a local file.  If FILE is located on a remote system,
  ;; you can use something like `file-local-copy' to edit FILE.
  (if (file-remote-p file)
    (user-error "Cannot handle remote Djvu file `%s'" file))
  (unless (and (file-regular-p file)
               (file-readable-p file))
    (user-error "Cannot open Djvu file `%s'" file))
  (let* ((inhibit-quit t)
         (buf-basename (file-name-nondirectory file))
         (file-truename (abbreviate-file-name (file-truename file)))
         (file-number (nthcdr 10 (file-attributes file)))
         (dir (file-name-directory file))
         (read-only (not (file-writable-p file)))
         (old-buf (if (equal buffer-file-truename file-truename)
                      (current-buffer)
                    (find-buffer-visiting file-truename)))
         (doc (and old-buf (buffer-local-value 'djvu-doc old-buf)))
         (old-bufs (and doc (mapcar 'buffer-live-p (djvu-buffers doc)))))
    ;; Sanity check.  We should never need this.
    (when (and old-bufs (memq nil old-bufs))
      (message "Killing dangling Djvu buffers...")
      (djvu-kill-doc doc)
      (setq doc nil old-bufs nil)
      (message "Killing dangling Djvu buffers...Done")
      (sit-for 2))
    ;; Do nothing if we are already visiting FILE such that all buffers
    ;; are properly defined and FILE's modtime matches what we expect.
    (unless (and old-bufs
                 (or (and (equal file-number
                                 (buffer-local-value 'buffer-file-number doc))
                          (verify-visited-file-modtime doc))
                     ;; If a file on disk and a Djvu session are out of sync,
                     ;; we can only continue in hairy, limited ways because
                     ;; Emacs does not copy the contents of FILE into a buffer.
                     ;; Instead, we entirely rely on djvused.
                     (not (or noconfirm
                              (yes-or-no-p
                               (format "Revert buffer from file %s? "
                                       (djvu-ref file doc)))))))
      (unless old-bufs
        (cl-flet ((fun (n)
                       ;; Instead of `generate-new-buffer', we take a detour
                       ;; via `create-file-buffer' so that uniquify can do
                       ;; its job, too.  It does not matter that the arg of
                       ;; `create-file-buffer' does not match `buffer-file-name'
                       ;; because `uniquify-buffer-file-name' only cares
                       ;; about DIR.
                       (create-file-buffer ; needed by uniquify
                        (expand-file-name
                         (concat buf-basename
                                 (nth n djvu-buffer-name-extensions))
                         dir))))
          (if old-buf
              ;; This applies if `find-file-noselect' created OLD-BUF
              ;; in order to visit FILE.  Hence recycle OLD-BUF as Read
              ;; buffer so that `find-file-noselect' can do its job.
              ;; FIXME: this ignores `djvu-buffer-name-extensions'
              ;; because renaming OLD-BUF would break `uniquify'.
              (with-current-buffer old-buf
                (let ((inhibit-read-only t)
                      (buffer-undo-list t))
                  (erase-buffer))
                (setq buffer-file-coding-system 'prefer-utf-8)
                (setq doc old-buf))
            (setq doc (fun 0)))
          (djvu-set read-buf doc doc)
          (djvu-set text-buf (fun 1) doc)
          (djvu-set annot-buf (fun 2) doc)
          (djvu-set shared-buf (fun 3) doc)
          (djvu-set bookmarks-buf (fun 4) doc)
          (djvu-set outline-buf (fun 5) doc)))
      ;; Of course, we have
      ;; `djvu-doc-read-buf' = `djvu-doc'
      ;; `djvu-doc-file' = `buffer-file-name'.  Bother?
      ;; It seems Emacs does not like aliases for buffer-local variables.
      (djvu-set file file doc)
      ;; We could set the resolve-url flag heuristically, if the Djvu file
      ;; happens to have bookmarks or internal urls on the current page.
      ;; (djvu-set resolve-url nil doc)

      ;; (Re-)Initialize all buffers.
      (with-current-buffer (djvu-ref read-buf doc)
        (djvu-read-mode))
      (with-current-buffer (djvu-ref outline-buf doc)
        (djvu-outline-mode))
      (with-current-buffer (djvu-ref text-buf doc)
        (djvu-script-mode)
        (setq djvu-buffer 'text))
      (with-current-buffer (djvu-ref annot-buf doc)
        (djvu-script-mode)
        (setq djvu-buffer 'annot
              header-line-format '(:eval (djvu-header-line "page annotations"))))
      (with-current-buffer (djvu-ref shared-buf doc)
        (djvu-script-mode)
        (setq djvu-buffer 'shared
              header-line-format '(:eval (djvu-header-line "shared annotations"))))
      (with-current-buffer (djvu-ref bookmarks-buf doc)
        (djvu-script-mode)
        (setq djvu-buffer 'bookmarks
              header-line-format '(:eval (djvu-header-line "bookmarks"))))
      (djvu-all-buffers doc
        (setq djvu-doc doc ; propagate DOC to all buffers
              buffer-file-name file
              ;; A non-nil value of `buffer-file-truename' enables file-locking,
              ;; see call of `lock_file' in `prepare_to_modify_buffer_1'
              buffer-file-truename file-truename
              buffer-file-number file-number
              buffer-file-read-only read-only
              ;; We assume that all buffers for a Djvu document have the same
              ;; read-only status.  Should we allow different values for the
              ;; buffers of one document?  Or do we need a `djvu-read-only-mode'?
              buffer-read-only read-only
              default-directory dir)
        (set-visited-file-modtime)
        (add-hook 'post-command-hook 'djvu-modified nil t)
        (add-hook 'kill-buffer-hook 'djvu-kill-doc-all nil t))

      (with-temp-buffer
        (djvu-djvused doc t "-e"
                      "create-shared-ant; print-ant; n; ls; print-outline;")
        (goto-char (point-min))

        ;; shared annotations
        (save-restriction
          (narrow-to-region
           (point)
           ;; There is no delimiter in between the output strings
           ;; of multiple djvused commands indicating something like
           ;; the last shared annotation.
           ;; So we simply rely on the fact that annotations have a
           ;; parsable lisp-like syntax surrounded by braces,
           ;; whereas the next djvused command is `n', the output
           ;; of which is a plain number.
           (save-excursion
             (while (progn (skip-chars-forward " \t\n")
                           (looking-at "("))
               (forward-sexp))
             (point)))
          (djvu-init-annot (djvu-ref shared-buf doc) doc t))

        ;; page max
        (djvu-set pagemax (read (current-buffer)) doc)

        ;; page id:
        ;; The output lines of djvused -e "ls;" consists of several parts
        (let ((regexp (concat "\\(?:\\([0-9]+\\)[ \t]+\\)?" ; page number
                              "\\([PIAT]\\)[ \t]+"          ; file identifier
                              "\\([0-9]+\\)[ \t]+"          ; file size
                              ;; We have a problem when parsing the
                              ;; component file name followed by the optional
                              ;; page title: there is no unambiguous separator
                              ;; in between the two.  Note that the component
                              ;; file name may contain whitespace characters
                              ;; and its file name extension is not unique
                              ;; (if present at all).
                              "\\([^=\n]+\\)"
                              "\\(?:[ \t]+T=[^\t\n]+\\)?"  ; title (optional)
                              "$")) ; match a single line
              page-id)
          (while (progn (skip-chars-forward " \t\n")
                        (looking-at regexp))
            (if (match-string 1)
                ;; page-id is an alist with elements (PAGE-NUM . FILE-ID).
                ;; The remainder of the code assumes that djvused sets up
                ;; this association list properly.
                (push (cons (djvu-match-number 1)
                            (match-string 4))
                      page-id))
            (goto-char (match-end 0)))
          (unless (eq (djvu-ref pagemax doc) (length page-id))
            (error "Page id list broken %s - %s"
                   (djvu-ref pagemax doc) (length page-id)))
          (djvu-set page-id (nreverse page-id) doc))

        ;; bookmarks
        (skip-chars-forward " \t\n")
        (when (looking-at "(bookmarks")
          (let ((object (read (current-buffer))))
            (with-current-buffer (djvu-ref bookmarks-buf doc)
              (let (buffer-read-only)
                (insert "(bookmarks")
                (djvu-insert-bookmarks (cdr object) " ")
                (insert ")\n")
                (goto-char (point-min))
                (set-buffer-modified-p nil)
                (setq buffer-undo-list nil)))
            (djvu-init-outline (cdr object) doc))))

      (djvu-init-page page doc))

    (if view (djvu-view doc))
    (unless noselect (switch-to-buffer (djvu-ref read-buf doc)))
    (djvu-ref read-buf doc)
    (djvu-restore)))

(defun djvu-init-page (&optional page doc match)
  "Initialize PAGE for Djvu DOC.
PAGE is re-initialized if we are already viewing it."
  (interactive (list (djvu-read-page)))
  (unless doc (setq doc djvu-doc))
  ;; No need to save if only the bookmarks buffer
  ;; or shared annotations buffer got modified.
  (if (or (buffer-modified-p (djvu-ref text-buf doc))
          (buffer-modified-p (djvu-ref annot-buf doc)))
      (djvu-save doc t))
  ;; We process PAGE unconditionally, even if it equals the page
  ;; currently displayed.  Most often, PAGE equals the current page
  ;; if we want to redisplay PAGE.
  (unless (integerp page)
    (setq page (or (djvu-ref page doc) 1)))
  (if (or (< page 1)
          (< (djvu-ref pagemax doc) page))
      (user-error "Page `%s' out of range" page))

  (let ((inhibit-quit t))
    (if (and (djvu-ref page doc)
             (not (equal page (djvu-ref page doc))))
        (djvu-set history-backward (cons (djvu-ref page doc)
                                         (djvu-ref history-backward doc))
                  doc))
    (djvu-set history-forward nil doc)
    (djvu-set page page doc)
    ;; Fix me: Restore buffer positions if we revisit the same page.
    (djvu-set read-pos nil doc)
    (with-temp-buffer
      (djvu-djvused doc t "-e"
                    (format "select %d; size; print-txt; print-ant;"
                            (djvu-ref page doc)))
      (goto-char (point-min))

      ;; page size
      (skip-chars-forward " \t\n")
      (if (looking-at "width=\\([[:digit:]]+\\)[ \t]+height=\\([[:digit:]]+\\)\\(?:[ \t]+rotation=\\([[:digit:]]+\\)\\)?$")
          (djvu-set pagesize (cons (djvu-match-number 1)
                                   (djvu-match-number 2))
                    doc)
        ;; This may fail if the file list we read previously contained
        ;; thumbnails.  We should really ignore these thumbnails.
        (error "No page size"))

      ;; Raw text:
      ;; This is exactly one object that we can swallow in one bite.
      ;; Hence we do this before we swallow the unknown number of annotations.
      (goto-char (match-end 0))
      (skip-chars-forward " \t\n")
      (let ((object (if (looking-at "(\\(page\\|column\\|region\\|para\\|line\\|word\\|char\\)")
                        (read (current-buffer)))))
        ;; Set up annotations buffer.
        ;; This also initializes `djvu-doc-rect-list' that we need
        ;; for propertizing the read buffer.
        (save-restriction
          (narrow-to-region (point) (point-max))
          (djvu-init-annot (djvu-ref annot-buf doc) doc))

        ;; Set up text buffer
        (djvu-init-text object doc t)

        ;; Set up read buffer
        (djvu-init-read object doc t match)))))

(defun djvu-init-read (object &optional doc reset match)
  (with-current-buffer (djvu-ref read-buf doc)
    (let ((djvu-rect-list (djvu-ref rect-list doc))
          (dpos (unless reset (djvu-read-dpos nil doc)))
          buffer-read-only djvu-last-rect)
      (erase-buffer)
      (djvu-insert-read object)
      (djvu-insert-read-prop)
      (if reset
          (goto-char (point-min))
        (djvu-goto-read dpos)))
    (set-buffer-modified-p nil)
    (setq buffer-read-only t)
    (djvu-image nil t match)))

(defun djvu-image-invert (im)
  "For PPM image specified via EVENT mark rectangle by inverting bits."
  ;; FIXME: Can the following be implemented more efficiently in the
  ;; image display code?  Could this be useful for other packages, too?
  (let* (
         ;; (image (copy-sequence (nth 6 (djvu-ref image))))
         (image (image-property im :data))
         (_ (unless (string-match "\\`P6\n\\([0-9]+\\) +\\([0-9]+\\)\n\\([0-9]+\\)\n" image)
              (error "Not a PPM image")))
         (width (djvu-match-number 1 image))
         (height (djvu-match-number 2 image))
         (x1 0)
         (y1 0)
         (x2 width)
         (y2 height)
         (depth (djvu-match-number 3 image))
         (i0 (match-end 0))
         (old-image (get-text-property (point-min) 'display)))
    (unless (= depth 255)
      (error "Cannot handle depth %d" depth))
    (cl-flet ((invert (i imax)
                      (while (< i imax)
                        ;; Invert bits
                        (aset image i (- 255 (aref image i)))
                        (setq i (1+ i)))))
      (while (< y1 y2)
        ;; i = i0 + 3 * (y * width + x)
        (let ((i (+ i0 (* 3 (+ x1 (* width y1))))))
          (invert i (+ i (* 3 (- x2 x1)))))
        (setq y1 (1+ y1)))
      (if (< (abs (- x2 x1)) (abs (- y2 y1)))
          (let ((dx (/ (- x2 x1) (float (- y2 y1))))
                (y y1) (step (cl-signum (- y2 y1))))
            (while (/= y y2)
              ;; x = (y - y1) * dx + x1
              (let ((i (+ i0 (* 3 (+ (* y width) x1
                                     (round (* (- y y1) dx)))))))
                (invert i (+ i 3)))
              (setq y (+ y step))))
        (let ((dy (/ (- y2 y1) (float (- x2 x1))))
              (x x1) (step (cl-signum (- x2 x1))))
          (while (/= x x2)
            ;; y = (x - x1) * dy + y1
            (let ((i (+ i0 (* 3 (+ x (* (+ y1 (round (* (- x x1) dy)))
                                        width))))))
              (invert i (+ i 3)))
            (setq x (+ x step)))))
      image)))
  ;; (with-silent-modifications
  ;;   (put-text-property
  ;;    (point-min) (point-max) 'display
  ;;    (create-image image 'pbm t)))
  ;; (image-flush old-image)
  ;; ;; Restore unmodified image
  ;; (let ((old-image (get-text-property (point-min) 'display)))
  ;;   (with-silent-modifications
  ;;     (put-text-property (point-min) (point-max)
  ;;                        'display (nthcdr 2 (djvu-ref image))))
  ;;   (image-flush old-image)))

(provide 'djvu3)
