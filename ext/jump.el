;;create marks in buffer so you can jump between them using these functions
(defvar na-cm-ring nil
  "List of markers that points to buffer-positions.")
(defun na-cm-same-pos ()
  (and na-cm-ring
       (equal (point) (marker-position (car na-cm-ring)))
       (equal (current-buffer) (marker-buffer (car na-cm-ring)))))

(defun na-cm-save-point (arg)
  (interactive "P")
  (if (or (and arg (< (prefix-numeric-value arg) 0))
          (na-cm-same-pos))
      (progn
        (setq na-cm-ring (cdr na-cm-ring))
        (message "Point deleted from stack (%d left)" (length na-cm-ring)))
    (setq na-cm-ring (cons (point-marker) na-cm-ring))
    (message "Point saved (%d saved)" (length na-cm-ring))))

(defun na-cm-rotate (num)
  "If point differ from first position in ring then goto that.
Otherwise rotate the ring of points and go to the now newest point in the ring"
  (interactive "P")
  (if (not na-cm-ring)
      (error "No points saved!"))
  (setq num
        (if (null num) (if (na-cm-same-pos) 1 0)
          (prefix-numeric-value num)))
  (setq num (mod num (length na-cm-ring)))
  (let ((top nil))
    (while (> num 0)
      (setq top (cons (car na-cm-ring) top))
      (setq na-cm-ring (cdr na-cm-ring))
      (setq num (1- num)))
    (setq na-cm-ring (append na-cm-ring (nreverse top)))
    (if (marker-position (car na-cm-ring))
        (progn
          (switch-to-buffer (marker-buffer (car na-cm-ring)))
          (goto-char (car na-cm-ring)))
      (setq na-cm-ring (cdr na-cm-ring))
      (na-cm-rotate 1))))
