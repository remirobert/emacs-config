;;Indenter toute la page
(defun indent-whole ()
  (interactive)
  (mark-whole-buffer)
  (indent-region (region-beginning) (region-end))
  )

;;Remplacement