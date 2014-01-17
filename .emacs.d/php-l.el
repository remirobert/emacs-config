;;
;; php-l.el for php-l.el in /u/all/le-gue_t/cu/public/childporn
;;
;; Made by le-gue_t
;; Login   <le-gue_t@epitech.net>
;;
;; Started on  Thu Feb 18 16:22:30 2010 le-gue_t
;; Last update Fri Mar 12 19:37:47 2010 marandon
;;

(defun php-l(&optional b e)
  (interactive)
  (save-excursion
    (goto-char 1)
    (push-mark)
    (mark-whole-buffer)
    (shell-command-on-region b e "php -l")))

(add-hook 'php-mode-hook
          '(lambda ()
             (global-set-key (kbd "C-x C-l") 'php-l)))
