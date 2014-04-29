
(setq scroll-step            1
      scroll-conservatively  10000)

(require 'package)
(add-to-list 'package-archives
	     '("melpa" . "http://melpa.milkbox.net/packages/") t)
(package-initialize)


(require 'tool-bar)
(tool-bar-mode nil)
; don't show the scroll bar
(scroll-bar-mode nil)

(require 'fringe)
(fringe-mode 10)

(global-linum-mode 1)
(setq linum-format "%3d ")
; show the current line and column numbers in the stats bar as well
(line-number-mode t)
(column-number-mode t)


(show-paren-mode 1)

(menu-bar-mode -1)
(scroll-bar-mode -1)


;====================================
(require 'color-theme)
(color-theme-initialize)
(setq color-theme-is-global t)
(color-theme-subtle-hacker)

(custom-set-faces
 '(default ((t (:overline nil :inherit nil :stipple nil :background "gray2"
                :foreground "#FFF991" :inverse-video nil :box nil
                :strike-through nil :underline nil
                :slant normal :weight normal :height 83 :width normal
                :foundry "unknown" :family "DejaVu Sans Mono"))))
 '(border ((t nil)))
 '(cursor ((t (:background "firebrick1" :foreground "black"))))
 '(font-lock-comment-delimiter-face
    ((default (:inherit font-lock-comment-face :weight ultra-bold))
    (((class color) (min-colors 16)) nil)))
 '(font-lock-comment-face ((t (:foreground "lime green"))))
 '(font-lock-doc-face ((t (:foreground "tomato" :slant italic))))
 '(font-lock-function-name-face
    ((t (:foreground "deep sky blue" :underline t :weight bold))))
 '(font-lock-keyword-face ((t (:foreground "gold" :weight bold))))
 '(font-lock-string-face ((t (:foreground "tomato" :slant italic))))
 '(fringe ((nil (:background "black"))))
 '(highlight ((t (:background "khaki1" :foreground "black"
                  :box (:line-width -1 :color "firebrick1")))))
 '(highlight-current-line-face ((t (:inherit highlight))))
 '(lazy-highlight ((t (:background "paleturquoise" :foreground "black"))))
 '(link ((t (:foreground "DodgerBlue3" :underline t))))
 '(menu ((t (:background "gray2" :foreground "#FFF991"))))
 '(minibuffer-prompt ((t (:foreground "royal blue"))))
 '(mode-line ((t (:background "dark olive green"
                  :foreground "dark blue"
                  :box (:line-width -1 :color "gray75")
                  :weight bold))))
 '(mode-line-buffer-id ((t (:background "dark olive green" :foreground "beige"))))
 '(mode-line-highlight ((((class color) (min-colors 88)) nil)))
 '(mode-line-inactive ((t (:background "dark olive green"
                           :foreground "dark khaki" :weight light))))
 '(mouse ((t (:background "Grey" :foreground "black"))))
 '(trailing-whitespace ((((class color) (background dark))
                          (:background "firebrick1")))))

; make sure the frames have the dark background mode by default
(setq default-frame-alist (quote (
  (frame-background-mode . dark)
)))

;====================================

; lines which are exactly as wide as the window (not counting the
; final newline character) are not continued. Instead, when point is
; at the end of the line, the cursor appears in the right fringe.
(setq overflow-newline-into-fringe t)


(require 'package)
(setq package-archives '(("gnu" . "http://elpa.gnu.org/packages/")
			 ("marmalade" . "http://marmalade-repo.org/packages/")
			 ("melpa" . "http://melpa.milkbox.net/packages/")))

(package-initialize)

(add-to-list 'load-path "~/.emacs.d/el-get/el-get")

(unless (require 'el-get nil 'noerror)
  (with-current-buffer
      (url-retrieve-synchronously
       "https://raw.github.com/dimitri/el-get/master/el-get-install.el")
    (goto-char (point-max))
    (eval-print-last-sexp)))

(add-to-list 'el-get-recipe-path "~/.emacs.d/el-get-user/recipes")
(el-get 'sync)

;;(add-hook 'python-mode-hook 'jedi:setup)
;;(setq jedi:setup-keys t)                      ; optional
;;(setq jedi:complete-on-dot t)                 ; optional

(add-to-list 'load-path "~/.emacs.d")    ; This may not be appeared if you have already added.
(require 'auto-complete-config)
(add-to-list 'ac-dictionary-directories "~/.emacs.d/ac-dict")
(ac-config-default)

(add-hook 'after-init-hook 'global-company-mode)
(add-to-list 'custom-theme-load-path "~/.emacs.d/themes/")

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes (quote ("0603fb5696ab4af05e7c8bb11498bd189bdb7930c7c88dd6ac1e5ec2fc3efb2b" default))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

(add-to-list 'load-path "~/.emacs.d/fill-column-indicator-1.83")
(require 'fill-column-indicator)
(setq-default fci-rule-column 80)
(define-globalized-minor-mode
 global-fci-mode fci-mode (lambda () (fci-mode 1)))
(setq fci-rule-color "darkblue")
(global-fci-mode t)


(setq-default show-trailing-whitespace t) ; affiche les espaces en fin de ligne
(setq-default delete-trailing-whitespace t) ; affiche les espaces en fin de ligne

(add-hook 'c++-mode-hook '(lambda ()
  (add-hook 'write-contents-hooks 'delete-trailing-whitespace nil t)))
(add-hook 'c-mode-hook '(lambda ()
  (add-hook 'write-contents-hooks 'delete-trailing-whitespace nil t)))


(add-to-list 'load-path
"~/.emacs.d/plugins/yasnippet")
(require 'yasnippet)
(yas-global-mode 1)

(global-set-key [M-left] 'windmove-left) ; move to left windnow
(global-set-key [M-right] 'windmove-right) ; move to right window
(global-set-key [M-up] 'windmove-up) ; move to upper window
(global-set-key [M-down] 'windmove-down) ; move to downer windows

;; Lancer le man avec F3
(defun vectra-man-on-word ()
  "Appelle le man sur le mot pointe par le curseur"
  (interactive)
  (manual-entry (current-word)))
(global-set-key [f3] 'vectra-man-on-word)

(fset 'yes-or-no-p 'y-or-n-p)

(electric-pair-mode 1)
(setq electric-pair-pairs '(
                            (?\" . ?\")
                            (?\{ . ?\})
                            ) )


(global-hl-line-mode 1)
(set-face-background 'hl-line "#3e444")
;;(set-face-background 'hl-line "blue")
(set-face-foreground 'highlight nil)
(set-face-foreground 'hl-line nil)
