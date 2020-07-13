;; Better to put custom settings in their own file
(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
(load custom-file 'noerror)

;; Setup package.el
(require 'package)
(setq package-check-signature nil)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
(package-initialize)
;;(setq package-enable-at-startup nil)

;; Setup use-package (makes installing other packages much easier)
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))
(eval-when-compile
  (require 'use-package))

(add-to-list 'load-path "~/.emacs.d/site-lisp")

;;
;; Now various packages
;;
;; (use-package evil
;;   :ensure t
;;   :config

;;   ; apparently evil-leader-mode should be enabled before enabling evil-mode
;;   (use-package evil-leader
;;     :ensure t
;;     :config
;;     (global-evil-leader-mode)
;;     (evil-leader/set-leader "<SPC>")
;;     (evil-leader/set-key
;;       ;; files
;;      "f e" (lambda () (interactive) (find-file user-init-file))
;;      "f f" 'counsel-find-file
;;      "f o" (lambda () (interactive) (find-file "~/notes/organizer.org"))
;;      "f r" 'counsel-recentf
;;      "f v" 'find-alternate-file

;;      ;; org-mode
;;      "o l" 'org-store-link
;;      "o a" 'org-agenda
;;      "o c" 'org-capture
;;      "o b" 'org-switchb

;;      ;; buffers
;;      "b b" 'counsel-ibuffer
;;      "b k" (lambda () (interactive) (kill-buffer (current-buffer)))

;;      ;; other
;;      "d" 'deft
;;      "g" 'magit-status
;;      "TAB" 'dtb-switch-to-other-buffer
;;      "r" 'counsel-rg
;;      )
;;     )

;;   (evil-mode 1)
  
;;   (use-package evil-surround
;;     :ensure t
;;     :config
;;     (global-evil-surround-mode))

;;   ;(use-package evil-indent-plus
;;   ;  :ensure t)

;;   (evil-add-hjkl-bindings occur-mode-map 'emacs
;;     (kbd "/")       'evil-search-forward
;;     (kbd "n")       'evil-search-next
;;     (kbd "N")       'evil-search-previous
;;     (kbd "C-d")     'evil-scroll-down
;;     (kbd "C-u")     'evil-scroll-up
;;     (kbd "C-w C-w") 'other-window)

;;   (use-package evil-magit
;;     :ensure t
;;     :config
;;     (setq evil-magit-state 'motion)
;;     )
  
;;   ;; I like to use arrow keys for command line history, at least in
;;   ;; insert mode
;;   (evil-define-key 'insert shell-mode-map
;;     (kbd "<up>")   'comint-previous-input
;;     (kbd "<down>") 'comint-next-input)
;;   (evil-define-key 'insert inferior-python-mode-map
;;     (kbd "<up>")   'comint-previous-input
;;     (kbd "<down>") 'comint-next-input)

;;   ;; I prefer M-. to run xref-find-defintions in normal mode
;;   (define-key evil-normal-state-map (kbd "M-.") 'xref-find-definitions)

;;   )

;; "diminish" minor modes by not dislaying them in the mode-line
(use-package diminish
  :ensure t
  :config
  ;; More configuration goes here
  )

;; (use-package undo-tree
;;   :ensure t
;;   :config
;;   ;; More configuration goes here
;;   )

(use-package magit
  :ensure t
  :config
  ;; More configuration goes here
  )

(use-package deft
  :ensure t
  :config
  ;; More configuration goes here
  (setq deft-default-extension "org")
  (setq deft-directory "~/notes/notes")
  (setq deft-org-mode-title-prefix t)
  (setq deft-use-filter-string-for-filename t)
  (setq deft-file-naming-rules
	'((noslash . "-")
	  (nospace . "-")
	  (case-fn . downcase)))

  )

(use-package org
  :ensure t
  :config
  (setq org-hide-leading-starts t)
  
  (global-set-key (kbd "C-c a") 'org-agenda)
  
  ;; could not get this to work using use-package, so do it this way
;  (add-to-list 'load-path "~/.emacs.d/site-lisp/evil-org-mode")
;  (require 'evil-org)
;  (add-hook 'org-mode-hook 'evil-org-mode)
;  (evil-org-set-key-theme '(navigation insert textobjects additional calendar))
;  (require 'evil-org-agenda)
;  (evil-org-agenda-set-keys)

  ;(use-package org-bullets
  ;  :ensure t
  ;  :config
  ;  (add-hook 'org-mode-hook (lambda () (org-bullets-mode t))))

  )


(defun dtb-switch-to-other-buffer ()
  "Switch to 'other' buffer.
Repeated invocations toggle between the two most recently open buffers."
  (interactive)
  (switch-to-buffer (other-buffer)))

; loading this allows counsel-M-x to show most recent commands first
(use-package smex
  :ensure t
  :config
  ;; More configuration goes here
  )

; loads ivy and swiper too
(use-package counsel
  :ensure t
  :diminish (ivy-mode . "")
  :config
  (ivy-mode 1)
  (setq ivy-use-virtual-buffers t)
  (setq ivy-count-format "(%d/%d) ")
  (setq ivy-initial-inputs-alist nil)
  (global-set-key (kbd "M-x") 'counsel-M-x)
  )

;;
;; Python setup. Let's try elpy
;;
;; Apparently the only way to turn off highlight-indentation-mode is
;; to remove it from the list of elpy modules, which can be done
;; through M-x customize-variable RET elpy-modules
;;
(use-package elpy
  :ensure t
  :config
  (elpy-enable)
  (add-hook 'python-mode-hook
	    (lambda () (auto-fill-mode t)))
  (add-hook 'python-mode-hook
	    (lambda () (linum-mode t)))
  (setq python-shell-interpreter "ipython"
	python-shell-interpreter-args "-i --simple-prompt --matplotlib"
	elpy-rpc-virtualenv-path 'current)
  )

;;
;; Themes
;;

;; Trying this from Prot Stavrous. There are many options to explore
;; https://gitlab.com/protesilaos/modus-themes 
(use-package modus-operandi-theme
  :ensure t)

;;
;; Julia
;;

(require 'julia-mode)
(require 'julia-repl)
(add-hook 'julia-mode-hook 'julia-repl-mode)
(setq julia-repl-executable-records
      '((julia-1.1.0 "/home/dan/installs/julia-1.1.0/bin/julia")))
;(setq julia-repl-executable-records
;      '((default "julia")                  ; in the executable path
;        (master "~/src/julia-git/julia"))) ; compiled from the repository

;;
;; C/C++
;;
(setq-default c-default-style "linux"
	      c-basic-offset 4)

;;
;; Other stuff
;;

;; don't need toolbar
(if (fboundp 'tool-bar-mode) (tool-bar-mode -1))

;; better scrolling http://etherplex.org/static/emacs.html
(setq scroll-conservatively 10)
(setq scroll-margin 7)
(setq mouse-wheel-scroll-amount '(1 ((shift) . 1))) ;; one line at a time
(setq mouse-wheel-progressive-speed nil) ;; don't accelerate scrolling
(setq mouse-wheel-follow-mouse 't) ;; scroll window under mouse
(setq scroll-step 1) ;; keyboard scroll one line at a time

;; from http://xenon.stanford.edu/~manku/dotemacs.html
(setq inhibit-startup-screen t)
;(setq inhibit-startup-echo-area-message t)
(setq require-final-newline t)
(setq display-time-day-and-date t) (display-time)

(blink-cursor-mode 0)

;;
;; Stuff specific to particular computers
;;

(global-font-lock-mode t)

(defun dtb-set-default-font (font-name)
  (set-face-attribute 'fixed-pitch nil :font font-name)
  (set-face-attribute 'default     nil :font font-name)
  )

(cond ((string-match "817thzdev" system-name)
       (dtb-set-default-font "Inconsolata-12"))
      ((string-match "twiggy" system-name)
       (dtb-set-default-font "Inconsolata-9"))
      ((string-match "686db1-linux" system-name)
       (dtb-set-default-font "DejaVu Sans Mono-10"))
      ((string-match "686DB1" system-name)
       (dtb-set-default-font "Consolas-11"))
      ((string-match "dan-homePC" system-name)
       (dtb-set-default-font "Consolas-10"))
      ((string-match "harold-xubuntu-" system-name)
       (dtb-set-default-font "Inconsolata-10"))
      ((string-match "686db2" system-name)
       (dtb-set-default-font "DejaVu Sans Mono-10")
       (dtb-set-default-font "Hack-9")
       (add-to-list 'default-frame-alist '(left . 0))
       (add-to-list 'default-frame-alist '(top . 0))
       (add-to-list 'default-frame-alist '(height . 58))
       (add-to-list 'default-frame-alist '(width . 128)))
      ((string-match "687db2-vm4" system-name)
       ;; (dtb-set-default-font "Hack-9")
       (dtb-set-default-font "Hack-9:autohint=true:hintstyle=hintfull:embeddedbitmap=false")
       (add-to-list 'default-frame-alist '(left . 0))
       (add-to-list 'default-frame-alist '(top . 0))
       (add-to-list 'default-frame-alist '(height . 65))
       (add-to-list 'default-frame-alist '(width . 258)))
      ((string-match "xubuntu-work-2" system-name)
       (dtb-set-default-font "DejaVu Sans Mono-10"))
      ((string-match "xubuntu-1" system-name)
       (dtb-set-default-font "DejaVu Sans Mono-10")
       (add-to-list 'default-frame-alist '(left . 0))
       (add-to-list 'default-frame-alist '(top . 0))
       (add-to-list 'default-frame-alist '(height . 54))
       (add-to-list 'default-frame-alist '(width . 84)))
      )

;; I perfer to not scatter these files all over the place
(make-directory "~/.emacs.d/autosaves/" t)
(make-directory "~/.emacs.d/backups/" t)
(setq backup-directory-alist `(("." . "~/.emacs.d/backups")))
(setq auto-save-file-name-transforms '((".*" "~/.emacs.d/autosaves/\\1" t)))
(setq delete-old-versions t
  kept-new-versions 6
  kept-old-versions 2
  version-control t)

;; TRAMP defaults
(setq tramp-default-method "ssh")

;; start server for use of emacs from command line
;; (server-start)

(require 'guru-mode)
(guru-global-mode)

(require 'jump-char)
(global-set-key (kbd "M-m") 'jump-char-forward)
(global-set-key (kbd "M-M") 'jump-char-backward)

(global-set-key (kbd "C-x C-f") 'counsel-find-file)
(global-set-key (kbd "C-x f") 'counsel-recentf)

(defun dtb/ctrl-a ()
  "Move point to indentation or beginning of line, toggling with repeated calls"
  (interactive)
  (let ((indent (save-excursion
		  (back-to-indentation)
		  (point)))
	(beg (save-excursion
	       (move-beginning-of-line nil)
	       (point)))
	(pt (point)))
    (cond
     ((eq pt indent) (move-beginning-of-line nil))
     ((eq pt beg) (back-to-indentation))
     ((> pt indent) (back-to-indentation))
     (t (move-beginning-of-line nil)))))

(global-set-key (kbd "C-a") 'dtb/ctrl-a)

(defun dtb/pull-up-line ()
  "Join the following line onto the current one aa in `C-u M-^' or `C-u M-x join-line'."
  (interactive)
  (join-line -1))

(global-set-key (kbd "C-j") 'dtb/pull-up-line)
  
(defun dtb/copy-to-end-of-buffer ()
 (interactive)
 (copy-region-as-kill (point) (point-max))
 )

(global-set-key (kbd "C-c c b") 'dtb/copy-to-end-of-buffer)
