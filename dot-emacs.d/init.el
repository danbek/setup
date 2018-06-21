;; Better to put custom settings in their own file
(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
(load custom-file 'noerror)

;; Setup package.el
(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
(add-to-list 'package-archives '("org" . "https://orgmode.org/elpa/") t)
(package-initialize)
;;(setq package-enable-at-startup nil)

;; I had hoped that this would install the latest version of org, but
;; (1) it seems flakey - doesn't always work (2) when it does work, I
;; can't get code execution in an org file working. So just use
;; default org version for now.
;; 
;; If old older version of org is installed, force an upgrade. The
;; version check is a hack, sorry.
;(if (string< (substring (org-version) 0 1) "9")
;    (progn
;      (package-refresh-contents)
;      (package-install 'org))
;  )

;; Setup use-package (makes installing other packages much easier
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))
(eval-when-compile
  (require 'use-package))

;;
;; Now various packages
;;

;; "diminish" minor modes by not dislaying them in the mode-line
(use-package diminish
  :ensure t
  :config
  ;; More configuration goes here
  )

(use-package undo-tree
  :ensure t
  :config
  ;; More configuration goes here
  )

(use-package magit
  :ensure t
  :config
  ;; More configuration goes here
  )


(defun dtb-switch-to-other-buffer ()
  "Switch to 'other' buffer.
Repeated invocations toggle between the two most recently open buffers."
  (interactive)
  (switch-to-buffer (other-buffer)))

(use-package evil
  :ensure t
  :config

  ; apparently evil-lead-mode should be enabled before enabling evil-mode
  (use-package evil-leader
    :ensure t
    :config
    (global-evil-leader-mode)
    (evil-leader/set-leader "<SPC>")
    (evil-leader/set-key
      ;; files
     "f e" (lambda () (interactive) (find-file user-init-file))
     "f f" 'counsel-find-file
     "f o" (lambda () (interactive) (find-file "~/notes/organizer.org"))
     "f r" 'counsel-recentf
     "f v" 'find-alternate-file

     ;; org-mode
     "o l" 'org-store-link
     "o a" 'org-agenda
     "o c" 'org-capture
     "o b" 'org-switchb

     ;; other
     "b" 'counsel-ibuffer
     "g" 'magit-status
     "TAB" 'dtb-switch-to-other-buffer
     "r" 'counsel-rg
     )
    )

  (evil-mode 1)
  
  (use-package evil-surround
    :ensure t
    :config
    (global-evil-surround-mode))

  (use-package evil-indent-textobject
    :ensure t)

  (evil-add-hjkl-bindings occur-mode-map 'emacs
    (kbd "/")       'evil-search-forward
    (kbd "n")       'evil-search-next
    (kbd "N")       'evil-search-previous
    (kbd "C-d")     'evil-scroll-down
    (kbd "C-u")     'evil-scroll-up
    (kbd "C-w C-w") 'other-window)

  (use-package evil-magit
    :ensure t
    :config
    (setq evil-magit-state 'motion)
    )
  
  ;; I like to use arrow keys for command line history, at least in
  ;; insert mode
  (evil-define-key 'insert shell-mode-map
    (kbd "<up>")   'comint-previous-input
    (kbd "<down>") 'comint-next-input)
  (evil-define-key 'insert inferior-python-mode-map
    (kbd "<up>")   'comint-previous-input
    (kbd "<down>") 'comint-next-input)
  )

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
	python-shell-interpreter-args "-i --simple-prompt --matplotlib")

  )

;;
;; Themes
;;
(use-package color-theme-sanityinc-tomorrow
  :ensure t
  :config
  )

(use-package solarized-theme
  :ensure t
  :config
  (setq solarized-high-contrast-mode-line nil)
  (setq x-underline-at-descent-line t)
  (load-theme 'solarized-light)
  )

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

;; Stuff specific to particular computers
(global-font-lock-mode t)
(cond ((string-match "817thzdev" system-name)
       (set-default-font "Inconsolata-12"))
      ((string-match "twiggy" system-name)
       (set-default-font "Inconsolata-9"))
      ((string-match "686db1-linux" system-name)
       (set-default-font "DejaVu Sans Mono-10"))
      ((string-match "686DB1" system-name)
       (set-default-font "Consolas-11"))
      ((string-match "dan-homePC" system-name)
       (set-default-font "Consolas-10"))
      ((string-match "harold-xubuntu-" system-name)
       (set-default-font "Inconsolata-10"))
      ((string-match "687db2vm1" system-name)
       (when window-system
	 (set-frame-size (selected-frame) 130 67)
	 (set-frame-position (selected-frame) 0 0))
       (set-default-font "DejaVu Sans Mono-10"))
      ((string-match "ovid-xubuntu5" system-name)
       (set-default-font "DejaVu Sans Mono-10")
       (when window-system
	 (set-frame-size (selected-frame) 94 44)
	 (set-frame-position (selected-frame) 0 0)))
       ;(set-default-font "Source Code Pro-9"))
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
(server-start)
