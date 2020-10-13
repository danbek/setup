;; Better to put custom settings in their own file
(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
(load custom-file 'noerror)

;; Setup package.el
(require 'package)
(setq package-check-signature nil)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
(package-initialize)
;;(setq package-enable-at-startup nil)

;;
;; Setup use-package (makes installing other packages much easier)
;;
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(eval-and-compile
  ;; Write hooks using their real name instead of a shorter version:
  ;;    after-init ==> `after-init-hook'.
  ;;
  ;; This is to empower help commands with their contextual awareness,
  ;; such as `describe-symbol'.
  (setq use-package-hook-name-suffix nil))

(eval-when-compile
  (require 'use-package))

;;
;; For local lisp
;;
(add-to-list 'load-path "~/.emacs.d/site-lisp")

;;
;; Now various packages
;;
(use-package undo-tree
  :ensure t
  :config
  (global-undo-tree-mode 1)
  )

(use-package evil
  :ensure t
  :after undo-tree
  :init
  ;; Must do these before loading evil to get evil-collection working
  (setq evil-want-keybinding nil)
  (setq evil-want-integration t)
  
  :config
  ;; I prefer emacs binds for terminals
  (evil-set-initial-state 'term-mode 'emacs)
  (evil-set-initial-state 'eshell-mode 'emacs)

  ; apparently evil-leader-mode should be enabled before enabling evil-mode
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
     "o a" 'org-agenda-list
     "o t" 'org-todo-list
     "o c" 'org-capture
     "o b" 'org-switchb

     ;; buffers
     "b b" 'counsel-ibuffer
     "b k" (lambda () (interactive) (kill-buffer (current-buffer)))

     ;; other
     "d" 'deft
     "g" 'magit-status
     "TAB" 'dtb/switch-to-other-buffer
     "r" 'counsel-rg
     )

    )

  (evil-mode 1)
  
  ;; I like to use arrow keys for command line history, at least in
  ;; insert mode
  (evil-define-key 'insert shell-mode-map
    (kbd "<up>")   'comint-previous-input
    (kbd "<down>") 'comint-next-input)
  (evil-define-key 'insert inferior-python-mode-map
    (kbd "<up>")   'comint-previous-input
    (kbd "<down>") 'comint-next-input)

  ;; I prefer M-. to run xref-find-defintions in normal mode
  (define-key evil-normal-state-map (kbd "M-.") 'xref-find-definitions)

  :custom
  (evil-undo-system 'undo-tree) ; had to make this a custom to get it to work
  )

(use-package evil-surround
  :after evil
  :ensure t
  :config
  (global-evil-surround-mode 1))

(use-package evil-indent-plus
  :after evil
  :ensure t
  :config
  (evil-indent-plus-default-bindings))

(use-package evil-collection
  :after evil
  :ensure t
  :config
  (setq evil-collection-mode-list '(dired ibuffer (occur replace)))
  (evil-collection-init))

;; "diminish" minor modes by not dislaying them in the mode-line
(use-package diminish
  :ensure t
  :config
  ;; More configuration goes here
  )

(use-package magit
  :ensure t
  :config
  ;; More configuration goes here
  )

(use-package evil-magit
  :after evil
  :ensure t
  :config
  (setq evil-magit-state 'motion)
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
  (setq org-agenda-files '("~/notes"))

  ;(use-package org-bullets
  ;  :ensure t
  ;  :config
  ;  (add-hook 'org-mode-hook (lambda () (org-bullets-mode t))))
  )

(use-package evil-org
  :ensure t
  :after (evil org)
  :config
  (add-hook 'org-mode-hook 'evil-org-mode)
  (add-hook 'evil-org-mode-hook
            (lambda ()
              (evil-org-set-key-theme '(navigation insert textobjects additional calendar))))
  (require 'evil-org-agenda)
  (evil-org-agenda-set-keys))

; loading this allows counsel-M-x to show most recent commands first
(use-package smex
  :ensure t
  :config
  ;; More configuration goes here
  )

(use-package hydra
  :ensure t
  :config
  ;; More configuration goes here
  )

(use-package ivy
  :ensure t
  :diminish (ivy-mode . "")
  :config
  (ivy-mode 1)
  (setq ivy-use-virtual-buffers t)
  (setq ivy-count-format "(%d/%d) ")
  (setq ivy-initial-inputs-alist nil)
  )

(use-package counsel
  :ensure t
  :config
  (counsel-mode 1)
  )

;; dired
;; much from protesilaos
(use-package dired
  :config
  (setq dired-recursive-copies 'always)
  (setq dired-recursive-deletes 'always)
  (setq delete-by-moving-to-trash t)
  (setq dired-listing-switches
        "-AFhlv --group-directories-first --time-style=long-iso")
  (setq dired-dwim-target t)
  :hook ((dired-mode-hook . dired-hide-details-mode))
  ;; (add-hook 'dired-mode-hook 'hl-line-mode);
  )

(use-package wdired
  :after dired
  :commands wdired-change-to-wdired-mode
  :config
  (setq wdired-allow-to-change-permissions t)
  (setq wdired-create-parent-directories t))

(use-package dired-x
  :after dired
  :config
  (setq dired-clean-up-buffers-too t)
  (setq dired-clean-confirm-killing-deleted-buffers t)
  )

;;
;; ibuffer
;;
(use-package ibuffer
  :config
  (setq ibuffer-expert t)
  (setq ibuffer-saved-filter-groups
	(quote (("default"
		 ("magit" (name . "^magit"))
		 ("org" (mode . org-mode))
		 ("dired" (mode . dired-mode))
		 ("python" (mode . python-mode))
		 ))))
  :hook (ibuffer-mode-hook . (lambda ()
			       (ibuffer-switch-to-saved-filter-groups "default")))
  :bind (("C-x C-b" . ibuffer)))

  ;(global-set-key (kbd "C-x C-b") 'ibuffer)

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

(window-divider-mode +1)

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

;;
;; Personal functions
;;

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

;; Ideas came from https://www.reddit.com/r/emacs/comments/6bu3yt/fast_buffer_switching/
(defun dtb/switch-to-other-buffer (&optional aggr)
  "Switch to 'other' buffer.
Repeated invocations toggle between the two most recently open
buffers. With C-u, bury current buffer. With C-u C-u, kill current
buffer (unless it's modified)."
  (interactive "P")
  (cond
   ((eq aggr nil)
    (dolist (buf '("*Buffer List*" "*Ibuffer*"))
      (when (get-buffer buf)
	(bury-buffer buf)))
    (switch-to-buffer (other-buffer)))
   ((equal aggr '(4)) (bury-buffer))
   ((equal aggr '(16)) (kill-buffer-if-not-modified (current-buffer)))))

; (global-set-key (kbd "C-a") 'dtb/ctrl-a)

(defun dtb/copy-to-end-of-buffer ()
 (interactive)
 (copy-region-as-kill (point) (point-max))
 )

(global-set-key (kbd "C-c c b") 'dtb/copy-to-end-of-buffer)

;;
;; Stuff specific to particular computers
;;

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
      ((string-match "687db2-vm5" system-name)
       (dtb-set-default-font "Hack-9:autohint=true:hintstyle=hintfull:embeddedbitmap=false")
       (add-to-list 'default-frame-alist '(left . 0))
       (add-to-list 'default-frame-alist '(top . 0))
       (add-to-list 'default-frame-alist '(height . 65))
       (add-to-list 'default-frame-alist '(width . 147)))
      ((string-match "xubuntu-work-2" system-name)
       (dtb-set-default-font "DejaVu Sans Mono-10"))
      ((string-match "xubuntu-1" system-name)
       (dtb-set-default-font "DejaVu Sans Mono-10")
       (add-to-list 'default-frame-alist '(left . 0))
       (add-to-list 'default-frame-alist '(top . 0))
       (add-to-list 'default-frame-alist '(height . 54))
       (add-to-list 'default-frame-alist '(width . 84)))
      )

;;
;; TODO
;;
;; read lab-notebook post https://www.sciencemag.org/careers/2019/09/how-keep-lab-notebook
;;
;; on learnig: https://superorganizers.substack.com/p/how-to-build-a-learning-machine
;;
;; continue with this series on videos: https://www.youtube.com/watch?v=u00pglDfgX4&list=PLVtKhBrRV_ZkPnBtt_TD1Cs9PJlU0IIdE&index=7
;;
;; maybe some good ideas on python in emacs in this video? https://www.youtube.com/watch?v=6BlTGPsjGJk&index=15&list=PL8tzorAO7s0he-pp7Y_JDl7-Kz2Qlr_Pj
;;
;; read this: https://daedtech.com/how-developers-stop-learning-rise-of-the-expert-beginner/
 
