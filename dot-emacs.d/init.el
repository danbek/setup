;; Better to put custom settings in their own file
(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
(load custom-file 'noerror)

(setq gc-cons-threshold (* 10 1024 1024))

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
(use-package undo-fu
  :ensure t
  :config
  )

(use-package evil
  :ensure t
  :after undo-fu
  :init
  ;; Must do these before loading evil to get evil-collection working
  (setq evil-want-keybinding nil)
  (setq evil-want-integration t)
  (setq evil-undo-system 'undo-fu)
  
  :config
  ;; I'm now trying vi bindings in terms.
  ; (evil-set-initial-state 'term-mode 'emacs)
  ; (evil-set-initial-state 'eshell-mode 'emacs)

  ;; leader key stuff
  (evil-set-leader 'normal (kbd "SPC"))
  (evil-define-key 'normal 'global

    ;; loading files
    (kbd "<leader>fe") (lambda () (interactive) (find-file user-init-file))
    (kbd "<leader>ff") 'counsel-find-file
    (kbd "<leader>fo") (lambda () (interactive) (find-file "~/notes/organizer.org"))
    (kbd "<leader>fr") 'counsel-recentf
    (kbd "<leader>fv") 'find-alternate-file
  
    ;; org-mode
    (kbd "<leader>ol") 'org-store-line
    (kbd "<leader>oa") 'org-agenda-list
    (kbd "<leader>ot") 'org-todo-list
    (kbd "<leader>oc") 'org-capture
    (kbd "<leader>ob") 'org-switchb
    
    ;; buffers
    (kbd "<leader>bb") 'counsel-ibuffer
    (kbd "<leader>bi") 'ibuffer
    (kbd "<leader>bk") (lambda () (interactive) (kill-buffer (current-buffer)))
    
    ;; other
    (kbd "<leader>d") 'deft
    (kbd "<leader>g") 'magit-status
    (kbd "<leader><tab>") 'dtb/switch-to-other-buffer
    (kbd "<leader>r") 'counsel-rg
    (kbd "<leader>O") 'occur
    )
  
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
  
  (evil-mode 1)
  
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
  (setq evil-collection-mode-list '(dired ibuffer (occur replace) eshell term magit))
  (evil-collection-init)
  )

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
  (setq delete-by-moving-to-trash nil)
  (setq dired-listing-switches
        "-AFhlv --group-directories-first --time-style=long-iso")
  (setq dired-dwim-target t)
  :hook ((dired-mode-hook . dired-hide-details-mode)
	 (dired-mode-hook . hl-line-mode)
	 (dired-mode-hook .
			  ;; evil-collection clobbers my leader key in dired
			  ;; mode. Maybe there is a better way to do this,
			  ;; but I don't know what it is. Seem to need to put
			  ;; it in a hook
			  (lambda ()
			    (evil-define-key 'normal dired-mode-map (kbd "SPC") 'evil-send-leader))))
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

(use-package dired-subtree
  :after dired
  :config
  :bind (:map dired-mode-map
              ("<tab>" . dired-subtree-toggle)
              ("<C-tab>" . dired-subtree-cycle)
              ("<S-iso-lefttab>" . dired-subtree-remove)))


(use-package eshell
  :config
  ;; I think that the regexp only needs to match the *second* line [1]
  ;; [1]: https://emacs.stackexchange.com/questions/7370/eshell-prompt-regex
  (setq eshell-prompt-regexp "^[#$] "
	eshell-prompt-function
	(lambda ()
	  (concat "[" (user-login-name) "@" (system-name) ":"
		  (if (string= (eshell/pwd) (getenv "HOME"))
		      "~"
		    (eshell/pwd))
		  "]\n$ ")))
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
		 ("special" (name . "*"))
		 ))))
  :hook (ibuffer-mode-hook . (lambda ()
			       (ibuffer-switch-to-saved-filter-groups "default")))
  :bind (("C-x C-b" . ibuffer)))

  ;(global-set-key (kbd "C-x C-b") 'ibuffer)

;;
;; company ... allows completion via lsp-mode
;;
(use-package company
  :ensure t
  :init
  (setq company-minimum-prefix-len 1
	company-idle-delay 0)
  :config
  (bind-key "C-n" 'company-select-next company-active-map)
  (bind-key "C-p" 'company-select-previous company-active-map)
  (bind-key "C-n" 'company-complete evil-insert-state-map)
  (global-company-mode 1)
  )

;;
;; projectile
;;
(use-package projectile
  :ensure t
  :init
  (projectile-mode +1)
  :bind (:map projectile-mode-map
              ("s-p" . projectile-command-map)
              ("C-c p" . projectile-command-map)))


;;
;; flycheck
;; lsp-mode turns this on automatically when active. Not yet sure I want it on everywhere
;;
(use-package flycheck
  :ensure t
  ;;   :init (global-flycheck-mode)
  )

;;
;; Python setup. Let's try elpy
;;
;; Apparently the only way to turn off highlight-indentation-mode is
;; to remove it from the list of elpy modules, which can be done
;; through M-x customize-variable RET elpy-modules
;;

;; (use-package elpy
;;   :ensure t
;;   :config
;;   (elpy-enable)
;;   (add-hook 'python-mode-hook
;; 	    (lambda () (auto-fill-mode t)))
;;   (add-hook 'python-mode-hook
;; 	    (lambda () (linum-mode t)))
;;   (setq python-shell-interpreter "ipython"
;; 	python-shell-interpreter-args "-i --simple-prompt --matplotlib"
;; 	elpy-rpc-virtualenv-path 'current)
;;   )

;;
;; Python setup.
;;
;; Here is what I've found works for getting lsp-mode working with
;; mutliple virtual environments.
;;
;; Each project that wants to run in it's own virtual environment
;; needs a .dir-locals.el file. This contains code like the following:
;;
;;  ((python-mode
;;    (pyvenv-workon . "sledge9")))
;;
;; If a bunch of "projects" or "repos" all live in the same directory
;; (e.g. ~/gitrepo), and all use the same virtual env, then you put
;; the .dir-locals.el file in that same directory.
;;
;; Then if you turn on pyvenv-tracking-mode, then whenever you switch
;; to a buffer with a python file, it will change the global "active"
;; virtual env appropriately.
;;
;; For lsp-mode, you need to install the pyls LSP server in *each* of
;; these virtual environments:
;;
;; conda install python-language-server
;;
;; or
;;
;; pip install python-language-server
;; 
;; If you start lsp from within a python buffer, it will start (and
;; connect to) the pyls for the current environment.
;;
 
(use-package python
  :config
  :hook (
	 (python-mode-hook . (lambda () (linum-mode t)))
	 )
  )

(use-package pyvenv
  :ensure t
  :init
  (setenv "WORKON_HOME" (expand-file-name "~/installs/anaconda3/envs"))
  :config
  (pyvenv-mode 1)
  ;; Automatically use pyvenv-workon via dir-locals.
  ;; Note: in the dir-locals file, use a *string* for the env name, not a symbol
  (pyvenv-tracking-mode 1)
  )

(use-package lsp-mode
  :ensure t
  :config

  ;; Recommended by lsp-mode documentation
  (setq read-process-output-max (* 1024 1024)) ;; 1mb
  
  (defun dtb/lsp-setup()
    (setq lsp-idle-delay 0.5
          lsp-enable-symbol-highlighting nil
          lsp-enable-snippet nil  ;; Not supported by company capf, which is the recommended company backend
          lsp-pyls-plugins-flake8-enabled t)
    (lsp-register-custom-settings
     '(
;       ("pyls.plugins.pyls_mypy.enabled" t t)
;       ("pyls.plugins.pyls_mypy.live_mode" nil t)
;       ("pyls.plugins.pyls_black.enabled" t t)
;       ("pyls.plugins.pyls_isort.enabled" t t)

       ;; Disable these as they're duplicated by flake8
       ("pyls.plugins.pycodestyle.enabled" nil t)
       ("pyls.plugins.mccabe.enabled" nil t)
       ("pyls.plugins.pyflakes.enabled" nil t))))
  
  :init
  ;; set prefix for lsp-command-keymap (few alternatives - "C-l", "C-c l")
  (setq lsp-keymap-prefix "C-c l")
  :hook (
	 (lsp-before-initialize-hook . dtb/lsp-setup)
	 ;; It appears that python-mode-hook will run *before* the
	 ;; directory-local variables are loaded and the correct
	 ;; virtual env is activated. So in order to automatically
	 ;; start the LSP server we need to force a load of
	 ;; .dir-local.el and then force pyvenv to track the correct
	 ;; virtual env.
	 (python-mode-hook . (lambda ()
			       (hack-dir-local-variables-non-file-buffer)
			       (pyvenv-track-virtualenv)
			       (lsp)))
;         ;; if you want which-key integration
;         ;; (lsp-mode-hook . lsp-enable-which-key-integration)
	 )
  :commands lsp)

;; optionally
(use-package lsp-ui
  :requires lsp-mode
  :ensure t
  )
  

;; Provides access to workspace symbols, but unfortunately pyls
;; doesn't appear to support them.
(use-package lsp-ivy
  :requires lsp-mode
  :ensure
  )

;; (use-package lsp-treemacs :commands lsp-treemacs-errors-list)

;; optionally if you want to use debugger
;; (use-package dap-mode)
;; (use-package dap-LANGUAGE) to load the dap adapter for your language

;; optional if you want which-key integration
;; (use-package which-key
;;     :config
;;     (which-key-mode))

;;
;; Themes
;;

;; Trying this from Prot Stavrous. There are many options to explore
;; https://gitlab.com/protesilaos/modus-themes 
(use-package modus-themes
  :ensure
  :init
  ;; Add all your customizations prior to loading the themes
  (setq modus-themes-slanted-constructs t
        modus-themes-bold-constructs t)

  ;; Load the theme files before enabling a theme
  (modus-themes-load-themes)
  :config
  ;; Load the theme of your choice:
  (modus-themes-load-operandi) ;; OR (modus-themes-load-vivendi)
  )

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
(use-package emacs
  :config
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

  :hook (after-init-hook . column-number-mode)
  )

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

;; For fixing file permissions after unzipping certain data sets
(defun dtb/correct-data-file-permissions (dir)
  (let ((file-and-directory-names (directory-files-recursively dir "" t)))
    (mapcar (lambda (fname)
	      (cond ((file-directory-p fname)
		     (set-file-modes fname #o775))
		    (t
		     (set-file-modes fname #o664))))
	    file-and-directory-names))
  (set-file-modes dir #o775))

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
      ((string-match "182loane7240-vm1" system-name)
       (dtb-set-default-font "Hack-10:autohint=true:hintstyle=hintfull:embeddedbitmap=false")
       (add-to-list 'default-frame-alist '(left . 0))
       (add-to-list 'default-frame-alist '(top . 0))
       (add-to-list 'default-frame-alist '(height . 40))
       (add-to-list 'default-frame-alist '(width . 86)))
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
 
