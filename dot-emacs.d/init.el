;;
;; Stuff that I think needs to happen immediately
;;

;; Better to put custom settings in their own file
(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
(load custom-file 'noerror)

(setq gc-cons-threshold (* 10 1024 1024))

;;
;; Setup straight.el and use-package
;;

;; straight.el bootstrapping
(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
      (bootstrap-version 5))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/raxod502/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

;; I'm not sure whether this really needs to be wrapped in
;; eval-and-compare, but that's how it was done in the init.el that I
;; got this idea from, so ...
(eval-and-compile
  ;; Write hooks using their real name instead of a shorter version:
  ;;    after-init ==> `after-init-hook'.
  ;;
  ;; This is to empower help commands with their contextual awareness,
  ;; such as `describe-symbol'.
  (setq use-package-hook-name-suffix nil))


; setup use-package
(straight-use-package 'use-package)

;;
;; For local lisp
;;
(add-to-list 'load-path "~/.emacs.d/site-lisp")

;;
;; General emacs stuff
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

  (setq tramp-default-method "ssh")
  
  ;; Follow symlinks without prompting. If this isn't done, then you will
  ;; get a prompt every time you edit, e.g., .profile
  ;; that ~/.profile is a symlink, which is how my setup script
  ;; sets it up.
  (setq vc-follow-symlinks t)

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

  ;; modus themes are packaged with emacs starting with 28.1!
  (setq modus-themes-slanted-constructs t
        modus-themes-bold-constructs t)
  (load-theme 'modus-operandi)

  :hook (after-init-hook . column-number-mode)
  )

;;
;; Now various packages
;;
(use-package avy
  :straight t
  :config
  (setq avy-timeout-seconds 0.2)
  (global-set-key (kbd "M-j") 'avy-goto-char-timer)
  )

(use-package undo-fu
  :straight t
  :config
  )

(use-package evil
  :straight t
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
  (evil-set-leader 'motion (kbd "SPC"))
  (evil-define-key 'normal 'global

    ;; loading files
    (kbd "<leader>fe") (lambda () (interactive) (find-file user-init-file))
;;     (kbd "<leader>ff") 'counsel-find-file
    (kbd "<leader>ff") 'find-file
    (kbd "<leader>fo") (lambda () (interactive) (find-file "~/notes/organizer.org"))
;;    (kbd "<leader>fr") 'counsel-recentf
    (kbd "<leader>fr") 'recentf
    (kbd "<leader>fv") 'find-alternate-file
    (kbd "<leader>fb") (lambda () (interactive) (ivy-bibtex t))
  
    ;; org-mode
    (kbd "<leader>oa") 'org-agenda-list
    (kbd "<leader>ot") 'org-todo-list
    (kbd "<leader>ob") 'org-switchb
    
    ;; buffers & bibliography
;;     (kbd "<leader>bb") 'counsel-ibuffer
    (kbd "<leader>bb") 'consult-buffer
    (kbd "<leader>bi") 'ibuffer
    (kbd "<leader>bk") (lambda () (interactive) (kill-buffer (current-buffer)))
    (kbd "<leader>br") (lambda () (interactive) (revert-buffer t (not (buffer-modified-p)) t))
    
    (kbd "<leader>bp") 'doi-utils-get-bibtex-entry-pdf
    (kbd "<leader>be") 'doi-insert-bibtex
    
    ;; other
    (kbd "<leader>d") 'deft
    (kbd "<leader>g") 'magit-status
    (kbd "<leader><tab>") 'dtb/switch-to-other-buffer
;;     (kbd "<leader>r") 'counsel-rg
    (kbd "<leader>r") 'consult-ripgrep
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
  )

(use-package evil-surround
  :straight t
  :after evil
  :config
  (global-evil-surround-mode 1))

(use-package evil-indent-plus
  :straight t
  :after evil
  :config
  (evil-indent-plus-default-bindings))

(use-package evil-collection
  :straight t
  :after (evil magit)
  :config
  (setq evil-collection-mode-list '(dired ibuffer replace eshell term magit info help))
  (evil-collection-init)
)

;; "diminish" minor modes by not dislaying them in the mode-line
(use-package diminish
  :straight t
  :config
  ;; More configuration goes here
  )

(use-package magit
  :straight t
  :config
  ;; More configuration goes here
  )

;; (use-package deft
;;   :ensure t
;;   :config
;;   ;; More configuration goes here
;;   (setq deft-default-extension "org")
;;   (setq deft-directory "~/notes/notes")
;;   (setq deft-org-mode-title-prefix t)
;;   (setq deft-use-filter-string-for-filename t)
;;   (setq deft-file-naming-rules
;; 	'((noslash . "-")
;; 	  (nospace . "-")
;; 	  (case-fn . downcase)))
;;   )


(use-package org
  :straight t
  :config

  ;; general
  (setq org-startup-folded t)
  (setq org-hide-leading-stars t)

  (global-set-key (kbd "C-c l") 'org-store-link)
  (global-set-key (kbd "C-c a") 'org-agenda)
  (global-set-key (kbd "C-c c") 'org-capture)

  (defun gs-org-disable-evil-auto-indent ()
    "Disables evil's auto-indent for org."
    (setq evil-auto-indent nil)
    )
  (add-hook 'org-mode-hook #'gs-org-disable-evil-auto-indent)

  (add-hook 'org-mode-hook 'turn-on-auto-fill)

  ;; org-refile stuff
  (setq org-refile-targets (quote ((nil :maxlevel . 4)
                                   (org-agenda-files :maxlevel . 4))))
  (setq org-refile-use-outline-path 't)
  (setq org-outline-path-complete-in-steps nil)

  ;; org-capture stuff
  (setq org-default-notes-file "~/notes/inbox.org")
  (setq org-capture-templates
        '(("t" "Todo [inbox]" entry
           (file+headline "~/notes/inbox.org" "Tasks")
           "* TODO %i%?\n%a")
          ("n" "Todo [inbox, no link]" entry
           (file+headline "~/notes/inbox.org" "Tasks")
           "* TODO %i%?\n")
          ))

  ;; task stuff
  (setq org-todo-keywords
	'((sequence "TODO(t)" "PROJ(p)" "WAITING(w)" "|" "DONE(d)")
	  (sequence "|" "DEFERRED(f)" "SOMEDAY(s)" "CANCELLED(c)")))

  (setq org-todo-state-tags-triggers
	'(("PROJ" ("PROJ" . t))
	  ("TODO" ("PROJ" . nil))
	  ("WAITING" ("PROJ" . nil))
	  ("" ("PROJ" . nil))
	  ))

  ;; agenda stuff
  (setq org-agenda-files '("~/notes"))

  ;; For best use, need to do two things:
  ;;  1. Add a "PROJ" tag to each PROJ todo (C-c C-q) (trying to
  ;;     handle this automatically via org-todo-state-triggers)
  ;;  2. Add a property CATEGORY to each PROJ todo (keep name short)
  (setq org-agenda-custom-commands
	'(("o" "Open Tasks"
	   (
	    (tags-todo "PROJ/TODO|NEXT" ((org-agenda-overriding-header "Projects")))
	    (tags-todo "-PROJ/!TODO" ((org-agenda-overriding-header "Standalone")))
	    (todo "WAITING" ((org-agenda-overriding-header "WAITING Tasks")))
	    )
	   ((org-agenda-compact-blocks t)))
	  ("w" "WAITING Tasks"
	   todo "WAITING" ((org-agenda-overriding-header "WAITING Tasks")))
	  ("p" "Active Projects"
	   todo "PROJ" ((org-agenda-overriding-header "Active Projects")))))

  (add-hook 'org-agenda-finalize-hook #'hl-line-mode)

  ;;(use-package org-bullets
  ;;  :config
  ;;  (add-hook 'org-mode-hook (lambda () (org-bullets-mode t))))
  )

(use-package evil-org
  :straight t
  :after (evil org)
  :config
  (add-hook 'org-mode-hook 'evil-org-mode)
  (add-hook 'evil-org-mode-hook
            (lambda ()
              (evil-org-set-key-theme '(navigation insert textobjects additional calendar))))
  (require 'evil-org-agenda)
  (evil-org-agenda-set-keys))

;;
;; Lets try the vertico / consult / embark / orderless / marginalia stuff
;;

(use-package vertico
  :straight t
  :config
  (vertico-mode))

(use-package consult
  :straight t
  :bind
  (("C-c r" . consult-ripgrep)
   ("C-x b" . consult-buffer)))

(use-package embark
  :straight t
  :bind
  (("C-c a" . embark-act)))

(use-package embark-consult
  ;; comes bundled with Embark; no `:ensure t' necessary
  :after (embark consult))

;(use-package wgrep
;  :ensure t)

(use-package orderless
  :straight t
  :custom
  (completion-styles '(orderless basic))
  (completion-category-overrides '((file (styles basic partial-completion)))))

(use-package marginalia
  :straight t
  ;; Bind `marginalia-cycle' locally in the minibuffer.  To make the binding
  ;; available in the *Completions* buffer, add it to the
  ;; `completion-list-mode-map'.
  :bind (:map minibuffer-local-map
         ("M-A" . marginalia-cycle))

  ;; The :init section is always executed.
  :init

  ;; Marginalia must be activated in the :init section of use-package such that
  ;; the mode gets enabled right away. Note that this forces loading the
  ;; package.
  (marginalia-mode)
  )

;; https://github.com/abo-abo/ace-window
(use-package ace-window
  :straight t
  :bind
  (("M-o" . ace-window)))

;; ;;
;; ;; Dealing with bibliography stuff
;; ;;
;; ;; Three parts here. First, ivy-bibtex gives a way to search through a
;; ;; bibtex file (or set of bibtex files), via ivy-bibtex M-o on a
;; ;; selection gives you options to do various things with that
;; ;; selection.
;; ;;
;; ;; ivy-bibtex associates a pdf and "notes" file with each entry.
;; ;;
;; ;; Second, org-ref allows you to insert org-mode-style references,
;; ;; leaning on ivy-bibtex for the choice of item.
;; ;;
;; ;; Finally, doi-utils (which is part of org-ref) provides functions to
;; ;; insert a bibtex enetry gievn a doi, and to download a pdf for an
;; ;; entry. Thse are bound to keys in the evil setup
;; ;;
;; ;; Integrate someday?
;; ;;
;; ;; https://github.com/mpedramfar/zotra - integrate with an instance of zotero translation service
;; ;; 
;; (use-package ivy-bibtex
;;   :straight t
;;   :config
;;   (setq bibtex-completion-bibliography '("/home/beckerd/notes/report.bib" "/home/beckerd/notes/becker.bib"))
;;   (setq bibtex-completion-library-path "/home/beckerd/notes/pdf/")
;;   (setq bibtex-completion-notes-path "/home/beckerd/notes/notes/")

;;   ;; from John Kitchin's org-ref configuration instructions
;;   (setq bibtex-completion-display-formats
;; 	'((article       . "${=has-pdf=:1}${=has-note=:1} ${year:4} ${author:36} ${title:*} ${journal:40}")
;; 	  (inbook        . "${=has-pdf=:1}${=has-note=:1} ${year:4} ${author:36} ${title:*} Chapter ${chapter:32}")
;; 	  (incollection  . "${=has-pdf=:1}${=has-note=:1} ${year:4} ${author:36} ${title:*} ${booktitle:40}")
;; 	  (inproceedings . "${=has-pdf=:1}${=has-note=:1} ${year:4} ${author:36} ${title:*} ${booktitle:40}")
;; 	  (t             . "${=has-pdf=:1}${=has-note=:1} ${year:4} ${author:36} ${title:*}")))

;;   ;; Per the README.org, the following should allow the little icons
;;   ;; to appear without reloading the bib file. But it doesn't work,
;;   ;; because bibtex-completion-candidates only reloads if the .bib
;;   ;; file's hash has changed. So commented out for now
;;   ;;
;;   ;; (setq dtb/pdf-watch
;;   ;; 	(file-notify-add-watch bibtex-completion-library-path
;;   ;;                              '(change)
;;   ;;                              (lambda (event) (bibtex-completion-candidates))))
;;   ;; (setq dtb/notes-watch
;;   ;; 	(file-notify-add-watch bibtex-completion-notes-path
;;   ;;                              '(change)
;;   ;;                              (lambda (event) (bibtex-completion-candidates))))
;;   )

;; ;;
;; ;; An issue with doi-utils is that it doesn't use journal name
;; ;; abbreviations ... I guess makes sense, probably just uses whatever
;; ;; it find on relevant website. In principle seems like the LTWA [1] could
;; ;; be used a source for abbreviations, but it doesn't include the word
;; ;; "Physical" which is really odd.
;; ;;
;; ;; [1]: https://www.issn.org/services/online-services/access-to-the-ltwa/
;; ;;
;; (use-package org-ref
;;   :straight t
;;   :config
;;   (require 'org-ref-ivy)
;;   (setq
;;    org-ref-insert-link-function 'org-ref-insert-link-hydra/body
;;    org-ref-insert-cite-function 'org-ref-cite-insert-ivy
;;    org-ref-insert-label-function 'org-ref-insert-label-link
;;    org-ref-insert-ref-function 'org-ref-insert-ref-link
;;    org-ref-cite-onclick-function (lambda (_) (org-ref-citation-hydra/body))
   
;;    ;;org-ref-get-pdf-filename-function 'org-ref-get-pdf-filename-helm-bibtex
;;    ;;org-ref-default-bibliography (list "/home/haozeke/GDrive/zotLib.bib")
;;    ;;org-ref-bibliography-notes "/home/haozeke/Git/Gitlab/Mine/Notes/bibnotes.org"
;;    ;;org-ref-note-title-format "* TODO %y - %t\n :PROPERTIES:\n  :Custom_ID: %k\n  :NOTER_DOCUMENT: %F\n :ROAM_KEY: cite:%k\n  :AUTHOR: %9a\n  :JOURNAL: %j\n  :YEAR: %y\n  :VOLUME: %v\n  :PAGES: %p\n  :DOI: %D\n  :URL: %U\n :END:\n\n"
;;    ;;org-ref-notes-directory "/home/haozeke/Git/Gitlab/Mine/Notes/"
;;    ;;org-ref-notes-function 'orb-edit-notes
;;    )
;;   )

;; ;; to install prerequisites: sudo apt-get install libpng-dev zlib1g-dev libpoppler-glib-dev libpoppler-private-dev imagemagick
;; (use-package pdf-tools
;;   :straight t
;;   :config
;;   (pdf-tools-install)
;;   ;; open pdfs scaled to fit page
;;   (setq-default pdf-view-display-size 'fit-page)
;;   (setq pdf-view-resize-factor 1.1)

;;   ;; automatically annotate highlights
;;   (setq pdf-annot-activate-created-annotations t)
  
;;   ;; use normal isearch
;;   (define-key pdf-view-mode-map (kbd "C-s") 'isearch-forward)
;;   ;; (define-key pdf-view-mode-map (kbd "SPC") 'evil-send-leader)
;;   )

;; ; loading this allows counsel-M-x to show most recent commands first
;; (use-package smex
;;   :straight t
;;   :config
;;   ;; More configuration goes here
;;   )

;; (use-package hydra
;;   :straight t
;;   :config
;;   ;; More configuration goes here
;;   )

;; (use-package ivy
;;   :straight t
;;   :diminish (ivy-mode . "")
;;   :config
;;   (ivy-mode 1)
;;   (setq ivy-use-virtual-buffers t)
;;   (setq ivy-count-format "(%d/%d) ")
;;   (setq ivy-initial-inputs-alist nil)
;;   (global-set-key (kbd "M-j") 'avy-goto-char-timer)  ; better place for this?
;;   )

;; (use-package counsel
;;   :straight t
;;   :config
;;   (counsel-mode 1)
;;   )

(use-package bibtex
  :config
  (setq bibtex-entry-format
      '(opts-or-alts required-fields whitespace realign delimiters unify-case sort-fields)
      bibtex-align-at-equal-sign 't)
  :hook ((bibtex-mode-hook . (lambda () (setq fill-column 120))))
  )

;;
;; dired
;; much from protesilaos
;;

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
  :straight t
  :after dired
  :config
  :bind (:map dired-mode-map
              ("<tab>" . dired-subtree-toggle)
              ("<C-tab>" . dired-subtree-cycle)
              ("<S-iso-lefttab>" . dired-subtree-remove)))

;;
;; eshell 
;;

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
		 ("magit" (name . "^magit:"))
		 ("org" (mode . org-mode))
		 ("dired" (mode . dired-mode))
		 ("python-shells" (mode . inferior-python-mode))
		 ("python" (mode . python-mode))
		 ("special" (or (name . "*")
				(name . "^magit-process:")))
		 ))))
  :hook (ibuffer-mode-hook . (lambda ()
			       (ibuffer-switch-to-saved-filter-groups "default")))
  :bind (("C-x C-b" . ibuffer)))

  ;(global-set-key (kbd "C-x C-b") 'ibuffer)

;;
;; company ... allows completion via lsp-mode
;;
(use-package company
  :straight t
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
;; flycheck
;; lsp-mode turns this on automatically when active. Not yet sure I want it on everywhere
;;
(use-package flycheck
  :straight t
  ;;   :init (global-flycheck-mode)
  )

;;
;; Python setup using lsp
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
;; For lsp-mode, you need to install an LSP server in *each* of
;; these virtual environments:
;;
;; conda install -c python-lsp-server python-lsp-server
;;
;; or
;;
;; pip install python-lsp-server
;; 
;; If you start lsp from within a python buffer, it will start (and
;; connect to) the pyls for the current environment.
;;
 
(use-package python
  :config
  ;; This makes ipython the default for python-shell [1]. It seems to work.
  ;; [1]: https://stackoverflow.com/a/25687205
  (setq
   python-shell-interpreter "ipython"
   python-shell-interpreter-args "--colors=Linux --profile=default --simple-prompt"
   python-shell-prompt-regexp "In \\[[0-9]+\\]: "
   python-shell-prompt-output-regexp "Out\\[[0-9]+\\]: "
   python-shell-completion-setup-code "from IPython.core.completerlib import module_completion"
   python-shell-completion-module-string-code "';'.join(module_completion('''%s'''))\n"
   python-shell-completion-string-code "';'.join(get_ipython().Completer.all_completions('''%s'''))\n")

  ;; one python shell (via C-c C-p) per project. 'buffer and nil
  ;; (i.e., one global shell) are also options
  (setq python-shell-dedicated 'project)

  :hook (
	 (python-mode-hook . (lambda ()
			       (if (>= emacs-major-version 29)
				   (display-line-numbers-mode)
				 (linum-mode t))))
	 )
  )

(use-package pyvenv
  :straight t
  :init
  (cond ((string-match "68708DBLAP" system-name)
	 (setenv "WORKON_HOME" (expand-file-name "~/installs/anaconda3/envs")))
	(t
	 (setenv "WORKON_HOME" (expand-file-name "~/installs/miniconda3/envs"))))
  :config
  (pyvenv-mode 1)
  ;; Automatically use pyvenv-workon via dir-locals.
  ;; Note: in the dir-locals file, use a *string* for the env name, not a symbol
  (pyvenv-tracking-mode 1)
  )

(use-package lsp-mode
  :straight t
  :config

  ;; Recommended by lsp-mode documentation
  (setq read-process-output-max (* 1024 1024)) ;; 1mb
  
  (defun dtb/lsp-setup()
    (setq lsp-idle-delay 0.5
          lsp-enable-symbol-highlighting nil
          lsp-enable-snippet nil  ;; Not supported by company capf, which is the recommended company backend
          lsp-pyls-plugins-flake8-enabled t
	  lsp-ui-doc-enable nil
	  )
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

;; To turn off the documentation part of this, I had to set
;; lsp-ui-doc-enable to nil in my dtb/lsp-setup function. It didn't
;; get set to nil when I tried to do it here under either :config or
;; :init
(use-package lsp-ui
  :straight t
  :requires lsp-mode
  )
  

;; ;; Provides access to workspace symbols, but unfortunately pyls
;; ;; doesn't appear to support them.
;; (use-package lsp-ivy
;;   :straight t
;;   :requires lsp-mode
;;   )

;; (use-package lsp-treemacs :commands lsp-treemacs-errors-list)

;; optionally if you want to use debugger
;; (use-package dap-mode)
;; (use-package dap-LANGUAGE) to load the dap adapter for your language

;; optional if you want which-key integration
;; (use-package which-key
;;     :config
;;     (which-key-mode))

;; Latex
(use-package auctex
  :straight t
  ;;; :defer t
  :config
  (setq
   TeX-auto-save t
   TeX-parse-self t
   TeX-error-overview-open-after-TeX-run t
   ;; TeX-source-correlate-method 'synctex
   ;;TeX-source-correlate-mode t
   ;;TeX-source-correlate-start-server t
   )
  (setq-default TeX-master nil)
  ; TeX-PDF-mode t
  ; TeX-source-correlate-start-server t)
  )

;; spellchecking
(use-package flyspell
  :ensure t
  :hook ((LaTeX-mode-hook . flyspell-mode)          ; Enable Flyspell for LaTeX
	 (latex-mode-hook . flyspell-mode)
         (LaTeX-mode-hook . flyspell-buffer)       ; Optionally spellcheck the entire buffer
         (latex-mode-hook . flyspell-buffer))
  :config
  (setq ispell-program-name "hunspell"
	ispell-personal-dictionary "/home/becker/.hunspell_default"))

;;
;; Julia stuff
;;

(use-package julia-mode
  :straight t
  :init
  )

(use-package julia-repl
  :straight t
  :after julia-mode
  :init
  )


;;
;; Old approach
;;

;;(require 'julia-mode)
;;(require 'julia-repl)
;;(add-hook 'julia-mode-hook 'julia-repl-mode)
;;(setq julia-repl-executable-records
;;      '((julia-1.1.0 "/home/dan/installs/julia-1.1.0/bin/julia")))
;;;(setq julia-repl-executable-records
;;;      '((default "julia")                  ; in the executable path
;;;        (master "~/src/julia-git/julia"))) ; compiled from the repository

;;
;; C/C++
;;
(setq-default c-default-style "linux"
	      c-basic-offset 4)

;;
;; Other stuff
;;

;; start server for use of emacs from command line
(server-start)

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

;; TODO need a better shorcut for this
;;(global-set-key (kbd "C-c c b") 'dtb/copy-to-end-of-buffer)

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

(defun dtb/org-timestamp-up-week (&optional num-weeks)
  (interactive "p")
  (if (eq num-weeks nil)
      (setq num-weeks 1))
  (org-timestamp-up-day (* num-weeks 7)))

;;
;; Stuff specific to particular computers
;;

(defun dtb/set-default-font (font-name)
  (set-face-attribute 'fixed-pitch nil :font font-name)
  (set-face-attribute 'default     nil :font font-name)
  )

(cond ((string-match "817thzdev" system-name)
       (dtb/set-default-font "Inconsolata-12"))
      ((string-match "68708DBLAP" system-name)
       (dtb/set-default-font "Hack-10:autohint=true:hintstyle=hintfull:embeddedbitmap=false"))
      ((string-match "687db2-vm5" system-name)
       (dtb/set-default-font "Hack-10:autohint=true:hintstyle=hintfull:embeddedbitmap=false")
       (add-to-list 'default-frame-alist '(left . 0))
       (add-to-list 'default-frame-alist '(top . 0))
       (add-to-list 'default-frame-alist '(height . 65))
       (add-to-list 'default-frame-alist '(width . 140)))
      )

;;
;; TODO
;;
;; read lab-notebook post https://www.sciencemag.org/careers/2019/09/how-keep-lab-notebook
;;
;; on learning: https://superorganizers.substack.com/p/how-to-build-a-learning-machine
;;
;; continue with this series on videos: https://www.youtube.com/watch?v=u00pglDfgX4&list=PLVtKhBrRV_ZkPnBtt_TD1Cs9PJlU0IIdE&index=7
;;
;; maybe some good ideas on python in emacs in this video? https://www.youtube.com/watch?v=6BlTGPsjGJk&index=15&list=PL8tzorAO7s0he-pp7Y_JDl7-Kz2Qlr_Pj
;;
;; read this: https://daedtech.com/how-developers-stop-learning-rise-of-the-expert-beginner/

;; might be useful at some point?
;;
;; ;; WSL-specific setup
;; (when (and (eq system-type 'gnu/linux)
;;            (getenv "WSLENV"))
;;  ; stuff here
;; )

(put 'narrow-to-region 'disabled nil)
