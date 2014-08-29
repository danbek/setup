;; emacs tips - http://www.masteringemacs.org/

;; Note - the only way I could get sbcl in slime to use UTF-8 was to
;; set the LANG environment variable as at very top of script. Not
;; sure why slime-net-coding-system etc did not work.
(setenv "LANG" "en_US.UTF-8")

;; common lisp stuff - for example, labels ...
(require 'cl)

;; keeping this in case I ever need it
;;
;; (defvar home-root
;;   (cond ((or (eq system-type 'gnu/linux)
;;              (eq system-type 'linux))
;;          "/home/dan/")
;;         ((string-match "817becker" system-name)
;;          "/Users/beckerd/")
;;         (t nil))
;;   "The root of my personal emacs load-path.")

(defvar emacs-root "~/.emacs.d/")

(defun package-present-p (pkg-name)
  (or (file-exists-p (expand-file-name (concat emacs-root "site-lisp/" pkg-name)))
      (file-exists-p (expand-file-name (concat emacs-root "site-lisp/" pkg-name ".el")))))

(labels
    ((add-path (p)
	       (add-to-list 'load-path (concat emacs-root p))))
  (add-path "lisp") ;; personal elisp code
  (add-path "site-lisp") ;; external elisp packages & files
  (add-path "site-lisp/slime")
  )

(add-to-list 'custom-theme-load-path "~/.emacs.d/themes/");;

;
;(unless (package-installed-p 'clojure-mode)
;  (package-refresh-contents)
;  (package-install 'clojure-mode))
;(unless (package-installed-p 'clojure-test-mode)
;  (package-refresh-contents)
;  (package-install 'clojure-test-mode))
;(unless (package-installed-p 'nrepl)
;  (package-refresh-contents)
;  (package-install 'nrepl))

;; Need to declare the theme as safe prior to loading it.
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes (quote ("3ee402a796b1bf92ad3175ac5d6f48582aa232aa7854b5edaba54801a28dd08a" default))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

;; Need to load zenburn prior to evil in order for cursor color to be
;; set properly
(load-theme 'zenburn)

;; Emacs 24 has the package manager!
;;
(require 'package)
(add-to-list 'package-archives
             '("melpa" . "http://melpa.milkbox.net/packages/") t)
(package-initialize)

(defvar required-packages
  '(
    evil
    helm
    helm-gtags
    company
    ) "a list of packages to ensure are installed at launch.")

;; method to check if all packages are installed
(defun packages-installed-p ()
  (loop for p in required-packages
        when (not (package-installed-p p)) do (return nil)
        finally (return t)))

;; if not all packages are installed, check one by one and install the missing ones.
(unless (packages-installed-p)
  ; check for new packages (package versions)
  (message "%s" "Emacs is now refreshing its package database...")
  (package-refresh-contents)
  (message "%s" " done.")
  ; install the missing packages
  (dolist (p required-packages)
    (when (not (package-installed-p p))
      (package-install p))))

;; Not sure why, but I have this idea that I should load evil before
;; anything else
(require 'evil)
(evil-mode 1)

;;
;; general customizations
;;

(global-linum-mode 1)

;; ido mode. Has problems on 817thzdev, so don't use it there
(unless (string-match "817thzdev" system-name)
  (ido-mode 1)
  (setq ido-enable-flex-matching t))

;; appearance
(global-font-lock-mode t)
(cond ((string-match "817thzdev" system-name) (set-default-font "Inconsolata-12"))
      ((string-match "twiggy" system-name) (set-default-font "Inconsolata-9"))
      ((string-match "686DB1" system-name) (set-default-font "Consolas-11"))
      ((string-match "dan-homePC" system-name) (set-default-font "Consolas-10"))
      ((string-match "harold-xubuntu-1" system-name) (set-default-font "Droid Sans Mono-10"))
      )
;(setq-default cursor-type 'bar)
;(set-cursor-color "black")
(ansi-color-for-comint-mode-on) ;; allows colors to work in shell mode

;; tabs
(setq-default indent-tabs-mode nil)
(setq-default tab-width 4)

;; backups
(setq backup-directory-alist `(("." . "~/.emacs.d/backups")))
(setq delete-old-versions t
  kept-new-versions 6
  kept-old-versions 2
  version-control t)

;; better scrolling http://etherplex.org/static/emacs.html
(setq scroll-conservatively 10)
(setq scroll-margin 7)
(setq mouse-wheel-scroll-amount '(1 ((shift) . 1))) ;; one line at a time
(setq mouse-wheel-progressive-speed nil) ;; don't accelerate scrolling
(setq mouse-wheel-follow-mouse 't) ;; scroll window under mouse
(setq scroll-step 1) ;; keyboard scroll one line at a time

(if (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))
(if (fboundp 'tool-bar-mode) (tool-bar-mode -1))
;(if (fboundp 'menu-bar-mode) (menu-bar-mode -1))

;; from http://xenon.stanford.edu/~manku/dotemacs.html
(setq inhibit-startup-screen t
      inhibit-startup-echo-area-message t)
(setq require-final-newline t)
(setq display-time-day-and-date t) (display-time) 

;; stupid bell
(setq ring-bell-function 
      (lambda ()
        (unless (memq this-command
                      '(isearch-abort abort-recursive-edit exit-minibuffer keyboard-quit))
          (ding))))

(setq ring-bell-function 'ignore)

;; Some of my favorites
(global-set-key "\C-xg" 'goto-line)

(define-key global-map (kbd "RET") 'newline-and-indent)

;; register e holds emacs init file. Jump to it via C-x r j e
(set-register ?e (cons 'file user-init-file))

(blink-cursor-mode 0)

;; C-arrows to switch between windows
(windmove-default-keybindings)

;;
;; Non-standard modes
;;

;; paredit mode
(when (package-present-p "paredit")
  (autoload 'paredit-mode "paredit"  "Turn on pseudo-structural editing of Lisp code." t)
  (mapc (lambda (mode)
          (let ((hook (intern (concat (symbol-name mode)
                                      "-mode-hook"))))
            (add-hook hook (lambda () (paredit-mode +1)))))
        '(emacs-lisp lisp inferior-lisp slime slime-repl clojure nrepl))
  (show-paren-mode))

;; Smart Tab mode (see emacswiki)
;;(require 'smart-tab)
;;(global-smart-tab-mode 1)

;; helm
;; nice intro: http://tuhdo.github.io/helm-intro.html
(require 'helm-config)
(helm-mode 1)
(global-set-key (kbd "M-x") 'helm-M-x)
(global-set-key (kbd "C-x b") 'helm-mini)
(global-set-key (kbd "C-x C-f") 'helm-find-files)
(define-key helm-map (kbd "<tab>") 'helm-execute-persistent-action) ; rebihnd tab to do persistent action
(define-key helm-map (kbd "C-i") 'helm-execute-persistent-action) ; make TAB works in terminal
(define-key helm-map (kbd "C-z")  'helm-select-action) ; list actions using C-z

;; helm-gtags, intro: http://tuhdo.github.io/c-ide.html
(require 'helm-gtags)

(setq
 helm-gtags-ignore-case t
 helm-gtags-auto-update t
 helm-gtags-use-input-at-cursor t
 helm-gtags-pulse-at-cursor t

 helm-gtags-suggested-key-mapping t
 )

;; Enable helm-gtags-mode in Dired so you can jump to any tag
;; when navigate project tree with Dired
(add-hook 'dired-mode-hook 'helm-gtags-mode)

;; Enable helm-gtags-mode in Eshell for the same reason as above
(add-hook 'eshell-mode-hook 'helm-gtags-mode)

;; Enable helm-gtags-mode in languages that GNU Global supports
(add-hook 'c-mode-hook 'helm-gtags-mode)
(add-hook 'c++-mode-hook 'helm-gtags-mode)
(add-hook 'java-mode-hook 'helm-gtags-mode)
(add-hook 'asm-mode-hook 'helm-gtags-mode)

;; key bindings
(define-key helm-gtags-mode-map (kbd "M-s") 'helm-gtags-select)
(define-key helm-gtags-mode-map (kbd "M-.") 'helm-gtags-dwim)
(define-key evil-normal-state-map (kbd "M-.") 'helm-gtags-dwim)
(define-key helm-gtags-mode-map (kbd "M-,") 'helm-gtags-pop-stack)
(define-key helm-gtags-mode-map (kbd "C-c <") 'helm-gtags-previous-history)
(define-key helm-gtags-mode-map (kbd "C-c >") 'helm-gtags-next-history)

;; company mode (completion).
(require 'company)
(add-hook 'after-init-hook 'global-company-mode)

;; better buffer switching
;(iswitchb-mode)

;; nice comment autofill
(require 'newcomment)
(setq comment-auto-fill-only-comments 1)
(setq-default auto-fill-function 'do-auto-fill)

;;
;; SLIME setup
;;
;; To checkout the latest slime do the following:
;; $ cvs -d :pserver:anonymous:anonymous@common-lisp.net:/project/slime/cvsroot co slime
;;
;; To build a new core file do the following:
;;
;; $ sbcl
;; * (mapc 'require '(sb-bsd-sockets sb-posix sb-introspect sb-cltl2 asdf))
;; * (save-lisp-and-die "sbcl.core-for-slime")
(when (package-present-p "slime")
  (setq slime-lisp-implementations
        `((sbcl ,(let ((sbcl-core-filename
                        (expand-file-name (concat emacs-root "site-lisp/slime/sbcl.core-for-slime"))))
                   (if (file-exists-p sbcl-core-filename) 
                       (list "/usr/local/bin/sbcl"
                             "--core"
                             sbcl-core-filename)
                     (list "/usr/local/bin/sbcl"))))))
  
  (expand-file-name "~/.emacs.d")
  
  (require 'slime)
  (slime-setup '(slime-fancy))
  (global-set-key "\C-cs" 'slime-selector)
  
  ;; hyperspec lookup
  (require 'clhs)
  (setq common-lisp-hyperspec-root "file:///usr/share/doc/hyperspec/")
  )

;; url browsing
(setq browse-url-browser-function 'browse-url-firefox
      browse-url-new-window-flag  t
      browse-url-firefox-new-window-is-tab t)

;; TRAMP defaults
(setq tramp-default-method "ssh")

;; ispell mode
(setq ispell-program-name "aspell")
(setq ispell-list-command "list")

;; This is the crazy way to get ispell to ignore arguments to custom TeX
;; macros [2]. Unfortunately it is apparently a much bigger pain to
;; get flyspell to do the same.
; [2]: http://tex.stackexchange.com/a/150882/35130
(setq ispell-tex-skip-alists
      (list
       (append
        (car ispell-tex-skip-alists) 
        '(("\\\\figref"       ispell-tex-arg-end)
          ("\\\\tableref"    ispell-tex-arg-end)
          ("\\\\sectionref"    ispell-tex-arg-end)
          ("\\\\chapterref"    ispell-tex-arg-end)
          ("\\\\eqnref"    ispell-tex-arg-end)))
       (cadr ispell-tex-skip-alists)))

; This tries to get ispell to ignore $ ... $ inline math environments
; Is possibly quite fragile
(setcar ispell-tex-skip-alists
        (append
         (car ispell-tex-skip-alists)
         '(("[^\\]\\$" . "[^\\]\\$"))))

;; AUCTeX
(load "auctex.el" t nil t)
;(load "perview-latex.el" nil t t) ; not ready for this yet

(setq TeX-PDF-mode t)
(setq TeX-auto-save t)
(setq TeX-parse-self t)
(setq-default TeX-master nil)
(setq LaTeX-command-style '(("" "%(PDF)%(latex) -file-line-error %S%(PDFout)"))) ; see [1]

(add-hook 'TeX-mode-hook #'(lambda () (modify-syntax-entry ?- "w")))

; [1]: http://tex.stackexchange.com/questions/124246/uninformative-error-message-when-using-auctex

;;
;; Clojure support
;;
;(require 'clojure-mode)
;(require 'clojure-test-mode)
;(require 'nrepl)

;; following is from
;; http://stackoverflow.com/questions/13002685/kill-previous-nrepl-sessions-when-nrepl-jack-in-called

;; Disable prompt on killing buffer with a process
(setq kill-buffer-query-functions
      (remq 'process-kill-buffer-query-function
            kill-buffer-query-functions))

(defun nrepl-kill ()
  "Kill all nrepl buffers and processes"
  (interactive)
  (when (get-process "nrepl-server")
    (set-process-sentinel (get-process "nrepl-server")
                          (lambda (proc evt) t)))
  (dolist (buffer (buffer-list))
    (when (string-prefix-p "*nrepl" (buffer-name buffer))
      (kill-buffer buffer))))

(defun nrepl-me ()
  (interactive)
  (nrepl-kill)
  (nrepl-jack-in nil))

;; enable emacsclient
(server-start)

;; windows at startup
;(add-to-list 'default-frame-alist '(left . 0))
;(add-to-list 'default-frame-alist '(top . 0))
;(add-to-list 'default-frame-alist '(height . 65))
;(add-to-list 'default-frame-alist '(width . 270))
;(split-window-horizontally 80) ;;; this errors for some reason ...

;; run a shell at start
;(shell)

; put custom stuff in its own file
(setq custom-file "~/.emacs.d/custom.el")
(load custom-file t nil t)
