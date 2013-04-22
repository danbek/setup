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

(defun package-installed-p (pkg-name)
  (or (file-exists-p (expand-file-name (concat emacs-root "site-lisp/" pkg-name)))
      (file-exists-p (expand-file-name (concat emacs-root "site-lisp/" pkg-name ".el")))))

(labels
    ((add-path (p)
	       (add-to-list 'load-path (concat emacs-root p))))
  (add-path "lisp") ;; personal elisp code
  (add-path "site-lisp") ;; external elisp packages & files
  (add-path "site-lisp/slime")
  (add-path "site-lisp/matlab-emacs")
  )


;;
;; general customizations
;;

;; appearance
(global-font-lock-mode t)
(set-default-font
 (cond ((string-match "817becker" system-name) "Inconsolata-14")
       ((string-match "twiggy" system-name) "Inconsolata-9")
       ((string-match "686DB1" system-name) "Consolas-12")
       (t "Inconsolata-12")))
(setq-default cursor-type 'bar)
(set-cursor-color "black")
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
(setq inhibit-startup-screen t)
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

;; C-arrows to switch between windows
(windmove-default-keybindings)

;;
;; Non-standard modes
;;

;; paredit mode
(autoload 'paredit-mode "paredit"  "Turn on pseudo-structural editing of Lisp code." t)
(mapc (lambda (mode)   
        (let ((hook (intern (concat (symbol-name mode)   
                                    "-mode-hook"))))   
          (add-hook hook (lambda () (paredit-mode +1)))))   
      '(emacs-lisp lisp inferior-lisp slime slime-repl clojure))
(show-paren-mode)

;; Smart Tab mode (see emacswiki)
;;(require 'smart-tab)
;;(global-smart-tab-mode 1)

;; better buffer switching
(iswitchb-mode)

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
(when (package-installed-p "slime")
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

;; matlab-mode
(defun my-matlab-mode-hook ()
  (auto-fill-mode 0)
  (toggle-truncate-lines 0))

(when (package-installed-p "matlab-emacs")
  (load-library "matlab-load")
  (matlab-cedet-setup)
  (setq matlab-shell-emacsclient-command "/Applications/Emacs.app/Contents/MacOS/bin/emacsclient")
  (add-hook 'matlab-mode-hook 'my-matlab-mode-hook))

;; CEDET
(when (package-installed-p "cedet-1.0.1")
  (load-file "~/.emacs.d/site-lisp/cedet-1.0.1/common/cedet.el")
  (global-ede-mode 1)         ; Enable the Project management system
  (semantic-load-enable-code-helpers) ; Enable prototype help and smart completion 
  (global-srecode-minor-mode 1)) ; Enable template insertion menu

;;
;; Clojure support
;;
(require 'clojure-mode)
(require 'clojure-test-mode)
(require 'nrepl)

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
