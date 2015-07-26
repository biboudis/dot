;; Aggelos Biboudis 
;; February 2012

;;=================== Get system path ====================

(defun set-exec-path-from-shell-PATH ()
  (let ((path-from-shell (replace-regexp-in-string
                          "[ \t\n]*$"
                          ""
                          (shell-command-to-string "$SHELL --login -i -c 'echo $PATH'"))))
    (setenv "PATH" path-from-shell)
    (setq eshell-path-env path-from-shell) ; for eshell users
    (setq exec-path (split-string path-from-shell path-separator))))

(set-exec-path-from-shell-PATH)

;;=================== Load paths ==========================
(add-to-list 'load-path "~/.emacs.d/lisp")

;;=================== Packages ============================

(require 'package)
(require 'cl)
(setq package-archives '(("elpa" . "http://elpa.gnu.org/packages/")
			 ("marmalade" . "http://marmalade-repo.org/packages/")
                         ("melpa" . "http://melpa.milkbox.net/packages/")))

(package-initialize)

(defvar prelude-packages '(ack-and-a-half auctex auctex-latexmk
					  markdown-mode slime ghc clojure-mode caml csharp-mode
					  fsharp-mode magit org sml-mode scala-mode2 tuareg haskell-mode
					  zenburn-theme)
  "A list of packages to ensure are installed at launch.")

(defun prelude-packages-installed-p ()
  "Check if all packages in `prelude-packages' are installed."
  (every #'package-installed-p prelude-packages))

(defun prelude-require-package (package)
  "Install PACKAGE unless already installed."
  (unless (memq package prelude-packages)
    (add-to-list 'prelude-packages package))
  (unless (package-installed-p package)
    (package-install package)))

(defun prelude-require-packages (packages)
  "Ensure PACKAGES are installed.
Missing packages are installed automatically."
  (mapc #'prelude-require-package packages))

(define-obsolete-function-alias 'prelude-ensure-module-deps 'prelude-require-packages)

(defun prelude-install-packages ()
  "Install all packages listed in `prelude-packages'."
  (unless (prelude-packages-installed-p)
    ;; check for new packages (package versions)
    (message "%s" "Emacs Prelude is now refreshing its package database...")
    (package-refresh-contents)
    (message "%s" " done.")
    ;; install the missing packages
    (prelude-require-packages prelude-packages)))

;; run package installation
(prelude-install-packages)

;;=================== Settings ============================
(setq magit-last-seen-setup-instructions "1.4.0")
(setq compilation-scroll-output 'first-error)
(setq auto-mode-alist (cons '("\\.tex" . latex-mode) auto-mode-alist))
(setq auto-mode-alist (cons '("\\.jrag" . java-mode) auto-mode-alist))
(setq comint-prompt-read-only t)
(setq backup-directory-alist
      `((".*" . ,temporary-file-directory)))
(setq auto-save-file-name-transforms
      `((".*" ,temporary-file-directory t)))
(setq tool-bar-mode 0)
(line-number-mode 1)
(column-number-mode 1)
(setq-default fill-column 80)
(setq-default truncate-lines t)

;; Add color to a shell running in emacs 'M-x shell'
(autoload 'ansi-color-for-comint-mode-on "ansi-color" nil t)
(add-hook 'shell-mode-hook 'ansi-color-for-comint-mode-on)

;; Theme
(load-theme 'zenburn t)

(require 'win-switch)
(win-switch-setup-keys-ijkl "\C-xo")
(setq win-switch-idle-time 1.0)
(setq win-switch-other-window-first nil)

;; Tramp
(setq tramp-default-method "rsync")
(setq password-cache-expiry 3600)

;; Agda-mode setup
(load-file (let ((coding-system-for-read 'utf-8))
	     (shell-command-to-string "agda-mode locate")))

					; Make Emacs look in Cabal directory for binaries
(let ((my-cabal-path (expand-file-name "~/.cabal/bin")))
  (setenv "PATH" (concat my-cabal-path ":" (getenv "PATH")))
  (add-to-list 'exec-path my-cabal-path))

;; haskel-mode setup
;;(add-hook 'haskell-mode-hook 'structured-haskell-mode)
;;(add-hook 'haskell-mode-hook 'turn-on-haskell-indentation)
(add-hook 'haskell-mode-hook 'turn-on-haskell-indent)
;;(add-hook 'haskell-mode-hook 'turn-on-haskell-simple-indent)

(require 'project-root)

(custom-set-variables
 '(haskell-process-suggest-remove-import-lines t)
 '(haskell-process-auto-import-loaded-modules t)
 '(haskell-process-log t))
(eval-after-load 'haskell-mode '(progn
				  (define-key haskell-mode-map (kbd "C-c C-l") 'haskell-process-load-or-reload)
				  (define-key haskell-mode-map (kbd "C-c C-z") 'haskell-interactive-switch)
				  (define-key haskell-mode-map (kbd "C-c C-n C-t") 'haskell-process-do-type)
				  (define-key haskell-mode-map (kbd "C-c C-n C-i") 'haskell-process-do-info)
				  (define-key haskell-mode-map (kbd "C-c C-n C-c") 'haskell-process-cabal-build)
				  (define-key haskell-mode-map (kbd "C-c C-n c") 'haskell-process-cabal)
				  (define-key haskell-mode-map (kbd "SPC") 'haskell-mode-contextual-space)))
(eval-after-load 'haskell-cabal '(progn
				   (define-key haskell-cabal-mode-map (kbd "C-c C-z") 'haskell-interactive-switch)
				   (define-key haskell-cabal-mode-map (kbd "C-c C-k") 'haskell-interactive-mode-clear)
				   (define-key haskell-cabal-mode-map (kbd "C-c C-c") 'haskell-process-cabal-build)
				   (define-key haskell-cabal-mode-map (kbd "C-c c") 'haskell-process-cabal)))

;;; Lisp (SLIME) interaction
(setq inferior-lisp-program "clisp")
(add-to-list 'load-path "~/.slime")
(require 'slime)
(slime-setup) 

;; SML
(autoload 'sml-mode "sml-mode" "Major mode for editing SML." t)
(autoload 'run-sml "sml-proc" "Run an inferior SML process." t)
(add-to-list 'auto-mode-alist '("\\.\\(sml\\|sig\\)\\'" . sml-mode))

(custom-set-variables
 '(compilation-always-kill t)
 '(haskell-mode-hook (quote (turn-on-haskell-indentation)) t)
 '(sml-electric-pipe-mode nil)
 '(sml-indent-args 3)
 '(sml-indent-level 3)
 '(sml-rightalign-and nil)
 '(markdown-command "/usr/bin/pandoc"))
