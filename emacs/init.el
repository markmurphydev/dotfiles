;; ==== Early setup ====

;; == Elpaca ==
;; Elpaca package manager
(load (expand-file-name "elpaca.el" user-emacs-directory))

;; Elpaca settings
(elpaca elpaca-use-package
  ;; Enable use-package :ensure support for Elpaca.
  (elpaca-use-package-mode))
(setopt use-package-always-ensure t)
(setopt use-package-hook-name-suffix nil)

;; Make Emacs store autosave files in /tmp directory
;; https://www.reddit.com/r/emacs/comments/ym3t77/how_to_delete_auto_save_files_when_quitting_emacs/
(setq backup-directory-alist
      `((".*" . ,temporary-file-directory)))
(setq auto-save-file-name-transforms
      `((".*" ,temporary-file-directory t)))

;; Make Emacs `customize` use a separate file, instead of barfing things here
;; https://stackoverflow.com/questions/5052088/what-is-custom-set-variables-and-faces-in-my-emacs
(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
(load custom-file)


;; ==== Packages ====

;; == general ==
;; Keybindings
;; https://github.com/noctuid/general.el
;; NB: `(:wait t)` means that use-package declarations 
;;  _after_ this command can use `:general` keyword
;; NB: Can't have `:general` definitions _in_ the `general` load
(use-package general :ensure (:wait t) :demand)

;; == evil ==
(use-package evil :ensure (:tag "1.14.2") :demand t
  ;; I don't think :bind works correctly with evil.
  ;; I'll just do things evil's way.
  :init
  ;; Avoid errors about prefix keys
  (general-auto-unbind-keys)
  ;; Unbind keys from troubled modes
  (setf (cdr help-mode-map) nil)
  ;; Required for evil-collection
  ;; (setopt evil-want-keybinding nil)
  :config
  (evil-mode)
  :custom
  (evil-undo-system 'undo-redo)
  (evil-move-beyond-eol t)
  :general
  ;; Unbinds
  (general-unbind 'visual "s")
  (general-define-key
   ;; Command-backspace
   "s-<backspace>" #'mark-kill-to-line-start)
  (general-define-key
   :keymaps 'minibuffer-mode-map
   "M-<backspace>" #'backward-kill-word
   "s-<backspace>" #'mark-kill-to-line-start
   ;; minibuffer quit
   "<escape>" #'keyboard-escape-quit)
  ;; Global keybindings
  (general-define-key
    :states 'motion
    "s-p" #'evil-paste-pop
    "s-/" #'mark-comment-line)
  (general-define-key
    :states 'motion
    "U" #'evil-redo
    "C-u" #'evil-scroll-up)
  (general-def 'motion
    :prefix "SPC"
    "w s" #'evil-window-split
    "w d" #'evil-quit
    "w h" #'evil-window-left
    "w j" #'evil-window-down
    "w k" #'evil-window-up
    "w l" #'evil-window-right
    "w L" #'evil-window-bottom-right
    "w v" #'evil-window-vsplit
    "r i" #'mark-reload-init
    "o" #'find-file
    "p d" #'project-find-dir
    "p b" #'project-switch-to-buffer
    "p f" #'project-find-file
    "p p" #'project-switch-project
    "p /" #'consult-ripgrep
    "f f" #'find-file
    "f i" #'mark-edit-init-file
    "h f" #'describe-function
    "h k" #'describe-key
    "h v" #'describe-variable
    "h b" #'describe-bindings
    "h m" #'describe-mode
    ";" #'eval-expression
    "x" #'execute-extended-command
    "b b" #'consult-buffer
    "b p" #'previous-buffer
    "b d" #'kill-current-buffer
    "b n" #'next-buffer))

;; == evil-surround ==
;; https://github.com/emacs-evil/evil-surround
(use-package evil-surround
  :init
  (global-evil-surround-mode 1)
  :hook
  (lisp-mode-hook . slime-mode)
  :general
  (general-define-key
   :states 'visual
   :keymaps 'evil-surround-mode-map
   "s" #'evil-surround-region))

;; == evil-collection ==
;; https://github.com/emacs-evil/evil-collection
;; (use-package evil-collection
;;   :after evil
;;   :ensure t
;;   :custom
;;   (evil-collection-key-blacklist '("SPC"))
;;   :config
;;   (evil-collection-init '(slime)))

;; == dired overrides ==
;; If you need to override evil keys for a major mode, use this method:
(define-minor-mode dired-keys-mode
  "Add override keybindings for dired mode.")

(use-package dired
  :ensure nil
  :config
  (setf (cdr dired-mode-map) nil)
  :general
  (general-define-key
   :definer 'minor-mode
   :states 'normal
   :keymaps 'dired-keys-mode
   "<return>" #'dired-find-file
   "m" #'dired-mark
   "d" #'dired-flag-file-deletion
   "M" #'dired-unmark)
  (general-define-key
   :definer 'minor-mode
   :states 'normal
   :keymaps 'dired-keys-mode
   :prefix "SPC"
   "c d" #'dired-create-directory
   "m d" #'dired-do-flagged-delete
   "m r" #'dired-do-rename)
  :hook
  (dired-mode-hook . dired-keys-mode))

;; == treesit-auto ==
;; https://github.com/renzmann/treesit-auto
(use-package treesit-auto :ensure (:tag "v1.0.5") :demand t
  :custom
  (treesit-auto-install 'prompt)
  :config
  (global-treesit-auto-mode)
  (treesit-auto-add-to-auto-mode-alist 'all))

;; == vertico ==
;; completions
(use-package vertico
  :custom
  (vertico-resize t) ; Grow and shrink the Vertico minibuffer
  (setopt enable-recursive-minibuffers t) ; Support opening new minibuffers from inside existing minibuffers.
  (setopt read-extended-command-predicate #'command-completion-default-include-p) ; Hide commands in M-x which do not work in the current mode.
  (setopt minibuffer-prompt-properties '(read-only t cursor-intangible t face minibuffer-prompt)) ; Do not allow the cursor in the minibuffer prompt
  :init
  (vertico-mode))


;; == orderless ==
(use-package orderless
  :custom
  (completion-styles '(orderless basic))
  (completion-category-overrides '((file (styles partial-completion))))
  (completion-pcm-leading-wildcard t)) ;; Emacs 31: partial-completion behaves like substring


;; == marginalia ==
;; Annotations for completions
;; https://github.com/minad/marginalia
(use-package marginalia :ensure (:tag "2.3") :demand t
  :init
  (marginalia-mode))


;; == ultra-scroll ==
;; Smooth scroll
;; https://github.com/jdtsmith/ultra-scroll
(use-package ultra-scroll
  :ensure (:tag "v0.4.2") :demand t
  :custom
  (scroll-conservatively 3) ;; idk
  (scroll-margin 0)
  :config
  (ultra-scroll-mode t))


;; == consult ==
;; buffer-picker etc.
(use-package consult :ensure (:tag "2.8"))


;; == embark ==
;; list options at point (CMD-.)
(use-package embark :ensure (:tag "1.1")
  :general
  (general-def "s-." #'embark-act))

(use-package embark-consult
  :hook
  (embark-collect-mode . consult-preview-at-point-mode))


;; == corfu ==
;; Intellisense
(use-package corfu :ensure (:tag "2.3")
  :custom
  (corfu-auto t)
  (corfu-quit-no-match 'separator)
  :init
  (global-corfu-mode)
  :general
  (general-define-key
   :keymaps 'corfu-map
   "RET" nil))


;; == eldoc-box ==
;; hover-box info
(use-package eldoc-box :ensure (:tag "v1.14.1")
  :bind
  (("s-l" . eldoc-box-help-at-point)))

;; == which-key ==
(which-key-mode 1)
(setopt which-key-idle-delay 0.05)

;; == mode-minder ==
;; List major, minor modes
;; https://github.com/jdtsmith/mode-minder
(use-package mode-minder
  :ensure (:host github :repo "jdtsmith/mode-minder")
  :demand t)


;; == transient ==
;; Needed to install separately for magit
(use-package transient :ensure (:tag "v0.10.0"))


;; == magit ==
(use-package magit :ensure (:tag "v4.4.0"))


;; ==== Major Modes ====

;; == Cider ==
(use-package cider :demand t)

;; == markdown-mode ==
;; https://jblevins.org/projects/markdown-mode/
(use-package markdown-mode
  :mode ("*\\.md\\'" . gfm-mode))

;; == wat-mode ==
(use-package wat-ts-mode)

;; == slime-mode ==
;; Common lisp
;; https://github.com/slime/slime

(use-package slime
  :init
  (setopt inferior-lisp-program "/opt/homebrew/bin/sbcl")
  :general
  ;; Escape in slime debug
  (general-define-key
   :keymaps 'sldb-mode-map
   "<escape>" #'sldb-quit)
  (general-define-key
   :states 'global
   :keymaps 'slime-repl-mode-map
   "C-k" #'slime-repl-previous-input
   "C-j" #'slime-repl-next-input
   (general-define-key
    :states 'normal
    :keymaps 'slime-repl-mode-map
    "SPC k" #'consult-history
    "SPC c" #'slime-repl-clear-buffer)
   (general-define-key
    :states 'normal
    :keymaps 'slime-mode-map
    "g d" #'slime-edit-definition))
  (general-define-key
   :states 'visual
   :keymaps 'slime-mode-map
   :prefix "SPC"
   "c c" #'slime-compile-region)
  (general-define-key
   :states 'normal
   :keymaps 'slime-mode-map
   :prefix "SPC"
   "e e" #'slime-eval-defun
   "e m" #'slime-macroexpand-1-inplace
   "c c" #'slime-compile-defun
   "c f" #'slime-compile-file
   "m h" #'slime-hyperspec-lookup))


;; == sly-mode ==
;; Common lisp
;; https://github.com/joaotavora/sly
;; (use-package sly
;;   :custom
;;   (inferior-lisp-program "/opt/homebrew/bin/sbcl")
;;   :general
;;   (general-define-key
;;    :states 'normal
;;    :keymaps 'sly-mode-map
;;    "g d" #'sly-edit-definition)
;;   (general-define-key
;;    :states 'normal
;;    :keymaps 'sly-mode-map
;;    :prefix "SPC"
;;    "e e" #'mark-sly-eval-last-expression
;;    "e r" #'sly-eval-region
;;    "e f" #'sly-eval-defun
;;    "e b" #'sly-eval-buffer
;;    "e m" #'mark-sly-macroexpand-1
;;    "m h" #'sly-hyperspec-lookup)
;;   (general-define-key
;;    :states 'normal
;;    :keymaps 'sly-mrepl-mode-map
;;    "<return>" #'sly-mrepl-return
;;    "SPC k" #'consult-history
;;    "SPC c" #'sly-mrepl-clear-repl))

;; == geiser ==
;; GNU Guile
;; https://www.nongnu.org/geiser/
;; (use-package geiser
;;   :general
;;   (general-define-key
;;    :states 'normal
;;    :keymaps 'geiser-mode-map
;;    :prefix "SPC"
;;    "e e" #'mark-geiser-eval-last-sexp
;;    "e r" #'geiser-eval-region
;;    "e f" #'geiser-eval-definition
;;    "e b" #'geiser-eval-buffer
;;    "e m" #'mark-geiser-expand-last-sexp))

;; == paredit ==
;; (use-package paredit
;;   :hook
;;   ((lisp-mode-hook emacs-lisp-mode-hook). paredit-mode))

;; == evil-cleverparens ==
;; https://github.com/emacs-evil/evil-cleverparens?tab=readme-ov-file
;; (use-package evil-cleverparens
;;   :init
;;   (setopt evil-cleverparens-use-s-and-S nil)
;;   :hook
;;   (paredit-mode-hook . #'evil-cleverparens-mode))

;; == tuareg ==
;; Ocaml
;; https://github.com/ocaml/tuareg
;; (use-package tuareg)


;; ==== Emacs settings ====
;; Auto refresh buffers
(global-auto-revert-mode t)
;; Also auto refresh dired, but be quiet about it
(setq global-auto-revert-non-file-buffers t)
(setq auto-revert-verbose nil)

;; Tabs
(setopt indent-tabs-mode t)

;; == GUI settings ==
;; Start focused
;; https://apple.stackexchange.com/questions/467216/emacs-starts-up-without-window-focus-in-macos-sonoma
(select-frame-set-input-focus (selected-frame))
;; Theme
(use-package monokai-theme :demand t
  :config
  (load-theme 'monokai t))

;; == Follow symlinks ==
;; https://stackoverflow.com/questions/15390178/emacs-and-symbolic-links
(setopt vc-follow-symlinks t)

;; == Scratch buffer ==
;; Show scratch buffer on startup
(setopt inhibit-startup-screen t)
(setopt initial-scratch-message "")

;; Make ESC stop closing windows?
;; https://old.reddit.com/r/emacs/comments/10l40yi/how_do_i_make_esc_stop_closing_all_my_windows/j5usr8i/
(defun +keyboard-escape-quit-adv (fun)
  "Around advice for `keyboard-escape-quit' FUN.
Preserve window configuration when pressing ESC."
  (let ((buffer-quit-function (or buffer-quit-function #'ignore)))
    (funcall fun)))
(advice-add #'keyboard-escape-quit :around #'+keyboard-escape-quit-adv)



;; ==== Commands ====

(defun mark-kill-to-line-start ()
  "Kill to start of line."
  (interactive)
  (kill-line 0))

(defun mark-comment-line ()
  "Comment line or active region, without moving the point."
  (interactive)
  (let* ((start-point (point)))
    (if (use-region-p)
	(progn (comment-dwim nil)
	       ;; We're still in visual-state.
	       ;; To restore the previous visual-state, we need to return to normal-state first.
	       (evil-normal-state)
	       (evil-visual-restore))
      (progn (comment-line 1)
	     (goto-char start-point)))))

(defun mark-reload-init ()
  "Reload init.el"
  (interactive)
  (load-file user-init-file))

(defun mark--eval-last (body)
  (interactive)
  (save-excursion
    (evil-save-state
      (evil-insert 1)
      (right-char)
      (call-interactively body))))

(defun mark-sly-eval-last-expression ()
  (interactive)
  (mark--eval-last #'sly-eval-last-expression))

(defun mark-sly-eval-last-expression ()
  (interactive)
  (mark--eval-last #'sly-eval-last-expression))

(defun mark-sly-macroexpand-1 ()
  (interactive)
  (mark--eval-last #'sly-macroexpand-1))

(defun mark-geiser-eval-last-sexp ()
  (interactive)
  (mark--eval-last #'geiser-eval-last-sexp))

(defun mark-geiser-expand-last-sexp ()
  (interactive)
  (mark--eval-last #'geiser-expand-last-sexp))

(defun mark-edit-init-file ()
  (interactive)
  (find-file "~/.config/emacs/init.el"))
