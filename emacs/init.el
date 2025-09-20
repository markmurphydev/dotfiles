;; ==== Early setup ====

;; == Elpaca ==
;; Elpaca package manager
(load (expand-file-name "elpaca.el" user-emacs-directory))

;; Elpaca settings
(elpaca elpaca-use-package
  ;; Enable use-package :ensure support for Elpaca.
  (elpaca-use-package-mode))

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

;; ==== Keybindings ====
;; Uses `general.el`
;; https://github.com/noctuid/general.el
;; NB: `(:wait t)` means that use-package declarations 
;;  _after_ this command can use `:general` keyword
;; NB: Can't have `:general` definitions _in_ the `general` load
(use-package general :ensure (:wait t) :demand)

;; ==== Packages ====

; ;; == evil ==
(use-package evil :ensure (:tag "1.14.2") :demand t
  ;; I don't think :bind works correctly with evil.
  ;; I'll just do things evil's way.
  :init
  (general-auto-unbind-keys)
  :config
  (evil-mode)
  :custom
  (evil-undo-system 'undo-redo)
    :general
    (general-def
    ;; Command-backspace
    "s-<backspace>" #'mark--kill-to-line-start
    ;; minibuffer quit
    "<escape>" #'keyboard-escape-quit)
  ;; Minibuffer keybindings
  (general-def minibuffer-mode-map
    "M-<backspace>" #'backward-kill-word
    "s-<backspace>" #'mark--kill-to-line-start)
  (general-def 'motion
    "U" #'evil-redo
    "C-u" #'evil-scroll-up
    "s-/" #'comment-line)
  (general-def 'motion
    :prefix "SPC"
    "w d" #'evil-quit
    "w h" #'evil-window-left
    "w j" #'evil-window-down
    "w k" #'evil-window-up
    "w l" #'evil-window-right
    "w v" #'evil-window-vsplit
    "r r" #'mark-reload
    "o" #'find-file
    "f f" #'find-file
    "h f" #'describe-function
    "h k" #'describe-key
    "h v" #'describe-variable
    ";" #'eval-expression
    "x" #'execute-extended-command
    "b b" #'consult-buffer
    "b p" #'previous-buffer
    "b d" #'kill-current-buffer
    "b n" #'next-buffer))

;; == evil-collection ==
;; Vim keybindings outside of text buffers
; (use-package evil-collection :ensure (:ref "1cf0f9654bbb53a1093b545a64df299f4aca3f9d"))
(use-package orderless
  :ensure t
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


;; == consult ==
;; buffer-picker etc.
(use-package consult :ensure (:tag "2.8"))


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


;; == embark ==
;; list options at point (CMD-.)
(use-package embark :ensure (:tag "1.1")
  :general
  (general-def "s-." #'embark-act))

(use-package embark-consult
  :ensure t
  :hook
  (embark-collect-mode . consult-preview-at-point-mode))


;; == corfu ==
;; Intellisense
(use-package corfu :ensure (:tag "2.3")
  :custom
  (corfu-auto t)
  (corfu-quit-no-match 'separator)
  :init
  (global-corfu-mode))


;; == eldoc-box ==
;; hover-box info
(use-package eldoc-box :ensure (:tag "v1.14.1")
  :bind
  (("s-l" . eldoc-box-help-at-point)))


;; ==== Major Modes ====

;; == markdown-mode ==
;; https://jblevins.org/projects/markdown-mode/
(use-package markdown-mode
  :ensure t
  :mode ("*\\.md\\'" . gfm-mode))


;; ==== Emacs settings ====

;; Tabs
(setopt indent-tabs-mode t)

;; == GUI settings ==
;; Start focused
;; https://apple.stackexchange.com/questions/467216/emacs-starts-up-without-window-focus-in-macos-sonoma
(select-frame-set-input-focus (selected-frame))
;; Theme
(use-package monokai-theme :ensure t :demand t
  :config
  (load-theme 'monokai t))

;; == Vertico support ==
;; Support opening new minibuffers from inside existing minibuffers.
(setopt enable-recursive-minibuffers t)
;; Hide commands in M-x which do not work in the current mode.
(setopt read-extended-command-predicate #'command-completion-default-include-p)
;; Do not allow the cursor in the minibuffer prompt
(setopt minibuffer-prompt-properties '(read-only t cursor-intangible t face minibuffer-prompt))

;; ==== Commands ====

(defun mark--kill-to-line-start ()
  "kill to start of line."
  (interactive)
  (kill-line 0))

(defun mark-reload ()
  "Reload init.el"
  (interactive)
  (load-file user-init-file))
