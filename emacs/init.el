;; ==== Config folder setup ====

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

;; Elpaca package manager
(load (expand-file-name "elpaca.el" user-emacs-directory))

;; ==== Emacs settings ====

;; == GUI settings ==
;; Start focused
;; https://apple.stackexchange.com/questions/467216/emacs-starts-up-without-window-focus-in-macos-sonoma
(select-frame-set-input-focus (selected-frame))
;; Theme
(use-package monokai-theme :ensure t :demand t
    :config
    (load-theme 'monokai t))

;; == Keybindings ==
;; Command-backspace
(defun mark--kill-to-line-start ()
  "kill to start of line."
  (interactive)
  (kill-line 0))
(define-key global-map (kbd "s-<backspace>") #'mark--kill-to-line-start)
;; minibuffer quit
(define-key global-map (kbd "<escape>") #'keyboard-escape-quit)
;; Minibuffer keybindings
(define-key minibuffer-mode-map (kbd "M-<backspace>") #'backward-kill-word)
(define-key minibuffer-mode-map (kbd "s-<backspace>") #'mark--kill-to-line-start)


;; == Vertico support ==
;; Support opening new minibuffers from inside existing minibuffers.
(setopt enable-recursive-minibuffers t)
;; Hide commands in M-x which do not work in the current mode.
(setopt read-extended-command-predicate #'command-completion-default-include-p)
;; Do not allow the cursor in the minibuffer prompt
(setopt minibuffer-prompt-properties '(read-only t cursor-intangible t face minibuffer-prompt))


;; ==== Packages ====

;; == evil ==
(use-package evil :ensure (:tag "1.14.2") :demand t
    ;; I don't think :bind works correctly with evil.
    ;; I'll just do things evil's way.
    :custom
    (evil-undo-system 'undo-redo)
    :config
    (evil-mode)
    (evil-define-key 'normal 'global (kbd "U") #'evil-redo)
    (evil-define-key 'motion 'global (kbd "C-u") #'evil-scroll-up)
    (evil-define-key 'motion 'global (kbd "s-/") #'comment-line)
    (evil-define-key 'motion 'global (kbd "SPC") nil)
    (evil-define-key 'motion 'global (kbd "SPC w d") #'evil-quit)
    (evil-define-key 'motion 'global (kbd "SPC w h") #'evil-window-left)
    (evil-define-key 'motion 'global (kbd "SPC w j") #'evil-window-down)
    (evil-define-key 'motion 'global (kbd "SPC w k") #'evil-window-up)
    (evil-define-key 'motion 'global (kbd "SPC w l") #'evil-window-right)
    (evil-define-key 'motion 'global (kbd "SPC w v") #'evil-window-vsplit)
    (evil-define-key 'motion 'global (kbd "SPC r r") #'reload)
    (evil-define-key 'motion 'global (kbd "SPC o") #'find-file)
    (evil-define-key 'motion 'global (kbd "SPC f f") #'find-file)
    (evil-define-key 'motion 'global (kbd "SPC h f") #'describe-function)
    (evil-define-key 'motion 'global (kbd "SPC h k") #'describe-key)
    (evil-define-key 'motion 'global (kbd "SPC h v") #'describe-variable)
    (evil-define-key 'motion 'global (kbd "SPC ;") #'eval-expression)
    (evil-define-key 'motion 'global (kbd "SPC x") #'execute-extended-command)
    (evil-define-key 'motion 'global (kbd "SPC b b") #'consult-buffer)
    (evil-define-key 'motion 'global (kbd "SPC b p") #'previous-buffer)
    (evil-define-key 'motion 'global (kbd "SPC b d") #'kill-current-buffer)
    (evil-define-key 'motion 'global (kbd "SPC b n") #'next-buffer))

;; == evil-collection ==
;; Vim keybindings outside of text buffers
(use-package evil-collection :ensure (:ref "1cf0f9654bbb53a1093b545a64df299f4aca3f9d")
    :after evil
    :custom
    (evil-collection-key-blacklist '("SPC"))
    :config
    (evil-collection-init))

;; == vertico ==
;; Useable completions
;; https://github.com/minad/vertico
(use-package vertico :ensure (:tag "2.5") :demand t
    :init
    (vertico-mode)
    ; Persist history over Emacs restarts. Vertico sorts by history position.
    (savehist-mode)
    (vertico-multiform-mode))


;; == orderless ==
;; fuzzy completion
;; https://github.com/oantolin/orderless
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
  :bind
  (("s-." . embark-act)))

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


;; ==== Commands ====

(defun reload ()
  "Reload init.el"
  (interactive)
  (load-file user-init-file))
