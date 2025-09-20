;; == Elpaca ==
;; Disable package.el for Elpaca
;; NB: MUST NOT be `set-opt`. Idk why
(setq package-enable-at-startup nil)

;; == GUI Settings ==
;; Start maximized
;; https://emacs.stackexchange.com/questions/55004/how-do-i-configure-first-emacs-frame-position-on-startup
(setopt frame-resize-pixelwise t)
(setopt default-frame-alist
	'((top . 1)
	  (left . 1)
	  (width . (text-pixels . 1440))
	  (height . (text-pixels . 769))
	  (vertical-scroll-bars . nil)
	  (horizontal-scroll-bars . nil)
	  (tool-bar-lines . 0)
	  (font . "JetBrainsMono Nerd Font-14:weight=light")))
