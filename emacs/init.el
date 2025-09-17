; Init File


;; Make Emacs store autosave files in /tmp directory
;; https://www.reddit.com/r/emacs/comments/ym3t77/how_to_delete_auto_save_files_when_quitting_emacs/
(setq backup-directory-alist
      `((".*" . ,temporary-file-directory)))
(setq auto-save-file-name-transforms
      `((".*" ,temporary-file-directory t)))

