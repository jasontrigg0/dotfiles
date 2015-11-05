;to reload this file: 
;M-x load-file and then enter twice

;emacs-copy also copies to the clipboard
(load-file "/home/jason/.emacs.d/xclip.el")

;automatically run chmod +x when appropriate
(add-hook 'after-save-hook 'executable-make-buffer-file-executable-if-script-p)


;revert buffer
(global-set-key (kbd "C-c r") (lambda ()
				(interactive)
				(revert-buffer t t t)
				                                (message "buffer is reverted")))


;IDE stuff
(require 'ido)
(ido-mode t)

;ESS: emacs for R
(ess-toggle-underscore nil)

;Python
;; (require 'ipython)




;; Shift the selected region right if distance is postive, left if
;; negative

(defun shift-region (distance)
  (let ((mark (mark)))
    (save-excursion
      (indent-rigidly (region-beginning) (region-end) distance)
      (push-mark mark t t)
      ;; Tell the command loop not to deactivate the mark
      ;; for transient mark mode
      (setq deactivate-mark nil))))

(defun shift-right ()
  (interactive)
  (shift-region 1))

(defun shift-left ()
  (interactive)
  (shift-region -1))

;; Bind (shift-right) and (shift-left) function to your favorite keys. I use
;; the following so that Ctrl-Shift-Right Arrow moves selected text one 
;; column to the right, Ctrl-Shift-Left Arrow moves selected text one
;; column to the left:

(global-set-key [C-S-right] 'shift-right)
(global-set-key [C-S-left] 'shift-left)



;automatically create intermediate directories when opening new files 
(defadvice find-file (before make-directory-maybe (filename &optional wildcards) activate)
  "Create parent directory if not exists while visiting file."
  (unless (file-exists-p filename)
    (let ((dir (file-name-directory filename)))
      (unless (file-exists-p dir)
        (make-directory dir)))))
