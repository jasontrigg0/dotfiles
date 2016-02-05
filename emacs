;to reload this file: 
;M-x load-file and then enter twice

;MELPA package archive
(when (>= emacs-major-version 24)
  (require 'package)
  (add-to-list
   'package-archives
   '("melpa" . "http://melpa.org/packages/")
   t)
  (package-initialize))


;use 4 spaces instead of tabs
(setq-default indent-tabs-mode nil)
(setq tab-width 4)


;emacs-copy also copies + pastes to/from the clipboard
;possibly requires installing xclip 1.3 in emacs24 also?
(load-file "/home/jtrigg/.emacs.d/xclip.el")


;automatically run chmod +x when appropriate
(add-hook 'after-save-hook 'executable-make-buffer-file-executable-if-script-p)


;revert buffer
;; (global-set-key (kbd "C-c r") (lambda ()
;; 				(interactive)
;; 				(revert-buffer t t t)
;;                 (message "buffer is reverted")))


;IDE stuff
(require 'ido)
(ido-mode t)

;ESS: emacs for R
;jtrigg@20151219 this isn't working
;(ess-toggle-underscore nil)

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

;revert buffer
(global-set-key (kbd "C-c r") 'revert-buffer)

;regex search by default
(global-set-key (kbd "C-s") 'isearch-forward-regexp)
(global-set-key (kbd "\C-r") 'isearch-backward-regexp)
(global-set-key (kbd "C-M-s") 'isearch-forward)
(global-set-key (kbd "C-M-r") 'isearch-backward)



;save-as
(global-set-key (kbd "C-c R") 'rename-file-and-buffer)

;; source: http://steve.yegge.googlepages.com/my-dot-emacs-file
(defun rename-file-and-buffer (new-name)
  "Renames both current buffer and file it's visiting to NEW-NAME."
  (interactive "sNew name: ")
  (let ((name (buffer-name))
        (filename (buffer-file-name)))
    (if (not filename)
        (message "Buffer '%s' is not visiting a file!" name)
      (if (get-buffer new-name)
          (message "A buffer named '%s' already exists!" new-name)
        (progn
          (rename-file name new-name 1)
          (rename-buffer new-name)
          (set-visited-file-name new-name)
          (set-buffer-modified-p nil))))))


;automatically create intermediate directories when opening new files 
(defadvice find-file (before make-directory-maybe (filename &optional wildcards) activate)
  "Create parent directory if not exists while visiting file."
  (unless (file-exists-p filename)
    (let ((dir (file-name-directory filename)))
      (unless (file-exists-p dir)
        (make-directory dir)))))



;automatically set comment based on file extension
(defvar comment-based-on-extension-alist nil "alist of regexps and comment characters")
(setq comment-based-on-extension-alist '(("emacs$" ";" "") ("Snakefile$" "#" "") (".txt$" ">" "") (".scss$" "/* " " */")))

(defun setup-comment-based-on-extension ()
  (let ((alist comment-based-on-extension-alist))
    (while alist
      (when (string-match-p (caar alist) (buffer-file-name))
        (setq comment-start (car (cdr (car alist))))
        (setq comment-end (car (cdr (cdr (car alist)))))
        (setq alist nil))
      (setq alist (cdr alist)))))
(add-hook 'find-file-hook 'setup-comment-based-on-extension)



;setup files with certain endings to open in the proper mode
;(single quote matches end-of-string)
(add-to-list 'auto-mode-alist '("\\emacs\\'" . emacs-lisp-mode))
(add-to-list 'auto-mode-alist '("\\.scss\\'" . css-mode))
(add-to-list 'auto-mode-alist '("\\ashrc\\'" . sh-mode)) ;HACK: for some reason including the b in "bashrc" here doesn't work??


;mode for js inside html
(require 'web-mode)
(add-to-list 'auto-mode-alist '("\\.html$" . web-mode))
