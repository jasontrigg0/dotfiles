;;to reload this file:
;;M-x load-file and then enter twice
;;

;;install.el from here: https://github.com/fuqcool/emacs-setting/blob/master/.emacs
;;specify required packages in install.el
(package-initialize)
(load-file "~/.emacs.d/install.el")


;; I know that string is in my Emacs somewhere!
;;http://stackoverflow.com/a/2642655
;; (require 'cl)
;; (defcustom search-all-buffers-ignored-files (list (rx-to-string '(and bos (or ".bash_history" "TAGS") eos)))
;;   "Files to ignore when searching buffers via \\[search-all-buffers]."
;;   :type 'editable-list)

;; (require 'grep)
;; (defun search-all-buffers (regexp prefix)
;;   "Searches file-visiting buffers for occurence of REGEXP.  With
;; prefix > 1 (i.e., if you type C-u \\[search-all-buffers]),
;; searches all buffers."
;;   (interactive (list (grep-read-regexp)
;;                      current-prefix-arg))
;;   (message "Regexp is %s; prefix is %s" regexp prefix)
;;   (multi-occur
;;    (if (member prefix '(4 (4)))
;;        (buffer-list)
;;      (remove-if
;;       (lambda (b) (some (lambda (rx) (string-match rx  (file-name-nondirectory (buffer-file-name b)))) search-all-buffers-ignored-files))
;;       (remove-if-not 'buffer-file-name (buffer-list))))

;;    regexp))

;; (global-set-key [f7] 'search-all-buffers)


;; search in all open buffers
(defun my-multi-occur-in-matching-buffers (regexp &optional allbufs)
  "Show all lines matching REGEXP in all buffers."
  (interactive (occur-read-primary-args))
  (multi-occur-in-matching-buffers ".*" regexp))
(global-set-key (kbd "M-s /") 'my-multi-occur-in-matching-buffers)

(setq visible-bell t) ;; turn off annoying bell sound: http://emacsredux.com/blog/2016/02/14/disable-annoying-audio-notifications/

;;search this git project
;;
(global-set-key (kbd "M-s ;") 'ag-project)
(global-set-key (kbd "M-s :") 'ag-project-files) ;; search for a certain extension
(global-set-key (kbd "M-s '") 'ag-project-dired) ;; search files in the directory

;;git tools
(global-set-key (kbd "M-s g") 'git-timemachine-toggle)
(global-set-key (kbd "C-x g") 'magit-status)

;;remove all trailing whitespace before saving file
;;but don't remove it in markdown files
(add-hook 'before-save-hook 'delete-trailing-whitespace)

;; Automatically save and restore sessions
;; (setq desktop-dirname             "~/.emacs.d/desktop/"
;;       desktop-base-file-name      "emacs.desktop"
;;       desktop-base-lock-name      "lock"
;;       desktop-path                (list desktop-dirname)
;;       desktop-save                t
;;       desktop-files-not-to-save   "^$" ;reload tramp paths
;;       desktop-auto-save-timeout   300
;;       desktop-restore-eager 5
;;       desktop-load-locked-desktop nil)

;;bugfix for desktop-restore with daemon mode
;;https://www.reddit.com/r/emacs/comments/3t5zqs/daemon_mode_with_desktopsavemode/cx3e0ps/
;; Enable desktop-save-mode only when the first frame has come up.
;; This prevents Emacs from stalling when run as a daemon.
(add-hook 'after-make-frame-functions
    (lambda (frame)
        (with-selected-frame frame
            (unless desktop-save-mode
              ;; http://debbugs.gnu.org/cgi/bugreport.cgi?bug=17693
              (if (daemonp) (setq desktop-restore-frames nil))
              (setq desktop-dirname "~/.emacs.d/desktop/")
              (setq desktop-path (list desktop-dirname))
              (desktop-save-mode 1)
              (desktop-read)
              ))))

;use 4 spaces instead of tabs
(setq-default indent-tabs-mode nil)
(setq tab-width 4)


;2 spaces for js, web-mode
(setq js-indent-level 2)
(setq css-indent-offset 2)

;http://web-mode.org/
(setq web-mode-code-indent-offset 2)
(setq web-mode-markup-indent-offset 2)
(setq web-mode-css-indent-offset 2)

;set web-mode to comment javascript with // instead of /* */
;https://emacs.stackexchange.com/a/27714
;first solution gives loading error
(setq-default web-mode-comment-formats
              '(("javascript" . "//")
                ("ts" . "//")))

;emacs-copy also copies + pastes to/from the clipboard
;possibly requires installing xclip 1.3 in emacs24 also?
(load-file "/home/jason/.emacs.d/xclip.el")


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

;save-as
(global-set-key (kbd "C-c R") 'rename-file-and-buffer)

;regex search by default
(global-set-key (kbd "C-s") 'isearch-forward-regexp)
(global-set-key (kbd "\C-r") 'isearch-backward-regexp)
(global-set-key (kbd "C-M-s") 'isearch-forward)
(global-set-key (kbd "C-M-r") 'isearch-backward)


;; source: http://steve.yegge.googlepages.com/my-dot-emacs-file
;; added (shell-command (concat "chmod u+x " new-name))
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
          (set-buffer-modified-p nil)
          (shell-command (concat "chmod u+x " new-name)))))))


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
(add-to-list 'auto-mode-alist '("\\ashrc\\'" . sh-mode)) ;HACK: for some reason including the b in "bashrc" here doesn't work without the . as well??
(add-to-list 'auto-mode-alist '("\\.bash_private\\'" . sh-mode)) ;HACK: for some reason including the b in "bashrc" here doesn't work without the . as well??
(add-to-list 'auto-mode-alist '("\\.ses\\'" . ses-mode))

;mode for js inside html
(require 'web-mode)
(add-to-list 'auto-mode-alist '("\\.html\\'" . web-mode)) ;quote matches end of string: https://www.emacswiki.org/emacs/AutoModeAlist
(add-to-list 'auto-mode-alist '("\\.js\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.jsx\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.ts\\'" . web-mode))

(add-to-list 'auto-mode-alist '("\\.csv\\'" . csv-mode))

;;set by customize-face:
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(highlight-indentation-face ((t (:background "color-238" :foreground "white"))))
 '(web-mode-html-tag-bracket-face ((t (:foreground "white")))))



;http://emacsredux.com/blog/2013/03/27/copy-filename-to-the-clipboard/
(defun copy-file-name-to-clipboard ()
  "Copy the current buffer file name to the clipboard."
  (interactive)
  (let ((filename (if (equal major-mode 'dired-mode)
                      default-directory
                    (buffer-file-name))))
    (when filename
      (kill-new filename)
      (message "Copied buffer file name '%s' to the clipboard." filename))))


;;open-line copied from:
;;http://emacsredux.com/blog/2013/06/15/open-line-above/
(defun smart-open-line ()
    "Insert an empty line after the current line.
Position the cursor at its beginning, according to the current mode."
    (interactive)
    (move-end-of-line nil)
    (newline-and-indent))

(defun smart-open-line-above ()
    "Insert an empty line above the current line.
Position the cursor at it's beginning, according to the current mode."
    (interactive)
    (move-beginning-of-line nil)
    (newline-and-indent)
    (forward-line -1)
    (indent-according-to-mode))

(global-set-key (kbd "M-n") 'smart-open-line)
;;was using "M-O" for smart-open-line-above but then
;;touchpad scrolling inserted random "A"s and "B"s into my file
(global-set-key (kbd "M-N") 'smart-open-line-above)


;;tried flycheck but it hasn't been so helpful:
;; (eval-after-load 'flycheck
;;   '(flycheck-add-mode 'html-tidy 'web-mode))

;; (global-flycheck-mode)

;; (set-face-attribute 'flycheck-warning nil
;;                    :foreground "yellow"
;;                     :background "red")))

;; (eval-after-load 'flycheck
;;     '(flycheck-add-mode 'html-tidy 'web-mode))




;; python-mode.el (!= default emacs python mode)
;; (autoload 'python-mode "python-mode" "Python Mode." t)
;; (add-to-list 'auto-mode-alist '("\\.py\\'" . python-mode))
;; (add-to-list 'interpreter-mode-alist '("python" . python-mode))


;; csv hook to align
(add-hook 'csv-mode-hook (lambda () (define-key csv-mode-map (kbd "C-c C-c") (defun csv-align-visible (&optional arg) "Align visible fields" (interactive "P") (csv-align-fields nil (window-start) (window-end))))))


;https://github.com/jorgenschaefer/elpy
;sudo pip install jedi required for elpy
(elpy-enable)


(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ag-arguments (quote ("--smart-case" "--stats" "-A 5" "-B 5")))
 '(package-selected-packages (quote (ag csv-mode web-mode elpy))))

(defun module_fns ()
  (interactive)
  (elpy-goto-definition)
  (elpy-occur-definitions)
  (other-window 1)
  (previous-buffer))

(global-set-key (kbd "C-c C-a") 'module_fns)
