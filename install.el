(require 'package)
(require 'cl-lib)

(let* ((packages '(elpy
                   web-mode))
       (uninstalled-packages (cl-remove-if 'package-installed-p packages)))

  (when (> (length uninstalled-packages) 0)
    (setq package-archives '(("gnu" . "http://elpa.gnu.org/packages/")
                             ("marmalade" . "http://marmalade-repo.org/packages/")
                             ("melpa" . "http://melpa.milkbox.net/packages/")))

    (package-refresh-contents)

    (mapcar 'package-install uninstalled-packages)))
