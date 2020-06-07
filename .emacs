(require 'package)
(let* ((no-ssl (and (memq system-type '(windows-nt ms-dos))
                                        (not (gnutls-available-p))))
              (proto (if no-ssl "http" "https")))
    (when no-ssl (warn "\
                       Your version of Emacs does not support SSL connections,
                       which is unsafe because it allows man-in-the-middle attacks.
                       There are two things you can do about this warning:
                       1. Install an Emacs version that does support SSL and be safe.
                       2. Remove this warning from your init file so you won't see it again."))
                         (add-to-list 'package-archives (cons "melpa" (concat proto "://melpa.org/packages/")) t)
                           ;; Comment/uncomment this line to enable MELPA Stable
                           ;; if desired.  See `package-archive-priorities`
                             ;; and `package-pinned-packages`. Most users will not
                             ;; need or want to do this.
                               ;;(add-to-list 'package-archives (cons
                               ;;"melpa-stable" (concat proto
                               ;;"://stable.melpa.org/packages/")) t)
                                 )
(package-initialize)

(use-package org-roam
      :hook
      (after-init . org-roam-mode)
      :custom
      (org-roam-directory "/home/wonchul/org")
      :bind (:map org-roam-mode-map
              (("C-c n l" . org-roam)
               ("C-c n f" . org-roam-find-file)
               ("C-c n b" . org-roam-switch-to-buffer)
               ("C-c n g" . org-roam-graph-show))
              :map org-mode-map
              (("C-c n i" . org-roam-insert))))

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ansi-color-faces-vector
   [default default default italic underline success warning error])
 '(ansi-color-names-vector
   ["black" "red3" "ForestGreen" "yellow3" "blue" "magenta3" "DeepSkyBlue" "gray50"])
 '(company-quickhelp-color-background "#4F4F4F")
 '(company-quickhelp-color-foreground "#DCDCCC")
 '(custom-safe-themes
   (quote
    ("76c5b2592c62f6b48923c00f97f74bcb7ddb741618283bdb2be35f3c0e1030e3" default)))
 '(fci-rule-color "#383838")
 '(nrepl-message-colors
   (quote
    ("#CC9393" "#DFAF8F" "#F0DFAF" "#7F9F7F" "#BFEBBF" "#93E0E3" "#94BFF3" "#DC8CC3")))
 '(org-roam-directory "/home/wonchul/org")
 '(package-selected-packages
   (quote
    (lsp-mode company smartparens git-gutter magit ivy js2-mode evil zenburn-theme use-package org-roam)))
 '(pdf-view-midnight-colors (quote ("#DCDCCC" . "#383838")))
 '(vc-annotate-background "#2B2B2B")
 '(vc-annotate-color-map
   (quote
    ((20 . "#BC8383")
     (40 . "#CC9393")
     (60 . "#DFAF8F")
     (80 . "#D0BF8F")
     (100 . "#E0CF9F")
     (120 . "#F0DFAF")
     (140 . "#5F7F5F")
     (160 . "#7F9F7F")
     (180 . "#8FB28F")
     (200 . "#9FC59F")
     (220 . "#AFD8AF")
     (240 . "#BFEBBF")
     (260 . "#93E0E3")
     (280 . "#6CA0A3")
     (300 . "#7CB8BB")
     (320 . "#8CD0D3")
     (340 . "#94BFF3")
     (360 . "#DC8CC3"))))
 '(vc-annotate-very-old-color "#DC8CC3")
 '(git-gutter:update-interval 1))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

(load-theme 'zenburn t)

(require 'evil)
(evil-mode 1)

(require 'git-gutter)
(global-git-gutter-mode +1)

(require 'smartparens-config)


;; org config
(require 'org)
(define-key global-map "\C-cl" 'org-store-link)
(define-key global-map "\C-ca" 'org-agenda)
(setq org-log-done t)
(setq org-todo-keywords
  '((sequence "TODO" "WIP" "DONE")))

; ls-mode
;; if you want to change prefix for lsp-mode keybindings.
(setq lsp-keymap-prefix "\C-c C-l")

(require 'lsp-mode)
(add-hook 'XXX-mode-hook #'lsp)

; korean input
(setq default-input-method "korean-hangul")
;;;(define-key global-map "s-SPC" 'toglle-input-method)

; web dev configuration
;; set eslint - https://gist.github.com/CodyReichert/9dbc8bd2a104780b64891d8736682cea
(add-to-list 'auto-mode-alist '("\\.jsx?$" . web-mode))
(setq web-mode-content-types-alist '(("jsx" . "\\.js[x]?\\'")))

(defun web-mode-init-hook ()
  "Hooks for Web mode.  Adjust indent."
  (setq web-mode-markup-indent-offset 4))

(add-hook 'web-mode-hook  'web-mode-init-hook)

(require 'flycheck)
;;; disable jslint
(setq-default flycheck-disabled-checkers
              (append flycheck-disabled-checkers
                      '(javascript-jshint json-jsonlist)))
;;; use global eslint
;; Enable eslint checker for web-mode
(flycheck-add-mode 'javascript-eslint 'web-mode)
;;;; Enable flycheck globally
(add-hook 'after-init-hook #'global-flycheck-mode)
;;;; set add-node-modules-apth
(add-hook 'flycheck-mode-hook 'add-node-modules-path)

;;;; enable prettier
(defun web-mode-init-prettier-hook ()
  (add-node-modules-path)
  (prettier-js-mode))

(add-hook 'web-mode-hook  'web-mode-init-prettier-hook)

;;;; enable emmet
(add-hook 'web-mode-hook  'emmet-mode)
