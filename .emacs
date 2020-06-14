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
    ("16dd114a84d0aeccc5ad6fd64752a11ea2e841e3853234f19dc02a7b91f5d661" "ff6c5a15591b98a58a7a68a969f7143e3e663991c31bf55ff0807f17e223af4b" "aded61687237d1dff6325edb492bde536f40b048eab7246c61d5c6643c696b7f" "939ea070fb0141cd035608b2baabc4bd50d8ecc86af8528df9d41f4d83664c6a" "76c5b2592c62f6b48923c00f97f74bcb7ddb741618283bdb2be35f3c0e1030e3" default)))
 '(fci-rule-color "#383838")
 '(git-gutter:update-interval 1)
 '(nrepl-message-colors
   (quote
    ("#CC9393" "#DFAF8F" "#F0DFAF" "#7F9F7F" "#BFEBBF" "#93E0E3" "#94BFF3" "#DC8CC3")))
 '(org-roam-directory "/home/wonchul/org")
 '(package-selected-packages
   (quote
    (counsel nord-theme poet-theme lsp-mode company smartparens git-gutter magit ivy js2-mode evil zenburn-theme use-package org-roam)))
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
 '(vc-annotate-very-old-color "#DC8CC3"))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

(load-theme 'zenburn t)
;(load-theme 'nord t)

(require 'evil)
(evil-mode 1)

; ivy mode
(require 'ivy)
(require 'counsel)
(ivy-mode 1)
(setq ivy-use-virtual-buffers t)
(setq ivy-count-format "(%d/%d) ")
; Ivy-based interface to standard commands
(global-set-key (kbd "C-s") 'swiper-isearch)
(global-set-key (kbd "M-x") 'counsel-M-x)
(global-set-key (kbd "C-x C-f") 'counsel-find-file)
(global-set-key (kbd "M-y") 'counsel-yank-pop)
(global-set-key (kbd "<f1> f") 'counsel-describe-function)
(global-set-key (kbd "<f1> v") 'counsel-describe-variable)
(global-set-key (kbd "<f1> l") 'counsel-find-library)
(global-set-key (kbd "<f2> i") 'counsel-info-lookup-symbol)
(global-set-key (kbd "<f2> u") 'counsel-unicode-char)
(global-set-key (kbd "<f2> j") 'counsel-set-variable)
(global-set-key (kbd "C-x b") 'ivy-switch-buffer)
(global-set-key (kbd "C-c v") 'ivy-push-view)
(global-set-key (kbd "C-c V") 'ivy-pop-view)

; Ivy-based interface to shell and system tools
(global-set-key (kbd "C-c c") 'counsel-compile)
(global-set-key (kbd "C-c g") 'counsel-git)
(global-set-key (kbd "C-c j") 'counsel-git-grep)
(global-set-key (kbd "C-c L") 'counsel-git-log)
(global-set-key (kbd "C-c r") 'counsel-rg)
(global-set-key (kbd "C-c m") 'counsel-linux-app)
(global-set-key (kbd "C-c z") 'counsel-fzf)
(global-set-key (kbd "C-x l") 'counsel-locate)
(global-set-key (kbd "C-c J") 'counsel-file-jump)
(global-set-key (kbd "C-S-o") 'counsel-rhythmbox)
(global-set-key (kbd "C-c w") 'counsel-wmctrl)

; Ivy-resume and other commands
(global-set-key (kbd "C-c C-r") 'ivy-resume)
(global-set-key (kbd "C-c b") 'counsel-bookmark)
(global-set-key (kbd "C-c d") 'counsel-descbinds)
(global-set-key (kbd "C-c g") 'counsel-git)
(global-set-key (kbd "C-c o") 'counsel-outline)
(global-set-key (kbd "C-c t") 'counsel-load-theme)
(global-set-key (kbd "C-c F") 'counsel-org-file)

(require 'git-gutter)
(global-git-gutter-mode +1)

(require 'smartparens-config)


;; org config
(require 'org)
(add-to-list 'auto-mode-alist '("\\.org$" . org-mode))
(define-key global-map "\C-cl" 'org-store-link)
(define-key global-map "\C-ca" 'org-agenda)
(setq org-log-done t)
(setq org-todo-keywords
  '((sequence "TODO" "WIP" "DONE")))

(add-hook 'org-mode-hook  'org-roam-mode)
(add-hook 'org-mode-hook
           (lambda ()
            (evil-mode 0)))

; ls-mode
;; if you want to change prefix for lsp-mode keybindings.
(setq lsp-keymap-prefix "\C-c C-l")

(require 'lsp-mode)
(add-hook 'lsp-mode-hook #'smartparens-mode)
(add-to-list 'auto-mode-alist '("\\.[ch](c|\+\+|pp|xx)?$" . lsp-mode))

; korean input
(setq default-input-method "korean-hangul")
;;;(define-key global-map "s-SPC" 'toglle-input-method)

; web dev configuration
;; set eslint - https://gist.github.com/CodyReichert/9dbc8bd2a104780b64891d8736682cea
(add-to-list 'auto-mode-alist '("\\.jsx?$" . web-mode))
(add-to-list 'auto-mode-alist '("\\.html$" . web-mode))
(add-to-list 'auto-mode-alist '("\\.tsx?$" . web-mode))
(add-to-list 'auto-mode-alist '("\\.vue" . web-mode))
(setq web-mode-content-types-alist '(("jsx" . "\\.js[x]?\\'")))

(defun web-mode-init-hook ()
  "Hooks for Web mode.  Adjust indent."
  (setq web-mode-markup-indent-offset 2)
  (setq web-mode-code-indent-offset 2)
  ;(setq web-mode-css-indent-offset 2)
)

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
;;;; set add-node-modules-path
(add-hook 'flycheck-mode-hook 'add-node-modules-path)

;;;; enable prettier
(defun web-mode-init-prettier-hook ()
  (add-node-modules-path)
  (prettier-js-mode))

(add-hook 'web-mode-hook  'web-mode-init-prettier-hook)

;;;; enable emmet
(add-hook 'web-mode-hook  'emmet-mode)

;;;; enable smartparens
(add-hook 'web-mode-hook #'smartparens-mode)

; end web dev

; set line number
(when (version<= "26.0.50" emacs-version )
  (global-display-line-numbers-mode))

; disable indenting tab
(setq-default indent-tabs-mode nil)

; magit mode
(require 'magit)
(global-set-key (kbd "C-x g") 'magit-status)
