;; Set up package manager
(eval-when-compile
  (add-to-list 'load-path "~/.emacs.d/site-lisp/use-package/")
  (package-initialize)
  (require 'bind-key)
  (require 'use-package)
  )

(use-package package
  :config
  (add-to-list 'package-archives
               '("MELPA Stable" . "https://stable.melpa.org/packages/") t)
  (add-to-list 'package-archives
	       '("marmalade" . "http://marmalade-repo.org/packages/"))
  (add-to-list 'package-archives
               '("org" . "https://orgmode.org/elpa/") t)
  (package-initialize))

;; Theme stuff
(use-package zenburn-theme
	     :config
	     (add-to-list 'custom-theme-load-path "~/.emacs.d/themes")
	     (load-theme 'zenburn t)
	     :ensure t)

(use-package racket-mode
  :mode ("\\.rkt\\'" . racket-mode)
  :interpreter ("racket" . racket-mode)
  :config
  (define-abbrev-table 'racket-mode-abbrev-table
    '(("lambda" "λ" nil 1)))
  (setq abbrev-mode t)

  ;; Special indentation for common functions
  (defun racket-add-keywords (face-name keyword-rules)
    (let* ((keyword-list (mapcar #'(lambda (x)
				     (symbol-name (cdr x)))
				 keyword-rules))
	   (keyword-regexp (concat "(\\("
				   (regexp-opt keyword-list)
				   "\\)[ \n]")))
      (font-lock-add-keywords 'racket-mode
			      `((,keyword-regexp 1  ',face-name))))
    (mapc #'(lambda (x)
	      (put (cdr x)
		   'racket-indent-function
		   (car x)))
	  keyword-rules))

  (racket-add-keywords
   'font-lock-keyword-face
   '((1 . for/append)
     (1 . for/mutable-set)
     (1 . herbie-test)))

  :ensure t)
(use-package geiser
  :config
  (add-hook 'racket-mode-hook (lambda () (geiser-mode t)))
  :ensure t)

(use-package evil
  :config
  (progn
    (evil-mode 1)
    (define-prefix-command 'meta-map)
    (define-key evil-normal-state-map (kbd "m") 'meta-map)
    (define-key meta-map (kbd "x") 'smex)
    (define-key meta-map (kbd "q") 'fill-paragraph)
    (define-key evil-ex-map "e " 'ido-find-file)
    (define-key evil-ex-map "E " 'ido-find-file)
    (define-key evil-ex-map "M " 'magit-find-file)
    (define-key evil-ex-map "m " 'magit-find-file)
    (define-key evil-ex-map "b " 'ido-switch-buffer)
    (define-key evil-ex-map "B " 'ido-switch-buffer)
    (define-key evil-normal-state-map "c" nil)
    (define-key evil-motion-state-map "cu" 'universal-argument)
    (define-key key-translation-map (kbd "cx") (kbd "C-x"))
    (define-key evil-normal-state-map (kbd "M-.") nil)
    ))

;; (require 'mode-line-color)
;; (require 'evil-mode-line)
(add-to-list 'load-path "~/.emacs.d/site-lisp/")
(require 'relative-linum)
(add-to-list 'load-path "~/.emacs.d/site-lisp/evil-plugins/")
(require 'evil-little-word)
(add-hook 'evil-insert-state-entry-hook (lambda () (linum-mode 0)))
(add-hook 'evil-insert-state-exit-hook 'linum-mode)
(global-linum-mode 1)
(define-key evil-normal-state-map (kbd "w")
  'evil-forward-little-word-begin)
(define-key evil-normal-state-map (kbd "b")
  'evil-backward-little-word-begin)
(define-key evil-operator-state-map (kbd "w")
  'evil-forward-little-word-begin)
(define-key evil-operator-state-map (kbd "b")
  'evil-backward-little-word-begin)
(define-key evil-visual-state-map (kbd "w")
  'evil-forward-little-word-begin)
(define-key evil-visual-state-map (kbd "b")
  'evil-backward-little-word-begin)
(define-key evil-visual-state-map (kbd "i w")
  'evil-inner-little-word)

(with-eval-after-load 'info
  (info-initialize)
  (add-to-list 'Info-directory-list
               "~/.emacs.d/site-lisp/magit/Documentation/"))

(use-package tramp
  :ensure t
  :config
  (setq tramp-debug-buffer t)
  (setq tramp-default-method "ssh"))
(use-package em-tramp
  :config
  (setq eshell-prefer-lisp-functions t)
  (setq eshell-prefer-lisp-variables t)
  (setq password-cache t)
  (setq password-cache-expiry 120))
(use-package wgrep :ensure t)
(use-package dired-x)

(use-package better-defaults :ensure t)

(use-package idle-highlight-mode :ensure t)
(use-package haskell-mode
  :ensure t
  :config
  (add-hook 'haskell-mode-hook 'turn-on-haskell-indentation))

(use-package markdown-mode+
  :ensure t)

(use-package openwith
  :ensure t
  :config
  (openwith-mode t)
  (setq openwith-associations '(("\\.pdf\\'" "evince" (file)))))

(use-package rust-mode
  :ensure t)

(use-package unfill
  :ensure t)

;; Ido stuff
(use-package ido
  :config
  (setq ido-enable-flex-matching t)
  (setq ido-everywhere t)
  (ido-mode t)
  :ensure t)
(use-package ido-ubiquitous
  :config
  (ido-ubiquitous-mode t)
  :ensure t)
(use-package smex
  :config
  (smex-initialize)
  :bind (("M-x" . smex)
         ("M-X" . smex-major-mode-commands)
         ("C-c C-c M-x" . execute-extended-command))
  :ensure t)

;; Dafny Stuff
;; (setq flycheck-dafny-executable
;;       "/home/alex/builds/uw-dafny/dafny")
;; (setq flycheck-boogie-executable
;;       "/home/alex/builds/uw-dafny/dafny-server")
;; (setq flycheck-z3-smt2-executable
;;       "/usr/bin/z3")

;; Proof general
(load-file "/home/alex/.emacs.d/site-lisp/ProofGeneral/generic/proof-site.el")
(setq coq-prog-name "/usr/local/bin/coqtop -emacs")

(load "/home/alex/.opam/4.04.0/share/emacs/site-lisp/tuareg-site-file")

;; TeX stuff
(require 'tex-mode)
(add-hook 'latex-mode-hook 'flyspell-mode)
(setq ispell-program-name "aspell")
(setq ispell-list-command "--list")

;; Preferences
(add-hook 'before-save-hook 'delete-trailing-whitespace)
(setf inhibit-startup-screen t
      inhibit-startup-message t
      inhibit-startup-echo-area-message t)
(setf initial-buffer-choice default-directory)

(set-frame-parameter (selected-frame) 'alpha '(95 90))
(add-to-list 'default-frame-alist '(alpha 90 85))

(setq-default dired-omit-files-p t)
(setq dired-omit-files (concat dired-omit-files "\\|^\\..+$"))
(global-set-key (kbd "C-c C-a") 'recompile)
(set 'temporary-file-directory "/tmp")

(use-package key-chord
  :ensure t
  :config
  (setq key-chord-two-keys-delay 0.2)
  (key-chord-define evil-insert-state-map "jk" 'evil-normal-state)
  (key-chord-mode 1))

(use-package ggtags
  :ensure t
  :config
  (add-hook 'c-mode-common-hook
            (lambda ()
              (when (derived-mode-p 'c-mode)
                (ggtags-mode 1))))
  (define-key ggtags-mode-map (kbd "C-x g s") 'ggtags-find-other-symbol)
  (define-key ggtags-mode-map (kbd "C-x g h") 'ggtags-view-tag-history)
  (define-key ggtags-mode-map (kbd "C-x g r") 'ggtags-find-reference)
  (define-key ggtags-mode-map (kbd "C-x g f") 'ggtags-find-file)
  (define-key ggtags-mode-map (kbd "C-x g c") 'ggtags-create-tags)
  (define-key ggtags-mode-map (kbd "C-x g u") 'ggtags-update-tags)

  (define-key ggtags-mode-map (kbd "M-,") 'pop-tag-mark))

(use-package clean-aindent-mode
  :ensure t
  :config
  (clean-aindent-mode 1))

(use-package dtrt-indent
  :ensure t
  :config
  (dtrt-indent-mode 1)
  (setq dtrt-indent-verbosity 0))

(use-package ws-butler
  :ensure t
  :config
  (ws-butler-mode 1))

;; ;; Lean mode!
;; (setq lean-rootdir "~/builds-void/lean")
;; (add-to-list 'load-path "~/builds-void/lean/src/emacs")
;; (require 'lean-mode)

;; Shell stuff
(defun split-shell ()
  (split-window)
  (eshell 'a))
(defun fresh-shell ()
  (eshell 'a))
(set-face-attribute 'default nil :height 120)
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(coq-compiler "~/builds/coq/bin/coqc")
 '(coq-prog-name "~/builds/coq/bin/coqtop" t)
 '(coq-server-host "server")
 '(coq-server-local-dpipe "/home/alex/.emacs.d/site-lisp/coq-server.el/dpipe")
 '(coq-server-local-sftp-server "/usr/lib64/ssh/sftp-server")
 '(electric-indent-mode nil)
 '(magit-push-always-verify nil)
 '(package-selected-packages
   (quote
    (posframe flycheck-posframe web-mode flycheck-irony flycheck-color-mode-line flycheck org-plus-contrib evil-org php-mode merlin unicode-fonts f key-chord ws-butler dtrt-indent clean-aindent-mode ggtags zenburn-theme wgrep use-package unfill smex smartparens rust-mode racket-mode paredit openwith markdown-mode+ magit-tramp ido-ubiquitous idle-highlight-mode haskell-mode goto-last-change geiser exec-path-from-shell evil-magit dash-functional company-coq boogie-friends better-defaults)))
 '(proof-assistants (quote (coq)))
 '(send-mail-function (quote smtpmail-send-it)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

(setq abbrev-expand-function 'ignore)

;; (add-to-list 'load-path "~/.emacs.d/site-lisp/magit/lisp/")
(use-package magit
  :bind (("C-x m" . magit-status)))

;; (add-to-list 'load-path "~/.emacs.d/site-lisp/evil-magit/")
(use-package evil-magit
  :config
  (define-key magit-mode-map "c" nil)
  (define-key magit-file-section-map "C" nil)
  (define-key magit-hunk-section-map "C" nil)
  (define-key magit-mode-map "C" 'magit-commit-popup)
  (define-key evil-ex-map "em " 'magit-find-file)
  (define-key evil-ex-map "Em " 'magit-find-file)
  (define-key evil-ex-map "EM " 'magit-find-file)
  (setq magit-completing-read-function 'magit-ido-completing-read)
  )

(use-package org
  :ensure org-plus-contrib
  :pin org
  :config
  (define-key global-map "\C-cl" 'org-store-link)
  (define-key global-map "\C-ca" 'org-agenda)
  (setq org-log-done t)
  (setq org-agenda-files (list "~/org/todo/herbgrind.org"
                               "~/org/todo/proverbot9001.org"))
  (org-babel-do-load-languages
   'org-babel-load-languages
   '((shell . t)
     (sh . t)))
  )

(use-package evil-org
  :ensure t
  :after org
  :config
  (add-hook 'org-mode-hook 'evil-org-mode)
  (add-hook 'evil-org-mode-hook
            (lambda ()
              (evil-org-set-key-theme)))
  (evil-set-initial-state 'org-agenda-mode 'normal)
  )
(use-package flycheck
  :ensure t
  :init (global-flycheck-mode))

(use-package flycheck-posframe
  :ensure t
  :after flycheck
  :config (add-hook 'flycheck-mode-hook #'flycheck-posframe-mode))

(add-to-list 'load-path "~/.emacs.d/site-lisp/emacs-flycheck-mypy")

(defun my-reload-dir-locals-for-current-buffer ()
  "reload dir locals for the current buffer"
  (interactive)
  (let ((enable-local-variables :all))
    (hack-dir-local-variables-non-file-buffer)))
(defun my-reload-dir-locals-for-all-buffer-in-this-directory ()
  "For every buffer with the same `default-directory` as the
current buffer's, reload dir-locals."
  (interactive)
  (let ((dir default-directory))
    (dolist (buffer (buffer-list))
      (with-current-buffer buffer
        (when (equal default-directory dir))
        (my-reload-dir-locals-for-current-buffer)))))

(put 'flycheck-clang-args 'safe-local-variable (lambda (xx) t))
;; (use-package company-jedi
;;   :ensure t
;;   :config
;;   (defun my/python-mode-hook ()
;;     (add-to-list 'company-backends 'company-jedi))

;;   (add-hook 'python-mode-hook 'my/python-mode-hook)
;;   )
(provide 'init)
;;; init.el ends here
