(setq gc-cons-threshold (* 50 1000 1000))
(add-hook 'emacs-startup-hook
          (lambda ()
            (message "Emacs ready in %s with %d garbage collections."
                     (format "%.2f seconds"
                             (float-time
                              (time-subtract after-init-time before-init-time)))
                     gcs-done)))

;; Set up package manager
(eval-when-compile
  (add-to-list 'load-path "~/.emacs.d/site-lisp/use-package/")
  ;; (unless package-archive-contents
  ;;   (package-refresh-contents))
  (package-initialize)
  (require 'bind-key)
  (require 'use-package)
  )

(use-package package
  :config
  (add-to-list 'package-archives
               '("MELPA" . "https://melpa.org/packages/") t)
  (add-to-list 'package-archives
               '("MELPA Stable" . "https://stable.melpa.org/packages/") t)
  ;; (add-to-list 'package-archives
  ;;              '("marmalade" . "http://marmalade-repo.org/packages/"))
  ;; (add-to-list 'package-archives
  ;;              '("org" . "https://orgmode.org/elpa/") t))
  )
(use-package tuareg
  :ensure t
  :mode (("\\.ocamlinit\\'" . tuareg-mode)))
;; Merlin provides advanced IDE features
(use-package merlin
  :ensure t
  :config
  (add-hook 'tuareg-mode-hook #'merlin-mode)
  (add-hook 'merlin-mode-hook #'company-mode)
  ;; we're using flycheck instead
  (setq merlin-error-after-save nil))

;; This uses Merlin internally
(use-package flycheck-ocaml
  :ensure t
  :config
  (flycheck-ocaml-setup))
(setq-default indent-tabs-mode nil)
;; Theme stuff
(use-package zenburn-theme
	     :config
	     (add-to-list 'custom-theme-load-path "~/.emacs.d/themes")
             (defun load-my-theme (frame)
               (select-frame frame)
               (load-theme 'zenburn t))
             (if (daemonp)
                 (add-hook 'after-make-frame-functions #'load-my-theme)
               (load-theme 'zenburn t))
	     (menu-bar-mode -1)
	     (tool-bar-mode -1)
	     (scroll-bar-mode -1)
	     :ensure t)

(use-package evil
  :ensure t
  :init
  (setq evil-want-integration t)
  (setq evil-want-keybinding nil)
  :config
  (progn
    (evil-mode 1)
    (define-prefix-command 'meta-map)
    (define-key evil-normal-state-map (kbd "m") 'meta-map)
    (define-key meta-map (kbd "x") 'smex)
    (define-key meta-map (kbd "q") 'fill-paragraph)
    (evil-define-key 'normal rust-mode-map "M-q" 'rust-format-buffer)
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

(use-package evil-collection
  :after evil
  :ensure t
  :config
  (evil-collection-init))

(use-package smex
  :defer t
  :config
  (smex-initialize)
  :bind (("M-x" . smex)
         ("M-X" . smex-major-mode-commands)
         ("C-c C-c M-x" . execute-extended-command))
  :ensure t)

(use-package key-chord
  :ensure t
  :config
  (setq key-chord-two-keys-delay 0.2)
  (key-chord-define evil-insert-state-map "jk" 'evil-normal-state)
  (key-chord-mode 1))

(use-package magit
  :defer t
  :bind (("C-x m" . magit-status))
  :config
  (define-key magit-mode-map "c" nil)
  (evil-define-key evil-collection-magit-state
    magit-mode-map "C" 'magit-commit)
  (setq magit-completing-read-function 'magit-ido-completing-read)
  (define-key evil-ex-map "em " 'magit-find-file)
  (define-key evil-ex-map "Em " 'magit-find-file)
  (define-key evil-ex-map "EM " 'magit-find-file))

(use-package unicode-fonts
  :defer t
   :ensure t
   :config
   (unicode-fonts-setup))

(set-frame-font "iosevka 14" nil t)

(use-package better-defaults
  :defer t
  :ensure t)

;; Ido stuff
(use-package ido
  :defer t
  :config
  (setq ido-enable-flex-matching t)
  (setq ido-everywhere t)
  (ido-mode t)
  :ensure t)
(use-package ido-ubiquitous
  :defer t
  :config
  (ido-ubiquitous-mode t)
  :ensure t)


(use-package ws-butler
  :defer t
  :ensure t
  :config
  (ws-butler-mode 1))

(use-package flycheck
  :defer t
  :ensure t
  :init (global-flycheck-mode)
  :config
  (setq flycheck-python-pycompile-executable "python3")
  (flycheck-define-checker
      python-mypy ""
      :command ("python" "-m" "mypy"
                "--strict"
                "--follow-imports=silent"
                "--implicit-reexport"
                "--ignore-missing-imports"
                "--python-version" "3.10"
                source-original)
      :error-patterns
      ((error line-start (file-name) ":" line ": error:" (message) line-end))
      :modes python-mode)

  (add-to-list 'flycheck-checkers 'python-mypy t))

(use-package flymake-ruff
  :ensure t
  :config
  (add-hook 'python-mode-hook #'flymake-ruff-load))


(let ((opam-share (ignore-errors (car (process-lines "opam" "var" "share")))))
  (when (and opam-share (file-directory-p opam-share))
    ;; Register Merlin
    (add-to-list 'load-path (expand-file-name "emacs/site-lisp" opam-share))
    (autoload 'merlin-mode "merlin" nil t nil)
    ;; Automatically start it in OCaml buffers
    (add-hook 'tuareg-mode-hook 'merlin-mode t)
    (add-hook 'caml-mode-hook 'merlin-mode t)
    ;; Use opam switch to lookup ocamlmerlin binary
    (setq merlin-command 'opam)))
;; (use-package racket-mode
;;   :mode ("\\.rkt\\'" . racket-mode)
;;   :interpreter ("racket" . racket-mode)
;;   :config
;;   (define-abbrev-table 'racket-mode-abbrev-table
;;     '(("lambda" "λ" nil 1)))
;;   (setq abbrev-mode t)

;;   ;; Special indentation for common functions
;;   (defun racket-add-keywords (face-name keyword-rules)
;;     (let* ((keyword-list (mapcar #'(lambda (x)
;; 				     (symbol-name (cdr x)))
;; 				 keyword-rules))
;; 	   (keyword-regexp (concat "(\\("
;; 				   (regexp-opt keyword-list)
;; 				   "\\)[ \n]")))
;;       (font-lock-add-keywords 'racket-mode
;; 			      `((,keyword-regexp 1  ',face-name))))
;;     (mapc #'(lambda (x)
;; 	      (put (cdr x)
;; 		   'racket-indent-function
;; 		   (car x)))
;; 	  keyword-rules))

;;   (racket-add-keywords
;;    'font-lock-keyword-face
;;    '((1 . for/append)
;;      (1 . for/mutable-set)
;;      (1 . herbie-test))))
;; (use-package geiser
;;   :config
;;   (add-hook 'racket-mode-hook (lambda () (geiser-mode t)))
;;   :ensure t)




;; (add-to-list 'load-path "~/.emacs.d/site-lisp/")
;; (add-to-list 'load-path "~/.emacs.d/site-lisp/evil-plugins/")
;; (require 'evil-relative-linum)
;; (require 'evil-little-word)
;; (add-hook 'evil-insert-state-entry-hook (lambda () (linum-mode 0)))
;; (add-hook 'evil-insert-state-exit-hook 'linum-mode)
;; (global-linum-mode 1)
;; (define-key evil-normal-state-map (kbd "w")
;;   'evil-forward-little-word-begin)
;; (define-key evil-normal-state-map (kbd "b")
;;   'evil-backward-little-word-begin)
;; (define-key evil-operator-state-map (kbd "w")
;;   'evil-forward-little-word-begin)
;; (define-key evil-operator-state-map (kbd "b")
;;   'evil-backward-little-word-begin)
;; (define-key evil-visual-state-map (kbd "w")
;;   'evil-forward-little-word-begin)
;; (define-key evil-visual-state-map (kbd "b")
;;   'evil-backward-little-word-begin)
;; (define-key evil-visual-state-map (kbd "i w")
;;   'evil-inner-little-word)

;; (with-eval-after-load 'info
;;   (info-initialize)
;;   (add-to-list 'Info-directory-list
;;                "~/.emacs.d/site-lisp/magit/Documentation/"))

;; (use-package tramp
;;   :ensure t
;;   :config
;;   (setq tramp-debug-buffer t)
;;   (setq tramp-default-method "ssh"))
;; (use-package em-tramp
;;   :config
;;   (setq eshell-prefer-lisp-functions t)
;;   (setq eshell-prefer-lisp-variables t)
;;   (setq password-cache t)
;;   (setq password-cache-expiry 120))
;; (use-package wgrep :ensure t)
;; (use-package dired-x)

(use-package idle-highlight-mode :ensure t)
;; (use-package haskell-mode
;;   :ensure t
;;   :config
;;   (add-hook 'haskell-mode-hook 'turn-on-haskell-indentation))

;; (use-package openwith
;;   :ensure t
;;   :config
;;   (openwith-mode t)
;;   (setq openwith-associations '(("\\.pdf\\'" "evince" (file)))))

;; (use-package rust-mode
;;   :ensure t
;;   :config
;;   (define-key rust-mode-map (kbd "M-q") 'rust-format-buffer))

;; (use-package lsp
;;   :config
;;   (lsp-register-custom-settings
;;    '(("pyls.plugins.pyls_mypy.enabled" t t)
;;      ("pyls.plugins.flake8.enabled" t t)
;;      ("pyls.plugins.pyls_mypy.live_mode" nil t)
;;      ("pyls.plugins.pyls_black.enabled" t t)
;;      ("pyls.plugins.pyls_isort.enabled" t t)
;;      ("pyls.plugins.pycodestyle.enabled" nil t)
;;      ("pyls.plugins.mccabe.enabled" nil t)
;;      ("pyls.plugins.pyflakes.enabled" nil t)))
;;   (yas-global-mode 1)
;;   :hook
;;   ((rust-mode . lsp)
;;    (python-mode . lsp))
;;   :commands lsp)
;; ;; (lsp-register-client
;; ;;  (make-lsp-client :new-connection (lsp-tramp-connection "pyls")
;; ;;                   :major-modes '(python-mode)
;; ;;                   :remote? t
;; ;;                   :server-id 'pyls-remote))

;; (use-package unfill
;;   :ensure t)

;; ;; Dafny Stuff
;; ;; (setq flycheck-dafny-executable
;; ;;       "/home/alex/builds/uw-dafny/dafny")
;; ;; (setq flycheck-boogie-executable
;; ;;       "/home/alex/builds/uw-dafny/dafny-server")
;; ;; (setq flycheck-z3-smt2-executable
;; ;;       "/usr/bin/z3")


;; ;; TeX stuff
;; (require 'tex-mode)
;; (add-hook 'latex-mode-hook 'flyspell-mode)
;; (setq ispell-program-name "aspell")

;; ;; Preferences
(add-hook 'before-save-hook 'delete-trailing-whitespace)
(setf inhibit-startup-screen t
      inhibit-startup-message t
      inhibit-startup-echo-area-message t)
(setf initial-buffer-choice default-directory)

;; (set-frame-parameter (selected-frame) 'alpha '(95 90))
;; (add-to-list 'default-frame-alist '(alpha 90 85))

(setq-default dired-omit-files-p t)
;; (setq dired-omit-files (concat dired-omit-files "\\|^\\..+$"))
;; (global-set-key (kbd "C-c C-a") 'recompile)
;; (set 'temporary-file-directory "/tmp")

;; (use-package ggtags
;;   :ensure t
;;   :config
;;   (add-hook 'c-mode-common-hook
;;             (lambda ()
;;               (when (derived-mode-p 'c-mode)
;;                 (ggtags-mode 1))))
;;   (define-key ggtags-mode-map (kbd "C-x g s") 'ggtags-find-other-symbol)
;;   (define-key ggtags-mode-map (kbd "C-x g h") 'ggtags-view-tag-history)
;;   (define-key ggtags-mode-map (kbd "C-x g r") 'ggtags-find-reference)
;;   (define-key ggtags-mode-map (kbd "C-x g f") 'ggtags-find-file)
;;   (define-key ggtags-mode-map (kbd "C-x g c") 'ggtags-create-tags)
;;   (define-key ggtags-mode-map (kbd "C-x g u") 'ggtags-update-tags)

;;   (define-key ggtags-mode-map (kbd "M-,") 'pop-tag-mark))

(use-package dtrt-indent
  :ensure t
  :config
  (dtrt-indent-mode 1)
  (setq dtrt-indent-verbosity 0))

(use-package lean-mode
  :ensure t
  :defer t)
(use-package company-lean
  :after lean-mode
  :ensure t
  :defer t
  :config
  (global-set-key (kbd "S-SPC") #'company-complete))

(use-package clang-format
  :ensure t
  :defer t
  :config
  (setq clang-format-style "file"))

;; ;; ;; Lean mode!
;; ;; (setq lean-rootdir "~/builds-void/lean")
;; ;; (add-to-list 'load-path "~/builds-void/lean/src/emacs")
;; ;; (require 'lean-mode)

;; ;; Shell stuff
;; (defun split-shell ()
;;   (split-window)
;;   (eshell 'a))
;; (defun fresh-shell ()
;;   (eshell 'a))
;; ;; (set-face-attribute 'default nil :height 140)
;; (setq abbrev-expand-function 'ignore)

;; (use-package org
;;   :ensure org-plus-contrib
;;   :pin org
;;   :config
;;   (define-key global-map "\C-cl" 'org-store-link)
;;   (define-key global-map "\C-ca" 'org-agenda)
;;   (setq org-log-done t)
;;   (setq org-agenda-files (list "~/org/todo/herbgrind.org"
;;                                "~/org/todo/proverbot9001.org"))
;;   (org-babel-do-load-languages
;;    'org-babel-load-languages
;;    '((shell . t))
;;   ))

;; (use-package evil-org
;;   :ensure t
;;   :after org
;;   :config
;;   (add-hook 'org-mode-hook 'evil-org-mode)
;;   (add-hook 'evil-org-mode-hook
;;             (lambda ()
;;               (evil-org-set-key-theme)))
;;   (evil-set-initial-state 'org-agenda-mode 'normal)
;;   )
;; ;; (use-package flycheck
;; ;;   :ensure t
;; ;;   :init (global-flycheck-mode)
;; ;;   :config
;; ;;   (setq flycheck-python-pycompile-executable "python3"))


;; ;; (use-package elpy
;; ;;   :ensure t
;; ;;   :init (elpy-enable))

;; ;; (use-package flycheck-mypy
;; ;;   :ensure t
;; ;;   :config
;; ;;   (flycheck-add-next-checker 'python-flake8 'python-mypy t))

;; (load "~/.emacs.d/site-lisp/PG/generic/proof-site")
;; ;; (use-package flycheck-posframe
;; ;;   :ensure t
;; ;;   :after flycheck
;; ;;   :config (add-hook 'flycheck-mode-hook #'flycheck-posframe-mode))
;; ;; (add-to-list 'load-path "~/.emacs.d/site-lisp/emacs-flycheck-mypy")

;; (defun my-reload-dir-locals-for-current-buffer ()
;;   "reload dir locals for the current buffer"
;;   (interactive)
;;   (let ((enable-local-variables :all))
;;     (hack-dir-local-variables-non-file-buffer)))
;; (defun my-reload-dir-locals-for-all-buffer-in-this-directory ()
;;   "For every buffer with the same `default-directory` as the
;; current buffer's, reload dir-locals."
;;   (interactive)
;;   (let ((dir default-directory))
;;     (dolist (buffer (buffer-list))
;;       (with-current-buffer buffer
;;         (when (equal default-directory dir))
;;         (my-reload-dir-locals-for-current-buffer)))))

;; (put 'flycheck-clang-args 'safe-local-variable (lambda (xx) t))
;; (setq elpy-rpc-python-command "python3")
;; ;; (use-package company-jedi
;; ;;   :ensure t
;; ;;   :config
;; ;;   (defun my/python-mode-hook ()
;; ;;     (add-to-list 'company-backends 'company-jedi))

;; ;;   (add-hook 'python-mode-hook 'my/python-mode-hook)
;; ;;   )
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(evil-undo-system 'undo-redo)
 '(package-selected-packages
   '(clang-format company-lean dash-functional flycheck-mypy
                  flycheck-ocaml flycheck-pyflakes ido-ubiquitous jedi
                  key-chord lean-mode magit merlin openwith org-modern
                  smex tuareg unfill wgrep ws-butler))
 '(warning-suppress-log-types '((comp) (comp)))
 '(warning-suppress-types '((comp) (comp))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

; (require 'mode-line-color)
; (require 'evil-mode-line)


;; ## added by OPAM user-setup for emacs / base ## 56ab50dc8996d2bb95e7856a6eddb17b ## you can edit, but keep this line

;; (use-package opam-user-setup
;;   :load-path "~/.emacs.d/opam-user-setup.el")
;; ## end of OPAM user-setup addition for emacs / base ## keep this line

(setq c-basic-offset 4)

(setq gc-cons-threshold (* 2 1000 1000))


(provide 'init)
;;; init.el ends here
