
(require 'package)
(add-to-list 'package-archives '("tromey" . "http://tromey.com/elpa/"))
(add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/") t)
(setq package-user-dir (expand-file-name "elpa/" user-emacs-directory))
;; (package-initialize)

;; Install use-package that we require for managing all other dependencies
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))



(use-package ace-jump-buffer :ensure t)

(setq-default indent-tabs-mode nil)
(setq-default line-spacing 3)
(setq-default mastodon-active-user "dani")
(setq-default mastodon-instance-url "https://fosstodon.org")

(setq projectile-indexing-method 'hybrid)

(add-hook 'xref-backend-functions #'dumb-jump-xref-activate)
(setq xref-show-definitions-function #'xref-show-definitions-completing-read)

(setq helm-full-frame t)

(require 'package)
(let* ((no-ssl (and (memq system-type '(windows-nt ms-dos))
                    (not (gnutls-available-p))))
       (proto (if no-ssl "http" "https")))
  (when no-ssl
    (warn "\
Your version of Emacs does not support SSL connections,
which is unsafe because it allows man-in-the-middle attacks.
There are two things you can do about this warning:
1. Install an Emacs version that does support SSL and be safe.
2. Remove this warning from your init file so you won't see it again."))
  ;; Comment/uncomment these two lines to enable/disable MELPA and MELPA Stable as desired
  (add-to-list 'package-archives (cons "melpa" (concat proto "://melpa.org/packages/")) t)
  ;;(add-to-list 'package-archives (cons "melpa-stable" (concat proto "://stable.melpa.org/packages/")) t)
  (when (< emacs-major-version 24)
    ;; For important compatibility libraries like cl-lib
    (add-to-list 'package-archives (cons "gnu" (concat proto "://elpa.gnu.org/packages/")))))
(setq package-enable-at-startup nil)
(add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/"))
(add-to-list 'package-archives '("gnu" . "http://elpa.gnu.org/packages/"))
(package-initialize)

(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(eval-when-compile
  (require 'use-package))


(setq blink-cursor-blinks 0)

(add-to-list 'load-path "/home/dani/.emacs.d/lisp")

(require 'php-cs-fixer)

(menu-bar-mode -1)
(toggle-scroll-bar -1)
(tool-bar-mode -1)
(blink-cursor-mode 1)

(global-hl-line-mode +0)
(line-number-mode +1)
(global-display-line-numbers-mode 1)
(column-number-mode t)
(size-indication-mode t)


(setq scroll-margin 0
      scroll-conservatively 100000
      scroll-preserve-screen-position 1)

(use-package smart-mode-line-powerline-theme
  :ensure t)

(setq backup-directory-alist
      `((".*" . ,temporary-file-directory)))
(setq auto-save-file-name-transforms
      `((".*" ,temporary-file-directory t)))

(use-package expand-region
  :ensure t
  :bind ("M-m" . er/expand-region))

(use-package avy
  :ensure t
  :bind
  ("C-=" . avy-goto-char)
  :config
  (setq avy-background t))
(setq lsp-rust-analyzer-server-display-inlay-hints t)
(use-package company
  :ensure t
  :diminish company-mode
  :config
  (add-hook 'after-init-hook #'global-company-mode))
;;  (add-hook 'after-init-hook #'global-company-mode))

(use-package flycheck
  :ensure t
  :diminish flycheck-mode
  :config
  (add-hook 'after-init-hook #'global-flycheck-mode))

(setq flycheck-clang-language-standard "c++11")

(use-package magit
  :bind (("C-M-g" . magit-status)))





(use-package projectile
  :ensure t
  :diminish projectile-mode
  :bind
  (("C-c p f" . helm-projectile-find-file)
   ("C-c p p" . helm-projectile-switch-project)
   ("C-c p s" . projectile-save-project-buffers))
  :config
  (projectile-mode +1)
  )
; (setq projectile-project-search-path '("/opt/inmensys/projectes"))
(setq projectile-project-search-path '(("/opt/inmensys/projectes" . 1)))

(use-package helm
  :ensure t
  :defer 2
  :bind
  ("M-x" . helm-M-x)
  ("C-x C-f" . helm-find-files)
  ("M-y" . helm-show-kill-ring)
  ("C-x b" . helm-mini)
  ("C-0" . ace-jump-buffer)
  
  :config
  (require 'helm-config)
  (helm-mode 1)
  (setq helm-split-window-inside-p t
    helm-move-to-line-cycle-in-source t)
  (setq helm-autoresize-max-height 0)
  (setq helm-autoresize-min-height 20)
  (helm-autoresize-mode 1)
  (define-key helm-map (kbd "<tab>") 'helm-execute-persistent-action) ; rebind tab to run persistent action

  (define-key helm-map (kbd "C-z")  'helm-select-action) ; list actions using C-z
  )


(use-package helm-projectile
  :ensure t
  :config
  (helm-projectile-on))

(smartparens-global-mode)
(show-paren-mode 1)
(global-set-key (kbd "C-c m c") 'mc/edit-lines)

(add-to-list 'load-path "/home/dani/.emacs.d/neotree")
(require 'neotree)




(require 'iedit)

(global-set-key (kbd "C-l") 'bs-cycle-next)
(global-set-key (kbd "C-j") 'bs-cycle-previous)
(global-set-key (kbd "C-M-s-a") 'list-bookmarks)
(global-set-key (kbd "C-M-s-r") 'helm-projectile-rg)


(global-set-key (kbd "C-f") 'helm-occur)

(global-set-key [f8] 'neotree-toggle)
(defun bf-pretty-print-xml-region (begin end)
  "Pretty format XML markup in region. You need to have nxml-mode
http://www.emacswiki.org/cgi-bin/wiki/NxmlMode installed to do
this.  The function inserts linebreaks to separate tags that have
nothing but whitespace between them.  It then indents the markup
by using nxml's indentation rules."
  (interactive "r")
  (save-excursion
      (nxml-mode)
      (goto-char begin)
      (while (search-forward-regexp "\>[ \\t]*\<" nil t) 
        (backward-char) (insert "\n"))
      (indent-region begin end))
  (message "Ah, much better!"))

(defun comment-eclipse ()
  (interactive)
  (let ((start (line-beginning-position))
        (end (line-end-position)))
    (when (or (not transient-mark-mode) (region-active-p))
      (setq start (save-excursion
                    (goto-char (region-beginning))
                    (beginning-of-line)
                    (point))
            end (save-excursion
                  (goto-char (region-end))
                  (end-of-line)
                  (point))))
    (comment-or-uncomment-region start end)))


 ;; Run C programs directly from within emacs
(defun execute-c-program ()
  (interactive)
  (defvar foo)
  (setq foo (concat "gcc " (buffer-name) " && ./a.out" ))
  (shell-command foo))

(define-key global-map (kbd "C-c SPC") 'ace-jump-mode)
 
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ansi-color-faces-vector
   [default default default italic underline success warning error])
 '(ansi-color-names-vector
   ["black" "red3" "ForestGreen" "yellow3" "blue" "magenta3" "DeepSkyBlue" "gray50"])
 '(custom-enabled-themes '(misterioso))
 '(custom-safe-themes
   '("bfe046af7359f81d6bd4c47e09ecba8304940107752616124fbf405208b90c8f" "3199be8536de4a8300eaf9ce6d864a35aa802088c0925e944e2b74a574c68fd0" "cf922a7a5c514fad79c483048257c5d8f242b21987af0db813d3f0b138dfaf53" "c4063322b5011829f7fdd7509979b5823e8eea2abf1fe5572ec4b7af1dd78519" "f91395598d4cb3e2ae6a2db8527ceb83fed79dbaf007f435de3e91e5bda485fb" "da186cce19b5aed3f6a2316845583dbee76aea9255ea0da857d1c058ff003546" "a9a67b318b7417adbedaab02f05fa679973e9718d9d26075c6235b1f0db703c8" "0d01e1e300fcafa34ba35d5cf0a21b3b23bc4053d388e352ae6a901994597ab1" "3319c893ff355a88b86ef630a74fad7f1211f006d54ce451aab91d35d018158f" "1d5e33500bc9548f800f9e248b57d1b2a9ecde79cb40c0b1398dec51ee820daf" "234dbb732ef054b109a9e5ee5b499632c63cc24f7c2383a849815dacc1727cb6" "7a7b1d475b42c1a0b61f3b1d1225dd249ffa1abb1b7f726aec59ac7ca3bf4dae" "6c4c97a17fc7b6c8127df77252b2d694b74e917bab167e7d3b53c769a6abb6d6" "e8a0c94af8c0eeec7ae0f1633d29098ea722e5765f1e9c67b49da6f3414b9bfe" "7661b762556018a44a29477b84757994d8386d6edee909409fabe0631952dad9" "fc48cc3bb3c90f7761adf65858921ba3aedba1b223755b5924398c666e78af8b" "3c83b3676d796422704082049fc38b6966bcad960f896669dfc21a7a37a748fa" "b9e9ba5aeedcc5ba8be99f1cc9301f6679912910ff92fdf7980929c2fc83ab4d" "8db4b03b9ae654d4a57804286eb3e332725c84d7cdab38463cb6b97d5762ad26" default))
 '(elfeed-feeds
   '("https://www.jotdown.es/feed/" "https://www.reddit.com/r/PixelArt.rss" "https://planet.emacslife.com/atom.xml" "https://users.rust-lang.org/c/help.rss" "http://www.reddit.com/r/programming/.xml" "gemini://smol.pub/atom.xml"))
 '(inhibit-startup-screen t)
 '(org-agenda-files
   '("~/Documents/worklog.org" "/opt/inmensys/documents/worklog.org"))
 '(package-selected-packages
   '(company mastodon leuven-theme modus-vivendi-theme alert helm-rg ace-jump-buffer ace-window frog-jump-buffer drag-stuff adoc-mode asciidoc sticky doom-themes mood-one-theme twig-mode helm-ag ag ripgrep rg elfeed-dashboard elfeed ace-jump-helm-line ace-jump-mode elpher gruvbox-theme zenburn-theme vscode-dark-plus-theme lsp-mode req-package neotree php-mode php-runtime dumb-jump multi-web-mode yasnippet-snippets fold-this flycheck-rust auto-yasnippet rustic zzz-to-char rust-mode iedit smart-mode-line-powerline-theme flycheck lsp-ui eglot highlight-symbol git-timemachine wanderlust mu4e-conversation smartparens treemacs cycbuf web-mode elscreen tabbar lsp-javacomp helm-projectile projectile-speedbar lsp-java php-boris-minor-mode lsp-php all-the-icons smart-region color-theme-solarized ivy expand-region helm-swoop git-gutter magit vue-mode semi multiple-cursors jabber company-irony))
 '(php-cs-fixer-rules-fixer-part-options '("multiline_whitespace_before_semicolons" "concat_space"))
 '(projectile-enable-caching nil)
 '(warning-suppress-types '((lsp-mode))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:family "Cascadia Code" :height 130)))))

(setq backup-directory-alist `(("." . "~/.saves")))
(setq auto-save-default nil)

;; Run C programs directly from within emacs
(defun execute-c-program ()
  (interactive)
  (defvar foo)
  (setq foo (concat "make " (buffer-name) " && ./a.out" ))
  (shell-command foo))

(global-set-key [C-f1] 'execute-c-program)
(global-set-key [c-i] 'lsp-find-definition)




      


(add-to-list 'load-path "/home/dani/.emacs.d/lisp")
(require 'git-gutter)
(global-git-gutter-mode +1)




;; Rust configuration (lsp et all)


(use-package lsp-mode
  :ensure t
  :commands lsp
  :custom
  ;; what to use when checking on-save. "check" is default, I prefer clippy
  (lsp-rust-analyzer-cargo-watch-command "clippy")
;;  (lsp-eldoc-render-all t)
  (lsp-idle-delay 0.6)
  ;; enable / disable the hints as you prefer:
 ;;  (lsp-rust-analyzer-server-display-inlay-hints t)
  (lsp-rust-analyzer-display-lifetime-elision-hints-enable "skip_trivial")
  (lsp-rust-analyzer-display-chaining-hints t)
  (lsp-rust-analyzer-display-lifetime-elision-hints-use-parameter-names nil)
  (lsp-rust-analyzer-display-closure-return-type-hints t)
  (lsp-rust-analyzer-display-parameter-hints nil)
  (lsp-rust-analyzer-display-reborrow-hints nil)
  :config
  (add-hook 'lsp-mode-hook 'lsp-ui-mode)

  )

(setq lsp-enable-file-watchers nil)


(add-hook 'rust-mode-hook #'lsp-deferred)
(add-hook 'php-mode-hook #'lsp-deferred)

(use-package lsp-ui
  :ensure
  :commands lsp-ui-mode
  :custom
  (lsp-ui-peek-always-show t)
  (lsp-ui-sideline-show-hover t)
  (lsp-ui-doc-enable nil))



(use-package flycheck :ensure)


(defun eslint-fix-file ()
  (interactive)
  (message "eslint --fixing the file" (buffer-file-name))
  (shell-command (concat "phpcsfixer.php fix " (buffer-file-name))))



(set-cursor-color "#b703df") 

(put 'dired-find-alternate-file 'disabled nil)
