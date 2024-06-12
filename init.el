
(require 'package)
(add-to-list 'package-archives '("tromey" . "http://tromey.com/elpa/"))
(add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/") t)
(setq package-user-dir (expand-file-name "elpa/" user-emacs-directory))
;; (package-initialize)

;; Install use-package that we require for managing all other dependencies
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(setenv "JAVA_HOME"  "/opt/jdk-17.0.11")

(use-package ace-jump-buffer :ensure t)

;(setq-default indent-tabs-mode nil)
(setq-default line-spacing 3)
(setq-default mastodon-active-user "dani")
;(setq-default mastodon-instance-url "https://fosstodon.org")
(setq ediff-keep-variants nil)
(setq ediff-make-buffers-readonly-at-startup nil)
(setq ediff-merge-revisions-with-ancestor t)
(setq ediff-show-clashes-only t)
(setq ediff-split-window-function 'split-window-horizontally)
(setq ediff-window-setup-function 'ediff-setup-windows-plain)
(setq projectile-indexing-method 'hybrid)



;; Aixo es un hack per que funcioni el magit amb emacs28
(defun seq-keep (function sequence)
  "Apply FUNCTION to SEQUENCE and return the list of all the non-nil results."
  (delq nil (seq-map function sequence)))

;(add-hook 'xref-backend-functions #'dumb-jump-xref-activate)
;(setq xref-show-definitions-function #'xref-show-definitions-completing-read)

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

; (add-to-list 'load-path "/home/dani/.emacs.d/lisp")
;(require 'web-mode)
; (add-to-list 'auto-mode-alist '("\\.blade\\.php\\'" . web-mode))
;(setq web-mode-markup-indent-offset 4)
;(setq web-mode-css-indent-offset 4)
;(setq web-mode-code-indent-offset 4)
;(setq web-mode-indent-style 4)

;(setq web-mode-engines-alist
;      '(("php"    . "\\.phtml\\'")
;        ("blade"  . "\\.blade\\."))
                                        ;)

;; (require 'php-cs-fixer)

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

(use-package multiple-cursors)
(global-set-key (kbd "C-S-c C-S-c") 'mc/edit-lines)


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

;; (require 'multiple-cursors)
;; (global-set-key (kbd "C-S-c C-S-c") 'mc/edit-lines)
;; (global-set-key (kbd "C->") 'mc/mark-next-like-this)
;; (global-set-key (kbd "C-<") 'mc/mark-previous-like-this)
;; (global-set-key (kbd "C-c C-<") 'mc/mark-all-like-this)

(use-package flycheck
  :ensure t
  :diminish flycheck-mode
  :config
  (add-hook 'after-init-hook #'global-flycheck-mode))

(setq flycheck-clang-language-standard "c++11")

(use-package magit
  :bind (("C-M-g" . magit-status)))

(setq denote-file-type "markdown-toml")
(use-package denote
  :ensure t
  :custom (denote-directory "/home/dani/Nextcloud/Notes"))




(use-package projectile
  :ensure t
  :diminish projectile-mode
  :bind
  (("C-c p f" . helm-projectile-find-file)
   ("C-c p p" . helm-projectile-switch-project)
   ("C-c p s" . projectile-save-project-buffers))
  :config
  (projectile-mode +1)
  :init
  (when (file-directory-p "/opt/inmensys/projectes")
    (setq projectile-project-search-path '("/opt/inmensys/projectes")))
  )

; (setq projectile-project-search-path '("/opt/inmensys/projectes"))
; (setq projectile-project-search-path '(("/opt/inmensys/projectes" . 1)))

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
  (helm-mode 1)
  (setq helm-split-window-inside-p t
    helm-move-to-line-cycle-in-source t)
  (setq helm-autoresize-max-height 0)
  (setq helm-autoresize-min-height 20)
  (helm-autoresize-mode 1)
  (define-key helm-map (kbd "<tab>") 'helm-execute-persistent-action) ; rebind tab to run persistent action

  (define-key helm-map (kbd "C-z")  'helm-select-action) ; list actions using C-z
  )



(use-package consult-notes
  :commands (consult-notes
             consult-notes-search-in-all-notes
             ;; if using org-roam 
             consult-notes-org-roam-find-node
             consult-notes-org-roam-find-node-relation)
  :config
  ;; Set org-roam integration, denote integration, or org-heading integration e.g.:
  (consult-notes-org-headings-mode)
  (when (locate-library "denote")
    (consult-notes-denote-mode))
  ;; search only for text files in denote dir
  (setq consult-notes-denote-files-function (function denote-directory-text-only-files)))

(setq org-agenda-files (list "~/Nextcloud/Notes/20231218T223143--todo__todo.org"
                             "~/Nextcloud/worklog.org"))
(use-package helm-rg
  :config
  (setq helm-rg-ripgrep-executable "/usr/bin/rg")
  (setq helm-rg-default-directory 'git-root))

(use-package helm-projectile
  :ensure t
  :config
  (helm-projectile-on))

(use-package smartparens :ensure t);
 (smartparens-global-mode)
 (show-paren-mode 1)
;; (global-set-key (kbd "C-c m c") 'mc/edit-lines)

;;(add-to-list 'load-path "/home/dani/.emacs.d/neotree")
;; (require 'neotree)




;; (require 'iedit)

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

(setq text-scale-mode-step 1.02)
 ;; Run C programs directly from within emacs
(defun execute-c-program ()
  (interactive)
  (defvar foo)
  (setq foo (concat "gcc " (buffer-name) " && ./a.out" ))
  (shell-command foo))

(define-key global-map (kbd "C-c SPC") 'ace-jump-mode)

 (setq modus-vivendi-palette-overrides
  '(
    (bg-main "#292929")
    (bg-dim  "#292929")
    (fg-mode-line-active bg-red)
    )
  )


;; Java stuff

  (use-package lsp-java
  :ensure t
  :config
  (add-hook 'java-mode-hook 'lsp)
  (setq lsp-java-completion-max-results 10)
  (setenv "JAVA_HOME"  "/opt/jdk-17.0.11")
  (setq lsp-java-inhibit-message nil)
  (setq lsp-java-save-actions-organize-imports t)
  (setq lsp-java-java-path "/opt/jdk-17.0.11/bin/java")
  (setq lsp-java-configuration-runtimes '[(:name "JavaSE-19"
                                                 :path "/opt/jdk-17.0.11/"
                                                 :default t)]))

(use-package dap-mode
  :ensure t
  :after lsp-mode
  :config (dap-auto-configure-mode))

(use-package dap-java :ensure nil)

;; End Java stuff

;; (load-theme 'modus-vivendi :no-confirm)
(load-theme 'misterioso :no-confirm)
(setq elfeed-feeds (quote 
      (("https://hackaday.com/feed/" hacking)
       ("http://nedroid.com/feed/" webcomic)
       
       )))


(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ansi-color-faces-vector
   [default default default italic underline success warning error])
 '(ansi-color-names-vector
   ["black" "red3" "ForestGreen" "yellow3" "blue" "magenta3" "DeepSkyBlue" "gray50"])
 '(browse-url-browser-function 'eww-browse-url)
 '(custom-safe-themes
   '("9a8f64e03bb5907a1686cc528890875b68f3ed605f905605b3e82842d93de919" "a48173765fcfab205ba00ac608e91b083377b38eb709cc97c273733d8a2105d2" "66a2faa4788638bf25b2c5cd837edc6ef543fb2b2a64750d2f8ad65c2f597c48" "620684bd75fe4995c4984816422a6743654628ae4a453a8aeb8705731abe318e" "c154422213ef41c50f4f61eadef779695cacbf7fd62af9e28884f8420a1acb26" "99da2d606e3188c3a5729753fc9bf8f9c1eab9238f9f6cdae2c44507ff621785" "9e44484eae264cce413a3c973800483e6058524d475661e3d3d38c0a3677ea1a" "13ef995185f626c294ffeed34202c7de984385e290770c22d6789054da795214" "7887cf8b470098657395502e16809523b629249060d61607c2225d2ef2ad59f5" "9a23103f7e71d95b4b6545823c6197474888c21d0ece508fd9f0be50fff7e640" "277a5bce12d6957dbabb43a2f55ee2b6371388b749cbb29fd251df19334a1f0b" "01ca8e215adc3a3221b42db10218dd181244e2971071207a384daeb9cbf31e58" "cca1d386d4a3f645c2f8c49266e3eb9ee14cf69939141e3deb9dfd50ccaada79" "24b2a9551990485ee6762328a2eb964ba2afdc76d98c216d829b5ac1e51fd0e8" "28eb6d962d45df4b2cf8d861a4b5610e5dece44972e61d0604c44c4aad1e8a9d" "7a121b56eb7622d75e8323f9311b9797f4d06b8ba2f0b39c125ce33a5648d0a2" "7dc296b80df1b29bfc4062d1a66ee91efb462d6a7a934955e94e786394d80b71" "7d9b836cafb830f02f3d24df23054e6dc7656a061fcd47cbc39894dc6597d80a" "9ce97fbdb9dce917e2c0302ceb21f2db44e7b8e03aa09cc05de82af9af48be25" "3884928c24609bb9ca4b40e637b94b96f389f9175d5372e2f8a0d3549b556006" "4447c5a2dd07e4f70f4cecbdcd20fecd590dfadfbb1022e6cc41c69e6b504f0d" "1a3c10fc7fa10f814aa59833271eb4f547fe5b0b47a95482477c5df75b9375c5" "72422a99ddf421fc8d705332fd565c90405e73c2ec7ca10427792238ec2ff902" "bfe046af7359f81d6bd4c47e09ecba8304940107752616124fbf405208b90c8f" "3199be8536de4a8300eaf9ce6d864a35aa802088c0925e944e2b74a574c68fd0" "cf922a7a5c514fad79c483048257c5d8f242b21987af0db813d3f0b138dfaf53" "c4063322b5011829f7fdd7509979b5823e8eea2abf1fe5572ec4b7af1dd78519" "f91395598d4cb3e2ae6a2db8527ceb83fed79dbaf007f435de3e91e5bda485fb" "da186cce19b5aed3f6a2316845583dbee76aea9255ea0da857d1c058ff003546" "a9a67b318b7417adbedaab02f05fa679973e9718d9d26075c6235b1f0db703c8" "0d01e1e300fcafa34ba35d5cf0a21b3b23bc4053d388e352ae6a901994597ab1" "3319c893ff355a88b86ef630a74fad7f1211f006d54ce451aab91d35d018158f" "1d5e33500bc9548f800f9e248b57d1b2a9ecde79cb40c0b1398dec51ee820daf" "234dbb732ef054b109a9e5ee5b499632c63cc24f7c2383a849815dacc1727cb6" "7a7b1d475b42c1a0b61f3b1d1225dd249ffa1abb1b7f726aec59ac7ca3bf4dae" "6c4c97a17fc7b6c8127df77252b2d694b74e917bab167e7d3b53c769a6abb6d6" "e8a0c94af8c0eeec7ae0f1633d29098ea722e5765f1e9c67b49da6f3414b9bfe" "7661b762556018a44a29477b84757994d8386d6edee909409fabe0631952dad9" "fc48cc3bb3c90f7761adf65858921ba3aedba1b223755b5924398c666e78af8b" "3c83b3676d796422704082049fc38b6966bcad960f896669dfc21a7a37a748fa" "b9e9ba5aeedcc5ba8be99f1cc9301f6679912910ff92fdf7980929c2fc83ab4d" "8db4b03b9ae654d4a57804286eb3e332725c84d7cdab38463cb6b97d5762ad26" default))
 '(inhibit-startup-screen t)
 '(lsp-ui-doc-show-with-cursor t)
 '(package-selected-packages
   '(psgml denote-menu consult-notes denote projectile-ripgrep yasnippet cl-libify sweetgreen greenbar organic-green-theme green-is-the-new-black-theme green-phosphor-theme ef-themes olivetti markdown-preview-mode company mastodon leuven-theme modus-vivendi-theme alert helm-rg ace-jump-buffer ace-window frog-jump-buffer drag-stuff adoc-mode asciidoc sticky doom-themes mood-one-theme twig-mode helm-ag ag ripgrep rg elfeed-dashboard elfeed ace-jump-helm-line ace-jump-mode elpher gruvbox-theme zenburn-theme vscode-dark-plus-theme lsp-mode req-package neotree php-mode php-runtime dumb-jump multi-web-mode yasnippet-snippets fold-this flycheck-rust auto-yasnippet rustic zzz-to-char rust-mode iedit smart-mode-line-powerline-theme flycheck lsp-ui eglot highlight-symbol git-timemachine wanderlust mu4e-conversation smartparens treemacs cycbuf web-mode elscreen tabbar lsp-javacomp helm-projectile projectile-speedbar lsp-java php-boris-minor-mode lsp-php all-the-icons smart-region color-theme-solarized ivy expand-region helm-swoop git-gutter magit semi multiple-cursors jabber company-irony))
 '(projectile-enable-caching nil)
 '(shr-use-fonts nil)
 '(warning-suppress-log-types '((comp) (lsp-mode)))
 '(warning-suppress-types '((lsp-mode))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:family "Cascadia Code" :height 155))))
 '(ansi-color-black ((t (:background "dim gray" :foreground "#2d3743")))))

;; '(default ((t (:family "Cascadia Code" :average-width 11 :space-width 11 :max-width 11 :height 23 :size 18 :spacing 100)))))
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
(use-package git-gutter :ensure t);
  
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
 ;; (lsp-rust-analyzer-server-display-inlay-hints t)
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
  (lsp-ui-sideline-show-hover nil)
  (lsp-ui-doc-enable t))



(use-package flycheck :ensure)


(defun eslint-fix-file ()
  (interactive)
  (message "eslint --fixing the file" (buffer-file-name))
  (shell-command (concat "phpcsfixer.php fix " (buffer-file-name))))



(set-cursor-color "#b703df") 

(put 'dired-find-alternate-file 'disabled nil)
