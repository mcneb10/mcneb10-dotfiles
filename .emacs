;; Make emacs work with melpa
(require 'package)
(add-to-list 'package-archives
	     '("Melpa" . "https://melpa.org/packages/"))

(package-initialize)
(package-refresh-contents)

; Install Evil
(unless (package-installed-p 'evil)
  (package-install 'evil))

;; Enable Evil
(require 'evil)
(evil-mode 1)

;; Install lsp-mode
(unless (package-installed-p 'lsp-mode)
  (package-install 'lsp-mode))

;; Enable it
(require 'lsp-mode)

;; Install lsp-ui

(unless (package-installed-p 'lsp-ui)
  (package-install 'lsp-ui))

;; Install lsp-java

;; Set JAVA_HOME if it hasn't been already (change to your java home path)
(unless (getenv "JAVA_HOME")
  (setenv "JAVA_HOME" "/Library/Java/JavaVirtualMachines/jdk-18.0.2.1.jdk/Contents/Home/"))
(setq lsp-java-java-path (concat (getenv "JAVA_HOME") "/bin/java"))

(unless (package-installed-p 'lsp-java)
  (package-install 'lsp-java))

(require 'lsp-java)

;; Install bongo

(unless (package-installed-p 'bongo)
  (package-install 'bongo))

;; You probably know what this does by now..
(require 'bongo)

;; Install auto-complete

;; (unless (package-installed-p 'auto-complete)
;;   (package-install 'auto-complete))
;; (require 'auto-complete)

;; Stack overflow hack to force autocompletion to always turn on
;; (defun auto-complete-mode-maybe ()
;;   (unless (minibufferp (current-buffer))
;;     (auto-complete-mode 1)))


;; Install company-mode
(unless (package-installed-p 'company)
  (package-install 'company))
(add-hook 'after-init-hook 'global-company-mode)

(setq company-minimum-prefix-length 1
      company-idle-delay 0.0) ;; default is 0.2

;; Install neotree

(unless (package-installed-p 'neotree)
  (package-install 'neotree))

;; Some helper method(s)

(defun download-file (url filename-or-path)
  "Download a file to a specific location using cURL"
  (shell-command (concat "curl -L -o " filename-or-path " " url)))

(defun package-idiom (name &optional require)
  "Download a package if it hasn't already been installed"
  (unless (package-installed-p name)
    (package-install name))
  (if require
      (require name)))

;; Old vim habits die hard
(defun open (path)
  "Open a file like vim"
  (interactive "G")
  (find-file path))

;; Install tracker-mode because I can

;; Install stuff tracker-mode needs
(unless (package-installed-p 'osc)
  (package-install 'osc))
;; Set path as a variable
(setq tracker-mode-path (concat package-user-dir "/tracker-mode/"))
;; Set package filename as variable
(setq tracker-mode-module-path (concat tracker-mode-path "tracker-mode.el"))
;; Make directory for tracker mode
(unless (file-directory-p tracker-mode-path)
  (mkdir tracker-mode-path))
;; Download only if it hasn't been installed yet
(unless (file-exists-p tracker-mode-module-path)
  (download-file "https://raw.githubusercontent.com/defaultxr/tracker-mode/master/tracker-mode.el" tracker-mode-module-path))
;; Add module path to include path
(add-to-list 'load-path tracker-mode-path)

(require 'tracker-mode)

;; Install hl-todo

(unless (package-installed-p 'hl-todo)
  (package-install 'hl-todo))

(require 'hl-todo)

(global-hl-todo-mode 1)

;; Install emojify

(unless (package-installed-p 'emojify)
  (package-install 'emojify))

(add-hook 'after-init-hook #'global-emojify-mode)

;; Install mastodon

(unless (package-installed-p 'mastodon)
  (package-install 'mastodon))

;; TODO: set mastodon vars

;; (setq mastodon-instance-url ""
;;       mastodon-active-user "")

;; TODO: Install mastodon theme

;; Set up PerlNavigator

(add-to-list 'lsp-language-id-configuration
	     '(perl-mode . "perl"))
(add-to-list 'lsp-language-id-configuration
	     '(cperl-mode . "perl"))
;; Set to your system's path for PerlNavigator below

(setq perlnavigator-path "~/building/PerlNavigator/server/out/server.js")

(lsp-register-client
 (make-lsp-client :new-connection
		  (lsp-stdio-connection `("node " ,perlnavigator-path " --stdio"))
		  :activation-fn (lsp-activate-on "perl")
		  :major-modes '(cperl-mode perl-mode)
		  :priority 10
		  :server-id 'perl-ls))

(add-hook 'perl-mode-hook 'lsp)

;; Install which-key

(unless (package-installed-p 'which-key)
  (package-install 'which-key))

(require 'which-key)
(which-key-mode)

;; MOAR PACKAGES

(package-idiom 'try)
(package-idiom 'magit)
(package-idiom 'helm)
(helm-mode 1)
(helm-autoresize-mode 1)
(package-idiom 'helm-perldoc)
(package-idiom 'yasnippet)
(package-idiom 'license-snippets)
(package-idiom 'company-web)
(package-idiom 'dyalog-mode)
(package-idiom 'jupyter)
(package-idiom 'chess)
(package-idiom 'markdown-mode)

;; Enable line numbers
(global-display-line-numbers-mode t)

;; Enable terminal mouse support
(xterm-mouse-mode 1)

;; Hack from: https://github.com/emacs-lsp/lsp-ui/issues/607
;; Make lsp-ui shut up and stop making my terminal beep like crazy
;; When I hover over the menu bar
(let ((areas '("mode-line" "left-margin" "left-fringe" "right-fringe" "header-line" "vertical-scroll-bar"))
          loc)
      (while areas
        (setq loc (pop areas))
        (global-set-key
         (kbd (concat "<" loc "> <mouse-movement>")) #'ignore)))


;; Auto generated crap goes after this point

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   '(chess jupyter dyalog-mode company-web license-snippets yasnippet helm-perldoc helm magit try "try" evil-visual-mark-mode "try" emojify emojifiy hl-todo mastodon neotree evil)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

