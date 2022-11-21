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
(package-idiom 'neotree t)
;; Neotree config
(setq neo-smart-open t)
;; Neotree evil keybinds
;; Taken from
(evil-define-key 'normal neotree-mode-map (kbd "TAB") 'neotree-enter)
(evil-define-key 'normal neotree-mode-map (kbd "SPC") 'neotree-quick-look)
(evil-define-key 'normal neotree-mode-map (kbd "q") 'neotree-hide)
(evil-define-key 'normal neotree-mode-map (kbd "RET") 'neotree-enter)
(evil-define-key 'normal neotree-mode-map (kbd "g") 'neotree-refresh)
(evil-define-key 'normal neotree-mode-map (kbd "n") 'neotree-next-line)
(evil-define-key 'normal neotree-mode-map (kbd "p") 'neotree-previous-line)
(evil-define-key 'normal neotree-mode-map (kbd "A") 'neotree-stretch-toggle)
(evil-define-key 'normal neotree-mode-map (kbd "H") 'neotree-hidden-file-toggle)

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

;; Install theme
(package-idiom 'moe-theme t)
;; Choose a specific theme if not running under terminal
(when (display-graphic-p)
  (moe-light))

;; TODO: make this work
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
(package-idiom 'powerline t)
(package-idiom 'powerline-evil t)
(package-idiom 'restclient t)
(package-idiom 'realgud t)
(package-idiom 'evil-mc t)
(package-idiom 'evil-collection)
(evil-collection-init)

;; Fix some powerline stuff
(setq powerline-arrow-shape 'curve)
(setq powerline-default-separator-dir '(right . left))
;; '(right . left))

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

;; Powerline theme

(defface powerline-active2-visual '((t (:background "darkmagenta" :foreground "white" :inherit mode-line)))
  "Powerline face 2."
  :group 'powerline)

(defface powerline-active2-normal '((t (:background "grey40" :foreground "white" :inherit mode-line)))
  "Powerline face 2."
  :group 'powerline)

(defface powerline-active2-insert '((t (:background "darkgoldenrod1" :foreground "white" :inherit mode-line)))
  "Powerline face 2."
  :group 'powerline)

(defface powerline-active2-replace '((t (:background "firebrick1" :foreground "white" :inherit mode-line)))
  "Powerline face 2."
  :group 'powerline)

(defface powerline-active2-operator '((t (:background "sienna1" :foreground "white" :inherit mode-line)))
  "Powerline face 2."
  :group 'powerline)

(defface powerline-active2-motion '((t (:background "darkolivegreen2" :foreground "white" :inherit mode-line)))
  "Powerline face 2."
  :group 'powerline)

(defface powerline-active2-emacs '((t (:background "salmon1" :foreground "white" :inherit mode-line)))
  "Powerline face 2."
  :group 'powerline)

(defun powerline-custom-theme ()
  "Setup the default mode-line but without that LN symbol and a vim mode indicator. Stolen from powerline-themes.el"
  (interactive)
  (setq-default mode-line-format
                '("%e"
                  (:eval
                   (let* ((active (powerline-selected-window-active))
                          (mode-line-buffer-id (if active 'mode-line-buffer-id 'mode-line-buffer-id-inactive))
                          (mode-line (if active 'mode-line 'mode-line-inactive))
                          (face0 (if active 'powerline-active0 'powerline-inactive0))
                          (face1 (if active 'powerline-active1 'powerline-inactive1))
			  ;; Replacing with vim mode
                          ;;(face2 (if active 'powerline-active2 'powerline-inactive2))
                           (face2 (if active (cond
			   		     ((eq evil-state 'visual) 'powerline-active2-visual)
			  		     ((eq evil-state 'normal) 'powerline-active2-normal)
			  		     ((eq evil-state 'insert) 'powerline-active2-insert)
			  		     ((eq evil-state 'replace) 'powerline-active2-replace)
			  		     ((eq evil-state 'operator) 'powerline-active2-operator)
			  		     ((eq evil-state 'motion) 'powerline-active2-motion)
			  		     ((eq evil-state 'emacs) 'powerline-active2-emacs)
			  		     (t 'powerline-active2))
			  	   'powerline-inactive2))
                          (separator-left (intern (format "powerline-%s-%s"
                                                          (powerline-current-separator)
                                                          (car powerline-default-separator-dir))))
                          (separator-right (intern (format "powerline-%s-%s"
                                                           (powerline-current-separator)
                                                           (cdr powerline-default-separator-dir))))
                          (lhs (list (powerline-raw "%*" face0 'l)
                                     (when powerline-display-buffer-size
                                       (powerline-buffer-size face0 'l))
                                     (when powerline-display-mule-info
                                       (powerline-raw mode-line-mule-info face0 'l))
                                     (powerline-buffer-id `(mode-line-buffer-id ,face0) 'l)
                                     (when (and (boundp 'which-func-mode) which-func-mode)
                                       (powerline-raw which-func-format face0 'l))
                                     (powerline-raw " " face0)
                                     (funcall separator-left face0 face1)
                                     (when (and (boundp 'erc-track-minor-mode) erc-track-minor-mode)
                                       (powerline-raw erc-modified-channels-object face1 'l))
                                     (powerline-major-mode face1 'l)
                                     (powerline-process face1)
                                     (powerline-minor-modes face1 'l)
                                     (powerline-narrow face1 'l)
                                     (powerline-raw (cond
						    ((eq evil-state 'visual) " VISUAL ")
			  			    ((eq evil-state 'normal) " NORMAL ")
			  			    ((eq evil-state 'insert) " INSERT ")
			  			    ((eq evil-state 'replace) " REPLACE ")
			  			    ((eq evil-state 'operator) " OPERATOR ")
			  			    ((eq evil-state 'motion) " MOTION ")
			  			    ((eq evil-state 'emacs) " EMACS ")
			  			    (t 'powerline-active2))
						    face1)
                                     (funcall separator-left face1 face2)
                                     (powerline-vc face2 'r)
                                     (when (bound-and-true-p nyan-mode)
                                       (powerline-raw (list (nyan-create)) face2 'l))))
                          (rhs (list (powerline-raw global-mode-string face2 'r)
                                     (funcall separator-right face2 face1)
                                     ;; (unless window-system
                                     ;;  (powerline-raw (char-to-string #xe0a1) face1 'l))
                                     (powerline-raw "%4l" face1 'l)
                                     (powerline-raw ":" face1 'l)
                                     (powerline-raw "%3c" face1 'r)
                                     (funcall separator-right face1 face0)
                                     (powerline-raw " " face0)
                                     (powerline-raw "%6p" face0 'r)
                                     (when powerline-display-hud
                                       (powerline-hud face0 face2))
                                     (powerline-fill face0 0)
                                     )))
                     (concat (powerline-render lhs)
                             (powerline-fill face2 (powerline-width rhs))
                             (powerline-render rhs)))))))

(powerline-custom-theme)
;; Auto generated crap goes after this point

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   '(evil-collection evil-mc realgud moe-theme powerline-evil powerline chess jupyter dyalog-mode company-web license-snippets yasnippet helm-perldoc helm magit try "try" evil-visual-mark-mode "try" emojify emojifiy hl-todo mastodon neotree evil)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

