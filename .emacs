;;;; mcneb10's .emacs file
;; Read at your own risk


;; Make emacs work with melpa
(require 'package)
(add-to-list 'package-archives
	     '("Melpa" . "https://melpa.org/packages/"))

(package-initialize)

(unless package-archive-contents
  (package-refresh-contents))

; Install Evil
(unless (package-installed-p 'evil)
  (package-install 'evil))

;; Enable Evil
(setq evil-want-keybinding nil)
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
(add-hook 'buffer-list-update-hook (lambda ()
				       (when buffer-read-only
					 (company-mode -1))))

(setq company-minimum-prefix-length 1
      company-idle-delay 0.0) ;; default is 0.2

;; Some helper method(s)

(defun download-file (url filename-or-path)
  "Download a file to a specific location using cURL"
  (shell-command (concat "curl -Lso \"" filename-or-path "\" \"" url "\"")))

(defun package-idiom (name &optional require)
  "Download a package if it hasn't already been installed"
  (unless (package-installed-p name)
    (package-install name))
  (if require
      (require name)))

(defun package-from-el-link (url &optional require)
  "Download a package form a URL of a .el file if it hasn't been already
   and require if requested"
  (let
      ((el-filename (expand-file-name (file-name-nondirectory url)))
     (el-dirname (expand-file-name (concat package-user-dir "/" (file-name-base url))))
     (el-fullpath (expand-file-name (concat package-user-dir "/" (file-name-base url) "/" (file-name-nondirectory url)))))
    (unless (file-directory-p el-dirname)
      (make-directory el-dirname))
    (unless (file-exists-p el-fullpath)
      (download-file url el-fullpath))
    (when require
      (add-to-list 'load-path el-dirname)
      (require (intern (file-name-base url))))))

(defun new-empty-tab ()
  "Make a new tab with an empty buffer"
  (interactive)
  (display-buffer-in-new-tab (generate-new-buffer "*new-tab*") nil))

(defun messages ()
  "Open messages window in new tab"
  (interactive)
  (display-buffer-in-new-tab (messages-buffer) nil))

;; taken from https://stackoverflow.com/questions/6931909/finding-the-emacs-site-lisp-directory

(defconst my-lisp-dir (cond
    ;; ((equal system-type 'gnu/linux) "/usr/share/emacs/site-lisp/")
    ((equal system-type 'darwin) (concat "/usr/local/Cellar/emacs/" (number-to-string emacs-major-version) "." (number-to-string emacs-minor-version) "/share/emacs/site-lisp/"))
    (t (concat "/usr/local/emacs/site-lisp/"))))

;;(defun perl-repl ()
;;  (interactive)
;;  (term ))
;; Install neotree
(package-idiom 'neotree t)
;; Neotree config
(setq neo-smart-open t)

;; STOP INDENTING MY PERL SUBS TO THE MIDDLE OF THE LINE!!!
(add-hook 'perl-mode-hook (lambda ()
			    (setq perl-indent-level 4)))

;; TODO:

;; Make new tab button make a new empty tab instead
;; of making a tab with using the buffer that was open

;; Install theme
(package-idiom 'moe-theme t)
;; Choose a specific theme based on time if not running under terminal
(when (display-graphic-p)
  (run-with-timer 60 t
	      (lambda ()
		(if (or (>= (string-to-number (format-time-string "%H")) 20) (<= (string-to-number (format-time-string "%H")) 6))
		    (moe-dark) (moe-light)))))

;; Old vim habits die hard
;; (evil-ex-define-cmd "o[pen]" (lambda (path)
;; 			       "Open a file like vim"
;; 			      (find-file (expand-file-name path))))

(evil-ex-define-cmd "o[pen]" 'evil-edit)

;; TODO: fix
;; (evil-ex-define-cmd "open-new-tab" (lambda (path)
;; 				     (interactive "f")
;; 				     (find-file-other-tab (expand-file-name path))))

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

(setq perlnavigator-path (expand-file-name "~/building/PerlNavigator/server/out/server.js"))

(lsp-register-client
 (make-lsp-client :new-connection
		  (lsp-stdio-connection `("node" ,perlnavigator-path "--stdio"))
		  :activation-fn (lsp-activate-on "perl")
		  :major-modes '(cperl-mode perl-mode)
		  :priority 10
		  :server-id 'perl-ls))

(add-hook 'perl-mode-hook 'lsp)
(add-hook 'emacs-lisp-mode-hook (lambda () (prettify-symbols-mode)))

;; Install which-key

(unless (package-installed-p 'which-key)
  (package-install 'which-key))

(require 'which-key)
(which-key-mode)

;; MOAR PACKAGES

(package-idiom 'try)
(package-idiom 'magit)
;; I don't know why, but I have a sneaking suspicion helm is causing my emacs performance problems
;; Update: it was
;; (package-idiom 'helm)
;; (helm-mode 1)
;; (helm-autoresize-mode 1)
;; (package-idiom 'helm-perldoc)
(package-idiom 'yasnippet)
(package-idiom 'license-snippets)
(package-idiom 'company-web)
(package-idiom 'dyalog-mode)
(package-idiom 'jupyter)
(package-idiom 'ein t)
(package-idiom 'chess)
(package-idiom 'markdown-mode)
(package-idiom 'powerline t)
(package-idiom 'powerline-evil t)
(package-idiom 'restclient t)
;; realgud stuff
(package-idiom 'realgud t)
;; Install lldb support for realgud if on Mac OS
(when (eq system-type 'darwin)
  (package-idiom 'realgud-lldb t))
;; continue MOAR PACKAGES
(package-idiom 'evil-mc t) ;; TODO: figure out how this package works
(package-idiom 'evil-collection)
(evil-collection-init)
(package-idiom 'all-the-icons)
;; (obsidian-specify-path "")
(package-idiom 'obsidian t)
(package-idiom 'org-superstar)
(add-hook 'org-mode-hook 'org-superstar-mode)
(package-idiom 'htmlize)
(package-idiom 'system-packages t)
(package-idiom 'dyalog-mode)
;; multimedia stuff
(package-idiom 'eradio)
(system-packages-ensure "mpv")
(setq eradio-player :mpv)
(setq eradio-channels '(("Chiptune Station - Krelez" "http://79.120.11.40:8000/chiptune.ogg.m3u")
			("Vaporwave Station - Krelez" "http://79.120.11.40:8000/vapor.ogg.m3u")
			("Vaporwave Station - Soma FM" "https://somafm.com/vaporwaves64.pls")
			("La Mejor 99.9 FM Puerto Vallarta - streamtheworld" "https://provisioning.streamtheworld.com/pls/XHCJX.pls")
			("Classical Station 98.7 WFMT" "https://wfmt.com/global/stream/wfmtstream.pls")
			("Classical Station KUSC - streamtheworld" "https://provisioning.streamtheworld.com/pls/KUSC.pls")
			("140.3 JAMS WBMX" "https://playerservices.streamtheworld.com/pls/WBMXFM.pls")
			("101.1 WKQX" "https://playerservices.streamtheworld.com/pls/WKQXFM.pls")))

;; url to get xml of current or passed songs
;; https://np.tritondigital.com/public/nowplaying?mountName=<STATION>&eventType=track&numberToFetch=<HOW MANY TO FETCH>

(package-idiom 'emms)
;; zone stuff
(package-idiom 'fireplace)
(package-idiom 'zone-rainbow)
(package-idiom 'clippy)
;; org stuff
(package-idiom 'org-roam)
(package-idiom 'org-noter)
(package-idiom 'org-modern t)
(package-idiom 'gnuplot)
(package-from-el-link "https://raw.githubusercontent.com/alphapapa/org-graph-view/master/org-graph-view.el" t)
;; (add-to-list 'org-latex-classes '("per-file-class" "\\usepackage{chemfig}\n\\usepackage{mhchem}"))

(add-hook 'org-mode-hook #'org-modern-mode)
(add-hook 'org-agenda-finalize-hook #'org-modern-agenda)
(setq org-todo-keywords
  '((sequence "TODO" "DONE" "IN-PROGRESS" "WAITING" )))
(org-babel-do-load-languages
 'org-babel-load-languages
 '((gnuplot . t)
   (dot . t)))
;; install memacs if possible

(defun install-memacs (pip-executable-name)
  (unless (eq (shell-command (concat pip-executable-name " show memacs")) 0)
    (async-shell-command (concat pip-executable-name " install memacs\\[all\\]"))))

;; check if pip installed
(cond
 ((executable-find "pip3") (install-memacs "pip3"))
 ((executable-find "pip") (install-memacs "pip")))

;; install latex stuff
(unless (executable-find "pdflatex")
  (system-packages-install "basictex"))

;; Install latex addons
(when (executable-find "tlmgr")
  (unless (executable-find "dvipng") ;; we can easily check if dvipng is installed, but not wrapfig
    (async-shell-command "tlmgr install dvipng wrapfig mhchem chemfig"))) ;; make inline latex previews and latex export to pdf work

(add-to-list 'org-latex-packages-alist '("" "chemfig" t))
(add-to-list 'org-latex-packages-alist '("" "mhchem" t))

;; from https://gitlab.com/jabranham/system-packages/-/issues/29

(add-to-list 'system-packages-supported-package-managers
     '(choco .
             ((default-sudo . t)
              (install . "choco install")
              (search . "choco search")
              (uninstall . "choco uninstall")
              (update . "choco upgrade")
              (clean-cache . "choco optimize")
              (log . "type C:\\ProgramData\\chocolatey\\logs\\chocolatey.log")
              (get-info . "choco info --local-only")
              (get-info-remote . "choco info")
              (list-files-provided-by . nil)
              (verify-all-packages . nil)
              (verify-all-dependencies . nil)
              (remove-orphaned . nil)
              (list-installed-packages . "choco list --local-only")
              (list-installed-packages-all . "choco list --local-only --include-programs")
              (list-dependencies-of . nil)
              (noconfirm . "-y"))))


(when (display-graphic-p)
  (require 'all-the-icons))

;; Install mu via package manager for mu4e if on Unix like os
;; Otherwise figure out what to do on windows

;; TODO: finish
(defun install-mu4e ()
  (if (or (system-packages-ensure "mu") (system-packages-ensure "maildir-utils")) ;; mu may be called mu or maildir-utils
      (progn
	(add-to-list 'load-path my-lisp-dir)
	(require 'mu4e))
      (message "Failed to install mu on your platform. Trying mew instead")
      (install-mew)))
  
  (defun install-mew ()
    (package-idiom 'mew)
    (system-packages-ensure "gnupg")
      (setq read-mail-command 'mew))

;; (if (eq system-type 'windows)
;;     (progn
;;       (message "Running on windows, installing mew instead of mu4e")
;;       (install-mew))
;;   (install-mu4e))

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

;; Tell emacs SVG is a valid image format

(add-to-list 'image-type 'svg)

;; Powerline theme

(defface powerline-active2-visual '((t (:background "darkmagenta" :foreground "white" :inherit mode-line)))
  "Powerline face 2 visual mode."
  :group 'powerline)

(defface powerline-active2-normal '((t (:background "grey40" :foreground "white" :inherit mode-line)))
  "Powerline face 2 normal mode."
  :group 'powerline)

(defface powerline-active2-insert '((t (:background "darkgoldenrod1" :foreground "white" :inherit mode-line)))
  "Powerline face 2 insert mode."
  :group 'powerline)

(defface powerline-active2-replace '((t (:background "firebrick1" :foreground "white" :inherit mode-line)))
  "Powerline face 2 replace mode."
  :group 'powerline)

(defface powerline-active2-operator '((t (:background "sienna1" :foreground "white" :inherit mode-line)))
  "Powerline face 2 operator mode."
  :group 'powerline)

(defface powerline-active2-motion '((t (:background "darkolivegreen2" :foreground "white" :inherit mode-line)))
  "Powerline face 2 motion? mode."
  :group 'powerline)

(defface powerline-active2-emacs '((t (:background "salmon1" :foreground "white" :inherit mode-line)))
  "Powerline face 2 emacs mode."
  :group 'powerline)

(defun powerline-custom-theme ()
  "Setup the default mode-line but without that stupid LN symbol and a vim mode indicator and color coding. Stolen from powerline-themes.el"
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
                             (powerline-fill face2 (powerline-width rhs)) ;; Fix: dont make background of page view indicator the same as the color mode indicator bar
                             (powerline-render rhs)))))))

(powerline-custom-theme)

;; useful for debugging purpose
(message ".emacs init complete!")

;; Auto generated crap goes after this point

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(helm-minibuffer-history-key "M-p")
 '(package-selected-packages
   '(svg-lib svg-clock emms clippy zone-rainbow fireplace eradio org-babel-eval-in-repl org-babel gnuplot org-roam-bibtex org-roam htmlize org-superstar obsidian circe system-packages helm-system-packages all-the-icons realgud-lldb evil-collection evil-mc realgud moe-theme powerline-evil powerline chess jupyter dyalog-mode company-web license-snippets yasnippet helm-perldoc helm magit try "try" evil-visual-mark-mode "try" emojify emojifiy hl-todo mastodon neotree evil)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

