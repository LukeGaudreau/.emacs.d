;; Packages
(require 'package)
(setq package-archives
      '(("melpa" . "https://melpa.org/packages/")
	("gnu" . "http://elpa.gnu.org/packages/")))
(package-initialize)

(if (not (package-installed-p 'use-package))
    (progn
      (package-refresh-contents)
      (package-install 'use-package)))

(require 'use-package)
(setq use-package-always-ensure t)
(setq use-package-enable-imenu-support t)

;; Custom
(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
(when (file-exists-p custom-file)
  (load custom-file))

;; Appearance
(menu-bar-mode -1)
(tool-bar-mode -1)
(scroll-bar-mode -1)
(setq inhibit-startup-screen t)
(setq initial-scratch-message "")
(use-package zerodark-theme
  :demand t
  :config
  (progn
    (load-theme 'zerodark t)
    (zerodark-setup-modeline-format)
    (defun set-selected-frame-dark ()
      (interactive)
      (let ((frame-name (cdr (assq 'name (frame-parameters (selected-frame))))))
	(call-process-shell-command
	 (format
	  "xprop -f _GTK_THEME_VARIANT 8u -set _GTK_THEME_VARIANT 'dark' -name '%s'"
	  frame-name))))

    (when (window-system)
      (set-selected-frame-dark)
      (setq frame-title-format '(buffer-file-name "%f" ("%b"))))))

(temp-buffer-resize-mode)

(use-package frames-only-mode
  :config
  (frames-only-mode))

;; Evil
(use-package evil
  :config
  (use-package evil-leader)
  (setq evil-leader/in-all-states 1)
  (evil-leader/set-leader "<SPC>")
  (global-evil-leader-mode)
  (evil-mode 1))

;; Editor
(use-package autorevert
  :diminish auto-revert-mode
  :config
  (global-auto-revert-mode 1)
  (setq auto-revert-verbose nil))

(setq backup-directory-alist `(("." . "~/.emacs.d/backups")))
(setq backup-by-copying t)
(setq delete-old-versions t
  kept-new-versions 6
  kept-old-versions 2
  version-control t)
(setq auto-save-file-name-transforms '((".*" "~/.emacs.d/autosaves/\\1" t)))

(blink-cursor-mode -1)
(use-package expand-region
  :bind (("C-=" . er/expand-region)))

;; Lisp
(show-paren-mode 1)

;; Ivy
(use-package ivy
  :diminish ""
  :demand t
  :config (progn
	    (ivy-mode 1)
	    (setq ivy-use-virtual-buffers t)
	    (setq enable-recursive-minibuffers t)))

(use-package counsel
  :demand t
  :config (progn
	    (evil-leader/set-key
	      "<SPC>" 'counsel-M-x
	      "f" 'counsel-find-file
	      "b" 'ivy-switch-buffer
	      "k" 'kill-this-buffer
	      "K" 'kill-buffer
	      "s" 'save-buffer
	      "d" 'counsel-dired-jump
	      "/" 'swiper
	      "ha" 'counsel-apropos
	      "hf" 'counsel-describe-function
	      "hv" 'counsel-describe-variable
	      "hk" 'describe-key
	      "hi" 'counsel-info-lookup-symbol)))

;; Projectile
(use-package projectile
  :demand t
  :diminish ""
  :config (progn
	    (projectile-mode)
	    (setq projectile-find-dir-includes-top-level t)
	    (use-package counsel-projectile
	      :config (progn
			(counsel-projectile-on))))
  :config (progn
	    (evil-leader/set-key
	      "pd" 'projectile-find-dir
	      "pp" 'projectile-switch-project
	      "pf" 'projectile-find-file
	      "pb" 'projectile-switch-to-buffer)))

;; Magit
(use-package magit
  :config (progn
	    (evil-leader/set-key
	      "g" 'magit-status
	      "G" 'magit-dispatch-popup)))

;; Org
(use-package org
  :commands org-refile
  :config (progn
	    (evil-leader/set-key
	      "a" 'org-agenda
	      "c" 'org-capture
	      "o" 'my-org-jump))
  :init
  (defun my-org-jump ()
    (interactive)
    (org-refile '(GOTO)))
  :config
  (progn
    (setq org-file-apps '((auto-mode . emacs)
			  ("\\.mm\\'" . "xdg-open %s")
			  ("\\.x?html?\\'" . "xdg-open %s")
			  ("\\.pdf\\'" . "xdg-open %s")))
    (setq org-startup-indented t
	  org-fontify-done-headline t
	  org-blank-before-new-entry nil
	  org-separator-lines 0
	  org-refile-use-outline-path 'file)
    (add-hook 'org-mode-hook (lambda ()
			       (visual-line-mode t)
			       (setq line-spacing 0.5)
			       ))
    ;; Capture
    (setq org-capture-templates '(("t" "Todo" entry (file "~/notes/Inbox.org")
				   "* TODO %?\n%U\n%a\n" :clock-in t :clock-resume t)
				  ("r" "Respond" entry (file "~/notes/Inbox.org")
				   "* NEXT Respond to %:from on%:subject\nSCHEDULED: %t\n%U\n%a\n" :clock-in t :clock-resume t :immediate-finish t)
				  ("c" "Conversation" entry (file "~/notes/Inbox.org")
				   "* CONVERSATION with %? :Conversation:\n%U" :clock-in t :clock-resume t)))
    (setq org-refile-targets '((org-agenda-files :maxlevel . 4)))
    (setq org-refile-use-outline-path 'file)
    (setq org-outline-path-complete-in-steps nil))

  ;; After
  (eval-after-load "org"
    '(progn
       (setq org-todo-keywords
	     (quote ((sequence "NEXT(n)" "TODO(t)" "WAITING(w)" "|" "DONE(d)")
		     (sequence "TO-DO(T)" "IN-PROGRESS(P)" "|" "DONE(D)")
		     (sequence "CONVERSATION(c@)""|" "CANCELED(k@)" "SOMEDAY(s@)"))))
       (setq org-use-fast-todo-selection t
	     org-use-fast-tag-selection t)
       ;; Org Tags
       (setq org-tag-persistent-alist
	     '(("Primo" . ?p)
	       ("ILLiad" . ?i)
	       ("OJS" . ?o)
	       ("EZProxy" . ?z)
	       ("Website" . ?w)
	       ))
       (setq org-complete-tags-always-offer-all-agenda-tags t)
       (setq org-bullets-bullet-list '("•"))
       (setq org-agenda-block-separator 32)
       (setq org-tags-column -0)
       (setq org-agenda-tags-column -100)
       ))
      (eval-after-load "org-agenda"
      '(progn
	 (define-key org-agenda-mode-map (kbd "RET") 'org-agenda-goto)
	 (define-key org-agenda-mode-map (kbd "TAB") 'org-agenda-switch-to)
	 (advice-add 'org-agenda-goto :after
		     (lambda (&rest args)
		       (org-narrow-to-subtree)))

	 ;; Hide categories from Agenda
	 (setq org-agenda-prefix-format '((agenda . "%?-12t% s")
					  (timeline . "  % s")
					  (todo . "")
					  (tags . "")
					  (search . "")))
	 (setq org-agenda-scheduled-leaders '("" "")
	       org-agenda-deadline-leaders '("Due:  " "Due in %3d d.: " "%2d d. Overdue: "))
	 (setq org-agenda-timegrid-use-ampm t
	       org-agenda-window-setup 'only-window
	       org-deadline-warning-days 0)
	 ;; Don't show tags used to generate the agenda. They are implicit in the view.
	 (setq org-agenda-hide-tags-regexp "Project\\|Inbox\\|LIB*")
	 (setq org-stuck-projects '("+LEVEL=1-Inbox-Support-Jira" ("NEXT") nil nil))
	 (setq org-agenda-custom-commands
	       (quote (("." "Daily"
			((agenda "/-DONE"
				 ((org-agenda-overriding-header "Daily Agenda")
				  (org-agenda-entry-types '(:timestamp :deadline :scheduled))
				  (org-agenda-skip-deadline-if-done t)
				  (org-agenda-skip-scheduled-if-done t)
				  (org-agenda-span 1)
				  ))
			 (tags "Inbox/-DONE"
			       ((org-agenda-overriding-header "Inbox")
				(org-agenda-ignore-scheduled t)
				(org-agenda-tags-todo-honor-ignore-options t)
				(org-agenda-todo-ignore-deadlines t)
				(org-agenda-todo-ignore-scheduled t)
				(org-tags-match-list-sublevels nil)
				))
			 (todo "NEXT|IN\-PROGRESS"
			       ((org-agenda-overriding-header "Doing")
				(org-agenda-ignore-scheduled t)
				(org-agenda-ignore-deadlines nil)
				(org-agenda-todo-ignore-deadlines nil)
				(org-agenda-todo-ignore-scheduled t)
				(org-agenda-tags-todo-honor-ignore-options t)
				))
			 (todo "WAITING"
			       ((org-agenda-overriding-header "Waiting")))
			 (stuck ""
				((org-agenda-overriding-header "Stuck Projects")))
			 ))
		       ("j" tags "+LEVEL=1+Jira")
		       ("P" tags "Project")
		       ))))))
;; Evil Org
(use-package evil-org
  :ensure t
  :after org
  :config
  (add-hook 'org-mode-hook 'evil-org-mode)
  (add-hook 'evil-org-mode-hook
	    (lambda ()
	      (evil-org-set-key-theme))))

;; Jira
(use-package org-jira
  :config
  (setq jiralib-url "https://itsjira.bc.edu:8883"
	org-jira-use-status-as-todo t
	org-jira-property-overrides (list (cons "labels" "labels"))))


;; Web
(use-package web-mode
  :config
  (add-to-list 'auto-mode-alist '("\\.html?\\'" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.phtml\\'" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.php\\.tpl\\'" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.mustache\\'" . web-mode))
  )

;; Javascript
(use-package js2-mode
  :config
  (add-to-list 'auto-mode-alist '("\\.js\\'" . js2-mode))
  ;; Better imenu
  (add-hook 'js2-mode-hook #'js2-imenu-extras-mode))

;; Rainbows
(use-package rainbow-mode)

;; Vagrant Tramp
(use-package vagrant-tramp)

;; Gulp
(use-package gulp-task-runner)
