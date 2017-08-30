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
(setq inhibit-startup-screen t)
(setq initial-scratch-message "")
(use-package zerodark-theme
  :demand t
  :config
  (progn
    (defun set-selected-frame-dark ()
      (interactive)
      (let ((frame-name (cdr (assq 'name (frame-parameters (selected-frame))))))
	(call-process-shell-command
	 (format
	  "xprop -f _GTK_THEME_VARIANT 8u -set _GTK_THEME_VARIANT 'dark' -name '%s'"
	  frame-name))))

    (when (window-system)
      (load-theme 'zerodark t)
      (zerodark-setup-modeline-format)
      (set-selected-frame-dark)
      (setq frame-title-format '(buffer-file-name "%f" ("%b"))))))

(temp-buffer-resize-mode)

;; Editor
(use-package autorevert
  :diminish auto-revert-mode
  :config
  (global-auto-revert-mode 1)
  (setq auto-revert-verbose nil))
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
  :bind (("M-x" . counsel-M-x)
	 ("C-x x" . counsel-M-x)
	 ("C-x C-f" . counsel-find-file)
	 ("C-x d" . counsel-dired-jump)
	 ("C-s" . swiper)
	 ("C-h a" . counsel-apropos)
	 ("C-h f" . counsel-describe-function)
	 ("C-h v" . counsel-describe-variable)
	 ("C-h i" . counsel-info-lookup-symbol)
	 ("C-c g" . counsel-git)
	 ("M-i" . counsel-imenu)
	 ("M-y" . counsel-yank-pop)))

;; Magit
(use-package magit
  :bind (("C-x g" . magit-status)
	 ("C-x M-g" . magit-dispatch-popup)))

;; Org
(use-package org
  :bind ("C-c a" . org-agenda)
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
	     (quote ((sequence "NEXT(n)" "TODO(t)" "|" "DONE(d)")
		     (sequence "TO-DO" "IN-PROGRESS" "|" "DONE")
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
