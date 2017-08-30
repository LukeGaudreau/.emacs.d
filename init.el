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
